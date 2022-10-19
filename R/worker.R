#' Pack the job from `work_dl`
#'
#' Pack output from `worker_dl` as a single tar.gz
#' 
#' @param output_file archive
#' @param rds rds file
#' @param output_dir html directory
#' @param delete whether to delete `rds` and `output_dir` after packing
#' @return `output_file`, invisibily.
#' @author Chung-hong Chan
#' @export
#' @importFrom utils tar
pack_work <- function(output_file = "job.tar.gz", rds, output_dir, delete = FALSE) {
    tar(output_file , files = c(rds, output_dir), compression = "gzip")
    if (delete) {
        unlink(rds, recursive = TRUE)
        unlink(output_dir, recursive = TRUE)
    }
    return(invisible(output_file))
}

#' Genrate a random hash
#'
#' This function generates a random hash that can be used as a burner file name
#' @param ending the ending of the hash
#' @param length the length of the random part
#' @return a hash
#' @author Chung-hong Chan
#' @export
generate_hash <- function(ending = ".RDS", length = 40) {
    paste0(stringi::stri_rand_strings(1, length, '[A-Za-z0-1]'), ending)
}

#' Get links (urls) from the DB
#'
#' This function gets links (urls) from the DB where the field `htmlfile` is empty, i.e. not yet scraped.
#'
#' @param fname file name of the RDS file. If null, the function returns a vector of links
#' @param size number of links to fetch
#' @param safe whether to only get links from "safe" sources
#' @param db MongoDB db
#' @param collection Mongodb collection
#' @param unsafe_pubs a vector of publications to avoid
#' @return either a vector of links (if `fname` is null) or the `fname` invisibly
#' @author Chung-hong Chan
#' @export
get_links <- function(fname = NULL, size = 300, safe = FALSE, collection = "articles", db = "main", unsafe_pubs = c("Zeit", "Saarbr\u00fccker Zeitung", "RT deutsch")) {
    con <- mongolite::mongo(collection = collection, db = db)
    if (safe) {
        links <- con$aggregate(paste0('[ {"$match": {"htmlfile": "", "pub" : { "$in": ["Bild", "Tagesschau", "Heute", "Freitag", "T-Online"]}}}, { "$sample": { "size": ', size, '} }]'))$link
    } else {
        links <- con$aggregate(paste0('[ {"$match": {"htmlfile": "", "pub": { "$nin": [', paste0(purrr::map_chr(unsafe_pubs, ~paste0('"', ., '"')), collapse = ", ") , ']}}}, { "$sample": { "size": ', size, '} }]'))$link
    }
    if (is.null(fname)) {
        return(links)
    } else {
        saveRDS(links, fname)
        return(invisible(fname))
    }
}

#' Request some links from host
#'
#' This is a function mostly for workers to request links from the remote host with the DB. It runs [get_links()] remotely and transfer the RDS file locally to the current [tempdir()]. Please make sure you have passwordless access to the remote host, i.e. your public key is in the remote host.
#' @param fname file name of the job, default to a random hash
#' @param host host string in the format of "username@ipaddress", default to the envvar "FODIRA_HOST"
#' @param verbose whether to display messages
#' @param check whether to check for the existence of the file `fname` after the SSH session
#' @return the actual path of the saved RDS file
#' @author Chung-hong Chan
#' @inheritParams get_links
#' @export
request_links <- function(fname = generate_hash(), host = Sys.getenv("FODIRA_HOST"), size = 300, safe = FALSE, verbose = TRUE, check = TRUE) {
    if (host == "") {
        stop("Host can't be empty. If you are using the default, please set the envvar `FODIRA_HOST`.")
    }
    .session <- ssh::ssh_connect(host, verbose = verbose)
    ssh::ssh_exec_wait(.session, glue::glue("Rscript -e 'fodira::get_links(fname = \"{fname}\", size = {size}, safe = {safe})'", fname = fname, size = size, safe = safe))
    current_tempdir <- tempdir()
    ssh::scp_download(.session, files = fname, to = current_tempdir, verbose = verbose)
    ssh::ssh_exec_wait(.session, glue::glue("rm {fname}", fname = fname))
    ssh::ssh_disconnect(.session)
    output_path <- file.path(current_tempdir, fname)
    if (check) {
        stopifnot(file.exists(output_path))
    }
    return(output_path)
}

#' Submit the output from [pack_work()] back to host
#'
#' This is a function mostly for workers to return a job packed with [pack_work()] back to the host. Please make sure you have passwordless access to the remote host, i.e. your public key is in the remote host. By default, it will return to the "finished_job" directory in the host
#' @param job_fname job file name from [pack_work()]
#' @param host_directory path to upload to in the host
#' @return return 0 if everything is alright
#' @author Chung-hong Chan
#' @inheritParams request_links
#' @export
submit_job <- function(job_fname, host = Sys.getenv("FODIRA_HOST"), verbose = FALSE, check = TRUE, delete = TRUE, host_directory = "finished_jobs") {
    if (host == "") {
        stop("Host can't be empty. If you are using the default, please set the envvar `FODIRA_HOST`.")
    }
    if (check) {
        stopifnot(file.exists(job_fname))
    }
    .session <- ssh::ssh_connect(host, verbose = verbose)
    ssh::scp_upload(.session, files = job_fname, to = host_directory, verbose = verbose)
    if (check) {
        stopifnot(ssh::ssh_exec_wait(.session, glue::glue("file {host_directory}/{fname}", host_directory = host_directory, fname = basename(job_fname)), std_out = "/dev/null") == 0)
    }
    ssh::ssh_disconnect(.session)
    if (delete) {
        unlink(job_fname)
    }
    return(0)
}

