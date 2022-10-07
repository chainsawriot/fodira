#' Pack the job from `work_dl`
#'
#' Pack the job from `work_dl` as a single tar.gz
#' 
#' @param output_file archive
#' @param rds rds file
#' @param output_dir html directory
#' @param delete whether to delete `rds` and `output_dir` after packing
#' @return `output_file`, invisibily.
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
