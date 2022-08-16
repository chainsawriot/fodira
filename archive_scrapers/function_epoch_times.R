require(RSelenium)

require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

remDr$setTimeout(type = "page load", milliseconds = 10000000)
remDr$setTimeout(type = "script", milliseconds = 10000000)
remDr$setTimeout(type = "implicit", milliseconds = 10000000)

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
epoch_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'col-12 col-md-8 border-r')]//div[contains(@class, 'card-body')]/a") %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'col-12 col-md-8 border-r')]//div[contains(@class, 'card-body')]/a") %>% 
    rvest::html_attr("href")  -> item_link
  
  
  df <- data.frame(item_title, item_link)
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'col-12 col-md-8 border-r')]//div[contains(@class, 'd-flex flex-row align-items-center')]/a") %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'col-12 col-md-8 border-r')]//div[contains(@class, 'd-flex flex-row align-items-center')]/a") %>% 
    rvest::html_attr("href")  -> item_link
  
  df <- rbind(data.frame(item_title, item_link), df)
    return(df)
}


epoch_getlink_url <- function(url){
  remDr$navigate(url)
  print(url)
  df <- epoch_getlink(remDr$getPageSource()[[1]])
  return(df)
}


epoch_go_thr_columns <- function(rubrik, endnr){

  # k <- 0
  valid_links <- data.frame()
  for (i in 1:endnr) {
    paste0("https://www.epochtimes.de/", rubrik, "/page/", i, "/") %>%
      purrr::map_df(~epoch_getlink_url(.)) -> subset_links
    valid_links <- rbind(valid_links, subset_links)
    # k=k+1
    # if (k > 50){
    #   print("wait")
    #   Sys.sleep(90)
    #   k <- 0
    # }
  }

  return(valid_links)
}


c("politik", "wirtschaft", "gesundheit", 
  "meinung", "china", "feuilleton") %>% 
  purrr::map_dfr(~epoch_go_thr_columns(., endnr = 400)) -> valid_links

valid_links <- dplyr::distinct(valid_links)

valid_links %>% dplyr::rename(title = item_title, link = item_link) %>% 
  dplyr::mutate(pub = "Epoch Times", description = NA, pubdate = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

valid_links$pub <- "Epoch Times"

saveRDS(valid_links, "Epoch Times.RDS")

remDr$close()
z <- rD$server$stop()

