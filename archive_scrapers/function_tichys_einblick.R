# 
# require(webdriver)
# require(magrittr)
# pjs_instance <- run_phantomjs()
# pjs_session <- Session$new(port = pjs_instance$port)
# 

require(RSelenium)
require(magrittr)
#eCap <- list(phantomjs.binary.path = "C:/phantomjs-2.1.1/bin.exe")
#fprof <- makeFirefoxProfile(list(permissions.default.image = 21))
rD <- RSelenium::rsDriver(browser = "firefox", 
                          #chromever = "103.0.5060.134", 
                          port = sample(c(5678L, 
                                          5679L, 
                                          5680L, 
                                          5681L, 
                                          5682L
                          ), size = 1), 
                          #phantomver = "2.1.1",
                          #extraCapabilities = fprof,
                          check = FALSE, verbose = FALSE)

remDr <- rD[["client"]]



#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
tichy_getlink <- function(html){

  html <- remDr$getPageSource()[[1]]
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'entry-content')]//div[contains(@class, 'category-content-title')]/a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'entry-content')]//div[contains(@class, 'category-content-title')]/a") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'entry-content')]//span[contains(@class, 'date')]") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    stringr::str_replace(., "März", "March") %>%
    lubridate::dmy() -> item_pubdate
    
    df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}

tichy_getlink_url <- function(url){
  remDr$navigate(url)
  print(url)
  return(tichy_getlink(remDr$getPageSource()[[1]]))
}

tichy_go_thr_columns <- function(rubrik, startdate){
  i <- 1
  j <- 1
  valid_links <- data.frame()
  while (i > 0) {
    tichy_getlink_url(paste0("https://www.tichyseinblick.de/", rubrik, "/page/", j, "/")) %>% 
      subset(item_pubdate>=as.Date(startdate)) -> subset_links
    i <- nrow(subset_links)
    j <- j + 1
    valid_links <- rbind(valid_links, subset_links)
    Sys.sleep(sample(150:500/100, 1))
  }
  return(valid_links)
}


c("tichys-einblick", "kolumnen", "gastbeitrag", "daili-es-sentials", 
  "meinungen", "feuilleton", "wirtschaft") %>% 
  purrr::map_dfr(~tichy_go_thr_columns(., startdate = "2023-01-01")) -> valid_links

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Tichys Einblick", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "TichysEinblick.RDS")



remDr$close()
z <- rD$server$stop()

