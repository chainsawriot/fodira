require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(#5678L, 
                                                               5679L, 
                                                               5680L, 
                                                               #5681L, 
                                                               5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

################################################################################
### install javascript-blocker manually!!     ##################################
################################################################################


# require(webdriver)
# require(magrittr)
# pjs_instance <- run_phantomjs()
# pjs_session <- Session$new(port = pjs_instance$port)

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#pjs_session$go("https://www.badische-zeitung.de/archiv/2022/01/03")




#function for geting links from page
nordkur_getlink <- function(html){
  #html <- pjs_session$getUrl()
  
  remDr$getPageSource()[[1]] -> html 
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = paste0("//div[contains(@class, 'view-content')]", 
                                        "//div[contains(@class, 'row')]/div/a")) %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = paste0("//div[contains(@class, 'view-content')]", 
                                        "//div[contains(@class, 'row')]/div/a")) %>% 
    rvest::html_attr("href") %>% paste0("https://www.nordkurier.de",.) -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = paste0("//div[contains(@class, 'view-content')]",
                                        "//div[contains(@class, 'formatted-time')]")) %>% 
    rvest::html_text(., trim = TRUE) %>%
    stringr::str_replace("vor [0-9]+ Stunde", format(Sys.Date(), "%d.%m.%Y")) %>%
    stringr::str_replace("vor [0-9]+ Minute", format(Sys.Date(), "%d.%m.%Y")) %>%
    stringr::str_extract("[0-9]+\\.[0-9]+\\.[0-9]+") %>% 
    lubridate::dmy() -> item_pubdate
  df <- data.frame(item_title[!is.na(item_title)], item_link[!is.na(item_title)], item_pubdate)
    
  return(df)
}

#pjs_session$go("https://www.sueddeutsche.de/archiv/m%C3%BCnchen/2022/01")

#pjs_session$getUrl()

nordkur_getlink_url <- function(url){
  remDr$navigate(url)
  print(url)
  df <- nordkur_getlink(remDr$getPageSource()[[1]])
  # print(1)
  # pjs_session$go(url)
  # print(pjs_session$getUrl())
  # df <- bad_z_getlink(pjs_session$getSource())
  print(nrow(df))
  # 
  # remDr$getPageSource()[[1]] %>% rvest::read_html(html) %>% 
  #   rvest::html_elements(xpath = "//li[contains(@class, 'navigation')]//li[last()]") %>%
  #   rvest::html_text(., trim = TRUE) -> n
  # 
  
  return(df)
}


nordkur_go_thr_archive <- function(startdate, rubrik, startn, endn){
  n <- 1
  df <- data.frame()
  i <- startn
  while (i <= endn) {

    paste0("https://www.nordkurier.de/", rubrik, "?page=", i) %>%
      nordkur_getlink_url() %>% subset(., item_pubdate >= as.Date(startdate))-> df2
    nrow(df2) -> n
    print(n)
    if(n == 0){
      i <- endn + 1
    }
    
    df <- rbind(df, df2)
    i <- i+ 1
  }
  
  return(df)
}

c("nachrichten", "ratgeber", "meine-region") %>%
  purrr::map_df(~nordkur_go_thr_archive(startdate = "2021-12-01", ., startn = 1, endn = 500)) -> valid_links1

c("nachrichten", "ratgeber", "meine-region") %>%
  purrr::map_df(~nordkur_go_thr_archive(startdate = "2021-12-01", ., startn = 501, endn = 1000)) -> valid_links2

c("nachrichten", "ratgeber", "meine-region") %>%
  purrr::map_df(~nordkur_go_thr_archive(startdate = "2021-12-01", ., startn = 1001, endn = 1500)) -> valid_links3

c("nachrichten", "ratgeber", "meine-region") %>%
  purrr::map_df(~nordkur_go_thr_archive(startdate = "2021-12-01", ., startn = 1501, endn = 2500)) -> valid_links4

valid_links <- rbind(valid_links1, valid_links2, valid_links3, valid_links4)

#saveRDS(valid_links, "nordkurier1.RDS")

valid_links %>% dplyr::rename(title = item_title..is.na.item_title.., link = item_link..is.na.item_title.., pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Nordkurier", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

valid_links <- dplyr::distinct(valid_links)

saveRDS(valid_links, "Nordkurier.RDS")
remDr$close()
z <- rD$server$stop()
