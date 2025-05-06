#devtools::install_github("ropensci/RSelenium")
#install.packages("RSelenium")
# require(RSelenium)
# require(magrittr)
# 
# rD <- RSelenium::rsDriver(browser = "firefox", 
#                           #chromever = "103.0.5060.134", 
#                           port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), 
#                           #phantomver = "2.1.1",
#                           check = FALSE, verbose = FALSE)
# 
# remDr <- rD[["client"]]

#binman::list_versions("phantomjs")

require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

pjs_session$go("https://www.tagesspiegel.de/politik/archiv/2023/07/08/")


#function for geting links from page
tagesspiegel_get_links <- function(html, datethru){
  
  #html <- remDr$getPageSource()[[1]]
  html <- pjs_session$getSource()
  
  pjs_session$getUrl()
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article//h2") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article//a[contains(@class, 'tspBDh2')]") %>% 
    rvest::html_attr("href") %>% paste0("https://www.tagesspiegel.de",.)-> item_link
  
  datethru -> item_pubdate
  # rvest::read_html(html) %>% 
  #   rvest::html_elements(xpath = "//main//h1") %>%
  #   rvest::html_text(trim = TRUE)
  #   stringr::str_extract(pattern = "[0-9]{1,2}.[0-9]{1,2}.[0-9]{1,4}")%>%
  #   lubridate::dmy()-> item_pubdate
  # 
  # if(length(item_pubdate) == 0){
  #   pjs_session$go(pjs_session$getUrl())
  # }
  # 
  # if(length(item_pubdate)==0){
  #   print("wait")
  #   url <- pjs_session$getUrl()
  #   Sys.sleep(600)
  #   pjs_session$go(url)
  # 
  #   html <- pjs_session$getSource()
  #   rvest::read_html(html) %>%
  #     rvest::html_elements(xpath = "//time") %>%
  #     rvest::html_attr("datetime") %>%
  #     lubridate::ymd_hms()-> item_pubdate
  # 
  # }
  
  # rvest::read_html(html) %>% 
  #   rvest::html_elements(xpath = "//main//p[contains(@class, 'Gaw')]") %>%
  #   rvest::html_text(trim = TRUE) -> item_empty
  # 
  if(length(item_title) == 0){
 
  df <- data.frame()

  } else {
    df <- data.frame(item_title, item_link, item_pubdate)
  }
  

  return(df)
}

tagesspiegel_get_url <- function(datethru, sub){
  # remDr$navigate(url)
  # print(remDr$getCurrentUrl())
  # remDr$getPageSource()[[1]] %>% tagesspiegel_get_links() -> df
  
  datethru %>% 
    format.Date(format="/%Y/%m/%d/") %>% 
    paste0("https://www.tagesspiegel.de/", sub, "/archiv", .) -> url
  
  pjs_session$go(url)
  print(pjs_session$getUrl())
  #Sys.sleep(2)
  pjs_session$getSource() %>% tagesspiegel_get_links(., datethru) -> df

  return(df)
}

tagesspiegel_go_thr_archive <- function(sub, startdate){

  seq(as.Date(startdate), Sys.Date(), by="days") -> V1
  
  
  V1 %>%
    purrr::map_df(~tagesspiegel_get_url(., sub)) -> valid_links
  
  return(valid_links)
}


tagesspiegel_go_thr_archive("politik", "2023-01-01") -> valid_links1

tagesspiegel_go_thr_archive("internationales", "2023-01-01") -> valid_links2

tagesspiegel_go_thr_archive("berlin", "2023-01-01") -> valid_links3

tagesspiegel_go_thr_archive("gesellschaft", "2023-01-01") -> valid_links4

tagesspiegel_go_thr_archive("wirtschaft", "2023-01-01") -> valid_links5

tagesspiegel_go_thr_archive("kultur", "2023-01-01") -> valid_links6

tagesspiegel_go_thr_archive("wissen", "2023-01-01") -> valid_links7

tagesspiegel_go_thr_archive("gesundheit", "2023-01-01") -> valid_links8

tagesspiegel_go_thr_archive("sport", "2023-01-01") -> valid_links9

tagesspiegel_go_thr_archive("meinung", "2023-01-01") -> valid_links10

tagesspiegel_go_thr_archive("potsdam", "2023-01-01") -> valid_links11


 # remDr$close()
 # z <- rD$server$stop()

valid_links <- dplyr::distinct(rbind(valid_links1, valid_links10,
                                     valid_links11, valid_links2,
                                     valid_links3, valid_links4,
                                     valid_links5, valid_links6,
                                     valid_links7, valid_links8,
                                     valid_links9))

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Tagesspiegel", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "Tagesspiegel.RDS")
