#devtools::install_github("ropensci/RSelenium")
#install.packages("RSelenium")
require(RSelenium)
require(magrittr)

rD <- RSelenium::rsDriver(browser = "firefox",
                          #chromever = "103.0.5060.134",
                          port = sample(c(#5678L, 
                            5679L, #5680L, 
                            5681L, 5682L), size = 1),
                          #phantomver = "2.1.1",
                          check = FALSE, verbose = FALSE)

remDr <- rD[["client"]]

#binman::list_versions("phantomjs")

# require(webdriver)
# require(magrittr)
# pjs_instance <- run_phantomjs()
# pjs_session <- Session$new(port = pjs_instance$port)

# pjs_session$go("https://rp-online.de/archiv/2022/08/01/")
# 
# elem <- pjs_session$findElement(xpath = "//a[contains(@id, 'consentAccept')]")
# elem$click()
# pjs_session$getUrl()

remDr$navigate("https://rp-online.de/archiv/2022/08/01/")

#elem <- remDr$findElement(using = "xpath", "//a[contains(@id, 'consentAccept')]")

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#writeLines(html, "test.html")

# article_crawl <- function(i, html){
#   i <- 3
#   rvest::read_html(html) %>% 
#     rvest::html_elements(xpath = paste0("//ul/li[", i,"]/article/a//h2")) %>% 
#     rvest::html_text(trim = TRUE) -> item_title
#   if(length(item_title) == 0){
#     item_title <- NA
#   }
#   print(i)
#   rvest::read_html(html) %>% 
#     rvest::html_elements(xpath = paste0("//ul/li[", i,"]/article/a")) %>% 
#     rvest::html_attr("href") %>% paste0("https://rp-online.de",.)-> item_link
#   df <- data.frame(item_title, item_link)
#   return(df)
# }

#function for geting links from page
rpon_get_links <- function(html){
  
  html <- remDr$getPageSource()[[1]]
  #html <- pjs_session$getSource()
  #pjs_session$getUrl()
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = paste0("//div[contains(@class, 'w-full')]//article//a/span")) %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = paste0("//div[contains(@class, 'w-full')]//article//a")) %>% 
    rvest::html_attr("href") %>% paste0("https://rp-online.de",.)-> item_link
  
  df <- data.frame(item_title, item_link)
  # if(length(item_title) == length(item_link)){
  #   df <- data.frame(item_title, item_link)
  # } else {
  #   rvest::read_html(html) %>% 
  #     rvest::html_elements(xpath = "//ul/li/article") %>% 
  #     rvest::html_attr("href") %>% paste0("https://rp-online.de",.) %>%
  #     length() -> n
  #   print("crawl")
  #   (1:n) %>% purrr::map_df(~article_crawl(., html)) -> df
  # }
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//section[contains(@class, 'relative')]/header") %>%
    rvest::html_text(trim = TRUE) %>% stringr::str_extract("[0-9]+\\. [A-Za-zäöü]+ [0-9]+") %>%
    stringr::str_replace(., "März", "March") %>%
    stringr::str_replace(., "Dezember", "December") %>%
    stringr::str_replace(., "Oktober", "October") %>%
    stringr::str_replace(., "Januar", "January") %>%
    stringr::str_replace(., "Februar", "February") %>%
    stringr::str_replace(., "Mai", "May") %>%
    stringr::str_replace(., "Juni", "June") %>%
    stringr::str_replace(., "Juli", "July") %>%
    lubridate::dmy() -> df$item_pubdate
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'w-full')]//nav//div[contains(@class, 'text-40')]") %>%
    rvest::html_text(trim = TRUE) %>% print()
  
  return(df)
}


rpon_get_url <- function(url){
  remDr$navigate(url)
  print(remDr$getCurrentUrl())
  remDr$getPageSource()[[1]] %>% rpon_get_links() -> df
  print(nrow(df))
  return(df)
}

rpon_go_thr_archive <- function(startdate){

  seq(as.Date(startdate), Sys.Date(), by="days") %>% 
    format.Date(format="/%Y/%m/%d/") -> V1
  
  
  V1 %>%
    paste0("https://rp-online.de/archiv", .) %>%
    purrr::map_df(~rpon_get_url(.)) -> valid_links
  
  return(valid_links)
}

  
rpon_go_thr_archive("2022-08-01") -> valid_links


valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "RP Online", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

valid_links <- dplyr::distinct(valid_links)

saveRDS(valid_links, "RP Online.RDS")

 remDr$close()
 z <- rD$server$stop()
