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

#pjs_session$go("https://www.waz.de/suche/?q=der+OR+die+OR+das+OR+eine+OR+ein+OR+einer+OR+eines+OR+ist+OR+sein+OR+er+OR+sie+OR+es&sort=neu")

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#writeLines(html, "test.html")

#function for geting links from page
waz_get_links <- function(html){
  
  #html <- remDr$getPageSource()[[1]]
  #html <- pjs_session$getSource()
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'teaser teaser--medium')]//span[contains(@class, 'teaser__headline')]") %>% 
    rvest::html_text(trim=TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'teaser teaser--medium')]/a") %>% 
    rvest::html_attr("href") %>% paste0("",.)-> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'teaser teaser--medium')]//span[contains(@class, 'teaser__date')]") %>%
    rvest::html_text(trim = TRUE) %>% stringr::str_extract("[0-9]+\\.[0-9]+\\.[0-9]+") %>%
    lubridate::dmy()-> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
  return(df)
}


waz_get_url <- function(url){
  # remDr$navigate(url)
  # print(remDr$getCurrentUrl())
  # remDr$getPageSource()[[1]] %>% tagesspiegel_get_links() -> df
  pjs_session$go(url)
  print(pjs_session$getUrl())
  pjs_session$getSource() %>% waz_get_links() -> df
  #print(nrow(df))
  return(df)
}

waz_go_thr_archive <- function(startdate, startn, endn){

  paste0("https://www.waz.de/suche/?q=der+OR+die+OR+das+OR+eine+OR+ein+OR+einer+OR+eines+OR+ist+OR+sein+OR+er+OR+sie+OR+es&sort=neu&p=1") %>%
    purrr::map_df(~waz_get_url(.)) %>% 
    subset(., item_pubdate >= startdate) -> df

  
  nrow(df) -> n
  print(n)
  i <- startn
  while (i <= endn) {

    paste0("https://www.waz.de/suche/?q=der+OR+die+OR+das+OR+eine+OR+ein+OR+einer+OR+eines+OR+ist+OR+sein+OR+er+OR+sie+OR+es&sort=neu&p=", i) %>%
      purrr::map_df(~waz_get_url(.)) %>% 
      subset(., item_pubdate >= startdate) -> df2
    nrow(df2) -> n
    if(n == 0){
      i <- endn + 1
    }
    print(n)
    i <- i+1
    df <- rbind(df, df2)
  }

  return(df)
}

# df <- zeit_getlink_url("https://www.zeit.de/thema/krieg-in-ukraine", "2022-01-01")
  
  
waz_go_thr_archive("2022-08-01", 2, 500) -> valid_links1
waz_go_thr_archive("2022-08-01", 501, 1000) -> valid_links2
waz_go_thr_archive("2022-08-01", 1001, 1500) -> valid_links3

waz_go_thr_archive("2022-08-01", 1501, 2000) -> valid_links4
waz_go_thr_archive("2022-08-01", 2001, 2500) -> valid_links5
waz_go_thr_archive("2022-08-01", 2501, 3000) -> valid_links6

waz_go_thr_archive("2022-08-01", 3001, 3500) -> valid_links7
waz_go_thr_archive("2022-08-01", 3501, 4000) -> valid_links8
waz_go_thr_archive("2022-08-01", 4001, 4500) -> valid_links9

waz_go_thr_archive("2022-08-01", 4501, 5000) -> valid_links10
waz_go_thr_archive("2022-08-01", 5001, 5500) -> valid_links11
waz_go_thr_archive("2022-08-01", 5501, 6000) -> valid_links12
waz_go_thr_archive("2022-08-01", 6001, 6500) -> valid_links13
waz_go_thr_archive("2022-08-01", 6501, 7000) -> valid_links14
waz_go_thr_archive("2022-08-01", 7001, 7500) -> valid_links15
waz_go_thr_archive("2022-08-01", 7501, 8000) -> valid_links16
waz_go_thr_archive("2022-08-01", 8001, 8500) -> valid_links17
waz_go_thr_archive("2022-08-01", 8501, 9000) -> valid_links18
waz_go_thr_archive("2022-08-01", 9001, 9500) -> valid_links19
waz_go_thr_archive("2022-08-01", 9501, 10000) -> valid_links20
waz_go_thr_archive("2022-08-01", 10001, 15000) -> valid_links21
waz_go_thr_archive("2022-08-01", 15001, 20000) -> valid_links22
waz_go_thr_archive("2022-08-01", 20001, 25000) -> valid_links23
waz_go_thr_archive("2022-08-01", 25001, 30000) -> valid_links24


valid_links <- dplyr::distinct(rbind(valid_links1, valid_links2, valid_links3,
                     valid_links4, valid_links5, valid_links6,
                     valid_links7, valid_links8, valid_links9,
                     valid_links10, valid_links11, valid_links12,
                     valid_links13, valid_links14, valid_links15, 
                     valid_links16, valid_links17, valid_links18,
                     valid_links19, valid_links20, valid_links21,
                     valid_links22
                     ))



valid_links$pub = "WAZ"

valid_links$description = NA

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "waz_2.RDS")
# remDr$close()
 # z <- rD$server$stop()
