
require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
spiegel_getlink <- function(html){
  html <- remDr$getPageSource()[[1]]

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//header//h2/a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//header//h2/a") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h2[contains(@class, 'lg:mr-24 md:mr-24 sm:mx-16')]") %>%
    rvest::html_text(., trim = TRUE) %>% stringr::str_extract(., "[0-9]+[.] [A-za-zäöü]+ [0-9]+") %>%
    as.Date(tryFormat = c("%d. %B %Y")) -> item_pubdate

    df <- data.frame(item_title, item_link, item_pubdate)
    
    return(df)
}

spiegel_getlink_url <- function(url){
  remDr$navigate(url)
  print(url)
  return(spiegel_getlink(remDr$getPageSource()[[1]]))
}

spiegel_getlink_url("https://www.spiegel.de/nachrichtenarchiv/artikel-19.07.2022.html")

###doesn't work with headless browser

spiegel_go_thr_archive <- function(startdate){
  seq(as.Date(startdate), Sys.Date(), by="days") %>% 
    format.Date(format="%d-%m-%Y") %>% stringr::str_replace_all(., "-", ".")-> V1
  
  V1 %>%
    paste0("https://www.spiegel.de/nachrichtenarchiv/artikel-", ., ".html") %>%
    purrr::map_df(~spiegel_getlink_url(.)) -> valid_links
  
  return(valid_links)
}


spiegel_go_thr_archive(startdate = "2022-01-01") -> valid_links

