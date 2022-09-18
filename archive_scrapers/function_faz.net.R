#devtools::install_github("ropensci/RSelenium")
#install.packages("RSelenium")
require(RSelenium)
require(magrittr)

rD <- RSelenium::rsDriver(browser = "firefox", 
                          #chromever = "103.0.5060.134", 
                          port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), 
                          #phantomver = "2.1.1",
                          check = FALSE, verbose = FALSE)

remDr <- rD[["client"]]

#binman::list_versions("phantomjs")

# require(webdriver)
# require(magrittr)
# pjs_instance <- run_phantomjs()
# pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")


#function for geting links from page
faz_get_links <- function(html){
  
  #html <- remDr$getPageSource()[[1]]
  #html <- pjs_session$getSource()
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'Teaser620')]//a//h2") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'Teaser620')]/a[1]") %>% 
    rvest::html_attr("href") %>% paste0("https://www.faz.net",.)-> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//span[contains(@class, 'ThemaAuszeichnung')]") %>% 
    rvest::html_text(trim = TRUE) %>% stringr::str_extract("[0-9]+\\.[0-9]+\\.[0-9]+") %>%
    lubridate::dmy()-> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
  return(df)
}


faz_get_titles_url <- function(url){
  remDr$navigate(url)
  print(url)
  Sys.sleep(10)
  rvest::read_html(remDr$getPageSource()[[1]]) %>% 
    rvest::html_elements(xpath = "//body/h1") %>% 
    rvest::html_text(trim = TRUE) -> error
  
  if(length(error)==1){
    if(error == "HTTP Status 429 – Zu viele Anfragen"){
      Sys.sleep(65)
      remDr$navigate(url)
      print("retry")
      print(url)
    }
  }

  
  df <- faz_get_titles(remDr$getPageSource()[[1]])
  print(nrow(df))
  
  i <- 1
  j <- 30
  while (i > 0) {
    remDr$navigate(paste0(url, "&offset=", j))
    print(remDr$getCurrentUrl())
    Sys.sleep(5)
    rvest::read_html(remDr$getPageSource()[[1]]) %>% 
      rvest::html_elements(xpath = "//body/h1") %>% 
      rvest::html_text(trim = TRUE) -> error
    
    if(length(error)==1){
      if(error == "HTTP Status 429 – Zu viele Anfragen"){
        Sys.sleep(65)
        paste0(url, "&offset=", j)
        print("retry")
        print(remDr$getCurrentUrl())
      }
    }
    
    df2 <- faz_get_titles(remDr$getPageSource()[[1]])

    j <- j+30
    
    if(j == 300){
      Sys.sleep(5)
    }
    
    i <- nrow(df2)
    print(nrow(df2))
    df <- rbind(df, df2)
    
    
  }
  # pjs_session$go(url)
  # print(pjs_session$getUrl())
  # df <- faz_get_titles(pjs_session$getSource())
  df$item_pubdate <- stringr::str_extract(url, "[0-9]+[.][0-9]+[.][0-9]+")
  return(df)
}

faz_go_thr_archive_titles <- function(startdate, enddate){
  #startdate <- "2022-01-01"
  seq(as.Date(startdate), as.Date(enddate), by="days") %>% 
    format.Date(format="%d.%m.%Y&DT_to=%d.%m.%Y") -> V1
  
  V1 %>% paste0("https://fazarchiv.faz.net/faz-portal/faz-archiv?q=&max=30&DT_from=", .) %>%
    purrr::map_df(~faz_get_titles_url(.)) -> df
  
  # pjs_session$go(url)
  # print(url)
  
  return(df)
}

# df <- zeit_getlink_url("https://www.zeit.de/thema/krieg-in-ukraine", "2022-01-01")
  
  

faz_go_thr_archive_titles("2022-01-01", "2022-01-10")-> valid_titles

faz_get_urls <- function(title){
  remDr$navigate(paste0("https://duckduckgo.com/?q=", 
                        stringr::str_replace(title, " ", "+"), 
                        "+site%3Afaz.net"))
  
  print(remDr$getCurrentUrl())
  
  remDr$getPageSource()[[1]] -> html
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@id, 'links')]//h2//a") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@id, 'links')]//h2//a") %>% 
    rvest::html_attr("href") -> item_link
  
  
  # rvest::read_html(html) %>% 
  #   rvest::html_elements(xpath = "//div[contains(@id, 'links')]//a[contains(@class, 'result__url')]") %>% 
  #   rvest::html_attr("href") -> item_link
  
  item_link <- item_link[stringr::str_which(item_link, "faz.net.+[0-9]+.html")]
  
  return(item_link[1])
}

paste0('"', valid_titles$item_title, '" "', 
       valid_titles$item_title2, '" ', 
       valid_links$item_pubdate) %>% purrr::map_chr(faz_get_urls) -> valid_titles$item_link

valid_links <- dplyr::distinct(valid_links)

 remDr$close()
 z <- rD$server$stop()

# 