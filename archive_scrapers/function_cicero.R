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

pjs_session$go("https://www.cicero.de/innenpolitik?_wrapper_format=html&page=%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C0")

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#writeLines(html, "test.html")

#function for geting links from page
cicero_get_links <- function(html){
  
  #html <- remDr$getPageSource()[[1]]
  html <- pjs_session$getSource()
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'view-content')]//h2//a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'view-content')]//h2//a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.cicero.de",.)-> item_link
  
  if(length(item_title) == 0){
    df <- data.frame()
  } else {
    df <- data.frame(item_title, item_link)
  }

  
  return(df)
}


cicero_get_url <- function(url){
  # remDr$navigate(url)
  # print(remDr$getCurrentUrl())
  # remDr$getPageSource()[[1]] %>% tagesspiegel_get_links() -> df
  pjs_session$go(url)
  print(pjs_session$getUrl())
  pjs_session$getSource() %>% cicero_get_links() -> df
  #print(nrow(df))
  return(df)
}

#allg_z_get_url("https://www.allgemeine-zeitung.de/ratgeber/freizeittipps/unterwegs-mit-kindern?page=6") -> test

cicero_go_thr_columns <- function(category, startdate){
  df <- data.frame()
  i <- 0
  j <- TRUE
  while (j) {
    
    
    #i <- 5
    #category <- "innenpolitik"
    paste0("https://www.cicero.de/", category, "?_wrapper_format=html&page=%2C%2C%2C%2C%2C%2C%2C%2C%2C%2C", i) %>%
      purrr::map_df(~cicero_get_url(.)) -> df2
    nrow(df2) -> n
    print(n)
    if(n == 0){
      j <- FALSE
    } else {
      i <- i+1
      df <- rbind(df, df2)
      if(class(df2$item_link[1]) == "character"){
        pjs_session$go(df2$item_link[1])
        pjs_session$getUrl()
        rvest::read_html(pjs_session$getSource()) %>% 
          rvest::html_elements(xpath = "//p[contains(@class, 'byline__author')]") %>%
          rvest::html_text(trim = TRUE) %>%
          #          stringr::str_extract(., "Aktualisiert am [0-9]+[.][0-9]+[.][0-9]+") %>% 
          stringr::str_extract(., "[0-9]+[.] [A-Za-zäöü]+ [0-9]+") -> date
        date[!is.na(date)] %>% as.Date(., format = "%d. %B %Y") -> date
      }
      print(date)   
      if(length(date)==0){
        if(class(df2$item_link[2]) == "character"){
          if(nrow(df2) > 1){
            pjs_session$go(df2$item_link[2])
            rvest::read_html(pjs_session$getSource()) %>% 
              rvest::html_elements(xpath = "//p[contains(@class, 'byline__author')]") %>%
              rvest::html_text(trim = TRUE) %>%
              #          stringr::str_extract(., "Aktualisiert am [0-9]+[.][0-9]+[.][0-9]+") %>% 
              stringr::str_extract(., "[0-9]+[.] [A_Za-zäöü][0-9]+")-> date
            date[!is.na(date)] %>% as.Date(., format = "%d. %B %Y") -> date
          } else {
            j <- FALSE
          }
        }
      }
      if(length(date)==0){
        
      } else if (!is.na(date)){
        if (date < as.Date(startdate)){
          j <- FALSE
        }
      }
      
      
    }
    }
    #print(n)
    

  return(df)
}


c("innenpolitik", "aussenpolitik", "wirtschaft",
  "kultur", "cicero-plus") %>% purrr::map_df(~cicero_go_thr_columns(., "2021-12-01")) -> valid_links


valid_links %>% dplyr::rename(title = item_title, link = item_link) %>% 
  dplyr::mutate(pub = "Cicero", description = NA, pubdate = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "Cicero.RDS")
#remDr$close()
#z <- rD$server$stop()
