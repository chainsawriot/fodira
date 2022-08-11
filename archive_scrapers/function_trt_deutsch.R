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

pjs_session$go("https://www.trtdeutsch.com/news?page=50")

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#writeLines(html, "test.html")

#function for geting links from page
trt_get_links <- function(html){
  
  #html <- remDr$getPageSource()[[1]]
  #html <- pjs_session$getSource()
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'col-xs-12 col-md-8')]//div[contains(@class, 'card-content')]//a[contains(@class, 'card-title')]") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'col-xs-12 col-md-8')]//div[contains(@class, 'card-content')]//a[contains(@class, 'card-title')]") %>% 
    rvest::html_attr("href") %>% 
    #stringr::str_remove(., "^\\.") %>%
    paste0("https://www.trtdeutsch.com",.)-> item_link
  
  if(length(item_title) == 0){
    df <- data.frame()
  } else {
    df <- data.frame(item_title, item_link)
  }
  return(df)
}


trt_get_url <- function(url){
  # remDr$navigate(url)
  # print(remDr$getCurrentUrl())
  # remDr$getPageSource()[[1]] %>% tagesspiegel_get_links() -> df
  pjs_session$go(url)
  print(pjs_session$getUrl())
  pjs_session$getSource() %>% trt_get_links(.) -> df
  #print(nrow(df))
  return(df)
}

trt_go_thr_columns <- function(category, startdate){
  i <- 10
  j <- TRUE
  while (j) {
    Sys.sleep(.5)
    paste0("https://www.trtdeutsch.com/", category, "?page=", i, "") %>%
      purrr::map_df(~trt_get_url(.)) -> df
    nrow(df) -> n
    print(n)
    
    if(class(df$item_link[nrow(df)]) == "character"){
      pjs_session$go(df$item_link[nrow(df)])
      print(pjs_session$getUrl())
      rvest::read_html(pjs_session$getSource()) %>% 
        rvest::html_elements(xpath = "//div[contains(@class, 'article-date')]") %>%
        rvest::html_text(trim = TRUE) %>% stringr::str_replace(., "März", "March") -> date
      date[!is.na(date)] %>% lubridate::dmy() -> date
    }
    print(date)
    if(date < as.Date(startdate)){
      j <- FALSE
    }
    i <- i + 10
    print(j)
    
  }


  return(df)
}


c("news", "politik", "wirtschaft", "meinung",
  "exklusiv", "kultur",
  "gesellschaft", "sport", "wissenschaft") %>%
  purrr::map_df(~trt_go_thr_columns(., "2021-12-31")) -> valid_links 

#trt_go_thr_columns("news", "2021-12-31") -> test

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  mutate(pub = "TRT", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "TRT_1.RDS")

# remDr$close()
 # z <- rD$server$stop()
