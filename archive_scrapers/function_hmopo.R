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

#pjs_session$go("https://www.mopo.de/page/1/?s")

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#writeLines(html, "test.html")

#function for geting links from page
hmopo_get_links <- function(html){
  
  #html <- remDr$getPageSource()[[1]]
  html <- pjs_session$getSource()
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'main-preview__post-title')]//a") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//a[contains(@class, 'main-preview__img-link')]") %>% 
    rvest::html_attr("href") %>% paste0("",.)-> item_link
  
  # rvest::read_html(html) %>% 
  #   rvest::html_elements(xpath = "//span[contains(@class, 'teaser__date')]") %>%
  #   rvest::html_text(trim = TRUE) %>% stringr::str_extract("[0-9]+\\.[0-9]+\\.[0-9]+") %>%
  #   lubridate::dmy()-> item_pubdate
  
  df <- data.frame(item_title, item_link)
  return(df)
}


hmopo_get_url <- function(url){
  # remDr$navigate(url)
  # print(remDr$getCurrentUrl())
  # remDr$getPageSource()[[1]] %>% tagesspiegel_get_links() -> df
  pjs_session$go(url)
  print(pjs_session$getUrl())
  pjs_session$getSource() %>% hmopo_get_links() %>% subset -> df
  return(df[stringr::str_detect(df$item_link, "mopo.de"),])
}


hmopo_go_thr_archive <- function(maxnum){

  paste0("https://www.mopo.de/page/1/?s") %>%
    purrr::map_df(~hmopo_get_url(.)) -> df
  
  1 -> n
  print(nrow(df))
  i <- 2
  while (n>0) {
    paste0("https://www.mopo.de/page/", i, "/?s") %>%
      purrr::map_df(~hmopo_get_url(.)) -> df2
    print(nrow(df2))
    i <- i+1
    
    if(i > maxnum){
        n <- 0
    }
    
    df <- rbind(df, df2)
  }
  
  return(df)
}

 
hmopo_go_thr_archive(750) -> valid_links


 # remDr$close()
 # z <- rD$server$stop()
