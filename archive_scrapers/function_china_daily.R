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

pjs_session$go("http://german.chinatoday.com.cn/ch/bilderserien/index_9.html")

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#writeLines(html, "test.html")

#function for geting links from page
ch_d_get_links <- function(html, rubrik){
  
  #html <- remDr$getPageSource()[[1]]
  html <- pjs_session$getSource()
  #pjs_session$getUrl()
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'CT_CLLiBox')]//a") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'CT_CLLiBox')]//a") %>% 
    rvest::html_attr("href") %>% 
    stringr::str_remove(., "^\\.") %>%
    paste0("http://german.chinatoday.com.cn/ch/", rubrik,.)-> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'CT_CLLiBox')]//div[contains(@class, 'CT_CLW')]/span") %>% 
    rvest::html_text(trim = TRUE) %>% as.Date(tryFormat = "%H:%M, %m-%d, %Y")-> item_pubdate
  
  if(length(item_title) == 0){
    df <- data.frame()
  } else {
    df <- data.frame(item_title, item_link, item_pubdate)
  }
  return(df)
}


ch_d_get_url <- function(url, rubrik){
  # remDr$navigate(url)
  # print(remDr$getCurrentUrl())
  # remDr$getPageSource()[[1]] %>% tagesspiegel_get_links() -> df
  pjs_session$go(url)
  print(pjs_session$getUrl())
  pjs_session$getSource() %>% ch_d_get_links(., rubrik) -> df
  #print(nrow(df))
  return(df)
}

ch_d_go_thr_columns <- function(category, startdate){
  df <- paste0("http://german.chinatoday.com.cn/ch/", category) %>%
    purrr::map_df(~ch_d_get_url(., rubrik = category)) %>% subset(., item_pubdate >= as.Date(startdate))
  i <- 1
  j <- TRUE
  while (j) {
    #category <- "bilderserien"
    #i <- 1
    #startdate <- "2021-12-31"
    paste0("http://german.chinatoday.com.cn/ch/", category, "/index_", i, ".html") %>%
      purrr::map_df(~ch_d_get_url(., rubrik = category)) %>% subset(., item_pubdate >= as.Date(startdate)) -> df2
    nrow(df2) -> n
    print(n)
    df <- rbind(df, df2)
    Sys.sleep(1)
    if(n == 0){
      j <- FALSE
    } 
    i <- i + 1
  }
  #print(n)
  

  return(df)
}


c("aufmacher", "aktuelles", "politik", "wirtschaft", "gesellschaft",
  "nachrichten_im_bild", "bildung_kultur_und_reisen",
  "spezial", "videos", #"bilderserien", 
  "leserfavoriten") %>%
  purrr::map_df(~ch_d_go_thr_columns(., "2021-12-01")) -> valid_links 

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "China heute", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "China heute_1.RDS")

# remDr$close()
 # z <- rD$server$stop()
