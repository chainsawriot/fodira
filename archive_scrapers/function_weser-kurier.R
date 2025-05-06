require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(
  5678L, 
  5679L, 5680L, 5681L, 
  5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

#remDr$setTimeout(type = "page load", milliseconds = 10000000)
#remDr$setTimeout(type = "script", milliseconds = 10000000)
#remDr$setTimeout(type = "implicit", milliseconds = 10000000)

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
weser_getlink <- function(html, date){
  
  print(html)
  
  remDr$navigate(html)
  
  print(remDr$getCurrentUrl())
  
  html <- remDr$getPageSource()[[1]]
  
  rvest::read_html(html) %>% 
      rvest::html_elements(xpath = "//div[contains(@class, 'error-container')]//div[contains(@class, 'error-content')]//h2[contains(@class, 'error-title')]") %>% 
    rvest::html_text(., trim = TRUE) -> item_empty
  
  if(length(item_empty) > 0){
    return(data.frame())
  } 
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//section[contains(@class, 'webpagefragment-section')]//div[contains(@class, 'teaser-text')]//span[contains(@class, 'title')]") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  if(length(item_title) == 0){
    return(data.frame())
  } 
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//section[contains(@class, 'webpagefragment-section')]//article//a") %>% 
    rvest::html_attr("href") -> item_link
  
  date -> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
  df <- df[stringr::str_detect(df$item_link, "https://www.weser-kurier.de/"),]
    return(df)
}



weser_getlink_url <- function(V1, rubrik){
  
  V1 %>%
    format.Date(format="/%Y/%m/%d/") %>%
    paste0("https://www.weser-kurier.de/", rubrik, .) %>%
    purrr::map_df(~weser_getlink(., V1)) -> subset_links
  
  return(subset_links)
}


# df <- tonline_getlink(remDr$getPageSource()[[1]])
# df$huh <- (df$item_number > 91372000)

#tonline_getlink_url("https://www.t-online.de/leben/essen-und-trinken/page_4/")

weser_go_thr_columns <- function(rubrik, startdate){

  seq(as.Date(startdate), Sys.Date(), by="days") -> V1
  
  V1 %>%
    purrr::map_df(~weser_getlink_url(., rubrik)) -> valid_links

  return(valid_links)
}


weser_go_thr_page <- function(startdate, range){
  remDr$navigate("https://www.weser-kurier.de/")
  
  remDr$getPageSource()[[1]] %>% rvest::read_html() %>% 
    rvest::html_elements(xpath = "//ul[contains(@class, 'main-navi__list__item__sub-nav')]//li//a") %>% 
    rvest::html_attr("href") %>% stringr::str_remove("https://www.weser-kurier.de/") -> categories
  
  categories[range] %>% 
    purrr::map_dfr(~weser_go_thr_columns(., startdate)) -> df
  
  # c(#"deutschland-welt", 
  #   "sport/sport-in-der-region/landkreis-osterholz",
  #   "sport/sport-in-der-region/landkreis-verden", "sport/sport-in-der-region/landkreis-diepholz",
  #   "sport/sport-in-der-region/stadt-delmenhorst", "sport/sport-in-der-region/landkreis-wesermarsch",
  #   "sport/sport-in-der-region/landkreis-oldenburg", "sport/sport-in-der-region/landkreis-rotenburg",
  #   "sport/sport-in-der-region/landkreis-cuxhaven/") %>% 
  #   purrr::map_dfr(~weser_go_thr_columns(., startdate)) %>% rbind(df, .) -> df
  # 
}


weser_go_thr_page(startdate = "2023-01-01", c(1:5)) -> valid_links1
weser_go_thr_page(startdate = "2023-01-01", c(6:10)) -> valid_links2
weser_go_thr_page(startdate = "2023-01-01", c(11:15)) -> valid_links3
weser_go_thr_page(startdate = "2023-01-01", c(16:20)) -> valid_links4
weser_go_thr_page(startdate = "2023-01-01", c(21:25)) -> valid_links5
weser_go_thr_page(startdate = "2023-01-01", c(26:30)) -> valid_links6
weser_go_thr_page(startdate = "2023-01-01", c(31:35)) -> valid_links7
weser_go_thr_page(startdate = "2023-01-01", c(36:41)) -> valid_links8

valid_links <- dplyr::distinct(rbind(valid_links1, valid_links2, valid_links3, valid_links4, 
                                     valid_links5, valid_links6, valid_links7, valid_links8))

valid_links$pub <- "Weser Kurier"


valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Weser Kurier", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "Weser Kurier.RDS")


remDr$close()
z <- rD$server$stop()

# 