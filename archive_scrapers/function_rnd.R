
require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
rnd_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//a[contains(@data-is-element-rendered, 'true')]//h2") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//a[contains(@data-is-element-rendered, 'true')]") %>% 
    rvest::html_attr("href") %>% paste0("https://www.rnd.de", .) -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//a[contains(@data-is-element-rendered, 'true')]//time") %>%
    rvest::html_text(., trim = TRUE) %>% stringr::str_extract(., "[0-9]+[.][0-9]+[.][0-9]+") %>%
    as.Date(tryFormat = c("%d.%m.%Y")) -> item_pubdate

  if(length(item_pubdate) != length(item_link)){
    for (i in 1:(length(item_link) - length(item_pubdate))){
      item_pubdate <- c(item_pubdate, item_pubdate[1])
      print("add")
    }
  }
  
    df <- data.frame(item_title, item_link, item_pubdate)
    
    return(df)
}

rnd_getlink_url <- function(url){
  remDr$navigate(url)
  print(url)
  return(rnd_getlink(remDr$getPageSource()[[1]]))
}

## doesn't work headless - no idea why, individual pages work - sometimes not

rnd_go_thr_archive <- function(startdate){
  seq(as.Date(startdate), Sys.Date(), by="days") %>% 
    format.Date(format="-%d-%m-%Y") -> V1
  
  V1 %>%
    paste0("https://www.rnd.de/archiv/artikel", ., "/") %>%
    purrr::map_df(~rnd_getlink_url(.)) -> valid_links
  
  return(valid_links)
}


rnd_go_thr_archive(startdate = "2022-01-01") -> valid_links

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "RND", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "RND.RDS")

