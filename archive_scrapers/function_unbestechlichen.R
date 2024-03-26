require(RSelenium)
require(magrittr)

rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]
Sys.sleep(5)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
unbest_getlink <- function(html){
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'entry-content')]//h2/a") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'entry-content')]//h2/a") %>% 
    rvest::html_attr("href") -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'entry-content')]//time[contains(@class, 'entry-date published')]")  %>% 
    rvest::html_text(., trim = TRUE) %>% 
    as.Date(., tryFormat = c("%d.%m.%Y")) -> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
  return(df)
}

unbest_go_thr_2022 <- function(startpage){
  remDr$navigate(startpage)
  print(startpage)
  Sys.sleep(10)
  #webElem <- remDr$findElement(using = "css", "a[class='_brlbs-btn _brlbs-btn-accept-all _brlbs-cursor']")
  #webElem$clickElement()
  rvest::read_html(remDr$getPageSource()[[1]]) %>% 
    rvest::html_elements(xpath = "//a[contains(@class, 'page-numbers')]") %>% 
    rvest::html_text(., trim = TRUE) %>% stringr::str_extract(pattern = "[0-9]+") %>%
    as.numeric()  -> end

   
  valid_links <- data.frame()
  for (i in 1:sort(end , TRUE)[1]) {
    paste0(startpage, "page/", i, "/") %>% remDr$navigate()
    unbest_getlink(remDr$getPageSource()[[1]]) -> subset_links
    valid_links <- rbind(valid_links, subset_links)
    print(remDr$getCurrentUrl())
    
  }
  

  return(valid_links)
}

unbest_go_thr_2022("https://dieunbestechlichen.com/2023/") -> valid_links_1

unbest_go_thr_2022("https://dieunbestechlichen.com/2022/") -> valid_links_2

unbest_go_thr_2022("https://dieunbestechlichen.com/2024/") -> valid_links_3


valid_links <- dplyr::distinct(rbind(valid_links_1, valid_links_2, valid_links_3))

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "DieUnbestechlichen", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "DieUnbestechlichen.RDS")

remDr$close()
z <- rD$server$stop()
