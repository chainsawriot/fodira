require(RSelenium)
require(magrittr)

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
jouw_getlink <- function(html){
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'elementor-post__text')]//h1/a") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'elementor-post__text')]//h1/a") %>% 
    rvest::html_attr("href") -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'elementor-post__meta-data')]//span[contains(@class, 'elementor-post-date')]") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    stringr::str_extract(., "[0-9]+. [A-Z][a-zäöü]+ [0-9]+") %>%
    as.Date(., tryFormat = c("%d. %B %Y")) -> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
  return(df)
}

jouw_go_thr_2022 <- function(startpage){
  rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
  remDr <- rD[["client"]]
  Sys.sleep(5)
  remDr$navigate(startpage)
  print(startpage)
  Sys.sleep(10)
  webElem <- remDr$findElement(using = "css", "button[style='color: rgb(255, 255, 255); background-color: rgb(74, 144, 226);']")
  webElem$clickElement()
  rvest::read_html(remDr$getPageSource()[[1]]) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'e-load-more-anchor')]") %>% 
    rvest::html_attr(., "data-max-page") %>% as.numeric() -> end
  valid_links <- data.frame()
  for (i in 1:end) {
    paste0(startpage, "page/", i, "/") %>% remDr$navigate()
    jouw_getlink(remDr$getPageSource()[[1]]) -> subset_links
    valid_links <- rbind(valid_links, subset_links)
    print(remDr$getCurrentUrl())
    
  }
  
  remDr$close()
  z <- rD$server$stop()
  return(valid_links)
}

jouw_go_thr_2022("https://journalistenwatch.com/2022/") -> valid_links

