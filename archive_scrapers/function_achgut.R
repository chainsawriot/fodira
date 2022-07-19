
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
achgut_getlink <- function(html){
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'teaser_blog')]//h3//a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'teaser_blog')]//h3//a") %>% 
    rvest::html_attr("href")  -> item_link

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'teaser_blog')]//div[contains(@class, 'teaser_text_meta')]") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    stringr::str_extract(., "[0-9]+[.][0-9]+[.][0-9]+") %>%
    as.Date(., tryFormat = c("%d.%m.%Y")) -> item_pubdate
    
    df <- data.frame(item_title, item_link, item_pubdate)
    
    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = "//div[contains(@class, 'teaser_fundstueck')]//h4") %>% 
      rvest::html_text(., trim = TRUE) -> item_title
    
    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = "//div[contains(@class, 'teaser_fundstueck')]//div[1]/a[1]") %>% 
      rvest::html_attr("href")  -> item_link
    
    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = "//div[contains(@class, 'teaser_fundstueck')]//div[contains(@class, 'teaser_text_meta collapsed')]") %>% 
      rvest::html_text(., trim = TRUE) %>% 
      stringr::str_extract(., "[0-9]+[.][0-9]+[.][0-9]+") %>%
      as.Date(., tryFormat = c("%d.%m.%Y")) -> item_pubdate
    
    df <- rbind(df, data.frame(item_title, item_link, item_pubdate))
    
    return(df)
}

achgut_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(achgut_getlink(pjs_session$getSource()))
}

achgut_go_thr_archive <-  function(startdate){
  i <- 1
  j <- 0
  k <- 0
  valid_links <- data.frame()
  while (i > 0) {
    achgut_getlink_url(paste0("https://www.achgut.com/P", j)) %>% 
      subset(item_pubdate>=as.Date(startdate)) -> subset_links
    i <- nrow(subset_links)
    print(i)
    j <- j + 13
    valid_links <- rbind(valid_links, subset_links)
    k <- k +1 
    if (k > 79){
      print("wait")
      Sys.sleep(30)
      k <- 0
    }
    if (j == 1950){
      print("wait")
      Sys.sleep(45)
      k <- 0
    }
  }
  return(valid_links)
}

valid_links <- achgut_go_thr_archive("2022-01-01")

