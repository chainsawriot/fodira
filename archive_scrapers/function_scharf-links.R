require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
sch_li_getlink <- function(html){
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//tbody//table[contains(@width, 600)]//tbody/tr[1]/td/a") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  item_title <- item_title[!stringr::str_detect(item_title, "Weiter lesen>>")]
  
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//tbody//table[contains(@width, 600)]//tbody/tr[1]/td/a") %>% 
    rvest::html_attr("href") %>% paste0("http://www.scharf-links.de/", .) -> item_link
  item_link <- unique(item_link)
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//tbody//table[contains(@width, 600)]//tbody/tr[1]/td/font") %>% 
    rvest::html_text(trim = TRUE) %>% as.Date(., tryFormat = c("%A %d. %B %Y")) -> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}


sch_li_getlink_url <- function(url){
  remDr$navigate(url)
  print(url)
  i <- 1
  j <- 2
  html <- remDr$getPageSource()[[1]]
  rvest::read_html(html) %>% rvest::html_elements(xpath = "//div[contains(@class, 'tx-ttnews-browsebox')]//td[last()]/p/a") %>%
    rvest::html_text(trim = TRUE) -> nexturl_txt
  rvest::read_html(html) %>% rvest::html_elements(xpath = "//div[contains(@class, 'tx-ttnews-browsebox')]//td[last()]/p/a") %>%
    rvest::html_attr("href") -> nexturl
  df <- sch_li_getlink(remDr$getPageSource()[[1]])
  while(stringr::str_detect(nexturl_txt, "nächste")){
    remDr$navigate(paste0("http://www.scharf-links.de/", nexturl))
    print(paste0("http://www.scharf-links.de/", nexturl))
    df <- rbind(df, sch_li_getlink(remDr$getPageSource()[[1]]))
    html <- remDr$getPageSource()[[1]]
    rvest::read_html(html) %>% rvest::html_elements(xpath = "//div[contains(@class, 'tx-ttnews-browsebox')]//td[last()]/p/a") %>%
      rvest::html_text(trim = TRUE) -> nexturl_txt
    rvest::read_html(html) %>% rvest::html_elements(xpath = "//div[contains(@class, 'tx-ttnews-browsebox')]//td[last()]/p/a") %>%
      rvest::html_attr("href") -> nexturl
  }
  print(nrow(df))
  return(df)
}




sch_li_go_thr_columns <- function(){
  
  remDr$navigate("http://www.scharf-links.de/78.0.html")
  html <- remDr$getPageSource()[[1]]
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//table[contains(@align, 'left')]//font/a") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//table[contains(@align, 'left')]//font/a") %>% 
    rvest::html_attr("href") %>% paste0("http://www.scharf-links.de/", .) -> item_link
  
  df <- data.frame(item_title, item_link)
  df <- df[stringr::str_detect(df$item_title, "202[234]"),]
  
  df$item_link -> x
  x %>%
      purrr::map_df(~sch_li_getlink_url(.)) -> valid_links

  return(valid_links)
}


sch_li_go_thr_columns() -> valid_links

valid_links <- dplyr::distinct(valid_links)

remDr$close()
z <- rD$server$stop()

# 