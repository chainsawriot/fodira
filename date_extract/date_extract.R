
require(magrittr)

.date_extract <- function(html){
  
  html %>% 
    rvest::html_elements(xpath = "//meta[contains(@property, 'article:published_time')]") %>% 
    rvest::html_attr(., "content")  %>%
    as.Date() -> date
  
  date <- date[!is.na(date)][1]
  
  if(!is.na(date)){
    return(as.character(date))
  }
  
  html %>% 
    rvest::html_elements(xpath = "//script[contains(@type, 'application/ld+json')]") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    stringr::str_extract(., pattern = '"datePublished":".*?"') %>%
    stringr::str_extract(., pattern = '\\d{4}-\\d{2}-\\d{2}') %>%
    as.Date() -> date
  
  
  date <- date[!is.na(date)][1]
  
  if(!is.na(date)){
    return(as.character(date))
  }
  
  html %>% 
    rvest::html_elements(xpath = "//script[contains(@type, 'application/ld+json')]") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    stringr::str_extract(., pattern = '"datePublished": ".*?"') %>%
    stringr::str_extract(., pattern = '\\d{4}-\\d{2}-\\d{2}') %>%
    as.Date() -> date
  
  date <- date[!is.na(date)][1]
  
  if(!is.na(date)){
    return(as.character(date))
  }
  
  html %>% 
    rvest::html_elements(xpath = "//script[contains(@type, 'application/ld+json')]") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    stringr::str_extract(., pattern = '"datePublished":".*?"') %>%
    stringr::str_extract(., pattern = '\\d{4}-\\d{2}-\\d{2}') %>%
    as.Date() -> date
  
  date <- date[!is.na(date)][1]
  
  if(!is.na(date)){
    return(as.character(date))
  }
  
  html %>% 
    rvest::html_elements(xpath = "//meta[contains(@property, 'article:modified_time')]") %>% 
    rvest::html_attr(., "content")  %>%
    as.Date() -> date
  
  date <- date[!is.na(date)][1]
  
  if(!is.na(date)){
    return(as.character(date))
  }
  
  html %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'section_beitrag')]//div[contains(@class, 'teaser_text_meta')]") %>% 
    rvest::html_text(., trim = TRUE) %>%
    stringr::str_extract(., pattern = '\\d{2}[.]\\d{2}[.]\\d{4}') %>%
    lubridate::dmy() -> date
  
  date <- date[!is.na(date)][1]
  
  if(!is.na(date)){
    return(as.character(date))
  }
  
  html %>% 
    rvest::html_elements(xpath = "//span[contains(@class, 'time')]") %>% 
    rvest::html_text(., trim = TRUE) %>%
    stringr::str_extract(., pattern = '\\d{2}[.]\\d{2}[.]\\d{4}') %>%
    lubridate::dmy() -> date
  
  date <- date[!is.na(date)][1]
  
  if(!is.na(date)){
    return(as.character(date))
  }
  
  
  return(NA)
}
