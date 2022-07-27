
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
lcm_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'card-body')]//h4/a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'card-body')]//h4/a") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'card')]//div[contains(@class, 'lcmr-card-date-author')]") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    stringr::str_extract(., "[0-9]+. [A-Z][a-zäöü]+ [0-9]+") %>%
    as.Date(., tryFormat = c("%d. %B %Y")) -> item_pubdate
    
    df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}

lcm_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(lcm_getlink(pjs_session$getSource()))
}

lcm_go_thr_columns <- function(rubrik, startdate){
  i <- 1
  j <- 1
  valid_links <- data.frame()
  while (i > 0) {
    lcm_getlink_url(paste0("https://lowerclassmag.com/", rubrik, "/page/", j, "/")) %>% 
      subset(item_pubdate>=as.Date(startdate)) -> subset_links
    i <- nrow(subset_links)
    j <- j + 1
    valid_links <- rbind(valid_links, subset_links)
  }
  return(valid_links)
}


c("blog-2", "category/themen/inland", "category/themen/ausland", 
  "category/themen/wirtschaft", "category/themen/kultur", 
  "lcm-columns/hinter-feindlichen-linien", "lcm-columns/anger-management",
  "lcm-columns/schabers-hass", "lcm-columns/bafi-im-bann-der-daemonen") %>% 
  purrr::map_dfr(~lcm_go_thr_columns(., startdate = "2022-01-01")) -> valid_links

valid_links <- dplyr::distinct(valid_links)



