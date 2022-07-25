
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
opp_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h2//a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h2//a") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//time") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    as.Date(., tryFormat = c("%d. %B %Y")) -> item_pubdate
    
    df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}

opp_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(opp_getlink(pjs_session$getSource()))
}

opp_go_thr_archive <- function(startdate){
  i <- 1
  j <- 1
  k <- 0
  valid_links <- data.frame()
  
  while (i > 0) {
    opp_getlink_url(paste0("https://opposition24.com/page/", j, "/")) %>% 
      subset(item_pubdate>=as.Date(startdate)) -> subset_links
    i <- nrow(subset_links)
    j <- j + 1
    # k <- k + 1
    valid_links <- rbind(valid_links, subset_links)
    # if(k > 50){
    #   Sys.sleep(60)
    #   k <- 0
    # }
  }
  return(valid_links)
}

valid_links <- opp_go_thr_archive("2022-01-01")



