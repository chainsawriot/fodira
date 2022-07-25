
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
epoch_getlink <- function(html){

  rvest::read_html(pjs_session$getSource()) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'col-12 col-md-8 border-r')]//div[contains(@class, 'card-body')]/a") %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(pjs_session$getSource()) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'col-12 col-md-8 border-r')]//div[contains(@class, 'card-body')]/a") %>% 
    rvest::html_attr("href")  -> item_link
  
  
  df <- data.frame(item_title, item_link)
  
  rvest::read_html(pjs_session$getSource()) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'col-12 col-md-8 border-r')]//div[contains(@class, 'd-flex flex-row align-items-center')]/a") %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(pjs_session$getSource()) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'col-12 col-md-8 border-r')]//div[contains(@class, 'd-flex flex-row align-items-center')]/a") %>% 
    rvest::html_attr("href")  -> item_link
  
  df <- rbind(data.frame(item_title, item_link), df)
    return(df)
}


epoch_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  pjs_session$getSource()
  df <- epoch_getlink(pjs_session$getSource())
  return(df)
}

x <- FALSE

class(x)

epoch_go_thr_columns <- function(rubrik, endnr){

  # k <- 0
  valid_links <- data.frame()
  for (i in 1:endnr) {
    paste0("https://www.epochtimes.de/", rubrik, "/page/", i, "/") %>%
      purrr::map_df(~epoch_getlink_url(.)) -> subset_links
    valid_links <- rbind(valid_links, subset_links)
    # k=k+1
    # if (k > 50){
    #   print("wait")
    #   Sys.sleep(90)
    #   k <- 0
    # }
  }

  return(valid_links)
}


c("politik", "wirtschaft", "gesundheit", 
  "meinung", "china", "feuilleton") %>% 
  purrr::map_dfr(~epoch_go_thr_columns(., endnr = 300)) -> valid_links

df <- epoch_getlink(pjs_session$getSource())


