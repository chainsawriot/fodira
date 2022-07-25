
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
mmn_getlink <- function(html){

  rvest::read_html(pjs_session$getSource()) %>% 
    rvest::html_elements(xpath = "//article//h2/a") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(pjs_session$getSource()) %>% 
    rvest::html_elements(xpath = "//article//h2/a") %>% 
    rvest::html_attr("href")  -> paste0("https://www.mmnews.de", .) -> item_link
  
  
  df <- data.frame(item_title, item_link)
    return(df)
}


mmn_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  pjs_session$getSource()
  df <- mmn_getlink(pjs_session$getSource())
  return(df)
}


mmn_go_thr_columns <- function(rubrik, endnr){
  # k <- 0
  valid_links <- data.frame()
  for (i in 0:endnr) {
    paste0("https://www.mmnews.de/", rubrik, "?start=", i*15, "/") %>%
      purrr::map_df(~mmn_getlink_url(.)) -> subset_links
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


c("aktuelle-presse", "wirtschaft", "boerse", 
  "politik", "gold", "vermischtes", "witziges") %>% 
  purrr::map_dfr(~mmn_go_thr_columns(., endnr = 600)) -> valid_links


test <- mmn_getlink_url("https://www.mmnews.de/aktuelle-presse?start=15/")

