require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
mmn_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article//h2/a") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article//h2/a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.mmnews.de", .) -> item_link
  
  stringr::str_extract(item_link, "[0-9]{4}[0-9]+") -> item_number
  
  df <- data.frame(item_title, item_link, item_number)
    return(df)
}




mmn_getlink_url <- function(url){
  remDr$navigate(url)
  print(url)
  df <- mmn_getlink(remDr$getPageSource()[[1]])
  return(df)
}


mmn_go_thr_columns <- function(rubrik){
  i <- 1
  j <- 0
  m <- ifelse(rubrik == "aktuelle-presse", 15,7)
  valid_links <- data.frame()
  while (i > 0) {
    paste0("https://www.mmnews.de/", rubrik, "?start=", j*m, "/") %>%
      purrr::map_df(~mmn_getlink_url(.)) %>% subset(., item_number > 174900)-> subset_links
    i <- nrow(subset_links)
    j <- j+1
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
  purrr::map_dfr(~mmn_go_thr_columns(.)) -> valid_links

remDr$close()
z <- rD$server$stop()
