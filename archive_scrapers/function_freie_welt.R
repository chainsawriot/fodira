
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
freiewelt_getlink <- function(html){
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'articlepreview news')]//h1/a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'articlepreview news')]//h1/a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.freiewelt.net", .) -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'articlepreview news')]//time") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    as.Date(tryFormat = c("%d.%m.%Y | %H:%M")) -> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
  return(df)
}

freiewelt_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(freiewelt_getlink(pjs_session$getSource()))
}


x <- freiewelt_getlink("https://www.freiewelt.net/?tx_ttnews%5Bpointer%5D=1&cHash=d56a40b20074af4e4e87f130f41896d6")


###????

klggkl_go_thr_columns <- function(rubrik, startdate){
  i <- 1
  j <- 1
  valid_links <- data.frame()
  while (i > 0) {
    klggkl_getlink_url(paste0("https://www.klassegegenklasse.org/kategorie/", rubrik, "/page/", j, "/")) %>% 
      subset(item_pubdate>=as.Date(startdate)) -> subset_links
    i <- nrow(subset_links)
    j <- j + 1
    valid_links <- rbind(valid_links, subset_links)
  }
  return(valid_links)
}





