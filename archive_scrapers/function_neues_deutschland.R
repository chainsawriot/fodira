
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)

#function for geting links from page

nd_getlink <- function(html){
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'Teaser-Small')]//div[contains(@class, 'Title')]") %>% 
    rvest::html_text() -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'Teaser-Small')]//div[contains(@class, 'Title')]/a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.nd-aktuell.de", .) -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'Teaser-Small')]//time") %>% 
    rvest::html_text() %>% as.Date(tryFormats = c("%d.%m.%Y")) -> item_pubdate
  
  return(data.frame(item_title, item_link, item_pubdate))
}

#function for going through each archive page

nd_go_thr_columns <- function(rubrik, startdate){
  i <- 1
  j <- 0
  valid_links <- data.frame()
  while (i > 0) {
    pjs_session$go(paste0("https://www.nd-aktuell.de/", rubrik, "/?s=", j))
    pjs_session$getSource() %>% nd_getlink() %>% 
      subset(item_pubdate>=as.Date(startdate)) -> subset_links
    i <- nrow(subset_links)
    print(paste(rubrik, j))
    j <- j + 25
    valid_links <- rbind(valid_links, subset_links)
  }
  return(valid_links)
}

c("rubrik/meinung", "rubrik/politik-oekonomie", "rubrik/hauptstadtregion", 
  "rubrik/feuilleton", "rubrik/sport", 
  "die-woche/rubrik/politik-oekonomie", "die-woche/rubrik/kunst-kritik", 
  "die-woche/rubrik/mikroskop", "die-woche/rubrik/begegnungen", 
  "die-woche/rubrik/metropole", "die-woche/rubrik/sport", "die-woche/rubrik/reise") %>% 
  purrr::map_dfr(~nd_go_thr_columns(., startdate = "2022-01-01")) -> valid_links

valid_links <- dplyr::distinct(valid_links)


valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Neues Deutschland", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Neues Deutschland.RDS")








