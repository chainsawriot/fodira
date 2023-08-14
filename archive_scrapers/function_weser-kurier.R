require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(
  5678L, 
  #5679L, 5680L, 5681L, 
  5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

#remDr$setTimeout(type = "page load", milliseconds = 10000000)
#remDr$setTimeout(type = "script", milliseconds = 10000000)
#remDr$setTimeout(type = "implicit", milliseconds = 10000000)

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
weser_getlink <- function(html){
  html <- remDr$getPageSource()[[1]]
  
  
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//section[contains(@class, 'webpagefragment-section')]//article//a") %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//section[contains(@class, 'webpagefragment-section')]//article//a") %>% 
    rvest::html_attr("href") -> item_link

  df <- data.frame(item_title, item_link)
  df <- df[stringr::str_detect(df$item_link, "https://www.weser-kurier.de/"),]
    return(df)
}



weser_getlink_url <- function(url){
  remDr$navigate(url)
  print(url)
  df <- weser_getlink(remDr$getPageSource()[[1]])
  return(df)
}

# df <- tonline_getlink(remDr$getPageSource()[[1]])
# df$huh <- (df$item_number > 91372000)

#tonline_getlink_url("https://www.t-online.de/leben/essen-und-trinken/page_4/")

weser_go_thr_columns <- function(rubrik, startdate){
  i <- 1
  j <- 1
  valid_links <- data.frame()
  while (i > 0) {
    paste0("https://www.weser-kurier.de/", rubrik, "/?p=", j) %>%
      purrr::map_df(~weser_getlink_url(.)) -> subset_links
    
    valid_links <- rbind(valid_links, subset_links)  
    i <-  nrow(subset_links)
    print(i)
    rvest::read_html(remDr$getPageSource()[[1]]) %>% 
      rvest::html_elements(xpath = "//nav[contains(@class, 'text-center pagination')]//a[last()-1]") %>% 
      rvest::html_text(., trim = TRUE) -> n
    if(!is.na(n[1])){
      if(j==n[1]){
        i <- 0
      } else if(length(n)==0) {
      i <- 0
      } else if (j > 2){
        
        remDr$navigate(subset_links$item_link[1])
        rvest::read_html(remDr$getPageSource()[[1]]) %>% 
          rvest::html_elements(xpath = "//div[contains(@class, 'article-header__more-info__publication-time')]") %>%
          rvest::html_text(trim = TRUE) %>%
          #          stringr::str_extract(., "Aktualisiert am [0-9]+[.][0-9]+[.][0-9]+") %>% 
          stringr::str_extract(., "[0-9]+[.][0-9]+[.][0-9]+")-> date
        date[!is.na(date)] %>% as.Date(., format = "%d.%m.%Y") -> date   
        if(length(date)==0){
          remDr$navigate(subset_links$item_link[2])
          rvest::read_html(remDr$getPageSource()[[1]]) %>% 
            rvest::html_elements(xpath = "//div[contains(@class, 'article-header__more-info__publication-time')]") %>%
            rvest::html_text(trim = TRUE) %>%
            #          stringr::str_extract(., "Aktualisiert am [0-9]+[.][0-9]+[.][0-9]+") %>% 
            stringr::str_extract(., "[0-9]+[.][0-9]+[.][0-9]+")-> date
          date[!is.na(date)] %>% as.Date(., format = "%d.%m.%Y") -> date
        }
        if(length(date)==0){

        } else if(date < as.Date(startdate)){
          i <-0
        }
      }
      
    }

    print(j)
    j <- j+1
    
    
    
    
    # k=k+1
    # if (k > 50){
    #   print("wait")
    #   Sys.sleep(90)
    #   k <- 0
    # }
  }

  return(valid_links)
}


weser_go_thr_page <- function(startdate){
  remDr$navigate("https://www.weser-kurier.de/")
  
  remDr$getPageSource()[[1]] %>% rvest::read_html() %>% 
    rvest::html_elements(xpath = "//ul[contains(@class, 'main-navi__list__item__sub-nav')]//li//a") %>% 
    rvest::html_attr("href") %>% stringr::str_remove("https://www.weser-kurier.de/") -> categories
  
  categories[c(2:6, 8:19, 21:22, 
               
               25:50, 52:58)] %>% 
    purrr::map_dfr(~weser_go_thr_columns(., startdate)) -> df
  
  c("thema/corona-q278567", "deutschland-welt", "sport/sport-in-der-region/landkreis-osterholz",
    "sport/sport-in-der-region/landkreis-verden", "sport/sport-in-der-region/landkreis-diepholz",
    "sport/sport-in-der-region/stadt-delmenhorst", "sport/sport-in-der-region/landkreis-wesermarsch",
    "sport/sport-in-der-region/landkreis-oldenburg", "sport/sport-in-der-region/landkreis-rotenburg",
    "sport/sport-in-der-region/landkreis-cuxhaven/") %>% 
    purrr::map_dfr(~weser_go_thr_columns(., startdate)) %>% rbind(df, .) -> df
  
}


weser_go_thr_page(startdate = "2022-08-01") -> valid_links1

valid_links <- dplyr::distinct(valid_links1)

valid_links$pub <- "Weser Kurier"


valid_links %>% dplyr::rename(title = item_title, link = item_link) %>% 
  dplyr::mutate(pub = "Weser Kurier", description = NA, pubdate = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "Weser Kurier.RDS")


remDr$close()
z <- rD$server$stop()

# 