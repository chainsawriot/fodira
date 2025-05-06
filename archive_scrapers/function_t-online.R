require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

remDr$setTimeout(type = "page load", milliseconds = 10000000)
remDr$setTimeout(type = "script", milliseconds = 10000000)
remDr$setTimeout(type = "implicit", milliseconds = 10000000)

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
tonline_getlink <- function(html){
  html <- remDr$getPageSource()[[1]]
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@data-testid, 'StreamLayout.Stream')]//article//t-fin-stream-ticker//div//a") %>% 
    rvest::html_text(trim = TRUE) -> item_fin
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@data-testid, 'StreamLayout.Stream')]//article//div//a") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  item_title <- item_title[(length(item_fin)+1):length(item_title)]
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@data-testid, 'StreamLayout.Stream')]//article//div//a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.t-online.de", .) -> item_link
  item_link <- item_link[(length(item_fin)+1):length(item_link)]
  
  # rvest::read_html(html) %>% 
  #   rvest::html_elements(xpath = "//div[contains(@data-testid, 'StreamLayout.Stream')]//article//img[contains(@data-tb-thumbnail, 'true')][1]") %>%
  #   rvest::html_attr("data-src") %>% unique() %>% stringr::str_extract("[0-9]+/[0-9]+") %>% 
  #   paste0(., "/01") %>%  as.Date(format="%Y/%m/%d") -> item_pubdate
  
  stringr::str_extract(item_link, "[0-9]+") %>% as.numeric() -> item_number
  
  df <- data.frame(item_title, item_link, item_number)
    return(df)
}


tonline_getlink_url <- function(url){
  remDr$navigate(url)
  print(url)
  df <- tonline_getlink(remDr$getPageSource()[[1]])
  return(df)
}

# df <- tonline_getlink(remDr$getPageSource()[[1]])
# df$huh <- (df$item_number > 91372000)

#tonline_getlink_url("https://www.t-online.de/leben/essen-und-trinken/page_4/")

tonline_go_thr_columns <- function(rubrik, startdate){
  i <- 1
  j <- 1
  valid_links <- data.frame()
  while (i > 0) {
    paste0("https://www.t-online.de", rubrik, "page_", j) %>%
      purrr::map_df(~tonline_getlink_url(.)) -> subset_links
    
    valid_links <- rbind(valid_links, subset_links)  
    i <-  nrow(subset_links)
    print(i)
    rvest::read_html(remDr$getPageSource()[[1]]) %>% 
      rvest::html_elements(xpath = "//nav[contains(@aria-label, 'Paginierung')]//a[last()]") %>% 
      rvest::html_text(., trim = TRUE)  %>% as.numeric() -> n
    if(!is.na(n[1])){
      if(j==n[1]){
        i <- 0
      } 
    } else if(length(n)==0) {
      i <- 0
    } else {
      subset_links %>% 
        subset(., item_number > 91257968) -> subset_links2
      if(nrow(subset_links2) == 0){
        
        remDr$navigate(subset_links$item_link[1])
        rvest::read_html(remDr$getPageSource()[[1]]) %>% 
          rvest::html_elements(xpath = "//div[contains(@data-testid, 'StreamLayout.Stream')]//header") %>%
          rvest::html_text(trim = TRUE) %>%
#          stringr::str_extract(., "Aktualisiert am [0-9]+[.][0-9]+[.][0-9]+") %>% 
          stringr::str_extract(., "[0-9]+[.][0-9]+[.][0-9]+")-> date
        
        if(length(date) > 1){
          date <- date[1]
        }
        
        date <- date[!is.na(date)] 
        
        if(length(date) == 0){
          rvest::read_html(remDr$getPageSource()[[1]]) %>% 
            rvest::html_elements(xpath = "//span[contains(@class, 'text-manatee')]") %>%
            rvest::html_text(trim = TRUE)  %>%
            stringr::str_extract(., "[0-9]+[.][0-9]+[.][0-9]+")-> date
        }
          
        date <- date[!is.na(date)]     
        
        if(stringr::str_detect(remDr$getCurrentUrl(), "[.]html")){
          date[!is.na(date)] %>% as.Date(., format = "%d.%m.%Y") -> date        
          if(date < as.Date(startdate)){
            i <-  nrow(subset_links2)
          }
        } else {
          i <-  nrow(subset_links2)
        }
        

      }
    }

    print(i)
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

remDr$navigate("https://t-online.de")

html <- remDr$getPageSource()[[1]]

rvest::read_html(html) %>% 
  rvest::html_elements(xpath = "//div[contains(@class, 'group')]//a") %>% 
  rvest::html_attr("href")  -> categories

categories[stringr::str_which(categories, "^/.*/.*/$")] -> categories

categories[1:10] %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2023-01-01")) -> valid_links1

categories[11:20] %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2023-01-01")) -> valid_links2

categories[21:30] %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2023-01-01")) -> valid_links3

categories[31:40] %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2023-01-01")) -> valid_links4

categories[41:50] %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2023-01-01")) -> valid_links5

categories[51:60] %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2023-01-01")) -> valid_links6

categories[61:70] %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2023-01-01")) -> valid_links7

categories[71:80] %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2023-01-01")) -> valid_links8

categories[81:90] %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2023-01-01")) -> valid_links9

categories[91:100] %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2023-01-01")) -> valid_links10

categories[101:110] %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2023-01-01")) -> valid_links11

categories[111:120] %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2023-01-01")) -> valid_links12

categories[121:132] %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2023-01-01")) -> valid_links13



valid_links <- rbind(valid_links1, valid_links2, valid_links3,
                     valid_links4, valid_links5, valid_links6,
                     valid_links7, valid_links8, valid_links9,
                     valid_links10, valid_links11, valid_links12,
                     valid_links13)

remDr$close()
z <- rD$server$stop()

valid_links %>% dplyr::distinct() %>% 
  dplyr::rename(title = item_title, link = item_link) %>% 
  dplyr::mutate(pub = "T-Online", description = NA, pubdate = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

valid_links <- dplyr::distinct(valid_links)

saveRDS(valid_links, "T-Online.RDS") 




