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
    rvest::html_attr("href") %>% paste0("https://www.t-online.de/", .) -> item_link
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
    paste0("https://www.t-online.de/", rubrik, "/page_", j, "/") %>%
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
        date[!is.na(date)] %>% as.Date(., format = "%d.%m.%Y") -> date        
        if(date < as.Date(startdate)){
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


c("finanzen/geld-vorsorge", "finanzen/unternehmen-verbraucher", "finanzen/versicherungen",
  "finanzen/immobilien-wohnen", "finanzen/beruf-karriere", "unterhaltung/stars", 
  "unterhaltung/stars/royals", "unterhaltung/kino", "unterhaltung/tv", "unterhaltung/musik") %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2022-01-01")) -> valid_links1

c("nachrichten/panorama/menschen-schicksale", "nachrichten/panorama/katastrophen",
  "nachrichten/panorama/kriminalitaet", "nachrichten/panorama/justiz",
  "nachrichten/panorama/buntes-kurioses", "nachrichten/panorama/wissen/geschichte", 
  "nachrichten/panorama/quiz", "gesundheit/krankheiten-symptome", "gesundheit/krankheiten-symptome/coronavirus",
  "gesundheit/ernaehrung", "gesundheit/fitness", "gesundheit/gesund-leben", "gesundheit/heilmittel-medikamente",
  "gesundheit/schwangerschaft", "gesundheit/selbsttests") %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2022-01-01")) -> valid_links3

c("leben/corona-krise", 
  "leben/essen-und-trinken", "leben/reisen", "leben/familie", "leben/alltagswissen",
  "leben/liebe", "leben/mode-beauty", "nachhaltigkeit/klima-und-umwelt", "nachhaltigkeit/mobilitaet-und-verkehr",
  "nachhaltigkeit/heim-garten-und-wohnen", "nachhaltigkeit/energie", "nachhaltigkeit/finanzen-und-beruf",
  "nachhaltigkeit/ernaehrung", "nachhaltigkeit/konsum", "nachhaltigkeit/klima-lexikon") %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2022-01-01")) -> valid_links4


c("auto/neuheiten-fahrberichte", "auto/recht-und-verkehr", "auto/elektromobilitaet",
  "auto/technik", "digital/handy", "digital/computer", "digital/internet-sicherheit/sicherheit", 
  "digital/internet-sicherheit/internet", "digital/netzpolitik", "heim-garten/garten", 
  "heim-garten/haushaltstipps", "heim-garten/bauen", "heim-garten/wohnen", "heim-garten/energie",
  "ratgeber/deals", "ratgeber/technik", "ratgeber/haushalt-und-wohnen", "ratgeber/genuss",
  "ratgeber/leben-und-freizeit", "ratgeber/haus-und-garten", "ratgeber/gesundheit") %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2022-01-01")) -> valid_links5

c("nachrichten/deutschland", "nachrichten/ausland", "nachrichten/corona-krise", 
  "nachrichten/tagesanbruch", "nachrichten/ukraine", "region/berlin", "region/hamburg",
  "region/muenchen", "region/koeln", "region/frankfurt-am-main", "sport/fussball/bundesliga",
  "sport/fussball/2-bundesliga", "sport/fussball", "sport/fussball/frauenfussball/em-2022", 
  "sport/mehr-sport/radsport/tour-de-france", "sport/formel-1", "sport/mehr-sport") %>% 
  purrr::map_dfr(~tonline_go_thr_columns(., startdate = "2022-01-01")) -> valid_links2

valid_links <- dplyr::distinct(valid_links)

remDr$close()
z <- rD$server$stop()

# 