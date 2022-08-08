require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 
                                                               5679L, 
                                                               5680L, 
                                                               5681L, 
                                                               5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

# require(webdriver)
# require(magrittr)
# pjs_instance <- run_phantomjs()
# pjs_session <- Session$new(port = pjs_instance$port)

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#pjs_session$go("https://www.badische-zeitung.de/archiv/2022/01/03")

nordb_click_function <- function(){
  elem <- remDr$findElement(using = "xpath", "//section[contains(@class, 'modul modul--newslist')]//button[contains(@class, 'btn btn__center btn__loadmore')]")
  elem$clickElement()
}

sclick <- purrr::safely(nordb_click_function, quiet = TRUE)

sclick()


nord_gothr_elem <- function(html, i, j){
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = paste0("//section[contains(@class, 'modul modul--newslist')]/div[", 
                                        i, "]/div[contains(@class, 'row')][", 
                                        j, "]//a//h6")) %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = paste0("//section[contains(@class, 'modul modul--newslist')]/div[", 
                                        i, "]/div[contains(@class, 'row')][", 
                                        j, "]/div/a[contains(@class, 'box')]")) %>% 
    rvest::html_attr("href") %>%
    paste0("https://www.nordbayern.de",.) -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = paste0("//section[contains(@class, 'modul modul--newslist')]/div[", 
                                        i, "]/div[contains(@class, 'row')][", 
                                        j, "]/div/a//time")) %>% 
    rvest::html_text(., trim = TRUE) %>% 
    stringr::str_extract("[0-9]+\\.[0-9]+\\.[0-9]+") %>% 
    lubridate::dmy() -> item_pubdate
  
  print(paste0(i," - ", j))
  if(length(item_title) == 0){
    df <- data.frame()
  } else if(length(item_pubdate) == 1){
    df <- data.frame(item_title, item_link, item_pubdate)
  } else {
    df <- data.frame(item_title, item_link, item_pubdate=NA)
  }

  return(df)  
                           
}

nord_gothr_largechunk <- function(html, i){
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = paste0("//section[contains(@class, 'modul modul--newslist')]/div[", 
                                        i, "]/div[contains(@class, 'row')]//a//h6")) %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = paste0("//section[contains(@class, 'modul modul--newslist')]/div[", 
                                        i, "]/div[contains(@class, 'row')]/div/a[contains(@class, 'box')]")) %>% 
    rvest::html_attr("href") %>%
    paste0("https://www.nordbayern.de",.) -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = paste0("//section[contains(@class, 'modul modul--newslist')]/div[", 
                                        i, "]/div[contains(@class, 'row')]/div/a//time")) %>% 
    rvest::html_text(., trim = TRUE) %>% 
    stringr::str_extract("[0-9]+\\.[0-9]+\\.[0-9]+") %>% 
    lubridate::dmy() -> item_pubdate
  
  if(length(item_title) == 0){
    df <- data.frame()
  } else if(length(item_pubdate) == length(item_title)){
    df <- data.frame(item_title, item_link, item_pubdate)
  } else {
    (1:length(item_title)) %>% purrr::map_df(~nord_gothr_elem(html=html, i, .)) -> df
  }
  print(i)
  return(df)
}

nordb_getlink <- function(category, startdate){
  remDr$navigate(paste0("https://www.nordbayern.de/", category))
  
  i <- TRUE
  j <- 1
  
  while (i) {
    
    # remDr$getPageSource()[[1]] %>%   
    #   rvest::read_html() %>% 
    #   rvest::html_elements(xpath = "//button[contains(@class, 'btn btn__center')]") %>% 
    #   rvest::html_text(trim = TRUE) -> mehrbutton
    # 
    # while (length(mehrbutton) == 0) {
    #   Sys.sleep(.1)
    #   print("oop")
    #   remDr$getPageSource()[[1]] %>%   
    #     rvest::read_html() %>% 
    #     rvest::html_elements(xpath = "//button[contains(@class, 'btn btn__center')]") %>% 
    #     rvest::html_text(trim = TRUE) -> mehrbutton
    # }
    j <- j + 1
    
    if(j > 25){
      remDr$getPageSource()[[1]] %>%
        rvest::read_html() %>% 
        rvest::html_elements(xpath = paste0("//section[contains(@class, 'modul modul--newslist')]/div/div[contains(@class, 'row')]/div/a//time")) %>% 
        rvest::html_text(., trim = TRUE) %>% 
        stringr::str_extract("[0-9]+\\.[0-9]+\\.[0-9]+") %>% 
        lubridate::dmy() -> item_pubdate
      print(item_pubdate[length(item_pubdate)])
      j <- 1
      
      if(item_pubdate[length(item_pubdate)] < as.Date(startdate)){
        i <- FALSE

      }
      print(i)
    }
    sclick()
    
    Sys.sleep(.3)
  }
  
  remDr$getPageSource()[[1]]-> html
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//section[contains(@class, 'modul modul--newslist')]/div") %>% length() -> n

  (1:n) %>% purrr::map_df(~nord_gothr_largechunk(html=html, .)) -> df
  
  return(df)
}


#function for geting links from page
nordb_getlink("politik", "2021-12-31") -> valid_links1
save(valid_links1, file = "nordb.RData")


c("region/polizeiberichte", "region/ansbach", "region/bamberg", "region/bayreuth", "region/erlangen",
  "region/forchheim", "region/fuerth", "region/gunzenhausen", "region/herzogenaurach",
  "region/hoechstadt", "region/neumarkt", "region/neustadt-aisch-bad-windsheim", "region/nuernberg",
  "region/nuernberger-land", "region/regensburg", "region/roth", "region/schwabach",
  "region/weißenburg") %>%
 purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links2



remDr$close()
z <- rD$server$stop()
