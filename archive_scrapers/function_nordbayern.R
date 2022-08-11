require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(#5678L, 
                                                               #5679L, 
                                                               #5680L, 
                                                               #5681L, 
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

#sclick()


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
      remDr$getPageSource()[[1]] %>%
        rvest::read_html() %>% 
        rvest::html_elements(xpath = paste0("//section[contains(@class, 'modul modul--newslist')]//button[contains(@class, 'btn btn__center btn__loadmore')]")) %>% 
        rvest::html_text(., trim = TRUE) -> buttonthere
        
      if(length(buttonthere)==0){
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
  
  print(item_pubdate[length(item_pubdate)])
  
  return(df)
}


#function for geting links from page

#save(valid_links1, file = "nordb.RData")

c("politik") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links29

c("region/polizeiberichte") %>%
 purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links2

c("region/ansbach") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links3

c("region/bamberg") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links4

c("region/bayreuth") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links5

c("region/erlangen") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links6

###########################################
c("region/forchheim") %>%
  purrr::map_df(~nordb_getlink(. , "2022-06-23")) -> valid_links7

###########################################
c("region/fuerth") %>%
  purrr::map_df(~nordb_getlink(. , "2022-02-10")) -> valid_links8

c("region/gunzenhausen") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links9

c("region/herzogenaurach") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links10

c("region/hoechstadt") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links11

c("region/neumarkt") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links12

c("region/neustadt-aisch-bad-windsheim") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links13

##############################################################
c("region/nuernberg") %>%
  purrr::map_df(~nordb_getlink(. , "2022-03-31")) -> valid_links14

c("region/nuernberger-land") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links15

c("region/regensburg") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links16

c("region/roth") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links17

c("region/schwabach") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links18

c("region/weißenburg") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links19

c("wirtschaft") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links20

c("panorama") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links21

c("kultur") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links22

c("freizeit-events") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links23

c("essen-trinken") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links24

c("boulevard") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links25

c("ratgeber") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links26

c("tv") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links27

c("sport") %>%
  purrr::map_df(~nordb_getlink(. , "2021-12-31")) -> valid_links28

save.image("nordb_.RData")

remDr$close()
z <- rD$server$stop()
