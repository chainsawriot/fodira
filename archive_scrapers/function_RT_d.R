#install.packages("binman")
require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 
                                                               #5679L, 
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

#pjs_session$go("https://deutsch.rt.com/aktuell/")

remDr$navigate("https://deutsch.rt.com/aktuell/")

rt_click_function <- function(){
  elem <- remDr$findElement(using = "xpath", "//div[contains(@class, 'Section-root')]//button[contains(@class, 'Button-root')]")
  elem$clickElement()
}

sclick <- purrr::safely(rt_click_function, quiet = TRUE)

#sclick()

rt_gothr_elem <- function(html, i, j){
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = paste0("//div[contains(@class, 'Section-container Section-isRow-isTop-isWrap Listing-root')]//div[contains(@class, 'HeaderNews-root')]/a")) %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = paste0("//div[contains(@class, 'Section-container Section-isRow-isTop-isWrap Listing-root')]//div[contains(@class, 'HeaderNews-root')]/a")) %>% 
    rvest::html_attr("href") %>%
    paste0("https://deutsch.rt.com",.) -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = paste0("//div[contains(@class, 'Section-container Section-isRow-isTop-isWrap Listing-root')]//time")) %>% 
    rvest::html_attr("datetime") %>%  
    lubridate::ymd_hms() -> item_pubdate
  

  df <- data.frame(item_title, item_link, item_pubdate)

  return(df)  
                           
}


rt_getlink <- function(category, startdate){
  remDr$navigate(paste0("https://deutsch.rt.com/", category))
  
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
        rvest::html_elements(xpath = paste0("//div[contains(@class, 'Section-container Section-isRow-isTop-isWrap Listing-root')]//time")) %>% 
        rvest::html_attr("datetime") %>%  
        lubridate::ymd_hms() -> item_pubdate
      print(item_pubdate[length(item_pubdate)])
      j <- 1
      
      if(item_pubdate[length(item_pubdate)] < as.Date(startdate)){
        i <- FALSE

      }
      remDr$getPageSource()[[1]] %>%
        rvest::read_html() %>%
        rvest::html_elements(xpath = paste0("//div[contains(@class, 'Section-root')]//button[contains(@class, 'Button-root')]")) %>%
        rvest::html_text(., trim = TRUE) -> buttonthere

      if(length(buttonthere)==0){
        i <- FALSE
      } else if(buttonthere[1]!="Weiter"){
          i <- FALSE
      }
      print(i)
    }
    sclick()
    
    Sys.sleep(.3)
  }
  
  remDr$getPageSource()[[1]]-> html
  
  rt_gothr_elem(html=html) -> df
  
  print(item_pubdate[length(item_pubdate)])
  
  return(df)
}


c("aktuell/", "themen/ukraine-krise/", "themen/ausland/",
  "themen/inland/", "themen/corona-pandemie/", "themen/russland/",
  "themen/kampagne-gegen-rt-de/", "themen/wirtschaft/",
  "tag/analyse/", "meinung/", "video/", "themen/in-eigener-sache/") %>%
  purrr::map_df(~rt_getlink(. , "2021-12-31")) -> valid_links_1

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "RT deutsch", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "RT_1.RDS")

remDr$close()
z <- rD$server$stop()
