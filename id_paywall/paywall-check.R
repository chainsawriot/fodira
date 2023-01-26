`tab-cic-vi` <- readRDS("c://paywall_check/tab-cic-vi.RDS")

require(magrittr)

check_paywall <- function(html, pub = ""){
  value <- NA
  if (pub == "Cicero") {
    html %>% rvest::html_elements(xpath = "//div[contains(@class, 'paywall-header')]") %>% 
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
  }
  if (pub == "Spiegel") {
    html %>% rvest::html_elements(xpath = "//div[contains(@data-target-id, 'paywall')]") %>% 
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
  }
  
  if (pub == "Stern") {
    html %>% rvest::html_elements(xpath = "//section[contains(@class, 'paid-barrier--context-article')]") %>% 
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
  }
  
  if (pub == "SZ") {
    html %>% rvest::html_elements(xpath = "//offer-page") %>% 
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
  }
  
  

  
  return(value)
}





checkfunction <- function(file, pub, folder) {
  value <- check_paywall(rvest::read_html(paste(folder, file, sep = "/")), pub = pub)
  return(value)
}

checkfunction_2 <- function(tab, folder) {
  value <- c()
  for (i in 1:nrow(tab)) {
    value <- c(value, checkfunction(stringr::str_replace_all(tab$htmlfile[i], ":", "_"), tab$pub[i], folder = folder))
    print(i)
  }
  return(value)
}

`tab-cic-vi`$paywall <- checkfunction_2(tab=`tab-cic-vi`, folder = "c://paywall_check/html-cic-vi")

`tab-sp-rnd`$paywall <- checkfunction_2(tab=`tab-sp-rnd`, folder = "c://paywall_check/html-sp-rnd")

`tab-st-zt`$paywall <- checkfunction_2(tab=`tab-st-zt`, folder = "c://paywall_check/html-st-zt")

`tab-sue-han`$paywall <- checkfunction_2(tab=`tab-sue-han`, folder = "c://paywall_check/html-sue-han")


test <- rvest::read_html("https://www.stern.de/panorama/stern-crime/wunstorf-in-trauer--viele-fragen-nach-toetung-von-14-jaehrigem-33137330.html")
