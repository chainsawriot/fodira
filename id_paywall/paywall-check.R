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
  
  if (pub == "FAZ") {
    html %>% rvest::html_elements(xpath = "//section[contains(@class, 'atc-ContainerPaywall')]") %>% 
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
  }
  
  
  
  if (pub == "SZ") {
    html %>% rvest::html_elements(xpath = "//offer-page") %>% 
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
  }
  
  
  if (pub == "Handelsblatt") {
    html %>% rvest::html_elements(xpath = "//div[contains(@class, 'o-paywall__content')]") %>% 
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
  }
  
  if (pub == "Welt") {
    html %>% rvest::html_elements(xpath = "//div[contains(@class, 'contains_walled_content')]") %>% 
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
  }
  
  

  
  if (pub == "Bild") {
    html %>% rvest::html_elements(xpath = "//div[contains(@class, 'offer-module')]") %>% 
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
    if (!value) {
      html %>% rvest::html_elements(xpath = "//body[contains(@id, 'article')]//div/div/h2") %>% 
        rvest::html_text() -> text_
      if(length(text_)>0){
        if(text_ == "Warum sehe ich BILD.de nicht?"){
          value <- TRUE
        } else {
          value <- FALSE
        }
      }else {
        value <- FALSE
      }

      
    }
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

`tab-ton-bild`$paywall <- checkfunction_2(tab=`tab-ton-bild`, folder = "c://paywall_check/html-ton-bild")

`tab-welt-faz`$paywall <- checkfunction_2(tab=`tab-welt-faz`, folder = "c://paywall_check/html-welt-faz")




test <- rvest::read_html("C://paywall_check/html-ton-bild/bf348d08fcb7c24ccd5a1ab084ca673c274d100f_2022-12-21_13_02_14.html")

