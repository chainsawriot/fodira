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
  
  if (pub == "Berliner Zeitung") {
    html %>% rvest::html_elements(xpath = "//div[contains(@class, 'paywall_overlay__CR1uO')]") %>% 
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
  }
  
  if (pub == "RP Online") {
    html %>% rvest::html_elements(xpath = "//div[contains(@class, 'park-paywall-content')]") %>% 
      rvest::html_text() -> text_
    if(identical(text_, character(0))) {
      html %>% rvest::html_elements(xpath = "//div[contains(@class, 'park-paywall-content')]") %>% 
        rvest::html_text() -> text_
    }
    if(identical(text_, character(0))) {
      html %>% rvest::html_elements(xpath = "//p[contains(@class, 'text-blurred')]") %>% 
        rvest::html_text() -> text_
    }
    value <- !identical(text_, character(0))
  }
  
  if (pub == "Hildesheimer Allgemeine Zeitung") {
    html %>% rvest::html_elements(xpath = "//div[contains(@class, 'o-paywall')]") %>% 
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
  }
  
  
  if (pub == "TA") {
    html %>% rvest::html_elements(xpath = "//p[contains(@class, 'obfuscated')]") %>% 
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
  }
  
  # if (pub == "nordbayern.de") {
  #   html %>% rvest::html_elements(xpath = "//div[contains(@class, 'paywall')]") %>% 
  #     rvest::html_text() -> text_
  #   value <- !identical(text_, character(0))
  # }
  
  if (pub == "Stuttgarter Zeitung") {
    html %>% rvest::html_elements(xpath = "//div[contains(@id, 'taboola-below-paid-article-thumbnails')]") %>%
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
  }
  
  if (pub == "Tagesspiegel") {
    html %>% rvest::html_elements(xpath = "//div[contains(@class, 'article--paid')]") %>%
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
  }
  
  if (pub == "Berliner Morgenpost") {
    html %>% rvest::html_elements(xpath = "//div[contains(@id, 'paywall-container')]") %>%
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
  }
  
  if (pub == "WAZ") {
    html %>% rvest::html_elements(xpath = "//div[contains(@id, 'paywall-container')]") %>%
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
  }
  
  
  if (pub == "Allgemeine Zeitung") {
    html %>% rvest::html_elements(xpath = "//div[contains(@class, 'storyElementWrapper__paywallContainer')]") %>%
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
    if(!value) {
      html %>% rvest::html_elements(xpath = "//div[contains(@class, 'vrm-cce__paywall')]") %>% 
        rvest::html_text() -> text_
      value <- !identical(text_, character(0))
    }
  }
  
 
  if (pub == "Weser Kurier") {
    html %>% rvest::html_elements(xpath = "//div[contains(@class, 'paywall__overlay')]") %>%
      rvest::html_text() -> text_
    value <- !identical(text_, character(0))
  }
  
  if (pub == "Hamburger MoPo") {
    html %>% rvest::html_elements(xpath = "//div[contains(@id, 'paywall')]") %>%
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


`tab-beze-nordk`$paywall <- checkfunction_2(tab=`tab-beze-nordk`, 
                                            folder = "c://paywall_check/regional_sample/html-beze-nordk")

`tab-haz-rp`$paywall <- checkfunction_2(tab=`tab-haz-rp`, 
                                        folder = "c://paywall_check/regional_sample/html-haz-rp")


`tab-kn-ta`$paywall <- checkfunction_2(tab=`tab-kn-ta`, 
                                        folder = "c://paywall_check/regional_sample/html-kn-ta")

`tab-me-nordb`$paywall <- checkfunction_2(tab=`tab-me-nordb`, 
                                       folder = "c://paywall_check/regional_sample/html-me-nordb")

`tab-st-ba`$paywall <- checkfunction_2(tab=`tab-st-ba`, 
                                       folder = "c://paywall_check/regional_sample/html-st-ba")

`tab-tag-bemo`$paywall <- checkfunction_2(tab=`tab-tag-bemo`, 
                                          folder = "c://paywall_check/regional_sample/html-tag-bemo")

`tab-waz-allg`$paywall <- checkfunction_2(tab=`tab-waz-allg`, 
                                          folder = "c://paywall_check/regional_sample/html-waz-allg")


`tab-we-hamo`$paywall <- checkfunction_2(tab=`tab-we-hamo`, 
                                          folder = "c://paywall_check/regional_sample/html-we-hamo")

test <- rvest::read_html("C://paywall_check/regional_sample/html-haz-rp/37fe189d4aa0029a90f79872e82f0cc9d3ccd6f4_2022-10-31_18_48_28.html")

