##require(magrittr)

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
    
    if (pub == "nordbayern.de") {
        html %>% rvest::html_elements(xpath = "//div[contains(@class, 'paywall')]") %>%
            rvest::html_text() -> text_
        value <- !identical(text_, character(0))
    }
    
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
    
    if (pub == "RND" || pub == "Ostsee-Zeitung" || pub == "LVZ" || pub == "KN") {
        html %>% rvest::html_elements(xpath = "//div[contains(@class, 'paywalledContent')]") %>%
            rvest::html_text() -> text_
        value <- !identical(text_, character(0))
    }
    
    if (pub == "Achse des Guten") {
        html %>% rvest::html_elements(xpath = "//title") %>%
            rvest::html_text() -> text_
        value <- identical(text_, "403 Forbidden")
    }
    
    if (pub == "akweb") {
        html %>% rvest::html_elements(xpath = "//section[contains(@class, 'wp-block-ak-subscription')]") %>%
            rvest::html_text() -> text_
        value <- !identical(text_, character(0))
    }
    
    
    if (pub == "Epoch Times") {
        html %>% rvest::html_elements(xpath = "//div[contains(@id, 'premium-content')]") %>%
            rvest::html_text() -> text_
        value <- !identical(text_, character(0))
    }
    
    if (pub == "Freitag") {
        html %>% rvest::html_elements(xpath = "//div[contains(@class, 'c-paywall-banner')]") %>%
            rvest::html_text() -> text_
        value <- !identical(text_, character(0))
    }
    
    if (pub == "DWN") {
        html %>% rvest::html_elements(xpath = "//div[contains(@class, 'message notice')]") %>%
            rvest::html_text() -> text_
        value <- !identical(text_, character(0))
        if(identical(text_, character(0))) {
            html %>% rvest::html_elements(xpath = "//div[contains(@class, 'gustly')]") %>% 
                rvest::html_text() -> text_
        }
        value <- !identical(text_, character(0))
        
        if(identical(text_, character(0))) {
            html %>% rvest::html_elements(xpath = "//div[contains(@id, 'article-teaser-blocks')]") %>% 
                rvest::html_text() -> text_
        }
        value <- !identical(text_, character(0))
        
    }
    
    if (pub == "Compact") {
        html %>% rvest::html_elements(xpath = "//div[contains(@id, 'wpmem_restricted_msg')]") %>%
            rvest::html_text() -> text_
        value <- !identical(text_, character(0))
    }
    
    
    if (pub == "Junge Freiheit") {
        html %>% rvest::html_elements(xpath = "//div[contains(@class, 'paywall-content-block')]") %>%
            rvest::html_text() -> text_
        value <- !identical(text_, character(0))
    }
    
    if (pub == "Jungle World") {
        html %>% rvest::html_elements(xpath = "//div[contains(@class, 'subscription-only-block')]") %>%
            rvest::html_text() -> text_
        value <- !identical(text_, character(0))
    }
    
    if (pub == "Junge Welt") {
        html %>% rvest::html_elements(xpath = "//a[contains(@title, 'Onlineabo abschließen')]") %>%
            rvest::html_text() -> text_
        value <- !identical(text_, character(0))
    }
    
    if (pub == "Jacobin") {
        html %>% rvest::html_elements(xpath = "//input[contains(@id, 'username')]") %>%
            rvest::html_text() -> text_
        value <- !identical(text_, character(0))
        if(identical(text_, character(0))) {
            html %>% rvest::html_elements(xpath = "//svg[contains(@class, 'spinner__svg')]") -> text_
            value <- (length(text_) > 0)
        }
        
    }
    
    if (pub == "blaetter.de") {
        html %>% rvest::html_elements(xpath = "//a[contains(@class, 'button--buy')]") %>%
            rvest::html_text() -> text_
        value <- !identical(text_, character(0))
    }
    
    
    

    
    
    
                                        # if (pub == "Badische Zeitung") {
                                        #   html %>% rvest::html_elements(xpath = "//section[contains(@id, 'regWalli')]") %>%
                                        #     rvest::html_text() -> text_
                                        #   value <- !identical(text_, character(0))
                                        # }
                                        # 
                                        # if (pub == "Hildesheimer Allgemeine Zeitung") {
                                        #   html %>% rvest::html_elements(xpath = "//div[contains(@class, 'o-paywall')]") %>%
                                        #     rvest::html_text() -> text_
                                        #   value <- !identical(text_, character(0))
                                        # }
                                        #
                                        # if (pub == "Saarbrücker Zeitung") {
                                        #   html %>% rvest::html_elements(xpath = "//span[contains(@title, 'Jetzt weiterlesen!')]") %>%
                                        #     rvest::html_text() -> text_
                                        #   value <- !identical(text_, character(0))
                                        # }
                                        #
                                        # if (pub == "Zeit") {
                                        #   html %>% rvest::html_elements(xpath = "//aside[contains(@id, 'paywall')]") %>%
                                        #     rvest::html_text() -> text_
                                        #   value <- !identical(text_, character(0))
                                        # }
    

    

    
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
