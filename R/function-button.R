### functions to click away cookie-prompts and other prompts

### waitfindElem waits for an element to load before clicking it

.waitfindElem <- function(xpath, trytime = 3, remDr=remDr){
  while (trytime > 0) {
    webElem <- tryCatch({
      suppressMessages({
        remDr$findElement(using = "xpath", xpath)
      })
    },
    error = function(e){NULL})
    
    if(!is.null(webElem)){
      trytime <- 0
      # print("found")
    } else {
      trytime <- trytime - 0.1
      Sys.sleep(0.1)
    }
  }
  return(webElem)
}

### waitfindElems waits for an element to load before clicking it,
### returns multiple elements

.waitfindElems <- function(xpath, trytime = 3, remDr=remDr){
  while (trytime > 0) {
    webElem <- tryCatch({suppressMessages({
      remDr$findElements(using = "xpath", xpath)
    })
    },
                        error = function(e){NULL})
    if(!is.null(webElem)){
      trytime <- 0
      # print("found")
    } else {
      trytime <- trytime - 0.1
      Sys.sleep(0.1)
    }
  }
  return(webElem)
}

### waitfindElem_shadow waits for an element that is in shadow-root 
### to load before clicking it

.waitfindElem_shadow <- function(path, trytime = 3, remDr=remDr){
  shadow_rd <- shadowr::shadow(remDr) 
  while (trytime > 0) {
    element <- tryCatch({suppressMessages({
      shadowr::find_elements(shadow_rd, path)[[1]]
    })
    },
    error = function(e){NULL})
    if(!is.null(element)){
      trytime <- 0
      #print("found")
    } else {
      trytime <- trytime - 0.1
      Sys.sleep(0.1)
    }
  }
  return(element)
}

### Clickaway... clicks away a prompt. 
### You just need to tell it which page you're on
### Zeit is still solved with a less nice wait, because waitElem 
### does have problems on that page


.clickaway <- function(clickaway="none", remDr=remDr){
  # if (clickaway == "Achse des Guten"){
  #   Sys.sleep(1) 
  #   webElem <-NULL
  #   webElem <- .waitfindElem_shadow(path =  'div[class="cmp_button cmp_button_bg cmp_button_font_color"]', remDr=remDr)
  #   #loop until element is found
  #   if(!is.null(webElem)){
  #     webElem$clickElement()
  #   }
  # }
  # Does not work - shadow-root closed
  if (clickaway == "Allgemeine Zeitung"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//a[contains(@class, 'purWrapper__button')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  if (clickaway == "akweb"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//button[contains(@class, 'button privacy-consent__agree-button')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  if (clickaway == "Badische Zeitung"){
    Sys.sleep(1) 
    webElem <-NULL
    webElem <- .waitfindElem_shadow(path =  'button[class="sc-eDvSVe dGhoDi"]', remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  if (clickaway == "Bild" || clickaway == "Berliner Zeitung" || clickaway == "Cicero" || clickaway == "FAZ" ||
      clickaway == "Freitag" || clickaway == "KN" || clickaway == "LVZ" || clickaway == "Nordkurier" ||
      clickaway == "NTV" || clickaway == "Ostsee-Zeitung" || clickaway == "RND" || clickaway == "Stern" ||
      clickaway == "SZ" || clickaway == "Tag24" || clickaway == "TAZ" || clickaway == "Vice" || clickaway == "Welt"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//iframe[contains(@title, 'SP Consent Message')]", remDr=remDr)
    if(!is.null(webElem)){
      remDr$switchToFrame(webElem)
      
      webElem <- .waitfindElem(xpath = "//button[contains(@class, 'sp_choice_type_11')]", remDr=remDr)
      if(!is.null(webElem)){
        webElem$clickElement()
      }
    }
    remDr$switchToFrame(NULL)
  }
  if (clickaway == "Telepolis"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//iframe[contains(@title, 'SP Consent Message')]", remDr=remDr)
    if(!is.null(webElem)){
      remDr$switchToFrame(webElem)
      
      webElem <- .waitfindElems(xpath = "//button[contains(@class, 'sp_choice_type_11')]", remDr=remDr)
      if(!is.null(webElem)){
        webElem[[2]]$clickElement()
      }
    }
    remDr$switchToFrame(NULL)
  }
  if (clickaway == "blaetter.de"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//button[contains(@class, 'agree-button eu-cookie-compliance-default-button button')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  if (clickaway == "Compact"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//a[contains(@class, 'cc-btn cc-allow')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  if (clickaway == "DWN"){
    webElem <-NULL
    Sys.sleep(1)
    webElem <- .waitfindElem(xpath =  "//button[contains(@id, 'ck_accept')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  if (clickaway == "Epoch Times"){
    webElem <-NULL
    Sys.sleep(1)
    webElem <- .waitfindElem(xpath =  "//button[contains(@id, 'CybotCookiebotDialogBodyLevelButtonLevelOptinAllowAll')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  if (clickaway == "Focus" || clickaway == "Handelsblatt" || clickaway == "Tagesspiegel"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//iframe[contains(@title, 'Iframe title')]", remDr=remDr)
    if(!is.null(webElem)){
      remDr$switchToFrame(webElem)
      
      webElem <- .waitfindElem(xpath = "//button[contains(@class, 'sp_choice_type_11')]", remDr=remDr)
      if(!is.null(webElem)){
        webElem$clickElement()
      }
    }
    remDr$switchToFrame(NULL)
  }
  if (clickaway == "T-Online"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//iframe[contains(@id, 'sp_message_iframe_741194')]", remDr=remDr)
    if(!is.null(webElem)){
      remDr$switchToFrame(webElem)
      webElem <- .waitfindElems(xpath = "//button[contains(@class, 'sp_choice_type_11')]", remDr=remDr)
      if(!is.null(webElem)){
        webElem[[2]]$clickElement()
      }
    }
    remDr$switchToFrame(NULL)
  }
  if (clickaway == "Free21"){
    webElem <-NULL
    Sys.sleep(1)
    webElem <- .waitfindElem(xpath =  "//a[contains(@class, '_brlbs-btn _brlbs-btn-accept-all _brlbs-cursor')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  if (clickaway == "Freie Welt"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//a[contains(@id, 'cmbutton-accept')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  if (clickaway == "Hildesheimer Allgemeine Zeitung"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//button[contains(@class, '_10ihg6sc')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  if (clickaway == "Berliner Morgenpost" || clickaway == "Hamburger MoPo" || clickaway == "TA" || 
      clickaway == "WAZ" || clickaway == "Weser Kurier"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//a[contains(@class, 'cmptxt_btn_yes')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  # 
  # if (clickaway == "HNA"){
  #   Sys.sleep(1) 
  #   webElem <-NULL
  #   webElem <- .waitfindElem_shadow(path =  'div[class="cmp_button cmp_button_bg cmp_button_font_color"]', remDr=remDr)
  #   #loop until element is found
  #   if(!is.null(webElem)){
  #     webElem$clickElement()
  #   }
  # }
  ## shadow-root - doesn't work!
  
  if (clickaway == "Jacobin"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//button[contains(@class, 'relative px-4 py-3')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  
  if (clickaway == "Jouwatch" || clickaway == "Klasse gegen Klasse" || clickaway == "Zuerst"){
    webElem <-NULL
    Sys.sleep(.5)
    webElem <- .waitfindElem(xpath =  "//button[contains(@class, 'cmplz-accept')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  
  if (clickaway == "Junge Freiheit" || clickaway == "DieUnbestechlichen"){
    webElem <-NULL
    
    webElem <- .waitfindElem(xpath =  "//a[contains(@class, '_brlbs-btn-accept-all')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  
  if (clickaway == "Jungle World"){
    webElem <-NULL
    
    webElem <- .waitfindElem(xpath =  "//a[contains(@class, 'cc_btn_accept_all')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  
  if (clickaway == "NeoPresse"){
    webElem <-NULL
    
    webElem <- .waitfindElem(xpath =  "//a[contains(@class, 'cc-dismiss')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }

  # if (clickaway == "Neues Deutschland"){
  #   Sys.sleep(1)
  #   webElem <-NULL
  #   webElem <- .waitfindElem_shadow(path =  'div[class="cmp_button cmp_button_bg"]', remDr=remDr)
  #   #loop until element is found
  #   if(!is.null(webElem)){
  #     webElem$clickElement()
  #   }
  # }
  # # shadow-root - doesn't work!
  
  if (clickaway == "nordbayern.de"){
    webElem <-NULL
    
    webElem <- .waitfindElem(xpath =  "//a[contains(@class, 'cmpboxbtnyes')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  
  if (clickaway == "Opposition24"){
    webElem <-NULL
    
    webElem <- .waitfindElem(xpath =  "//button[contains(@id, 'cc-cookie-accept')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  
  if (clickaway == "Ossietzky"){
    webElem <-NULL
    Sys.sleep(2)
    webElem <- .waitfindElem(xpath =  "//div[contains(@class, 'module_column sub_column col4-1 tb_6yys554 last')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  
  if (clickaway == "Politplatschquatsch"){
    webElem <-NULL
    Sys.sleep(2)
    webElem <- .waitfindElem(xpath =  "//a[contains(@id, 'cookieChoiceDismiss')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  if (clickaway == "Saarbrücker Zeitung"){
    webElem <-NULL
    Sys.sleep(2)
    webElem <- .waitfindElem(xpath =  "//a[contains(@id, 'consentAccept')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  
  if (clickaway == "Pravda TV"){
    webElem <-NULL
    Sys.sleep(2)
    webElem <- .waitfindElem(xpath =  "//button[contains(@class, ' css-5a47r')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
      webElem <- .waitfindElem(xpath =  "//div[contains(@class, 'qc-cmp2-buttons-desktop')]/button[contains(@class, ' css-k8o10q')]", remDr=remDr)
      if(!is.null(webElem)){
        webElem$clickElement()
      }
    }
  }
  
  if (clickaway == "Stuttgarter Zeitung"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//iframe[contains(@title, 'Consent Message')]", remDr=remDr)
    if(!is.null(webElem)){
      remDr$switchToFrame(webElem)
      
      webElem <- .waitfindElem(xpath = "//button[contains(@class, 'sp_choice_type_11')]", remDr=remDr)
      if(!is.null(webElem)){
        webElem$clickElement()
      }
    }
    remDr$switchToFrame(NULL)
  }
  
  if (clickaway == "Tichys Einblick"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//iframe[contains(@title, 'Iframe title')]", remDr=remDr)
    if(!is.null(webElem)){
      remDr$switchToFrame(webElem)
      
      webElem <- .waitfindElem(xpath = "//button[contains(@class, 'sp_choice_type_11')]", remDr=remDr)
      if(!is.null(webElem)){
        webElem$clickElement()
      }
    }
    remDr$switchToFrame(NULL)
    
    webElem <- .waitfindElem(xpath = "//div[contains(@class, 'link-continue-reading')]/a", remDr=remDr)
    if(!is.null(webElem)){
      webElem$clickElement()
    }

  }
  
  if (clickaway == "TRT"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//button[contains(@class, '_hj-ONMkJ__MinimizedWidgetMessage__close')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
    
    
    webElem <- .waitfindElem(xpath =  "//div[contains(@class, 'gdprCloser')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  
  if (clickaway == "Zeit"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//iframe[contains(@title, 'SP Consent Message')]", remDr=remDr)
    if(!is.null(webElem)){
      remDr$switchToFrame(webElem)
      Sys.sleep(1) 
      webElem <- .waitfindElem(xpath = "//button[contains(@class, 'sp_choice_type_11')]", remDr=remDr)
      if(!is.null(webElem)){
        webElem$clickElement()
      }
    }
    remDr$switchToFrame(NULL)
  }
}

