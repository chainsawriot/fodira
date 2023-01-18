### functions to click away cookie-prompts and other prompts

### waitfindElem waits for an element to load before clicking it

.waitfindElem <- function(xpath, trytime = 3, remDr=remDr){
  while (trytime > 0) {
    webElem <- tryCatch({remDr$findElement(using = "xpath", xpath)},
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
    webElem <- tryCatch({remDr$findElements(using = "xpath", xpath)},
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
    element <- tryCatch({shadowr::find_elements(shadow_rd, path)[[1]]},
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




# ### Testfunctions

# rD <- RSelenium::rsDriver(browser = "firefox",
#                           #chromever = "103.0.5060.134",
#                           port = sample(c(5678L,
#                             5679L, 5680L, 5681L,
#                             5682L), size = 1),
#                           #phantomver = "2.1.1",
#                           #extraCapabilities = fprof,
#                           check = TRUE, verbose = FALSE)
# 
# remDr <- rD[["client"]]
# 
# 
# remDr$navigate("https://www.allgemeine-zeitung.de/politik/politik-deutschland/medien-boris-pistorius-wird-neuer-verteidigungsminister-2226846")
# .clickaway(clickaway = "Allgemeine Zeitung", remDr = remDr)
# 
# remDr$navigate("https://www.zeit.de/politik/ausland/karte-ukraine-krieg-russland-frontverlauf-truppenbewegungen")
# .clickaway(clickaway = "Zeit", remDr = remDr)
# 
# remDr$navigate("https://www.bild.de/politik/inland/politik-inland/judenhass-in-luetzerath-klima-extremisten-verherrlichen-terroristin-82560194.bild.html")
# .clickaway(clickaway = "Bild", remDr = remDr)
# 
# remDr$navigate("https://www.badische-zeitung.de/alle-fakten-zum-jugendticket-fuer-busse-und-bahnen-in-baden-wuerttemberg")
# .clickaway(clickaway = "Badische Zeitung", remDr = remDr)
# 
# #remDr$navigate("https://www.achgut.com/artikel/peng_pistorius_wird_verteidigungsminister")
# #.clickaway(clickaway = "Achse des Guten", remDr = remDr) ## Doesn't work --> shadowroot closed
# 
# remDr$navigate("https://www.berliner-zeitung.de/news/polizei-razzia-wegen-kinderpornografie-in-berlin-li.307767")
# .clickaway(clickaway = "Berliner Zeitung", remDr = remDr)
# 
# remDr$navigate("https://www.blaetter.de/ausgabe/2023/januar/rechte-systemsprenger-die-politik-mit-dem-mythos")
# .clickaway(clickaway = "blaetter.de", remDr = remDr)
# 
# remDr$navigate("https://www.akweb.de/politik/china-foxconn-proteste-arbeiterinnen-klassenuebergreifende-solidaritaet/")
# .clickaway(clickaway = "akweb", remDr = remDr)
# 
# remDr$navigate("https://www.morgenpost.de/politik/article237389871/verteidigungsministerium-lambrecht-nachfolge-pistorius-spd.html")
# .clickaway(clickaway = "Berliner Morgenpost", remDr = remDr)
# 
# remDr$navigate("https://www.cicero.de/kultur/luisa-neubauer-und-die-klimabewegung-ein-bisschen-jeanne-darc-ein-bisschen-romy-schneider")
# .clickaway(clickaway = "Cicero", remDr = remDr)
# 
# remDr$navigate("https://www.compact-online.de/schuldkult-die-luege-vom-voelkermord-an-den-herero/")
# .clickaway(clickaway = "Compact", remDr = remDr)
# 
# remDr$navigate("https://deutsche-wirtschafts-nachrichten.de/701870/Krise-in-Schwedt-PCK-Raffinerie-jetzt-vom-Wohlwollen-Polens-abhaengig")
# .clickaway(clickaway = "DWN", remDr = remDr) ### Sometimes ad in the way
# 
# remDr$navigate("https://www.epochtimes.de/meinung/auf-ein-wort-die-lage-in-china-ist-ernst-sehr-ernst-a4113430.html")
# .clickaway(clickaway = "Epoch Times", remDr = remDr)
# 
# remDr$navigate("https://www.faz.net/aktuell/wirtschaft/oeffentlicher-dienst-und-die-grosse-unvernunft-hohe-lohnforderungen-18604129.html")
# .clickaway(clickaway = "FAZ", remDr = remDr)
# 
# remDr$navigate("https://www.focus.de/gesundheit/news/detox-fuer-die-leber-dry-january-was-passiert-wenn-sie-einen-monat-auf-alkohol-verzichten_id_10138023.html")
# .clickaway(clickaway = "Focus", remDr = remDr)
# 
# remDr$navigate("https://free21.org/wie-man-wissenschaftliche-studien-liest-und-interpretiert/")
# .clickaway(clickaway = "Free21", remDr = remDr)
# 
# remDr$navigate("https://www.freiewelt.net/nachricht/internationale-studie-deutschland-immer-weniger-wettbewerbsfaehig-10092003/")
# .clickaway(clickaway = "Freie Welt", remDr = remDr)
# 
# remDr$navigate("https://www.freitag.de/autoren/kathrin-gerlof/der-solidaritaetszuschlag-eine-fuer-die-fdp-fuerchterlich-gerechte-angelegenheit")
# .clickaway(clickaway = "Freitag", remDr = remDr)
# 
# remDr$navigate("https://www.handelsblatt.com/politik/deutschland/bundesfinanzhof-streit-um-solidaritaetszuschlag-koennte-vor-dem-bundesverfassungsgericht-landen/28927942.html")
# .clickaway(clickaway = "Handelsblatt", remDr = remDr)
# 
# remDr$navigate("https://www.hildesheimer-allgemeine.de/meldung/boris-pistorius-wird-verteidigungsminister-weil-muss-kabinett-umbauen.html")
# .clickaway(clickaway = "Hildesheimer Allgemeine Zeitung", remDr = remDr)
# 
# remDr$navigate("https://www.mopo.de/hamburg/polizei/unbekannte-gerueche-feuerwehr-versorgt-vier-schulbedienstete/")
# .clickaway(clickaway = "Hamburger MoPo", remDr = remDr)
# 
# #remDr$navigate("https://www.hna.de/kassel/unfall-kassel-vier-unfaelle-polizei-notruf-krankenwagen-zwei-schwerverletzt-blaulicht-news-92032782.html")
# # .clickaway(clickaway = "HNA", remDr = remDr) --> doesn't work
# 
# remDr$navigate("https://jacobin.de/artikel/anti-kolonialisten-wollten-eine-andere-welt-gestalten-postkolonialismus-antikolonialismus-internationalismus-adom-getachew/")
# .clickaway(clickaway = "Jacobin", remDr = remDr)
# 
# remDr$navigate("https://journalistenwatch.com/2023/01/17/eine-gute-nachricht-beim-windkraftausbau-hinken-viele-regionen-hinterher/")
# .clickaway(clickaway = "Jouwatch", remDr = remDr)
# 
# # remDr$navigate("https://www.jungewelt.de/rlk/de/article/443368.wo-bleiben-die-massen.html")
# # .clickaway(clickaway = "Junge Welt", remDr = remDr) # no cookie popup
# 
# remDr$navigate("https://jungefreiheit.de/politik/deutschland/2023/gericht-gegen-tempolimit/")
# .clickaway(clickaway = "Junge Freiheit", remDr = remDr)
# 
# remDr$navigate("https://jungle.world/artikel/2023/02/profitorientierter-notstand")
# .clickaway(clickaway = "Jungle World", remDr = remDr)
# 
# remDr$navigate("https://www.kn-online.de/lokales/kiel/ukraine-krieg-beschert-seehafen-kiel-einen-rueckgang-beim-umschlag-OVMMGBEF5ZKOA2HBGZNPQMSV4Y.html")
# .clickaway(clickaway = "KN", remDr = remDr)
# 
# remDr$navigate("https://www.klassegegenklasse.org/polizeigewalt-und-antisemitismusvorwuerfe-in-luetzerath/")
# .clickaway(clickaway = "Klasse gegen Klasse", remDr = remDr)
# 
# # remDr$navigate("https://lowerclassmag.com/2022/12/25/das-war-ein-gezielter-angriff-auf-die-aktivitaten-der-kurdischen-freiheitsbewegung-in-europa-mord-anschlag-paris/")
# # .clickaway(clickaway = "lowerclassmagazine", remDr = remDr) #no popup
# 
# remDr$navigate("https://www.lvz.de/lokales/nordsachsen/oschatz/streik-in-nordsachsen-mittwoch-und-donnerstag-stehen-viele-busse-still-KGP7OV4Z4EGNK6WEVX4G77SNXY.html")
# .clickaway(clickaway = "LVZ", remDr = remDr)
# 
# #remDr$navigate("https://www.merkur.de/politik/boris-pistorius-neuer-verteidigungsminister-loest-lambrecht-ab-92033359.html")
# # .clickaway(clickaway = "Merkur", remDr = remDr) # -->shadow-root closed
# 
# # remDr$navigate("https://www.mmnews.de/2-uncategorised/193004-neuer-rbb-skandal-1-4-millionen-euro-fuer-rechtsberater")
# # .clickaway(clickaway = "Mmnews", remDr = remDr) # no popup
# 
# # remDr$navigate("https://www.nachdenkseiten.de/?p=92612")
# # .clickaway(clickaway = "Nachdenkseiten", remDr = remDr) # no popup
# 
# remDr$navigate("https://www.neopresse.com/editorial/diese-notenbanken-erhoehen-ihre-goldbestaende/")
# .clickaway(clickaway = "NeoPresse", remDr = remDr)
# 
# # remDr$navigate("https://www.nd-aktuell.de/artikel/1170197.krieg-in-der-ukraine-friedensbewegung-im-dilemma.html")
# # .clickaway(clickaway = "Neues Deutschland", remDr = remDr) # -->shadow-root closed
# 
# remDr$navigate("https://www.nordbayern.de/politik/markus-soder-andert-meinung-und-schliesst-nun-dritte-amtszeit-nicht-aus-1.12915104")
# .clickaway(clickaway = "nordbayern.de", remDr = remDr) 
# 
# remDr$navigate("https://www.nordkurier.de/neustrelitz/so-viele-besucher-wie-nie-zuvor-im-tiergarten-neustrelitz-1851066501.html")
# .clickaway(clickaway = "Nordkurier", remDr = remDr) 
# 
# remDr$navigate("https://www.n-tv.de/politik/Ukrainischer-Innenminister-stirbt-bei-Hubschrauberabsturz-article23852170.html")
# .clickaway(clickaway = "NTV", remDr = remDr) 
# 
# remDr$navigate("https://opposition24.com/spekulatius/luftbesteuerung-eine-mehr-als-100-jahre-alte-idee/")
# .clickaway(clickaway = "Opposition24", remDr = remDr) 
# 
# remDr$navigate("https://www.ossietzky.net/artikel/25-jahre-ossietzky/")
# .clickaway(clickaway = "Ossietzky", remDr = remDr) 
# 
# remDr$navigate("https://www.ossietzky.net/artikel/25-jahre-ossietzky/")
# .clickaway(clickaway = "Ossietzky", remDr = remDr) 
# 
# remDr$navigate("https://www.ostsee-zeitung.de/lokales/nordwestmecklenburg/wismar/wismar-problem-mit-hundekot-in-neu-gestalteter-strasse-schaeden-an-den-pflanzen-ICFO2WJBBFFHDCCL57NYAU7XIM.html")
# .clickaway(clickaway = "Ostsee-Zeitung", remDr = remDr)
# 
# # remDr$navigate("https://www.pi-news.net/2023/01/deutsche-bundespolizei-acab-heisst-jetzt-all-cops-are-beautiful/")
# # .clickaway(clickaway = "PI News", remDr = remDr) # no popup
# 
# remDr$navigate("https://www.politplatschquatsch.com/2023/01/lutzerath-fashion-style-kohlschwarze.html")
# .clickaway(clickaway = "Politplatschquatsch", remDr = remDr)
# 
# remDr$navigate("https://www.pravda-tv.com/2023/01/sogar-der-fuehrende-kardiologe-von-saudi-arabien-forderte-dazu-auf-die-covid-impfstoffe-aufgrund-schwerer-herzerkrankungen-einzustellen/")
# .clickaway(clickaway = "Pravda TV", remDr = remDr)
# 
# # remDr$navigate("https://www.redglobe.de/2023/01/hausdurchsuchungen-gegen-radio-dreyeckland-frontalangriff-auf-die-pressefreiheit/")
# # .clickaway(clickaway = "Redglobe", remDr = remDr) # kein Popup
# 
# remDr$navigate("https://www.rnd.de/politik/was-die-ruestungsindustrie-vom-neuen-verteidigungsminister-erwartet-OZMXSQECBND73DM7ISPK3AOMBY.html")
# .clickaway(clickaway = "RND", remDr = remDr)
# 
# # remDr$navigate("https://rp-online.de/politik/deutschland/netz-amuesiert-sich-aehnlichkeit-zwischen-laschet-und-pistorius_aid-83209861")
# # .clickaway(clickaway = "RP Online", remDr = remDr) # no popup
# # 
# # remDr$navigate("https://www.rubikon.news/artikel/wolfe-wider-willen")
# # .clickaway(clickaway = "Rubikon", remDr = remDr) # no popup
# # 
# # remDr$navigate("https://www.saarbruecker-zeitung.de/blaulicht/geldtransporter-ueberfall-saarlouis-zwei-verdaechtige-sind-in-frankreich-beruechtigt_aid-83218401")
# # .clickaway(clickaway = "Saarbrücker Zeitung", remDr = remDr) # no popup
# # 
# # remDr$navigate("http://www.scharf-links.de/47.0.html?&tx_ttnews[tt_news]=81715&tx_ttnews[backPid]=56&cHash=0593ab7ebe")
# # .clickaway(clickaway = "scharf links", remDr = remDr) # no popup
# # 
# # remDr$navigate("https://www.spiegel.de/ausland/energiekrise-in-portugal-eltern-muessen-entscheiden-wer-frieren-soll-a-08a51ec1-d4dd-467e-ac9c-51eada4484cb")
# # .clickaway(clickaway = "Spiegel", remDr = remDr) # no popup
# 
# remDr$navigate("https://www.stern.de/politik/ausland/wagner-truppe--soeldner-flieht-nach-norwegen--was-er-zu-berichten-weiss-33106314.html")
# .clickaway(clickaway = "Stern", remDr = remDr) 
# 
# remDr$navigate("https://www.stuttgarter-zeitung.de/inhalt.schnee-in-stuttgart-landeshauptstadt-im-winterkleid.a2be3050-309b-461e-901a-9c125373be74.html")
# .clickaway(clickaway = "Stuttgarter Zeitung", remDr = remDr) 
# 
# remDr$navigate("https://www.sueddeutsche.de/politik/innenminister-ukraine-hubschrauberabsturz-selenskij-kiew-1.5734212")
# .clickaway(clickaway = "SZ", remDr = remDr) 
# 
# remDr$navigate("https://www.t-online.de/nachrichten/ukraine/id_100113202/ukraine-krieg-ist-das-nur-der-anfang-fuer-putin-ist-der-krieg-noch-lange-nicht-vorbei.html")
# .clickaway(clickaway = "T-Online", remDr = remDr) 
# 
# remDr$navigate("https://www.tag24.de/nachrichten/politik/deutschland/politiker/winfried-kretschmann/war-noch-fataler-vergleicht-kretschmann-missbrauchs-skandal-mit-holocaust-2722294")
# .clickaway(clickaway = "Tag24", remDr = remDr) 
# 
# # remDr$navigate("https://www.tagesschau.de/ausland/europa/ukraine-innenminister-hubschrauberabsturz-103.html")
# # .clickaway(clickaway = "Tagesschau", remDr = remDr) # no popup
# 
# remDr$navigate("https://www.tagesspiegel.de/internationales/unfall-in-der-region-kiew-ukrainischer-innenminister-bei-hubschrauberabsturz-getotet-9199227.html")
# .clickaway(clickaway = "Tagesspiegel", remDr = remDr) 
# 
# remDr$navigate("https://taz.de/Festnahme-bei-Protest-nahe-Luetzerath/!5909806/")
# .clickaway(clickaway = "TAZ", remDr = remDr) 
# 
# remDr$navigate("https://www.telepolis.de/features/Davos-Elite-Weltrettung-aus-der-Business-Perspektive-7462054.html")
# .clickaway(clickaway = "Telepolis", remDr = remDr) 
# 
# remDr$navigate("https://www.thueringer-allgemeine.de/leben/gesundheit-medizin/covid-19-war-2021-in-thueringen-haeufigste-todesursache-id237402083.html")
# .clickaway(clickaway = "TA", remDr = remDr) 
# 
# remDr$navigate("https://www.tichyseinblick.de/meinungen/union-politische-volks-demenz-als-kalkuel/")
# .clickaway(clickaway = "Tichys Einblick", remDr = remDr) 
# 
# remDr$navigate("https://www.trtdeutsch.com/politik-welt/pentagon-turkiye-wichtiger-partner-und-verbundeter-11734405")
# .clickaway(clickaway = "TRT", remDr = remDr) 
# 
# remDr$navigate("https://dieunbestechlichen.com/2023/01/twitter-files-jede-verschwoerungstheorie-ist-wahr-geworden/")
# .clickaway(clickaway = "DieUnbestechlichen", remDr = remDr) 
# 
# remDr$navigate("https://www.vice.com/de/article/xgyjvn/zeit-verbrechen-wir-haben-sabine-rueckert-gefragt-wie-sie-runterkommt")
# .clickaway(clickaway = "Vice", remDr = remDr) 
# 
# remDr$navigate("https://www.waz.de/staedte/gelsenkirchen/angriff-auf-mutter-gelsenkirchener-im-ausland-festgenommen-id237401795.html")
# .clickaway(clickaway = "WAZ", remDr = remDr) 
# 
# remDr$navigate("https://www.welt.de/politik/deutschland/article243289779/Kanzler-in-Davos-Scholz-sagt-Ukraine-dauerhafte-deutsche-Unterstuetzung-zu.html")
# .clickaway(clickaway = "Welt", remDr = remDr) 
# 
# remDr$navigate("https://www.weser-kurier.de/bremen/politik/bremen-mehr-kinder-mit-scharlach-erkrankungen-doc7oj67ny1j50rl0x1oy5")
# .clickaway(clickaway = "Weser Kurier", remDr = remDr) 
# 
# remDr$navigate("https://zuerst.de/2023/01/18/kroatischer-praesident-kritisiert-usa-und-nato-absurder-stellvertreterkrieg/")
# .clickaway(clickaway = "Zuerst", remDr = remDr) 
# 
# 
# 
# remDr$close()
# z <- rD$server$stop()

