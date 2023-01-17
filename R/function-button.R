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

  if (clickaway == "Berliner Morgenpost"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//a[contains(@class, 'cmpboxbtn cmpboxbtnyes cmptxt_btn_yes')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  if (clickaway == "Bild" || clickaway == "Berliner Zeitung" || clickaway == "Cicero" || clickaway == "FAZ" ||
      clickaway == "Freitag"){
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
  if (clickaway == "Focus" || clickaway == "Handelsblatt"){
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
  if (clickaway == "Hamburger MoPo"){
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
  
  if (clickaway == "Jouwatch"){
    webElem <-NULL
    Sys.sleep(.5)
    webElem <- .waitfindElem(xpath =  "//button[contains(@class, 'cmplz-accept')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  
  if (clickaway == "Junge Freiheit"){
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
#                           port = sample(c(#5678L,
#                             #5679L, #5680L, #5681L,
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
# 
# 
# 
# 
# remDr$close()
# z <- rD$server$stop()
