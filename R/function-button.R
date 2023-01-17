### functions to click away cookie-prompts and other prompts

### waitfindElem waits for an element to load before clicking it

.waitfindElem <- function(xpath, trytime = 6, remDr=remDr){
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

.waitfindElem_shadow <- function(path, trytime = 6, remDr=remDr){
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
  if (clickaway == "Berliner Zeitung"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//iframe[contains(@id, 'sp_message_iframe_752712')]", remDr=remDr)
    if(!is.null(webElem)){
      remDr$switchToFrame(webElem)
      
      webElem <- .waitfindElem(xpath = "//button[contains(@class, 'message-component message-button no-children focusable start-focus sp_choice_type_11')]", remDr=remDr)
      if(!is.null(webElem)){
        webElem$clickElement()
      }
    }
    remDr$switchToFrame(NULL)
  }
  if (clickaway == "Berliner Morgenpost"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//a[contains(@class, 'cmpboxbtn cmpboxbtnyes cmptxt_btn_yes')]", remDr=remDr)
    #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  if (clickaway == "Bild"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//iframe[contains(@id, 'sp_message_iframe_758168')]", remDr=remDr)
    if(!is.null(webElem)){
      remDr$switchToFrame(webElem)
      
      webElem <- .waitfindElem(xpath = "//button[contains(@class, 'message-component message-button no-children focusable sp_choice_type_11')]", remDr=remDr)
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
  if (clickaway == "Zeit"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//iframe[contains(@id, 'sp_message_iframe_701997')]", remDr=remDr)
    if(!is.null(webElem)){
      remDr$switchToFrame(webElem)
      Sys.sleep(1) 
      webElem <- .waitfindElem(xpath = "//button[contains(@class, 'message-component message-button ')]", remDr=remDr)
      if(!is.null(webElem)){
        webElem$clickElement()
      }
    }
    remDr$switchToFrame(NULL)
  }

}

webElem <- .waitfindElem(xpath =  "//div[contains(@class, 'option__accbtn box__accbtn')]")




# ### Testfunctions

# rD <- RSelenium::rsDriver(browser = "firefox",
#                           #chromever = "103.0.5060.134",
#                           port = sample(c(#5678L,
#                             #5679L, #5680L, #5681L,
#                             5682L), size = 1),
#                           #phantomver = "2.1.1",
#                           extraCapabilities = fprof,
#                           check = TRUE, verbose = FALSE)
# 
# remDr <- rD[["client"]]

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
# 
# remDr$close()
