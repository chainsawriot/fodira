### functions to click away cookie-prompts and other prompts

### waitfindElem waits for an element to load before clicking it

.waitfindElem <- function(xpath, trytime = 6){
  while (trytime > 0) {
    webElem <- tryCatch({remDr$findElement(using = "xpath", xpath)},
                        error = function(e){NULL})
    if(!is.null(webElem)){
      trytime <- 0
      print("found")
    } else {
      trytime <- trytime - 0.1
      Sys.sleep(0.1)
    }
  }
  return(webElem)
}

### Clickaway... clicks away a prompt. 
### You just need to tell it which page you're on
### Zeit is still solved with a less nice wait, because waitElem 
### does have problems on that page


.clickaway <- function(clickaway="none"){
  if (clickaway == "allgemeine_z"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//a[contains(@class, 'purWrapper__button')]")
      #loop until element is found
    if(!is.null(webElem)){
      webElem$clickElement()
    }
  }
  if (clickaway == "zeit"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//iframe[contains(@id, 'sp_message_iframe_701997')]")
    if(!is.null(webElem)){
      remDr$switchToFrame(webElem)
      Sys.sleep(1) 
      webElem <- .waitfindElem(xpath = "//button[contains(@class, 'message-component message-button ')]")
      if(!is.null(webElem)){
        webElem$clickElement()
      }
    }
    remDr$switchToFrame(NULL)
  }
  if (clickaway == "bild"){
    webElem <-NULL
    webElem <- .waitfindElem(xpath =  "//iframe[contains(@id, 'sp_message_iframe_758168')]")
    if(!is.null(webElem)){
      remDr$switchToFrame(webElem)
 
      webElem <- .waitfindElem(xpath = "//button[contains(@class, 'message-component message-button no-children focusable sp_choice_type_11')]")
      if(!is.null(webElem)){
        webElem$clickElement()
      }
    }
    remDr$switchToFrame(NULL)
  }
}

webElem <- .waitfindElem(xpath =  "//div[contains(@class, 'option__accbtn box__accbtn')]")




# ### Testfunctions
# 
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
# 
# remDr$navigate("https://www.allgemeine-zeitung.de/politik/politik-deutschland/medien-boris-pistorius-wird-neuer-verteidigungsminister-2226846")
# .clickaway(clickaway = "allgemeine_z")
# 
# remDr$navigate("https://www.zeit.de/politik/ausland/karte-ukraine-krieg-russland-frontverlauf-truppenbewegungen")
# .clickaway(clickaway = "zeit")
# 
# remDr$navigate("https://www.bild.de/politik/inland/politik-inland/judenhass-in-luetzerath-klima-extremisten-verherrlichen-terroristin-82560194.bild.html")
# .clickaway(clickaway = "bild")
# 
# remDr$close()
# z <- rD$server$stop()
