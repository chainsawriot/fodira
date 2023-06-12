#devtools::install_github("ropensci/RSelenium")
#install.packages("RSelenium")
# require(RSelenium)
# require(magrittr)
# 
# rD <- RSelenium::rsDriver(browser = "firefox", 
#                           #chromever = "103.0.5060.134", 
#                           port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), 
#                           #phantomver = "2.1.1",
#                           check = FALSE, verbose = FALSE)
# 
# remDr <- rD[["client"]]

#binman::list_versions("phantomjs")

require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)

pjs_session$go("https://www.allgemeine-zeitung.de/politik/deutschland")

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#writeLines(html, "test.html")

#function for geting links from page
allg_z_get_links <- function(html){
  
  #html <- remDr$getPageSource()[[1]]
  html <- pjs_session$getSource()
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article//h2/a") %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article//h2/a") %>% 
    rvest::html_attr("href") %>% paste0("",.)-> item_link
  
  if(length(item_title) == 0){
    df <- data.frame()
  } else {
    df <- data.frame(item_title, item_link)
  }

  
  return(df)
}


allg_z_get_url <- function(url){
  # remDr$navigate(url)
  # print(remDr$getCurrentUrl())
  # remDr$getPageSource()[[1]] %>% tagesspiegel_get_links() -> df
  pjs_session$go(url)
  print(pjs_session$getUrl())
  pjs_session$getSource() %>% allg_z_get_links() -> df
  #print(nrow(df))
  return(df)
}

#allg_z_get_url("https://www.allgemeine-zeitung.de/ratgeber/freizeittipps/unterwegs-mit-kindern?page=6") -> test

allg_z_go_thr_columns <- function(category, startdate){
  df <- data.frame()
  i <- 1
  j <- TRUE
  while (j) {
    #i <- 5
    #category <- "ratgeber/freizeittipps/unterwegs-mit-kindern"
    paste0(category, "?page=", i) %>%
      purrr::map_df(~allg_z_get_url(.)) -> df2
    nrow(df2) -> n
    print(n)
    if(n == 0){
      j <- FALSE
    } else {
      i <- i+1
      df <- rbind(df, df2)
      if(class(df2$item_link[1]) == "character"){
        pjs_session$go(df2$item_link[1])
        pjs_session$getUrl()
        rvest::read_html(pjs_session$getSource()) %>% 
          rvest::html_elements(xpath = "//div[contains(@class, 'vrm-articleDetail__date')]") %>%
          rvest::html_text(trim = TRUE) %>%
          #          stringr::str_extract(., "Aktualisiert am [0-9]+[.][0-9]+[.][0-9]+") %>% 
          stringr::str_extract(., "[0-9]+[.][0-9]+[.][0-9]+")-> date
        date[!is.na(date)] %>% as.Date(., format = "%d.%m.%Y") -> date
      }
         
      if(length(date)==0){
        if(class(df2$item_link[2]) == "character"){
          if(nrow(df2) > 1){
            pjs_session$go(df2$item_link[2])
            rvest::read_html(pjs_session$getSource()) %>% 
              rvest::html_elements(xpath = "//div[contains(@class, 'vrm-articleDetail__date')]") %>%
              rvest::html_text(trim = TRUE) %>%
              #          stringr::str_extract(., "Aktualisiert am [0-9]+[.][0-9]+[.][0-9]+") %>% 
              stringr::str_extract(., "[0-9]+[.][0-9]+[.][0-9]+")-> date
            date[!is.na(date)] %>% as.Date(., format = "%d.%m.%Y") -> date
          } else {
            j <- FALSE
          }
        }
      }
      if(length(date)==0){
        
      } else if(date < as.Date(startdate)){
        j <- FALSE
      }
      
    }
    }
    #print(n)
    

  return(df)
}

# df <- zeit_getlink_url("https://www.zeit.de/thema/krieg-in-ukraine", "2022-01-01")
  

# c("dossiers/coronavirus", "lokales/mainz/nachrichten-mainz", "lokales/ingelheim/landkreis-mainz-bingen",
#   "lokales/mainz/hochheim", "lokales/mainz/amoeneburg-kostheim-kastel", "lokales/mainz/budenheim",
#   "lokales/mainz/vg-nieder-olm/nieder-olm", "lokales/mainz/vg-nieder-olm/vg-nieder-olm",
#   "lokales/mainz/vg-nieder-olm/stadecken-elsheim", "lokales/mainz/vg-nieder-olm/ober-olm",
#   "lokales/mainz/vg-nieder-olm/soergenloch", "lokales/mainz/vg-nieder-olm/essenheim",
#   "lokales/mainz/vg-nieder-olm/klein-winterheim", "lokales/mainz/vg-nieder-olm/zornheim",
#   "lokales/mainz/vg-nieder-olm/jugenheim",
#   "lokales/mainz/vg-bodenheim/vg-bodenheim",
#   "lokales/mainz/vg-bodenheim/bodenheim",
#   "lokales/mainz/vg-bodenheim/gau-bischofsheim",
#   "lokales/mainz/vg-bodenheim/harxheim",
#   "lokales/mainz/vg-bodenheim/nackenheim",
#   "lokales/mainz/vg-bodenheim/loerzweiler",
#   "lokales/mainz/stadtteile-mainz/bretzenheim",
#   "lokales/mainz/stadtteile-mainz/drais", "lokales/mainz/stadtteile-mainz/ebersheim", 
#   "lokales/mainz/stadtteile-mainz/finthen", "lokales/mainz/stadtteile-mainz/gonsenheim",
#   "lokales/mainz/stadtteile-mainz/hartenberg-muenchfeld", "lokales/mainz/stadtteile-mainz/hechtsheim",
#   "lokales/mainz/stadtteile-mainz/laubenheim", "lokales/mainz/stadtteile-mainz/lerchenberg",
#   "lokales/mainz/stadtteile-mainz/marienborn", "lokales/mainz/stadtteile-mainz/mombach",
#   "lokales/mainz/stadtteile-mainz/neustadt", "lokales/mainz/stadtteile-mainz/altstadt",
#   "lokales/mainz/stadtteile-mainz/oberstadt","lokales/mainz/stadtteile-mainz/weisenau",
#   "lokales/alzey/alzey", "lokales/alzey/vg-alzey-land/vg-alzey-land", 
#   "lokales/alzey/vg-alzey-land/albig",
#   "lokales/alzey/vg-alzey-land/bechenheim",
#   "lokales/alzey/vg-alzey-land/bechtolsheim",
#   "lokales/alzey/vg-alzey-land/bermersheim-v-h-d",
#   "lokales/alzey/vg-alzey-land/biebelnheim",
#   "lokales/alzey/vg-alzey-land/bornheim",
#   "lokales/alzey/vg-alzey-land/dintesheim",
#   "lokales/alzey/vg-alzey-land/eppelsheim",
#   "lokales/alzey/vg-alzey-land/erbes-buedesheim",
#   "lokales/alzey/vg-alzey-land/esselborn",
#   "lokales/alzey/vg-alzey-land/flomborn",
#   "lokales/alzey/vg-alzey-land/flonheim",
#   "lokales/alzey/vg-alzey-land/framersheim",
#   "lokales/alzey/vg-alzey-land/freimersheim",
#   "lokales/alzey/vg-alzey-land/gau-heppenheim",
#   "lokales/alzey/vg-alzey-land/gau-odernheim",
#   "lokales/alzey/vg-alzey-land/kettenheim",
#   "lokales/alzey/vg-alzey-land/lonsheim",
#   "lokales/alzey/vg-alzey-land/mauchenheim",
#   "lokales/alzey/vg-alzey-land/nack",
#   "lokales/alzey/vg-alzey-land/nieder-wiesen",
#   "lokales/alzey/vg-alzey-land/ober-floersheim",
#   "lokales/alzey/vg-alzey-land/offenheim",
#   "lokales/alzey/vg-alzey-land/wahlheim",
#   
#   
#   "lokales/alzey/vg-woellstein/vg-woellstein",
#   "lokales/alzey/vg-woellstein/eckelsheim",
#   "lokales/alzey/vg-woellstein/gau-bickelheim",
#   "lokales/alzey/vg-woellstein/gumbsheim",
#   "lokales/alzey/vg-woellstein/siefersheim",
#   "lokales/alzey/vg-woellstein/stein-bockenheim",
#   "lokales/alzey/vg-woellstein/wendelsheim",
#   "lokales/alzey/vg-woellstein/wonsheim",
#   
#   "lokales/alzey/vg-woerrstadt/vg-woerrstadt", 
#   "lokales/alzey/vg-woerrstadt/ensheim",
#   "lokales/alzey/vg-woerrstadt/gabsheim",
#   "lokales/alzey/vg-woerrstadt/gau-weinheim",
#   "lokales/alzey/vg-woerrstadt/partenheim",
#   "lokales/alzey/vg-woerrstadt/saulheim",
#   "lokales/alzey/vg-woerrstadt/schornsheim",
#   "lokales/alzey/vg-woerrstadt/spiesheim",
#   "lokales/alzey/vg-woerrstadt/sulzheim",
#   "lokales/alzey/vg-woerrstadt/udenheim",
#   "lokales/alzey/vg-woerrstadt/vendersheim",
#   "lokales/alzey/vg-woerrstadt/wallertheim",
#   "lokales/alzey/vg-woerrstadt/woerrstadt",
#   
#   "lokales/alzey/vg-wonnegau/vg-wonnegau",
#   "lokales/alzey/vg-wonnegau/bechtheim",
#   "lokales/alzey/vg-wonnegau/bermersheim",
#   "lokales/alzey/vg-wonnegau/dittelsheim-hessloch",
#   "lokales/alzey/vg-wonnegau/frettenheim",
#   "lokales/alzey/vg-wonnegau/gundersheim",
#   "lokales/alzey/vg-wonnegau/hangen-weisheim",
#   "lokales/alzey/vg-wonnegau/hochborn",
#   "lokales/alzey/vg-wonnegau/monzernheim",
#   "lokales/alzey/vg-wonnegau/osthofen",
#   "lokales/alzey/vg-wonnegau/westhofen",
#   
#   "lokales/alzey/landkreis-alzey-worms", "lokales/oppenheim/oppenheim",
#   "lokales/oppenheim/vg-rhein-selz/vg-rhein-selz",
#   "lokales/oppenheim/vg-rhein-selz/dalheim",
#   "lokales/oppenheim/vg-rhein-selz/dexheim",
#   "lokales/oppenheim/vg-rhein-selz/dienheim",
#   "lokales/oppenheim/vg-rhein-selz/dolgesheim",
#   "lokales/oppenheim/vg-rhein-selz/dorn-duerkheim",
#   "lokales/oppenheim/vg-rhein-selz/eimsheim",
#   "lokales/oppenheim/vg-rhein-selz/friesenheim",
#   "lokales/oppenheim/vg-rhein-selz/guntersblum",
#   "lokales/oppenheim/vg-rhein-selz/hahnheim",
#   "lokales/oppenheim/vg-rhein-selz/hillesheim",
#   "lokales/oppenheim/vg-rhein-selz/koengernheim",
#   "lokales/oppenheim/vg-rhein-selz/ludwigshoehe",
#   "lokales/oppenheim/vg-rhein-selz/mommenheim",
#   "lokales/oppenheim/vg-rhein-selz/nierstein",
#   "lokales/oppenheim/vg-rhein-selz/selzen",
#   "lokales/oppenheim/vg-rhein-selz/uelversheim",
#   "lokales/oppenheim/vg-rhein-selz/undenheim",
#   "lokales/oppenheim/vg-rhein-selz/weinolsheim",
#   "lokales/oppenheim/vg-rhein-selz/wintersheim",
#   
#   
#   "lokales/ingelheim/landkreis-mainz-bingen",
#   "lokales/bingen/bingen", 
#   "lokales/bingen/vg-gau-algesheim/vg-gau-algesheim",
#   "lokales/bingen/vg-gau-algesheim/appenheim",
#   "lokales/bingen/vg-gau-algesheim/bubenheim",
#   "lokales/bingen/vg-gau-algesheim/engelstadt",
#   "lokales/bingen/vg-gau-algesheim/gau-algesheim",
#   "lokales/bingen/vg-gau-algesheim/nieder-hilbersheim",
#   "lokales/bingen/vg-gau-algesheim/ober-hilbersheim",
#   "lokales/bingen/vg-gau-algesheim/ockenheim",
#   "lokales/bingen/vg-gau-algesheim/schwabenheim",
#   
#   "lokales/bingen/vg-sprendlingen-gensingen/vg-sprendlingen-gensingen", 
#   "lokales/bingen/vg-sprendlingen-gensingen/aspisheim",
#   "lokales/bingen/vg-sprendlingen-gensingen/badenheim",
#   "lokales/bingen/vg-sprendlingen-gensingen/gensingen",
#   "lokales/bingen/vg-sprendlingen-gensingen/grolsheim",
#   "lokales/bingen/vg-sprendlingen-gensingen/horrweiler",
#   "lokales/bingen/vg-sprendlingen-gensingen/sprendlingen",
#   "lokales/bingen/vg-sprendlingen-gensingen/st-johann",
#   "lokales/bingen/vg-sprendlingen-gensingen/welgesheim",
#   "lokales/bingen/vg-sprendlingen-gensingen/wolfsheim",
#   "lokales/bingen/vg-sprendlingen-gensingen/zotzenheim",
#   
#   
#   "lokales/bingen/vg-rhein-nahe/vg-rhein-nahe",
#   "lokales/bingen/vg-rhein-nahe/bacharach",
#   "lokales/bingen/vg-rhein-nahe/breitscheid",
#   "lokales/bingen/vg-rhein-nahe/manubach",
#   "lokales/bingen/vg-rhein-nahe/muenster-sarmsheim",
#   "lokales/bingen/vg-rhein-nahe/niederheimbach",
#   "lokales/bingen/vg-rhein-nahe/oberdiebach",
#   "lokales/bingen/vg-rhein-nahe/oberheimbach",
#   "lokales/bingen/vg-rhein-nahe/trechtingshausen",
#   "lokales/bingen/vg-rhein-nahe/waldalgesheim",
#   "lokales/bingen/vg-rhein-nahe/weiler",
#   
#   "lokales/ingelheim/landkreis-mainz-bingen", "lokales/ingelheim/ingelheim",
#   "lokales/bad-kreuznach/stadt-bad-kreuznach", "lokales/bad-kreuznach/landkreis-bad-kreuznach",
#   "lokales/bad-kreuznach/vg-ruedesheim/vg-ruedesheim", 
#   "lokales/bad-kreuznach/vg-ruedesheim/allenfeld",
#   "lokales/bad-kreuznach/vg-ruedesheim/argenschwang",
#   "lokales/bad-kreuznach/vg-ruedesheim/bockenau",
#   "lokales/bad-kreuznach/vg-ruedesheim/boos",
#   "lokales/bad-kreuznach/vg-ruedesheim/braunweiler",
#   "lokales/bad-kreuznach/vg-ruedesheim/burgsponheim",
#   "lokales/bad-kreuznach/vg-ruedesheim/dalberg",
#   "lokales/bad-kreuznach/vg-ruedesheim/duchroth",
#   "lokales/bad-kreuznach/vg-ruedesheim/gebroth",
#   "lokales/bad-kreuznach/vg-ruedesheim/gutenberg",
#   "lokales/bad-kreuznach/vg-ruedesheim/hargesheim",
#   "lokales/bad-kreuznach/vg-ruedesheim/hergenfeld",
#   "lokales/bad-kreuznach/vg-ruedesheim/hueffelsheim",
#   "lokales/bad-kreuznach/vg-ruedesheim/mandel",
#   "lokales/bad-kreuznach/vg-ruedesheim/muenchwald",
#   "lokales/bad-kreuznach/vg-ruedesheim/niederhausen",
#   "lokales/bad-kreuznach/vg-ruedesheim/norheim",
#   "lokales/bad-kreuznach/vg-ruedesheim/oberhausen-nahe",
#   "lokales/bad-kreuznach/vg-ruedesheim/oberstreit",
#   "lokales/bad-kreuznach/vg-ruedesheim/roxheim",
#   "lokales/bad-kreuznach/vg-ruedesheim/ruedesheim",
#   "lokales/bad-kreuznach/vg-ruedesheim/sankt-katharinen",
#   "lokales/bad-kreuznach/vg-ruedesheim/schlossboeckelheim",
#   "lokales/bad-kreuznach/vg-ruedesheim/sommerloch",
#   "lokales/bad-kreuznach/vg-ruedesheim/spabruecken",
#   "lokales/bad-kreuznach/vg-ruedesheim/spall",
#   "lokales/bad-kreuznach/vg-ruedesheim/sponheim",
#   "lokales/bad-kreuznach/vg-ruedesheim/traisen",
#   "lokales/bad-kreuznach/vg-ruedesheim/waldboeckelheim",
#   "lokales/bad-kreuznach/vg-ruedesheim/wallhausen",
#   "lokales/bad-kreuznach/vg-ruedesheim/weinsheim",
#   "lokales/bad-kreuznach/vg-ruedesheim/winterbach",
# 
#   
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/vg-langenlonsheim-stromberg",
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/bretzenheim",
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/daxweiler",
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/doerrebach",
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/dorsheim",
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/eckenroth",
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/guldental",
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/langenlonsheim",
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/laubenheim",
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/roth",
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/ruemmelsheim",
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/schoeneberg",
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/schweppenhausen",
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/seibersbach",
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/stromberg-stadt",
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/waldlaubersheim",
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/warmsroth",
#   "lokales/bad-kreuznach/vg-langenlonsheim-stromberg/windesheim",
#   
#   "lokales/bad-kreuznach/vg-bad-kreuznach/vg-bad-kreuznach",
#   "lokales/bad-kreuznach/vg-bad-kreuznach/altenbamberg",
#   "lokales/bad-kreuznach/vg-bad-kreuznach/biebelsheim",
#   "lokales/bad-kreuznach/vg-bad-kreuznach/feilbingert",
#   "lokales/bad-kreuznach/vg-bad-kreuznach/frei-laubersheim",
#   "lokales/bad-kreuznach/vg-bad-kreuznach/fuerfeld",
#   "lokales/bad-kreuznach/vg-bad-kreuznach/hackenheim",
#   "lokales/bad-kreuznach/vg-bad-kreuznach/hallgarten",
#   "lokales/bad-kreuznach/vg-bad-kreuznach/hochstaetten",
#   "lokales/bad-kreuznach/vg-bad-kreuznach/neu-bamberg",
#   "lokales/bad-kreuznach/vg-bad-kreuznach/pfaffen-schwabenheim",
#   "lokales/bad-kreuznach/vg-bad-kreuznach/pleitersheim",
#   "lokales/bad-kreuznach/vg-bad-kreuznach/tiefenthal",
#   "lokales/bad-kreuznach/vg-bad-kreuznach/volxheim",
#   
#   "lokales/bad-sobernheim/sobernheim",
#   "lokales/bad-sobernheim/vg-nahe-glan", "lokales/kirn/kirn", 
#   "lokales/kirn/vg-kirner-land", "lokales/rheinhessen", 
#   "lokales/rhein-main") %>% purrr::map_df(~allg_z_go_thr_columns(., "2021-12-31")) -> valid_links1
#   

require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 
                                                               5679L, 
                                                               5680L, 
                                                               #5681L, 
                                                               5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

remDr$navigate("https://www.allgemeine-zeitung.de/")


e<- remDr$findElement(using = "xpath", "//div[contains(@class, 'iconLink')]")
e$clickElement()

e<- remDr$findElement(using = "xpath", "//a[contains(@href, '#')]")
e$clickElement()



rvest::read_html(remDr$getPageSource()[[1]]) %>% 
  rvest::html_elements(xpath = "//a[contains(@class, 'iconLink__text --clickable')]") %>%
  rvest::html_text(trim = TRUE) -> titel

rvest::read_html(remDr$getPageSource()[[1]]) %>% 
  rvest::html_elements(xpath = "//a[contains(@class, 'iconLink__text --clickable')]") %>%
  rvest::html_attr("href") %>% stringr::str_remove(., "^\\/") %>%
  stringr::str_replace(., "vrm.cue.cloud", "de") -> url

df <- data.frame(titel, url)



e<- remDr$findElements(using = "xpath", "//div[contains(@class, 'iconLink__textMain paragraph --regular')]")[[6]]
e$clickElement()

rvest::read_html(remDr$getPageSource()[[1]]) %>% 
  rvest::html_elements(xpath = "//a[contains(@class, 'iconLink__text --clickable')]") %>%
  rvest::html_text(trim = TRUE) -> titel


rvest::read_html(remDr$getPageSource()[[1]]) %>% 
  rvest::html_elements(xpath = "//a[contains(@class, 'iconLink__text --clickable')]") %>%
  rvest::html_attr("href") %>% stringr::str_remove(., "^\\/") %>%
  stringr::str_replace(., "vrm.cue.cloud", "de") %>%
  stringr::str_replace(., "schwerpunkte", "	https://www.allgemeine-zeitung.de/schwerpunkte") -> url


df <- dplyr::distinct(rbind(df, data.frame(titel, url)))


df <- df[c(6:21, 34:37),]

allg_z_go_thr_doss<- function(){
  rvest::read_html(remDr$getPageSource()[[1]]) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'small-12 xlarge-9 row')]//a") %>%
    rvest::html_text(trim = TRUE) -> titel
  
  rvest::read_html(remDr$getPageSource()[[1]]) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'small-12 xlarge-9 row')]//a") %>%
    rvest::html_attr("href") %>% stringr::str_remove(., "^https[:][/][/]www\\.allgemeine-zeitung\\.de\\/") -> url
  
  df <- data.frame(titel, url)
  return(df)
}

for (i in 1:5) {
  remDr$navigate(paste0("https://www.allgemeine-zeitung.de/dossiers?page=", i))
  allg_z_go_thr_doss() %>% rbind(df, .) -> df
}

rownames(df) <- 1:nrow(df)

#save(valid_links1, file= "allg_z.RData")

#save(valid_links2, file= "allg_z_4.RData")

#save(valid_links3, file= "allg_z_3.RData")

df$url %>% purrr::map_df(~allg_z_go_thr_columns(., "2021-12-01")) -> valid_links2

#nrow(dplyr::distinct(valid_links1))

#nrow(dplyr::distinct(rbind(valid_links2, valid_links3, valid_links4)))

valid_links <- dplyr::distinct(rbind(#valid_links1, 
                                     valid_links2, valid_links3#, valid_links4
                                     ))

valid_links %>% dplyr::rename(title = item_title, link = item_link) %>% 
  dplyr::mutate(pub = "Allgemeine Zeitung", description = NA, pubdate = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "allg_z_1.RDS")
remDr$close()
z <- rD$server$stop()
