require(webdriver)
require(magrittr)

pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
tonline_getlink <- function(html){
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@data-testid, 'StreamLayout.Stream')]//article//div//a") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@data-testid, 'StreamLayout.Stream')]//article//div//a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.t-online.de/", .) -> item_link
  
  stringr::str_extract(item_link, "[0-9]+") %>% as.numeric() -> item_number
  
  df <- data.frame(item_title, item_link, item_number)
    return(df)
}


tonline_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  df <- tonline_getlink(pjs_session$getSource())
  return(df)
}

# df <- tonline_getlink(remDr$getPageSource()[[1]])
# df$huh <- (df$item_number > 91372000)


tonline_go_thr_columns <- function(rubrik){
  i <- 1
  j <- 1
  valid_links <- data.frame()
  while (i > 0) {
    paste0("https://www.t-online.de/", rubrik, "/page_", j, "/") %>%
      purrr::map_df(~tonline_getlink_url(.)) %>% subset(., item_number > 91372000)-> subset_links
    
    i <-  nrow(subset_links)
    print(i)
    j <- j+1
    valid_links <- rbind(valid_links, subset_links)
    # k=k+1
    # if (k > 50){
    #   print("wait")
    #   Sys.sleep(90)
    #   k <- 0
    # }
  }

  return(valid_links)
}


c("nachrichten/deutschland", "nachrichten/ausland", "nachrichten/corona-krise", 
  "nachrichten/tagesanbruch", "nachrichten/ukraine", "region/berlin", "region/hamburg",
  "region/muenchen", "region/koeln", "region/frankfurt-am-main", "sport/fussball/bundesliga",
  "sport/fussball/2-bundesliga", "sport/fussball", "sport/fussball/frauenfussball/em-2022", 
  "sport/mehr-sport/radsport/tour-de-france", "sport/formel-1", "sport/mehr-sport",
  "finanzen/geld-vorsorge", "finanzen/unternehmen-verbraucher", "finanzen/versicherungen",
  "finanzen/immobilien-wohnen", "finanzen/beruf-karriere", "unterhaltung/stars", 
  "unterhaltung/stars/royals", "unterhaltung/kino", "unterhaltung/tv", "unterhaltung/musik",
  "nachrichten/panorama/menschen-schicksale", "nachrichten/panorama/katastrophen",
  "nachrichten/panorama/kriminalitaet", "nachrichten/panorama/justiz",
  "nachrichten/panorama/buntes-kurioses", "nachrichten/panorama/wissen/geschichte", 
  "nachrichten/panorama/quiz", "gesundheit/krankheiten-symptome", "gesundheit/krankheiten-symptome/coronavirus",
  "gesundheit/ernaehrung", "gesundheit/fitness", "gesundheit/gesund-leben", "gesundheit/heilmittel-medikamente",
  "gesundheit/schwangerschaft", "gesundheit/selbsttests", "leben/corona-krise", 
  "leben/essen-und-trinken", "leben/reisen", "leben/familie", "leben/alltagswissen",
  "leben/liebe", "leben/mode-beauty", "nachhaltigkeit/klima-und-umwelt", "nachhaltigkeit/mobilitaet-und-verkehr",
  "nachhaltigkeit/heim-garten-und-wohnen", "nachhaltigkeit/energie", "nachhaltigkeit/finanzen-und-beruf",
  "nachhaltigkeit/ernaehrung", "nachhaltigkeit/konsum", "nachhaltigkeit/klima-lexikon",
  "auto/neuheiten-fahrberichte", "auto/recht-und-verkehr", "auto/elektromobilitaet",
  "auto/technik", "digital/handy", "digital/computer", "digital/internet-sicherheit/sicherheit", 
  "digital/internet-sicherheit/internet", "digital/netzpolitik", "heim-garten/garten", 
  "heim-garten/haushaltstipps", "heim-garten/bauen", "heim-garten/wohnen", "heim-garten/energie",
  "ratgeber/deals", "ratgeber/technik", "ratgeber/haushalt-und-wohnen", "ratgeber/genuss",
  "ratgeber/leben-und-freizeit", "ratgeber/haus-und-garten", "ratgeber/gesundheit") %>% 
  purrr::map_dfr(~tonline_go_thr_columns(.)) -> valid_links

valid_links <- dplyr::distinct(valid_links)


# 