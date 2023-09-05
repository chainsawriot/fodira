# tabletest <- rio::import("sample_articles2022_RUS.xlsx")
# 
# sub <- subset(tabletest, pub == "Compact")
# 
# 
# sub$bool <- stringr::str_detect(sub$text, "^[0-9]* Kommentar")
# 
# require(webdriver)
# require(magrittr)
# pjs_instance <- run_phantomjs()
# pjs_session <- Session$new(port = pjs_instance$port)
# 
# pjs_session$go("https://www.compact-online.de/erdogan-gegen-nato-beitritt-finnlands-und-schwedens/")
# 
# pjs_session$getUrl()
# 
# pjs_session$getSource() -> html


## for checking whether compact articles are just the comments

check_comments_in_file <- function(pub, text, html){
  if (pub == "Compact"){
    if (stringr::str_detect(text, "^[0-9]* Kommentar")){
      html %>% rvest::read_html(html) %>%
        rvest::html_elements(xpath = "//div[contains(@class, 'post-content cf')]") %>% 
        rvest::html_text(., trim = TRUE) -> text
    } 
  } 
  return(text)
}

# check_comments_in_file(pub = "bild", text = sub$text[19], html = html)


