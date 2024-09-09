require(magrittr)


.extract_alt_function <- function(html, pub){
  if(pub == "Handelsblatt"){
    html %>% 
      rvest::html_elements(xpath = "//app-storyline-paragraph") %>% 
      rvest::html_text(., trim = TRUE) -> paragraphs
    
    text <- paste0(paragraphs, collapse = "\n\n")
    
    if(length(text) != 0 && nchar(text) > 0){
      return(text)
    } 
    
    html %>%
      rvest::html_elements(xpath = "//div[contains(@class, 'vhb-article-area--read')]") %>% 
      rvest::html_text(., trim = TRUE) -> text
    if(length(text) != 0 && nchar(text) > 0){
      return(text)
    } 
    
    html %>%
      rvest::html_elements(xpath = "//article/section") %>% 
      rvest::html_text(., trim = TRUE) -> text
    
    if(length(text) != 0 && nchar(text) > 0){
      return(text)
    } 
    
    html %>% 
      rvest::html_elements(xpath = "//app-storyline-element") %>% 
      rvest::html_text(., trim = TRUE) -> paragraphs
    
    text <- paste0(paragraphs, collapse = "\n\n")
    
    if(length(text) != 0 && nchar(text) > 0){
      return(text)
    } 
    return(NA)
    
  # } if(pub == "Jouwatch"){
  #   html %>% 
  #     rvest::html_elements(xpath = "//div[contains(@data-widget_type, 'theme-post-content.default')]") %>% 
  #     rvest::html_text(., trim = TRUE) -> text
  #   
  #   if(length(text) != 0 && nchar(text) > 0){
  #     return(text)
  #   } 
  #   return(NA)
  } else {
    stop("Wrong pub for this function.")
  }
}

