# install.packages("webdriver")
# install.packages("rio")
# install.packages("xml2")
# install.packages("stringr")
# library(webdriver)
# install_phantomjs()



library(webdriver)
library(xml2)
library(rio)
library(stringr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)

getlinks_rss <- function(url){
  # go to URL
  pjs_session$go(url)
  # download Feed
  if (pjs_session$getUrl() != url){
    #if reaching url fails
    errors <<- rbind(errors, data.frame(error = "urlfail", page = url, logtime = Sys.time()))
    return()
  } else {
    #get links from document
    rendered_source <- pjs_session$getSource()
    doc<-try(read_xml(rendered_source), silent = TRUE)
    links <- try(xml_find_all(doc, "//item/link"), silent = TRUE) 
    links2 <- try(xml_text(links), silent = TRUE)
    if((class(links2)[1] == "try-error")){
      #if getting link fails
      errors <<- rbind(errors, data.frame(error = "parsefail",page = url, logtime = Sys.time()))
      return()
    } else {
      #output links
      if (length(links2) > 0){
        return(unlist(links2))
      } else {
        errors <<- rbind(errors, data.frame(error = "empty",page = url, logtime = Sys.time()))
        return()
      }
    }
  }
}

# Import DF with rss-urls

rss_list <- import("rss_list.csv")

# Make empty logfile
errors <- data.frame()

# Return urls from rss-files
linkvector <- unlist(apply(rss_list[c("url")], 1, getlinks_rss))

getlinks_rss("https://www.handelsblatt.com/contentexport/feed/schlagzeilen")

# Save errorlog
if (nrow(errors)>0){
  export(errors, file = paste0("errors", str_replace_all(Sys.time(), ":", "_"), ".csv"))
}
