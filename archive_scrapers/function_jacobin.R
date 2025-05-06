require(rtoot)
require(dplyr)

x <- rtoot::search_accounts("Jacobin")

get_toots <- function(start_date){
  
  
  if(file.exists("aaa_toots_jacob.RDS")){
    toots_jacob <- readRDS("aaa_toots_jacob.RDS")
  } else {
    toots_jacob <- get_account_statuses("111330645850828468", limit = 1000)
  }
  
  
  
  
  last_date <- toots_jacob$created_at[nrow(toots_jacob)]
  print(last_date)
  while (as.Date(last_date) > as.Date(start_date)) {
    
    print(toots_jacob$id[nrow(toots_jacob)])
    
    toots_jacob <- rbind(toots_jacob,
                        get_account_statuses("111330645850828468", 
                                             max_id = toots_jacob$id[nrow(toots_jacob)], 
                                             limit = 1000))
    
    last_date <- toots_jacob$created_at[nrow(toots_jacob)]
    saveRDS(toots_jacob, "aaa_toots_jacob.RDS")
    Sys.sleep(600)
    print(last_date)
    print("save")
    print(nrow(toots_jacob))
    
  }
  return(toots_jacob)
}


toots_jacob <- get_toots("2023-01-01")

# y <- get_account_statuses("111330645850828468", 
#                           max_id = toots_jacob$id[nrow(toots_jacob)], 
#                           limit = 1000)



toots_jacob$url_1 <- stringr::str_extract(toots_jacob$content, '[^ "]+jacobin.de/[^ "]+')

toots_jacob$title_1 <- stringr::str_extract(toots_jacob$content, "<p>[^<>]+") %>% stringr::str_remove("<p>")

toots_jacob <- toots_jacob[stringr::str_which(toots_jacob$url_1, "jacobin.de"),]

toots_jacob %>% select(created_at, title_1, url_1) %>% rename(title = `title_1`, link = url_1, pubdate = created_at) %>% 
  mutate(pubdate = lubridate::ymd_hms(pubdate)) %>% mutate(pub = "Jacobin", description = NA) %>% 
  arrange(pubdate) %>% select(pub, link, pubdate, title, description) -> valid_links

valid_links <- valid_links[!is.na(valid_links$link),]

saveRDS(valid_links, "jacobin_archive.RDS")
