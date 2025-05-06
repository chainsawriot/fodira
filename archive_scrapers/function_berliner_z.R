require(rtoot)
require(dplyr)

get_toots <- function(start_date){
  
  
  if(file.exists("aaa_toots_berl.RDS")){
    toots_berl <- readRDS("aaa_toots_berl.RDS")
  } else {
    toots_berl <- get_account_statuses("112676915807218093", limit = 1000)
  }
  
  
  
  
  last_date <- toots_berl$created_at[nrow(toots_berl)]
  print(last_date)
  while (as.Date(last_date) > as.Date(start_date)) {
    
    print(toots_berl$id[nrow(toots_berl)])
    
    toots_berl <- rbind(toots_berl,
                        get_account_statuses("112676915807218093", 
                                             max_id = toots_berl$id[nrow(toots_berl)], 
                                             limit = 1000))
    
    last_date <- toots_berl$created_at[nrow(toots_berl)]
    saveRDS(toots_berl, "aaa_toots_berl.RDS")
    Sys.sleep(600)
    print(last_date)
    print("save")
    print(nrow(toots_berl))
    
  }
  return(toots_berl)
}


toots_berl <- get_toots("2023-01-01")





toots_berl$url_1 <- stringr::str_extract(toots_berl$content, 'https://www.berliner-zeitung.de/[^ "]+')

toots_berl$title_1 <- stringr::str_extract(toots_berl$content, "<p>[^<>]+") %>% stringr::str_remove("<p>")

toots_berl <- toots_berl[stringr::str_which(toots_berl$url_1, "www.berliner-zeitung.de"),]

toots_berl %>% select(created_at, title_1, url_1) %>% rename(title = `title_1`, link = url_1, pubdate = created_at) %>% 
  mutate(pubdate = lubridate::ymd_hms(pubdate)) %>% mutate(pub = "Berliner Zeitung", description = NA) %>% 
  arrange(pubdate) %>% select(pub, link, pubdate, title, description) -> valid_links

valid_links <- valid_links[!is.na(valid_links$link),]

saveRDS(valid_links, "berliner_z_archive.RDS")
# 
# valid_links$false_link <- valid_links$link
# 
# valid_links$link <- stringr::str_remove(valid_links$false_link, "[?].*")

#  saveRDS(valid_links, "berliner_z_archive.RDS")

