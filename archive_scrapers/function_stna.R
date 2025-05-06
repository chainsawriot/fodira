## Not good enough

# require(bskyr)
# require(dplyr)
# 
# b_s_auth <- readRDS("aaa_b_s_auth.RDS")
# 
# stuttg_posts <- bs_get_author_feed(actor = "stuttgarter-zeitung.de",
#                                    auth = b_s_auth,
#                                    limit = 567)
# 
# 
# 
# 
# 
# 
# stuttg_posts$url_1 <- stuttg_posts$post_record.embed.external.uri %>% stringr::str_replace("stuttgarter-zeitung", "stuttgarter-nachrichten")
# 
# stuttg_posts$title_1 <- stringr::str_extract(stuttg_posts$post_record.embed.external.title, "<p>[^<>]+") %>% stringr::str_remove("<p>")
# 
# stuttg_posts <- stuttg_posts[stringr::str_which(stuttg_posts$url_1, "stuttgarter-nachrichten.de"),]
# 
# stuttg_posts %>% select(created_at, title_1, url_1) %>% rename(title = `title_1`, link = url_1, pubdate = created_at) %>% 
#   mutate(pubdate = lubridate::ymd_hms(pubdate)) %>% mutate(pub = "Stuttgarter Zeitung", description = NA) %>% 
#   arrange(pubdate) %>% select(pub, link, pubdate, title, description) -> valid_links
# 
# #### split by date!
# 
# valid_links <- valid_links[!is.na(valid_links$link),]
# 
# valid_links_1 <- subset(valid_links, pubdate > as.Date("2022-09-08"))
# 
# valid_links_2 <- subset(valid_links, pubdate <= as.Date("2022-09-08"))
# 
# saveRDS(valid_links_1, "stuttgarter_Z_archive_new.RDS")
# 
# saveRDS(valid_links_2, "stuttgarter_Z_archive_old.RDS")
# 
# valid_links_1$false_link <- valid_links_1$link
# 
# valid_links_1$link <- stringr::str_remove(valid_links_1$false_link, "[?].*")
# 
# 
# valid_links_2$false_link <- valid_links_2$link
# 
# valid_links_2$link <- stringr::str_remove(valid_links_2$false_link, "[?].*")
# 
# saveRDS(valid_links_1, "stuttgarter_Z_archive_new.RDS")
# 
# saveRDS(valid_links_2, "stuttgarter_Z_archive_old.RDS")
