require(tidyverse)
articles <- rio::import("articles.csv") %>% tibble::as_tibble()

articles %>% filter(!(str_detect(link, "http[s].+http[s]://.+\\.sport") | str_detect(link, "http[s].+http[s]://.+\\.reisereporter"))) %>% mutate(link = str_replace(link, "^https:\\/\\/.+https:", "https:")) %>% mutate(link = str_replace(link, "^https:https", "https"))  -> qlink

## For these links, only the ones from Vice deserve being rescued
qlink %>% filter(str_detect(link, "^http", negate = TRUE)) %>% arrange(pub) %>% select(pub, link)

qlink %>% filter(pub == "Vice" & str_detect(link, "^http", negate = TRUE)) %>% mutate(link = str_remove(link, "\\?https:\\/\\/.+")) %>% mutate(link = purrr::map_chr(link, ~paste0("https://www.vice.com", .))) -> vice

qlink %>% filter(str_detect(link, "^http")) %>% bind_rows(vice) %>% arrange(id) %>% mutate(host = purrr::map_chr(link, ~httr::parse_url(.)$hostname)) %>% count(host) -> allhosts

excluded_hosts <- c("plus.tagesspiegel.deFALSCHERLINKhtml", "GuMo.html", "bit.ly", "radwege-check.de", "www.adac.de", "www.basel.com", "www.nrw-tourismus.de", "youtu.be", "www.youtube.com", "immobilien-neu-gedacht.de", "altersvorsorge-neu-gedacht.de")

qlink %>% filter(str_detect(link, "^http")) %>% bind_rows(vice) %>% arrange(id) %>% mutate(host = purrr::map_chr(link, ~httr::parse_url(.)$hostname)) %>% filter(!host %in% excluded_hosts) %>% select(-host) -> final

final %>% dplyr::select(-id, -insert_timestamp) %>% dplyr::mutate(htmlfile = stringr::str_replace(htmlfile, " ", "_")) %>% rio::export("articles_clean.csv")
