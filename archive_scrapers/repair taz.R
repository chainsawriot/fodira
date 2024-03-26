require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)

TAZ <- readRDS("TAZ_auflösen.RDS")

TAZ$false_link <- TAZ$link

function_resolve <- function(url){
  print(url)
  pjs_session$go(url)
  print(pjs_session$getUrl())
  return(pjs_session$getUrl())
}

for (i in 1:nrow(TAZ)) {
  print(i)
  TAZ$link[i] <- function_resolve(TAZ$false_link[i])
}


saveRDS(TAZ, "TAZ_02_2024.RDS")

TAZ_8 <- readRDS("C:/fodira/TAZ_8.RDS")

TAZ_8$false_link <- TAZ_8$link

for (i in 1:nrow(TAZ_8)) {
  print(i)
  TAZ_8$link[i] <- function_resolve(TAZ_8$false_link[i])
}


saveRDS(TAZ_8, "TAZ_8_2022.RDS")

TAZ_2 <- readRDS("C:/fodira/TAZ_2.RDS")

TAZ_2$false_link <- TAZ_2$link

for (i in 1:nrow(TAZ_2)) {
  print(i)
  TAZ_2$link[i] <- function_resolve(TAZ_2$false_link[i])
}


saveRDS(TAZ_2, "TAZ_9_2022.RDS")


