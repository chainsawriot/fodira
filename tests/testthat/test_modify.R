test_that("modify", {
    url <- "https://www.bild.de/regional/frankfurt/frankfurt-aktuell/frankfurt-nach-bild-bericht-awo-chefin-verzichtet-auf-buergermedaille-81360646.bild.html?wtmc=ob.feed"
    y1 <- modify_url(url)
    expect_equal(y1, "https://www.bild.de/regional/frankfurt/frankfurt-aktuell/frankfurt-nach-bild-bericht-awo-chefin-verzichtet-auf-buergermedaille-81360646.bild.html")
    y2 <- modify_url(url, rm_query = FALSE)
    expect_equal(y2, url)
    rt_url <- "https://de.rt.com/international/148141-sex-auf-verlangen-ex-mitarbeiterin/"
    y3 <- modify_url(rt_url)
    expect_equal(rt_url, y3)
    y4 <- modify_url(rt_url, replace_domains = "test.rtde.me")
    expect_equal(y4, "https://test.rtde.me/international/148141-sex-auf-verlangen-ex-mitarbeiterin/")
    urlfragment <- "https://www.spiegel.de/ausland/georgien-und-kasachstan-russische-kriegsdienstverweigerer-sorgen-fuer-unmut-video-a-4e049736-d9aa-4306-8bb8-9c004dd7f36f#ref=rss"
    y5 <- modify_url(urlfragment)
    expect_equal(y5, "https://www.spiegel.de/ausland/georgien-und-kasachstan-russische-kriegsdienstverweigerer-sorgen-fuer-unmut-video-a-4e049736-d9aa-4306-8bb8-9c004dd7f36f")
})

test_that("harmonize", {
    output <- readRDS("../testdata/newlinks.RDS")
    x1 <- harmonize_output(output)
    x2 <- harmonize_output(output, pubs_require_cleaning = c("Bild"))
    x3 <- harmonize_output(output, remove_duplicates = FALSE)
    expect_true(nrow(output) == nrow(x3))
    expect_true(nrow(output) > nrow(x1))
    expect_true(nrow(x2) > nrow(x1))
    bild_dirty_url <- "https://www.bild.de/geld/wirtschaft/wirtschaft/inflation-im-september-in-deutschland-lebensmittel-fast-20prozent-teurer-81607510.bild.html?wtmc=ob.feed"
    bild_clean_url <- "https://www.bild.de/geld/wirtschaft/wirtschaft/inflation-im-september-in-deutschland-lebensmittel-fast-20prozent-teurer-81607510.bild.html"
    spiegel_dirty_url <- "https://www.spiegel.de/panorama/leute/backstreet-boys-bedanken-sich-bei-fans-mit-signierten-unterhosen-a-2ff8887a-43e0-4569-89a5-cb30b5f2c3bf#ref=rss"
    spiegel_clean_url <- "https://www.spiegel.de/panorama/leute/backstreet-boys-bedanken-sich-bei-fans-mit-signierten-unterhosen-a-2ff8887a-43e0-4569-89a5-cb30b5f2c3bf"
    expect_false(bild_dirty_url %in% x1$link)
    expect_true(bild_clean_url %in% x1$link)
    expect_false(bild_dirty_url %in% x2$link)
    expect_true(bild_clean_url %in% x2$link)
    expect_false(bild_dirty_url %in% x3$link)
    expect_true(bild_clean_url %in% x3$link)
    expect_false(spiegel_dirty_url %in% x1$link)
    expect_true(spiegel_clean_url %in% x1$link)
    expect_true(spiegel_dirty_url %in% x2$link)
    expect_false(spiegel_clean_url %in% x2$link)
    ## sanity
    expect_true(bild_dirty_url %in% output$link)
    expect_true(spiegel_dirty_url %in% output$link)
    stern_dirty_url <- "https://www.stern.de/reise/fernreisen/iguaz%C3%BA-wasserfaelle-fuehren-zehnmal-so-viel-wasser-wie-normalerweise-32811188.html?utm_campaign=alle&utm_medium=rss-feed&utm_source=standard"
    stern_clean_url <- "https://www.stern.de/reise/fernreisen/iguaz%C3%BA-wasserfaelle-fuehren-zehnmal-so-viel-wasser-wie-normalerweise-32811188.html"
    expect_false(stern_dirty_url %in% x1$link)
    expect_true(stern_clean_url %in% x1$link)
    ## Stern's urls are not cleaned in x2
    expect_false(stern_clean_url %in% x2$link)
    expect_true(stern_dirty_url %in% x2$link)
    expect_false(stern_dirty_url %in% x3$link)
    expect_true(stern_clean_url %in% x3$link)
    ## sanity
    expect_false(stern_clean_url %in% output$link)
    expect_true(stern_dirty_url %in% output$link)
})
