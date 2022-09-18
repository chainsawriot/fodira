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
})
