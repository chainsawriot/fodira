##require(magrittr)


.paywall_xpath <- list()
.paywall_xpath[["Cicero"]] <- "//div[contains(@class, 'paywall-header')]"
.paywall_xpath[["Spiegel"]] <- "//div[contains(@data-target-id, 'paywall')]"
.paywall_xpath[["Stern"]] <- "//section[contains(@class, 'paid-barrier--context-article')]"
.paywall_xpath[["FAZ"]] <- "//section[contains(@class, 'atc-ContainerPaywall')]"
.paywall_xpath[["SZ"]] <- "//offer-page"
.paywall_xpath[["Handelsblatt"]] <- "//div[contains(@class, 'o-paywall__content')]"
.paywall_xpath[["Welt"]] <- "//div[contains(@class, 'contains_walled_content')]"
.paywall_xpath[["Berliner Zeitung"]] <- "//div[contains(@class, 'paywall_overlay__CR1uO')]"
.paywall_xpath[["Hildesheimer Allgemeine Zeitung"]] <- "//div[contains(@class, 'o-paywall')]"
.paywall_xpath[["TA"]] <- "//p[contains(@class, 'obfuscated')]"
.paywall_xpath[["nordbayern.de"]] <- "//div[contains(@class, 'paywall')]"
.paywall_xpath[["Stuttgarter Zeitung"]] <- "//div[contains(@id, 'taboola-below-paid-article-thumbnails')]"
.paywall_xpath[["Tagesspiegel"]] <- "//div[contains(@class, 'article--paid')]"
.paywall_xpath[["Berliner Morgenpost"]] <- "//div[contains(@id, 'paywall-container')]"
.paywall_xpath[["WAZ"]] <- "//div[contains(@id, 'paywall-container')]"
.paywall_xpath[["Weser Kurier"]] <- "//div[contains(@class, 'paywall__overlay')]"
.paywall_xpath[["Hamburger MoPo"]] <- "//div[contains(@id, 'paywall')]"
.paywall_xpath[["RND"]] <- "//div[contains(@class, 'paywalledContent')]"
.paywall_xpath[["Ostsee-Zeitung"]] <- .paywall_xpath[["RND"]]
.paywall_xpath[["LVZ"]] <- .paywall_xpath[["RND"]]
.paywall_xpath[["KN"]] <- .paywall_xpath[["RND"]]
.paywall_xpath[["akweb"]] <- "//section[contains(@class, 'wp-block-ak-subscription')]"
.paywall_xpath[["Epoch Times"]] <- "//div[contains(@id, 'premium-content')]"
.paywall_xpath[["Freitag"]] <- "//div[contains(@class, 'c-paywall-banner')]"
.paywall_xpath[["Compact"]] <- "//div[contains(@id, 'wpmem_restricted_msg')]"
.paywall_xpath[["Junge Freiheit"]] <- "//div[contains(@class, 'paywall-content-block')]"
.paywall_xpath[["Jungle World"]] <- "//div[contains(@class, 'subscription-only-block')]"
.paywall_xpath[["Junge Welt"]] <- "//a[contains(@title, 'Onlineabo abschließen')]"
.paywall_xpath[["blaetter.de"]] <- "//a[contains(@class, 'button--buy')]"

.check_element <- function(parsed_html, xpath) {
    isFALSE(identical(
        rvest::html_text(rvest::html_elements(parsed_html, xpath = xpath)),
        character(0)))
}

.check_other_funs <- list()

.check_other_funs[["Jacobin"]] <- function(parsed_html) {
    if(.check_element(parsed_html, "//input[contains(@id, 'username')]")) {
        return(TRUE)
    }
    length(rvest::html_elements(parsed_html, xpath = "//svg[contains(@class, 'spinner__svg')]")) > 0
}

.check_other_funs[["DWN"]] <- function(parsed_html) {
    xpaths <- c("//div[contains(@class, 'message notice')]",
                "//div[contains(@class, 'gustly')]",
                "//div[contains(@id, 'article-teaser-blocks')]")
    any(purrr::map_lgl(xpaths, ~.check_element(parsed_html, .)))
}

.check_other_funs[["RP Online"]] <- function(parsed_html) {
    xpaths <- c("//div[contains(@class, 'park-paywall-content')]",
                "//p[contains(@class, 'text-blurred')]")
    any(purrr::map_lgl(xpaths, ~.check_element(parsed_html, .)))
}

.check_other_funs[["Allgemeine Zeitung"]] <- function(parsed_html) {
    xpaths <- c("//div[contains(@class, 'storyElementWrapper__paywallContainer')]",
                "//div[contains(@class, 'vrm-cce__paywall')]")
    any(purrr::map_lgl(xpaths, ~.check_element(parsed_html, .)))
}

.check_other_funs[["Bild"]] <- function(parsed_html) {
    if(.check_element(parsed_html, "//div[contains(@class, 'offer-module')]")) {
        return(TRUE)
    }
    h2_text <- rvest::html_text(
                          rvest::html_elements(parsed_html,
                                               xpath = "//body[contains(@id, 'article')]//div/div/h2"))
    if (length(h2_text) == 0) {
        return(FALSE)
    }
    any(h2_text == "Warum sehe ich BILD.de nicht?")
}

.check_error <- function(parsed_html) {
    title <- rvest::html_text(rvest::html_elements(parsed_html, xpath = "//title"))
    if (title %in% c("403 Forbidden", "404 Not Found")) {
        return(TRUE)
    }
    return(FALSE)
}

.vcat <- function(verbose = TRUE, ...) {
    if (isTRUE(verbose)) {
        message(..., "\n")
    }
}

check_paywall <- function(fname, pub, verbose = FALSE) {
    parsed_html <- rvest::read_html(fname)
    if (.check_error(parsed_html)) {
        .vcat(verbose, "403/404, return NA")
        return(NA)
    }
    if (isFALSE(pub %in% c(names(.check_other_funs), names(.paywall_xpath)))) {
        .vcat(verbose, "Don't know how to check this `pub`, assume no paywall.")
        return(FALSE)
    }
    if (pub %in% names(.paywall_xpath)) {
        return(.check_element(parsed_html, .paywall_xpath[[pub]]))
    }
    .check_other_funs[[pub]](parsed_html)
}

## if (pub == "Achse des Guten") {
##     html %>% rvest::html_elements(xpath = "//title") %>%
##         rvest::html_text() -> text_
##     value <- identical(text_, "403 Forbidden")
## }


## if (pub == "Badische Zeitung") {
##   html %>% rvest::html_elements(xpath = "//section[contains(@id, 'regWalli')]") %>%
##     rvest::html_text() -> text_
##   value <- !identical(text_, character(0))
## }
## 
## if (pub == "Hildesheimer Allgemeine Zeitung") {
##   html %>% rvest::html_elements(xpath = "//div[contains(@class, 'o-paywall')]") %>%
##     rvest::html_text() -> text_
##   value <- !identical(text_, character(0))
## }
##
## if (pub == "Saarbrücker Zeitung") {
##   html %>% rvest::html_elements(xpath = "//span[contains(@title, 'Jetzt weiterlesen!')]") %>%
##     rvest::html_text() -> text_
##   value <- !identical(text_, character(0))
## }
##
## if (pub == "Zeit") {
##   html %>% rvest::html_elements(xpath = "//aside[contains(@id, 'paywall')]") %>%
##     rvest::html_text() -> text_
##   value <- !identical(text_, character(0))
## }

## checkfunction <- function(file, pub, folder) {
##     value <- check_paywall(rvest::read_html(paste(folder, file, sep = "/")), pub = pub)
##     return(value)
## }

## checkfunction_2 <- function(tab, folder) {
##     value <- c()
##     for (i in 1:nrow(tab)) {
##         value <- c(value, checkfunction(stringr::str_replace_all(tab$htmlfile[i], ":", "_"), tab$pub[i], folder = folder))
##         print(i)
##     }
##     return(value)
## }
