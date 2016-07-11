####################################################
# getPostUrls via listPageUrl
####################################################

#' get_url_post
#'
#' Get urls of posts that listed in the list page.
#'
#' @param listPageUrl The url of the list page.
#' @examples
#' listpage_urls = get_url_listpage("Gossiping")[1:5]
#' postUrls = unlist(lapply(listPageUrls,getPostUrls))
#' postUrls
#' @import httr rvest stringr
#' @export
get_url_post = function(listpage_urls) {
  # function input: listpage_urls
  # listpage_urls = get_url_listpage("Gossiping")[1:5]

  post_urls <- lapply(listpage_urls, FUN = get_url_post_) %>%
    unlist(use.names = FALSE)
  # function output: post_urls
}

# single url
get_url_post_ = function(listpage_url) {
  # listpage_url = get_url_listpage("Gossiping")[[10]]

  res <- GET(listpage_url, set_cookies(over18 = 1))
  node = content(res, encoding = "utf8")
  post_urls <- node %>%
    rvest::html_nodes(".title a") %>%
    rvest::html_attr("href") %>%
    sprintf("https://www.ptt.cc%s", .)
}
