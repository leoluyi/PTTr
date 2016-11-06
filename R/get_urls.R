#' Extract list of urls from a board
#'
#' @param board_name String of PTT board name.
#' @param max_post Maximun number of the lastest posts. Default 1000.
#'        If NULL, will get all of listpage_urls. [Caucious!]
#'
#' @examples
#' post_urls <- get_all_url("Gossiping", max_post = 100)
#' post_urls
#'
#' @export
get_urls <- function(board_name, max_post = 1000L, ...) {
  listpage_urls <- get_url_listpage(board_name)
  post_urls <- get_post_url(listpage_urls, max_post)
  post_urls
}

get_post_url = function(listpage_urls, max_post = 1000L, ...) {
  # function input: listpage_urls
  # listpage_urls = get_url_listpage("Gossiping")[1:5]

  if (is.null(max_post)) {
    post_urls <- lapply(listpage_urls, FUN = get_post_url_) %>%
      unlist(use.names = FALSE)
  } else {
    if (!is.numeric(max_post)) {stop("'max_post' must be integer")}
    url_front <- stringr::str_match(listpage_urls[[1]], "^(.*)index\\d+\\.html$")[,2]
    page_num <- stringr::str_match(listpage_urls, "index(\\d+)\\.html$")[,2]
    urls_sub <- page_num %>%
      as.integer %>%
      sort(decreasing = TRUE) %>%
      head(max_post/20 + 1) %>% # 20 posts per page
      paste0(url_front, "index", .,  ".html")
    post_urls <- lapply(urls_sub, FUN = get_post_url_) %>%
      unlist(use.names = FALSE)
    post_urls <- post_urls %>% head(max_post)
  }
  # function output: post_urls
  post_urls
}

# single url
get_post_url_ = function(listpage_url) {
  # listpage_url = get_url_listpage("Gossiping")[[10]]

  res <- GET(listpage_url, set_cookies(over18 = 1))
  node = content(res, encoding = "utf8")
  post_urls <- node %>%
    rvest::html_nodes(".title a") %>%
    rvest::html_attr("href") %>%
    sprintf("https://www.ptt.cc%s", .)
  post_urls
}
