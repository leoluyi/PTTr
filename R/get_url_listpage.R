####################################################
# getListPageUrls via board_name
####################################################

get_url_listpage = function(board_name) {
  # board_name = "Gossiping"

  board_url = sprintf("https://www.ptt.cc/bbs/%s/index.html", board_name)
  res <- GET(board_url, set_cookies(over18 = 1))

  if (http_error(res)) {
    stop("No existence of PTT board name: ", board_name)
  }

  node <- content(res, encoding = "utf8")
  maxPage = node %>%
    rvest::html_nodes(".wide:nth-child(2)") %>%
    rvest::html_attr("href") %>%
    stringr::str_match("(\\d+)\\.html$") %>% .[1, 2] %>%
    as.integer()

  allListUrls <- c(board_url,
                   sprintf("https://www.ptt.cc/bbs/%s/index%s.html",
                           board_name,
                           as.character(maxPage:1)))
  
  # function output: allListUrls
  allListUrls
}
