####################################################
# getListPageUrls via boardName
####################################################

#' get_url_listpage
#'
#' Get urls of the list pages, each contains certain posts of the input board.
#'
#' @param boardName The name of a board.
#' @examples
#' listPageUrls = get_url_listpage("Gossiping")[1:5]
#' listPageUrls
#' @export
get_url_listpage = function(boardName) {
  # function input: boardName
  # boardName = "Gossiping"

  boardUrl = sprintf("https://www.ptt.cc/bbs/%s/index.html", boardName)
  res <- GET(boardUrl, set_cookies(over18 = 1))
  node = content(res, encoding = "utf8")
  maxPage = node %>%
    rvest::html_nodes(".wide:nth-child(2)") %>%
    rvest::html_attr("href") %>%
    stringr::str_match("(\\d+)\\.html$") %>% .[1, 2] %>%
    as.integer()

  allListUrls = sprintf("https://www.ptt.cc/bbs/%s/index%s.html",
                        boardName,
                        as.character(maxPage:1))
  # function output: allListUrls
  allListUrls
}
