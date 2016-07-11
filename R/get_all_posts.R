#' get_all_posts
#'
#' Get all posts from a given board name
#'
#' @param board_name PTT board name
#' @param max_post See get_post_url()
#' @param ... other parameters passed to get_post_url()
#'
#' @return data.table
#'
#' @importFrom data.table data.table rbindlist
#' @export
#'
#' @examples
#' get_all_posts("gissiping")
#'
get_all_posts <- function(board_name, max_post = 1000, ...) {
  # bord_name <- "Gossiping"
  listpage_urls <- get_url_listpage(bord_name)
  post_urls <- get_post_url(listpage_urls, max_post, ...)

  get_post_content("https://www.ptt.cc/bbs/Gossiping/M.1468205607.A.865.html")
  res_list <- lapply(
    post_urls,
    function(x) {
      post_data <- get_post_content(x, max_post)
      if (!is.null(post_data))
        out <- data.table::as.data.table(post_data$post_main)
      else
        out <- NULL
      out
    })
  post_dt <- rbindlist(res_list)
  post_dt
}
