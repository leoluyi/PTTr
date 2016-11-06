#' Extract list of posts from a board
#'
#' @param board_name String PTT board name.
#' @param max_post See \code{\link{get_urls}}().
#' @param include.push Logical. Whether to include push data.
#' @param mc.core Parallel cores.
#' @param ... other parameters passed to \code{\link{get_urls}}().
#'
#' @return data.table
#' @export
#'
#' @examples
#' get_all_posts("Gossiping", max_post = 10)
#'
get_all_posts <- function(board_name, max_post = 1000, include.push = FALSE,
                          mc.core = NULL, ...) {
  # bord_name = "Gossiping"
  # max_post = 100
  # post_urls = "https://www.ptt.cc/bbs/Gossiping/M.1468224573.A.D15.html"

  post_urls <- get_urls(board_name, max_post)

  ## get articles
  res_list <- lapply(
    post_urls,
    function(x) {
      tryCatch({
        post_data <- get_post_content(x, verbose = FALSE)
        if (!is.null(post_data$post_main))
          out <- data.table::as.data.table(post_data$post_main)
        else
          out <- NULL
        out
      }, error = function(e) {
        message(e, "[url] ",x)
        return(NULL)
      }, warning = function(w) {
        message(w, "[url] ",x)
      })
    })
  post_dt <- data.table::rbindlist(res_list, use.names = TRUE, fill = TRUE)
  post_dt
}
