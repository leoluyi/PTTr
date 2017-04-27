#' Extract list of posts from a board
#'
#' @param board_name String PTT board name.
#' @param max_post See \code{\link{get_urls}}().
#' @param include_push Logical. Whether to include push data.
#' @param mc.cores Number of parallel cores to use. Use \code{-1} to auto detect.
#' @param ... other parameters passed to \code{\link{get_urls}}().
#'
#' @return data.table
#' @import parallel pbapply
#' @export
#'
#' @examples
#' get_all_posts("Gossiping", max_post = 10)
#'
get_all_posts <- function(board_name, max_post = 1000, list_push = FALSE,
                          parallel = TRUE, mc.cores = NULL, ...) {
  # board_name = "Gossiping"
  # max_post = 20
  # list_push = TRUE
  # parallel = TRUE
  # mc.cores = NULL
  # post_urls = "https://www.ptt.cc/bbs/Gossiping/M.1468224573.A.D15.html"

  ## Setting # of parallel cores
  if (!parallel) {
    mc.cores <- 1L
  } else if (is.null(mc.cores)) {
    mc.cores <- ceiling(parallel::detectCores() * 3/4)
  } else if (!is.numeric(mc.cores) || mc.cores < 1) {
    mc.cores <- 1L
  } else if (is.numeric(mc.cores)) {
    mc.cores <- as.integer(mc.cores)
  }

  post_urls <- get_urls(board_name, max_post)

  ## get articles
  message(sprintf("Getting %s posts with %s threads...",
                  length(post_urls), mc.cores))

  cl <- parallel::makeCluster(mc.cores)
  on.exit(stopCluster(cl))
  clusterExport(cl, c("post_urls"), envir = environment())
  clusterExport(cl, "get_post_content", envir = as.environment("package:PTTr"))
  invisible(clusterEvalQ(cl, library(data.table)))

  try_get_post_content <- function(x) {
    tryCatch({
      post_data <- get_post_content(x, verbose = FALSE)

      if (!is.null(post_data$post_main)) {
        out <- data.table::as.data.table(post_data$post_main)

        if (list_push) {
          out[, push_data := list(list(post_data$push))]
        } else {
          out$push_text <- post_data$push$push_text %>% paste(collapse=" ")
        }
      } else {
        out <- NULL
      }
      out
    }, error = function(e) {
      message(e, "[url] ",x)
    }, warning = function(w) {
      message(w, "[url] ",x)
    })
  }

  res_list <- pbapply::pblapply(
    cl=cl,
    post_urls,
    try_get_post_content
  )

  post_dt <- data.table::rbindlist(res_list, use.names = TRUE, fill = TRUE)
  post_dt
}
