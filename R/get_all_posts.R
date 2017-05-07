#' Extract list of posts from a board
#'
#' @param board_name String PTT board name.
#' @param max_post See \code{\link{get_post_urls}}().
#' @param list_push If \code{TRUE}, push data will be column of list. Default is \code{FALSE}.
#' @param parallel If \code{TRUE}, the default, will use parallel threads.
#' @param mc.cores Number of parallel cores to use. Default \code{NULL} to auto detect.
#' @param ... other parameters passed to \code{\link{get_post_urls}}().
#'
#' @return data.table
#' @import data.table parallel pbapply
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

  post_urls <- get_post_urls(board_name, max_post,
                        parallel = parallel, mc.cores = mc.cores)

  post_dt <- get_all_posts_from_url(post_urls, max_post = max_post, list_push = list_push,
                         parallel = parallel, mc.cores=mc.cores)
  post_dt[]
}

#' @param post_urls Post urls.
#' @export
#' @describeIn get_all_posts Extract list of posts from a URLs
get_all_posts_from_url <- function(post_urls, max_post = 1000, list_push = FALSE,
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
  
  ## get articles
  message(sprintf("Getting %s posts with %s thread(s)...",
                  length(post_urls), mc.cores))
  
  cl <- parallel::makeCluster(mc.cores)
  on.exit(stopCluster(cl))
  clusterExport(cl, c("post_urls"), envir = environment())
  clusterExport(cl, list("get_post_content"), envir = as.environment("package:PTTr"))
  invisible(clusterEvalQ(cl, library(data.table)))
  
  res_list <- pbapply::pblapply(
    cl=cl,
    post_urls,
    try_get_post_content,
    max_error_time = 3,
    list_push = list_push
  )
  
  # res_list <- lapply(
  #   post_urls,
  #   try_get_post_content
  # )
  
  post_dt <- data.table::rbindlist(res_list, use.names = TRUE, fill = TRUE)
  post_dt[]
}


try_get_post_content <- function(post_url, max_error_time = 3, list_push = FALSE) {
  # post_url = "https://www.ptt.cc/bbs/Gossiping/M.1468224573.A.D15.html"
  
  out <- tryCatch({
    post_data <- get_post_content(post_url, max_error_time = 3, verbose = FALSE)
    
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
    print(post_url)
    message(e, "[url] ", post_url)
    NULL
  }, warning = function(w) {
    message(w, "[url] ", post_url)
    NULL
  })
  out
}
