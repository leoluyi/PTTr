#' Extract list of urls, titles, n_push from a board
#'
#' @param board_name String of PTT board name.
#' @param max_post Maximun number of the lastest posts. Default 1000.
#'        If set to \code{-1}, will get all of listpage_urls. [Caucious!].
#' @param parallel If \code{TRUE}, the default, will use parallel threads.
#' @param mc.cores Number of parallel cores to use. Default \code{NULL} to auto detect.
#'
#' @examples
#' get_posts_list("Gossiping", max_post = 100)
#' get_urls("Gossiping", max_post = 100)
#' get_titles("Gossiping", max_post = 100)
#'
#' @export
get_posts_list <- function(board_name, max_post = 1000L,
                           parallel = TRUE, mc.cores = NULL, ...) {

  ## Setting # of parallel cores
  if (parallel) {
    if (is.null(mc.cores)) {
      mc.cores <- ceiling(parallel::detectCores() *3/4)
    } else if (!is.numeric(mc.cores) || mc.cores < 1) {
      mc.cores <- 1L
    } else {
      mc.cores <- as.integer(mc.cores)
    }
  } else {
    mc.cores <- 1L
  }

  message(sprintf("Getting urls using %s thread(s)...", mc.cores), appendLF = FALSE)
  listpage_urls <- get_url_listpage(board_name)
  out <- get_post_url(listpage_urls, max_post, mc.cores)
  message(sprintf("(Got %s)", length(out[[1]])))

  tryCatch(
    setDT(out),
    error = function(e) {
      sapply(out, length, USE.NAMES = TRUE) %>% print
      stop(e)
    }
  )
  out
}

#' @export
#' @describeIn get_posts_list Get urls of posts in a board.
get_urls <- function(board_name, max_post = 1000L, parallel = TRUE, mc.cores = NULL, ...) {
  get_posts_list(board_name,
                 max_post=max_post, 
                 parallel=parallel,
                 mc.cores=mc.cores)[["post_urls"]]
}

#' @export
#' @describeIn get_posts_list Get post titles of posts in a board.
get_titles <- function(board_name, max_post = 1000L, mc.cores = -1, ...) {
  get_posts_list(board_name, max_post, mc.cores)[["title"]]
}

get_post_url = function(listpage_urls, max_post = 1000L, mc.cores = 1, ...) {
  # function input: listpage_urls
  # listpage_urls = get_url_listpage("Gossiping")[1:5]

  if (!is.numeric(max_post)) {
    stop("'max_post' must be integer")
  } else {
    max_post <- as.integer(max_post)
  }

  if (max_post < 0) {
    urls <- listpage_urls
  } else {
    url_front <- stringr::str_match(listpage_urls[[1]], "^(.*)index\\d+\\.html$")[,2]
    page_num <- stringr::str_match(listpage_urls, "index(\\d+)\\.html$")[,2]
    urls <- page_num %>%
      as.integer %>%
      sort(decreasing = TRUE) %>%
      head(max_post/20 + 1) %>% # 20 posts per page
      paste0(url_front, "index", .,  ".html")
  }

  ## Setting arallel cores
  cl <- parallel::makeCluster(mc.cores)
  on.exit(parallel::stopCluster(cl))
  parallel::clusterExport(cl, c("urls"), envir = environment())
  parallel::clusterExport(cl, "get_post_url_", envir = parent.env(environment()))
  # parallel::clusterEvalQ(cl, library(data.table))

  res <- parallel::parLapply(cl, urls, get_post_url_)

  post_urls <- res %>%
    lapply(function(x) x[["post_urls"]]) %>%
    unlist(use.names = FALSE)
  n_push <- res %>%
    lapply(function(x) x[["n_push"]]) %>%
    unlist(use.names = FALSE)
  title <- res %>%
    lapply(function(x) x[["title"]]) %>%
    unlist(use.names = FALSE)
  author <- res %>%
    lapply(function(x) x[["author"]]) %>%
    unlist(use.names = FALSE)

  if (max_post > 0) {
    post_urls <- post_urls %>% head(max_post)
    n_push <- n_push %>% head(max_post)
    title <- title %>% head(max_post)
    author <- author %>% head(max_post)
  }

  # function output: post_urls
  list(post_urls = post_urls, n_push = n_push, title = title, author = author)
}

# single url
get_post_url_ = function(listpage_url) {
  # listpage_url = "https://www.ptt.cc/bbs/Gossiping/index18285.html"

  res <- httr::GET(listpage_url, httr::set_cookies(over18 = 1))
  node <- httr::content(res, encoding = "utf8")
  node
  post_urls <- node %>%
    rvest::html_nodes(".title a") %>%
    rvest::html_attr("href") %>%
    sprintf("https://www.ptt.cc%s", .)
  n_push <- node %>%
    rvest::html_nodes(".n_push") %>%
    rvest::html_text()
  title <- node %>%
    rvest::html_nodes(".title a") %>%
    rvest::html_text() %>%
    stringr::str_trim()
  author <- node %>%
    rvest::html_nodes(".author") %>%
    rvest::html_text()

  keep <- which(author != "-")
  author <- author[keep]
  n_push <- n_push[keep]

  if (identical(length(post_urls), length(n_push))) {
    warning(sprintf("length of post_urls and n_push are not the same in %s", listpage_url))
  }

  list(post_urls = post_urls, n_push = n_push, title = title, author = author)
}
