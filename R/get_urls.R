#' Extract list of urls from a board
#'
#' @param board_name String of PTT board name.
#' @param max_post Maximun number of the lastest posts. Default 1000.
#'        If set to \code{-1}, will get all of listpage_urls. [Caucious!]
#'
#' @examples
#' post_urls <- get_urls("Gossiping", max_post = 100)
#' post_urls
#'
#' @export
get_urls <- function(board_name, max_post = 1000L, mc.cores = -1, ...) {

  ## Setting # of parallel cores
  if (mc.cores == -1L) {
    mc.cores <- parallel::detectCores()-1
  } else if (!is.numeric(mc.cores) || mc.cores < 1) {
    mc.cores <- 1L
  } else {
    mc.cores <- as.integer(mc.cores)
  }

  listpage_urls <- get_url_listpage(board_name)
  post_urls <- get_post_url(listpage_urls, max_post, mc.cores)[["post_urls"]]
  post_urls
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
  nrec <- res %>%
    lapply(function(x) x[["nrec"]]) %>%
    unlist(use.names = FALSE)
  title <- res %>%
    lapply(function(x) x[["title"]]) %>%
    unlist(use.names = FALSE)

  if (max_post > 0) {
    post_urls <- post_urls %>% head(max_post)
    nrec <- nrec %>% head(max_post)
    title <- title %>% head(max_post)
  }

  # function output: post_urls
  list(post_urls = post_urls, nrec = nrec, title = title)
}

# single url
get_post_url_ = function(listpage_url) {
  # listpage_url = PTTr:::get_url_listpage("Gossiping")[[10]]

  res <- httr::GET(listpage_url, set_cookies(over18 = 1))
  node <- httr::content(res, encoding = "utf8")
  post_urls <- node %>%
    rvest::html_nodes(".title a") %>%
    rvest::html_attr("href") %>%
    sprintf("https://www.ptt.cc%s", .)
  nrec <- node %>%
    rvest::html_nodes(".nrec") %>%
    rvest::html_text()
  title <- node %>%
    rvest::html_nodes(".title a") %>%
    rvest::html_text()

  if (identical(length(post_urls), length(nrec))) {
    warning(sprintf("length of post_urls and nrec are not the same in %s", listpage_url))
  }

  list(post_urls = post_urls, nrec = nrec, title = title)
}
