####################################################
# getPostData via postUrl
####################################################

#' getPostData
#'
#' Get the content of the post.
#'
#' @param post_url URL of a post.
#' @examples
#' get_post_content("https://www.ptt.cc/bbs/Gossiping/M.1467117389.A.62D.html")
#'
#' @importFrom dplyr bind_rows
#' @export
get_post_content = function(post_url, max_error_time = 3) {
  # function input: postUrl
  # post_url <- "https://www.ptt.cc/bbs/Gossiping/M.1467117389.A.62D.html"
  if (! length(post_url) == 1) {
    post_url <- post_url[1]
    warning("only use first url\n[", post_url, "]")
  }
  res <- GET(post_url, set_cookies(over18 = 1))

  error_time = 1
  while (error_time <= max_error_time && http_error(res)) {
    Sys.sleep(0.5)
    message(sprintf("Connection error (try %s) [%s]: %s",
                    error_time ,
                    status_code(res),
                    post_url))
    res <- GET(post_url, set_cookies(over18 = 1))
    error_time = error_time + 1
    if (error_time > max_error_time) {
      return(NULL)
    }
  }

  node = content(res, encoding = "UTF-8")

  postData = list()
  postData$board = node %>%
    rvest::html_nodes(".article-metaline-right > .article-meta-value") %>%
    rvest::html_text()

  metaTemp = node %>%
    rvest::html_nodes(".article-metaline > .article-meta-value") %>%
    rvest::html_text()

  postData$author = metaTemp[[1]]
  postData$author_ip = node %>%
    html_text() %>%
    str_match_all('(?:From|來自): ([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})') %>%
    .[[1]] %>% .[,2] %>%
    tail(1)  # select last ip
  postData$title = metaTemp[[2]]
  postData$post_time = metaTemp[[3]]

  postData$post_url = post_url
  postData$post_id = gsub("[/]|\\.html", "",
                          unlist(strsplit(post_url, postData$board))[[2]])
  postData$post_text = node %>%
    rvest::html_nodes("*#main-content") %>%
    as.character() %>%
    str_replace_all('(?s)\\<div class="push"\\>.*\\<\\/div\\>', "") %>% # clear push
    str_replace_all('(?s).*\\<span class="article-meta-value"\\>.*\\d{2}:\\d{2}:\\d{2}[ ]\\d{4}\\<\\/span\\>', "") %>%
    read_html() %>%
    html_text() %>%
    str_replace('(?s)--\\n\\u203b \\u767c\\u4fe1\\u7ad9: \\u6279\\u8e22\\u8e22\\u5be6\\u696d\\u574a.*$', "")

  push_df_data = lapply(node %>%
                          rvest::html_nodes("div.push"),
                        function (x) {
                          push <- html_text(html_nodes(x, "span"))
                          push <- dplyr::data_frame(
                            push_tag = str_trim(push[1]),
                            push_uid = str_trim(push[2]),
                            push_text = str_replace(push[3], "^:", ""),
                            push_ip =  str_extract(
                              str_trim(push[4]),
                              "^(?:\\d{2,3}\\.){3}\\d{2,3}"),
                            push_time = str_extract(
                              str_trim(push[4]),
                              "\\d{2}\\/\\d{2}\\s\\d{2}:\\d{2}$"))
                        }) %>% dplyr::bind_rows()

  push_df <- cbind(post_id = postData$post_id,
                   post_url = post_url,
                   push_df_data)

  # function output:
  structure(list(post_main = postData,
                 push = push_df),
            class = "ptt_post")

}


#' @export
print.ptt_post <- function(x, ..., max.lines = 10, width = getOption("width")) {
  cat("[", x$post$post_url, "]\n\n", sep = "")
  cat("$", names(x)[[1]], "\n", sep = "")
  cat("  Board: ", x$post$board, "\n", sep = "")
  cat("  Title: ", x$post$title, "\n", sep = "")
  cat("  Author: ", x$post$author, "\n", sep = "")
  cat("  Post-Time: ", x$post$post_time, "\n\n", sep = "")

  ## print
  # code modified from:
  # https://github.com/hadley/httr/blob/25557c0327ef2a4cae13db017813fbd2ed86d3c0/R/response.r
  size <- length(x$post_main$post_text)
  if (size == 0) {
    cat("<EMPTY POST TEXT>\n")
  } else {
    cat("<POST TEXT>\n")
    breaks <- gregexpr("\n", x$post_main$post_text, fixed = TRUE)[[1]]
    last_line <- breaks[min(length(breaks), max.lines)]
    lines <- strsplit(substr(x$post_main$post_text, 1, last_line), "\n")[[1]]

    too_wide <- nchar(lines) > width
    lines[too_wide] <-
      paste0(substr(lines[too_wide], 1, width - 3), "...")

    cat(lines, sep = "\n")
    if (max.lines < length(breaks))
      cat("...\n")
  }

  cat("\n$", names(x)[[2]], "\n", sep = "")
  size_push <- nrow(x$push)
  if (size_push == 0) {
    cat("<EMPTY PUSH>\n")
  } else {
    cat("<PUSH CONTENT>\n")
    head_push <- head(x$push)[c("push_tag", "push_uid", "push_text")]
    apply(head_push, MARGIN = 1, FUN = function(x){
      cat("  ", x, "\n")
      invisible(NULL)
    })

    if (max.lines < size_push)
      cat("   ...\n")
  }

  invisible(x)
}
