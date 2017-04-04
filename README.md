# PTTr

PTTr is a web-based PTT crawler. Instead of `data.frame`, there are use of  `data.table` class to be convinient for data manipulation.

## Installation

PTTr is currently only available on github. Run the following code to install it:

```r
# install.packages("devtools")
devtools::install_github("leoluyi/PTTr")
```

## Get Started

```r
library(data.table)
library(PTTr)

get_post_content("https://www.ptt.cc/bbs/Gossiping/M.1467117389.A.62D.html")
dt <- get_all_posts("Gossiping", max_post = 10)


# get_all_posts("Gossiping", max_post = 10)
```

## To-do

- 推噓文數 (從文章頁的 xml 下手)
- datetime parsing


