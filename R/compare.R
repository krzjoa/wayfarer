#' @title Fetch and parse an Awesome List README.md file
#' @name awesome_list
#' @description Dowloads an Awesome List and transform it to the form of easy-to-use data.frame.
#' Optionally, the function may fetch some additional data describing libraries hosted on Github.
#' @param url URL for Github repository
#' @param github.basic.info Logical; fetch additional basic info about the repo
#' @return Summarized data frame
#' @example
awesome_list <- function(url, github.basic.info = FALSE){

  if(!is_github(url))
    print(paste0(url, " is not a corect Github URL."))

  # Converting URL to its raw form
  url <- github_readme_url(url)

  # Fetching markdown from the Web
  file.content <- suppressWarnings(readLines(url))

  # Parsing markdown list
  file.content %>%
    stringr::str_match("\\[(.*?)\\]\\((.*?)\\).?-(.*\\.)") -> awesome.list

  # Removing empty lines
  awesome.list <- awesome.list %>%
    na.omit() %>%
    as.data.frame() %>%
    `colnames<-`(c("original", "name", "url", "description"))

  # Username & repo
  awesome.list$url %>%
    stringr::str_match("github.com\\/(.*)\\/(.*)") %>%
    as.data.frame() %>%
    `colnames<-`(c("full.name", "user.name", "repo.name")) %>%
    select(-full.name) -> awesome.list.urls

  awesome.list <- cbind(awesome.list, awesome.list.urls)

  # if(github.basic.info){
  #
  #   github.urls <- awesome.list %>%
  #     filter(is_github(url)) %>%
  #     .$url
  #
  # }

  return(awesome.list)
}

# aws <- awesome_list("https://github.com/qinwf/awesome-r/")
#
# github.urls <- aws %>%
#   filter(is_github(url)) %>%
#   .$url %>% as.character()

# sapply(github.urls, github_repo_basic_info) -> basic.info

# github_repo_basic_info("https://github.com/r-lib/vroom")

# install.packages("pkgdown")
# devtools::document()
# pkgdown::build_site()

