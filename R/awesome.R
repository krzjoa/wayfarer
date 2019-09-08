library(magrittr)
library(dplyr)
library(httr)

url <- "https://github.com/qinwf/awesome-r/"

#' @title Get link to raw README.md file
#' @name raw_github_readme
#' @description This functions checks, if URL is not a link to the raw Github README.md file.
#' If doesn't, it extracts user and repo names from the URL and then fetches README.md URL using Github API v3.
#' @param url Github repo URL
#' @return Dowload URL to README.md
#' @examples
#' raw_github_readme("https://github.com/qinwf/awesome-r/")
#' raw_gihub_readme(""https://raw.githubusercontent.com/qinwf/awesome-R/master/README.md"")
#' @export
raw_github_readme <- function(url){
  if(grepl("(https://)?raw.githubusercontent.com.*\\.md", url)) return(url)

  # Strip trailing slash
  url <- sub("/+$", "", url)

  url.divided <- stringr::str_match(url, "github.com\\/(.*)\\/(.*)")
  user.name <- url.divided[,2]
  repo.name <- url.divided[,3]

  sprintf("http://api.github.com/repos/%s/%s/readme", user.name, repo.name) %>%
    GET() %>%
    content() -> readme
  return(readme$download_url)
}

#' @title Check, if URL is a Github link
#' @name is_github
#' @description This functions returns true if link leads us to github.com or raw.githubusercontent.com
#' @param url Github repo URL
#' @return Logical value: if URL is from Github?
#' @example is_github("https://github.com/qinwf/awesome-r/")
is_github <- function(url){
  grepl("(https://)?github.com/", url) |
    grepl("(https://)?raw.githubusercontent.com", url)
}

#'
awesome_list <- function(url, github.info = FALSE){

    if(!is_github(url))
      print(paste0(url, " is not a corect Github URL."))

    # Converting URL to its raw form
    url <- raw_github_readme(url)

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

    awesome.list <- cbind(awesome.list, awesome.list)

    return(awesome.list)
}


call_github_api_v3 <- function(user, repo){

  # Get basic info about repo
  sprintf("http://api.github.com/repos/%s/%s", user, repo) %>%
    GET() %>%
    content() -> basic.info

  # Get topics
  sprintf("http://api.github.com/repos/%s/%s/topics", user, repo) %>%
    GET(accept("application/vnd.github.mercy-preview+json")) %>%
    content() -> topics

  output <- list(
    # Basic info
    github.desc = basic.info$description,
    stars = basic.info$stargazers_count,
    forks = basic.info$forks_count,

    # Topics
    topics = paste0(topics %>% unlist(), collapse = ",")
  )

  return(output)
}

awesome_list("https://github.com/qinwf/awesome-r/")

