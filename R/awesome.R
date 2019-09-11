library(magrittr)
library(dplyr)
library(httr)

#' @title Get link to raw README.md file
#' @name raw_github_readme
#' @description This functions checks, if URL is not a link to the raw Github README.md file.
#' If doesn't, it extracts user and repo names from the URL and then fetches README.md URL using Github API v3.
#' @param url Github repo URL
#' @return Dowload URL to README.md
#' @import dplyr
#' @examples
#' github_readme_url("https://github.com/qinwf/awesome-r/")
#' github_readme_url(""https://raw.githubusercontent.com/qinwf/awesome-R/master/README.md"")
#' @export
github_readme_url <- function(url){
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
#' @example
#' is_github("https://github.com/qinwf/awesome-r/")
is_github <- function(url){
  grepl("(https://)?github.com/", url) |
    grepl("(https://)?raw.githubusercontent.com", url)
}

#' @title Fetch and parse an Awesome List README.md file
#' @name awesome_list
#' @description Dowloads an Awesome List and transform it to the form of easy-to-use data.frame.
#' Optionally, the function may fetch some additional data describing libraries hosted on Github.
#' @param url URL for Github repository
#' @param github.topics
awesome_list <- function(url, github.info = FALSE){

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

    awesome.list <- cbind(awesome.list, awesome.list)

    return(awesome.list)
}

# Github API v3 -----------------------------------------------------------

#' @title Get basic information about Github repo
#' @name github_repo_info
#' @param user character username
#' @param repo character repo name
#' @examples
#' github_topics("qinwf", "awesome-r")
#' @export
github_repo_info <- function(user, repo){
  sprintf("http://api.github.com/repos/%s/%s", user, repo) %>%
    GET() %>%
    content()
}


#' @title Get Github Topics for given username and repo
#' @name github_topics
#' @description Get Github Topics using Github API v3
#' @param user character username
#' @param repo character repo name
#' @examples
#' github_topics("qinwf", "awesome-r")
#' @export
github_topics <- function(user, repo){
  sprintf("http://api.github.com/repos/%s/%s/topics", user, repo) %>%
    GET(accept("application/vnd.github.mercy-preview+json")) %>%
    content()
}

#' @title Get basic info about Github repo
#' @name github_repo_basic_info
#' @description Get Github Topics using Github API v3
#' @param user character username
#' @param repo character repo name
#' @examples
#' github_topics("qinwf", "awesome-r")
#' @export
github_repo_basic_info <- function(user, repo){

  repo.info <- github_repo_info(user, repo)
  github.topics <- github_topics(user, repo)

  output <- list(
    # Basic info
    github.desc = repo.info$description,
    stars = repo.info$stargazers_count,
    forks = repo.info$forks_count,

    # Topics
    topics = paste0(github.topics %>% unlist(), collapse = ",")
  )
  return(output)
}

# awesome_list("https://github.com/qinwf/awesome-r/")
# install.packages("pkgdown")
# pkgdown::build_site()
