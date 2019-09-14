library(magrittr)
library(dplyr)
library(httr)

#' @title Get link to raw README.md file
#' @name github_readme_url
#' @description This functions checks, if URL is not a link to the raw Github README.md file.
#' If doesn't, it extracts user and repo names from the URL and then fetches README.md URL using Github API v3.
#' @param url Github repo URL
#' @return Dowload URL to README.md
#' @import dplyr httr magrittr stringr
#' @examples
#' github_readme_url("https://github.com/qinwf/awesome-r/")
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

#' @title Get basic information about Github repo
#' @name github_repo_info
#' @description Get all repo info offered by the main type of Gihub API requests.
#' @param user character username
#' @param repo character repo name
#' @return List of Github repo attributes
#' @import dplyr httr magrittr
#' @references
#' For detailed info see: https://developer.github.com/v3/
#' @examples
#' github_repo_info("qinwf", "awesome-r")
#' @export
github_repo_info <- function(repo.url, user=NA, repo=NA){

  repo.names <- github_extract_user_repo(repo.url)
  user <- repo.names$user
  repo <- repo.names$repo

  sprintf("http://api.github.com/repos/%s/%s", user, repo) %>%
    GET() %>%
    content()
}

#' @title Get Github Topics for given username and repo
#' @name github_topics
#' @description Get Github Topics using Github API v3.
#' @param user character username
#' @param repo character repo name
#' @return List of Github Topics which describe the repo
#' @import dplyr httr magrittr
#' @examples
#' github_topics("easystats", "see")
#' @export
github_topics <- function(repo.url, user=NA, repo=NA){

  repo.names <- github_extract_user_repo(repo.url)
  user <- repo.names$user
  repo <- repo.names$repo

  sprintf("http://api.github.com/repos/%s/%s/topics", user, repo) %>%
    GET(accept("application/vnd.github.mercy-preview+json")) %>%
    content()
}

#' @title Get basic info about Github repo
#' @name github_repo_basic_info
#' @description Get basic info about Githb repo, i. e. number of stars, number of forks and topics.
#' @param user character username
#' @param repo character repo name
#' @return Combined basic info from two functions: `github_repo_info` and `github_topics`
#' @import dplyr httr magrittr
#' @examples
#' github_repo_basic_info("tidyverts", "tsibble")
#' @export
github_repo_basic_info <- function(repo.url, user=NA, repo=NA){

  repo.info <- github_repo_info(repo.url)
  github.topics <- github_topics(repo.url)

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

#' @title Extract user name and repo name from a Github repo URL
#' @name github_extract_user_repo
#' @param repo.url A Github repo.url, character
#' @return List with two values: user.name and repo.name
#' @example
#' github_extract_user_repo("https://github.com/strengejacke/sjstats")
github_extract_user_repo <- function(repo.url){
  # Strip trailing slash
  url <- sub("/+$", "", repo.url)
  url.divided <- stringr::str_match(url, "github.com\\/(.*)\\/(.*)")
  user.name <- url.divided[,2]
  repo.name <- url.divided[,3]
  list(user.name = user.name,
       repo.name = repo.name)
}

# .parse_args <- function(repo.url, user, repo){
#
#   print(repo.url)
#   print(user)
#   print(repo)
#
#   if(!is.na(repo.url) & is.na(user) & is.na(repo)){}
#   else if(is.na(repo.url) & !is.na(user) & !is.na(repo)){
#     github_extract_user_repo()
#   } else {
#     stop("You should pass repo.url or (user and repo)")
#   }
# }
