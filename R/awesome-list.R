#' @title Fetch and parse an Awesome List README.md file
#' @name awesome_list
#' @description Dowloads an Awesome List and transform it to the form of easy-to-use data.frame.
#' Optionally, the function may fetch some additional data describing libraries hosted on Github.
#' @param url URL for Github repository
#' @import dplyr magrittr httr
#' @return Summarized data frame
#' @examples
#' awesome_list('https://github.com/lauris/awesome-scala')
awesome_list <- function(url){

  # TODO: parse headers of diffrerent levels
  if(!is_github(url))
    stop(paste0(url, " is not a corect Github URL."))

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

  return(awesome.list)
}

#' @title Compare two awesome lists
#' @name diff_awesome_lists
#' @param url1 Github URL to an awesome list
#' @param url2 Github URL to an awesome list
#' @param names.to.lower Use lowercase only in item names
#' @description Return items, which are present in the first list, but in the second one - they don't
#' Difference is being done by comparing library names
#' @return A data.frame, which contains ites existing only in the first awesome list
#' @examples
#' diff_awesome_lists("https://github.com/qinwf/awesome-r/", "https://github.com/rstudio/RStartHere")
diff_awesome_lists <- function(url1, url2, names.to.lower=TRUE){

  awesome.list.1 <- awesome_list(url1)
  awesome.list.2 <- awesome_list(url2)

  if(names.to.lower){
    awesome.list.1 <- awesome.list.1 %>% mutate(name = process.names(name))
    awesome.list.2 <- awesome.list.2 %>% mutate(name = process.names(name))
  }

  left_join(awesome.list.1, awesome.list.2, by=c("name"), suffix = c("", ".y")) %>%
    filter(is.na(url.y)) -> not.in.awesome.list.2

  cols.to.filter <- colnames(not.in.awesome.list.2) %>%
    .[grepl(".y", .)]

  not.in.awesome.list.2 %>%
      select(-!!cols.to.filter)
}

#' @title Transform string to a markdown url list
#' @name as_mardkown_url_list
#' @param items A data.frame, which should contain following columns: `name`, `url` and `description`.
#' Typically the list is condsidered to be produced by `awesome_list` or `diff_awesome_lists`.
#' @return String with markodwn-formatted awesome list
#' @examples
#' diff_awesome_lists("https://github.com/qinwf/awesome-r/", "https://github.com/rstudio/RStartHere") %>% as_mardkown_url_list()
as_markdown_url_list <- function(items){
  items %>%
    mutate(md.item = sprintf("* [%s](%s) - %s", name, url, description)) %>%
    .$md.item %>%
    paste()
}

# sapply(github.urls, github_repo_basic_info) -> basic.info
# devtools::document()
# pkgdown::build_site()

