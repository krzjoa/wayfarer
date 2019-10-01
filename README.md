# wayfarer <img src='man/figures/logo.png' align="right" height="139" />
[![Documentation](https://img.shields.io/badge/documentation-wayfarer-orange.svg?colorB=E91E63)](http://krzjoa.github.io/wayfarer)
</br>
Wayfarer was created to make working with awesome lists easier!
## Installation
```R
remotes::install_github('krzjoa/wayfarer')
```
## Usage
The simpliest usage scenario is a selection of a set of libraries by comparing two awesome lists.
```R
awesome.r <- 'https://github.com/qinwf/awesome-r/'
r.start.here <- 'https://github.com/rstudio/RStartHere'
diff_awesome_lists(awesome.r, r.start.here) %>% 
  as_markdown_url_list() %>% 
  write("lacking-items.md")
```
