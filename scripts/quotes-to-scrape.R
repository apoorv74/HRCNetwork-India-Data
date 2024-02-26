library(httr)
library(rvest)
library(purrr)
library(stringr)
library(dplyr)
url_search <- "https://quotes.toscrape.com/search.aspx"

x <- httr::handle(url = url_search)
res <- GET(handle = x, config = verbose(info = TRUE))

url_post <- "https://quotes.toscrape.com/filter.aspx"


list_of_authors <- res %>% read_html() %>% html_nodes(css = "#author option") %>% html_text() %>% str_trim()
list_of_authors <- list_of_authors[!list_of_authors %in% "----------"]

author_details_form <- res %>% read_html() %>% html_form() %>% pluck(1)
view_state_1 <- author_details_form$fields$`__VIEWSTATE` %>% pluck("value")
select_author <- list_of_authors[[1]]
tag <- "----------"

author_form_data <- list("author"=select_author,
                         "tag"=tag,
                         "__VIEWSTATE"=view_state_1)

get_tags <- POST(handle = x, url = url_post, body = author_form_data)

list_of_tags <- get_tags %>% read_html() %>% html_nodes(css = "#tag option") %>% html_text() %>% str_trim()
tag_details_form <- get_tags %>% read_html() %>% html_form() %>% pluck(1)
view_state_2 <- tag_details_form$fields$`__VIEWSTATE` %>% pluck("value")

select_tag <- list_of_tags[[5]]
tag_form_data <- list("author"=select_author,
                      "tag"=select_tag,
                      "__VIEWSTATE"=view_state_2,
                      "submit_button"="Search")

get_quotes <- POST(handle = x, url=url_post, body = tag_form_data)
quotes <- get_quotes %>% read_html() %>% html_nodes(css = ".quote .content") %>% html_text() 
cat(quotes)
