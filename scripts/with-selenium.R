# Declare Libraries -------------------------------------------------------
library(httr)
library(rvest)
library(purrr)
library(stringr)
library(dplyr)
library(RSelenium)
library(glue)
library(readr)
library(netstat)


# Source Files ------------------------------------------------------------

source("scripts/constants.R")

# User Agent --------------------------------------------------------------
#Not needed for selenium. 
user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36"


# Declare selenium client and server details ------------------------------
rd <- rsDriver(browser = "firefox",
               chromever = NULL,port = free_port())
remDr <- rd$client

# Fetch case details from the case listing page ---------------------------
case_details_url <- "https://hrcnet.nic.in/HRCNet/public/CaseStatus.aspx"
case_search_url <- "https://hrcnet.nic.in/HRCNet/public/SearchComplaint.aspx"
remDr$navigate(case_search_url)

# Steps before clicking on Search:
# 1. Select "Registered since 1993" from the "Total Cases" Dropdown
# 2. Enter the Captcha Code

search_element <- remDr$findElement(using = "id","ContentPlaceHolder1_btnSearch")
search_element$clickElement()

# Total Number of Records
total_records <- remDr$findElement(using = "id","ContentPlaceHolder1_lblSuccess")
total_records <- total_records$getElementText() %>% unlist()
total_records_number <-
  str_match(string = total_records, pattern = "[0-9].*") %>% as.numeric()

view_more <- remDr$findElement(using = "id","ContentPlaceHolder1_btnExpandGridViewSearResult")
view_more$clickElement()

# Clicking the last page link for the first `n` pages
nth_page <- 15000
last_page_num <- seq(10, nth_page, 10)
for (lp in 329:length(last_page_num)) {
  print(glue::glue("Page Num: {last_page_num[lp]}"))
  if(last_page_num[lp] %in% view_more_page_nums){
    view_more <- remDr$findElement(using = "id","ContentPlaceHolder1_btnExpandGridViewSearResult")
    view_more$clickElement()
  }
  xpath_num <- page_num_xpath$xpath_num[page_num_xpath$page_num == last_page_num[lp]]
  next_page_xpath <- glue::glue('//*[@id="ContentPlaceHolder1_GridViewSearResult"]/tbody/tr[22]/td/table/tbody/tr/td[{xpath_num}]/a')
  next_page <- remDr$findElement(using = "xpath",value = next_page_xpath)
  next_page$clickElement()
  Sys.sleep(15)
}

# Find case details from first 20 pages
all_cases_list <- c()
all_cases_details <- c()
# Fetch case list from other pages
for(page_num in 3518:pages_to_scrape){
  print(glue::glue("Page Num: {page_num}"))  
  if(page_num %in% view_more_page_nums){
    view_more <- remDr$findElement(using = "id","ContentPlaceHolder1_btnExpandGridViewSearResult")
    view_more$clickElement()
  }
  case_meta_table <- get_case_table()
  # Extract case type -------------------------------------------------------
  # sample_file_number <- "1020/35/6/2020-AD"
  # str_match(sample_file_number,case_type_pattern)[[2]]
  case_type_pattern <- "[0-9].*\\-(.*?)$"
  
  case_meta_table$case_type <-
    str_match(case_meta_table$`File Number`, case_type_pattern)[, 2]
  case_meta_table$case_type[is.na(case_meta_table$case_type)] <- ""
  
  form_vals <- read_html(x = remDr$getPageSource()[[1]]) %>% html_form() %>% pluck(1)
  view_state <- form_vals$fields$`__VIEWSTATE`$value
  view_state_gen <- form_vals$fields$`__VIEWSTATEGENERATOR`$value
  event_val <- form_vals$fields$`__EVENTVALIDATION`$value
  event_target_1 <- "ctl00$ContentPlaceHolder1$GridViewSearResult$ctl"
  event_target_2 <- "$LinkButton1"
  case_type_fetch <- c("AD","PCD")
  row_ids <- case_meta_table$id[case_meta_table$case_type == case_type_fetch]
  if(length(row_ids)>0){
    all_details <- list()
    total_vars <- nrow(xpath_df)
    for(i in 1:length(row_ids)){
      
      if(row_ids[i]<10){
        event_target_num <- paste0("0",row_ids[i] + 1) 
      } else {
        event_target_num <- paste0(row_ids[i] + 1) 
      }
      vals  <- list(
        "__EVENTTARGET" = paste0(event_target_1,event_target_num,event_target_2),
        "__VIEWSTATE" = view_state,
        "__VIEWSTATEGENERATOR" = view_state_gen,
        "__EVENTVALIDATION" = event_val,
        "ctl00$ContentPlaceHolder1$DropDownHRC" = 0,
        "ctl00$ContentPlaceHolder1$cbo_Incident_State" = "SELECT",
        "ctl00$ContentPlaceHolder1$cbo_actionStatus" = "ALLREGS"
      )
      
      # Send a POST reqest
      
      case_details_POST <- POST(url = case_search_url,
                                body = vals,
                                add_headers("User-Agent" = user_agent))
      
      case_details_GET <-
        GET(url = case_details_url, add_headers("User-Agent" = user_agent))
      
      case_details_html <- read_html(case_details_GET) 
      case_details_list <- c()
      for (j in 1:total_vars) {
        element_xpath <-
          xpath_df$xpath_value[j]
        if(j==4){
          list_of_actions <- case_details_html %>% html_nodes(xpath = element_xpath) %>% html_table
          case_details_list[["list_of_actions"]] <- list_of_actions
        } else {
          meta_text <-
            case_details_html %>% html_nodes(xpath = element_xpath) %>% html_text %>% stringr::str_trim()
          case_details_list[[xpath_df$element_name[j]]] <- meta_text
        }
      }
      
      all_details[[i]] <- case_details_list
      all_cases_details <- c(all_cases_details, all_details)
    }
    
    print(glue::glue("Total Case Details Scraped: {length(all_details)}\n"))
  }
  
  all_cases_list <- bind_rows(all_cases_list, case_meta_table)
  
  xpath_num <- page_num_xpath$xpath_num[page_num_xpath$page_num == page_num]
  next_page_xpath <- glue::glue('//*[@id="ContentPlaceHolder1_GridViewSearResult"]/tbody/tr[22]/td/table/tbody/tr/td[{xpath_num}]/a')
  next_page <- remDr$findElement(using = "xpath",value = next_page_xpath)
  next_page$clickElement()
  Sys.sleep(20)
  
  # Save data every 50th page
  if (page_num %% 50 == 0) {
    file_title <- glue::glue("data/case_lists/cases_{page_num}.csv")
    case_details_file_title <- glue::glue("data/case_details/cases_{page_num}.json")
    readr::write_csv(all_cases_list, file_title,append = FALSE)
    jsonlite::write_json(x = all_cases_details, path = case_details_file_title, auto_unbox=TRUE)
  }
  
  # print(glue::glue("Total Cases Addded: {nrow(all_cases_list)}\n"))
  
  }

jsonlite::write_json(x = all_cases_details, path = "data/all_case_details.json", auto_unbox=TRUE)
readr::write_csv(all_cases_list, "data/all_case_list.csv")
