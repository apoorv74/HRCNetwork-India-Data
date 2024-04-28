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
library(tidyr)

# User Agent
user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36"

# Source Files ------------------------------------------------------------

source("scripts/constants.R")

# Get all Case Numbers
case_numbers_file <- readr::read_csv("data/nhrc_data_2020/all_case_details.csv")
case_numbers <- case_numbers_file$case_file_number %>% unique()

# Declare selenium client and server details ------------------------------
rd <- rsDriver(browser = "firefox",
               chromever = NULL,port = free_port(random = TRUE))
remDr <- rd$client

# Fetch case details from the case listing page ---------------------------
case_details_url <- "https://hrcnet.nic.in/HRCNet/public/CaseStatus.aspx"
case_search_url <- "https://hrcnet.nic.in/HRCNet/public/SearchComplaint.aspx"
remDr$navigate(case_search_url)

for(i in 1:length(case_numbers)){
  if(i > 1){
    # Switch Windows
    web_page_id <- remDr$getWindowHandles()[[1]]
    remDr$switchToWindow(windowId = web_page_id)
    remDr$navigate(case_search_url)
    
  }
  # Enter Case Number
  case_num_i <- case_numbers[i]
  
  print(glue::glue("Case Number {i} - {case_num_i}"))
  
  select_case_number <- remDr$findElement(using = "id","ContentPlaceHolder1_textFileNo")
  select_case_number$clearElement()
  select_case_number$sendKeysToElement(list(case_num_i))

  # Enter captcha value
  captcha_input <- remDr$findElement(using="xpath",'//*[@id="ContentPlaceHolder1_txtCaptchaCodeOffline"]')
  captcha_value <- readline(prompt = "Enter captcha code: ")
  captcha_input$sendKeysToElement(list(captcha_value))
  
  search_element <- remDr$findElement(using = "id","ContentPlaceHolder1_btnSearch")
  search_element$clickElement()
  
  Sys.sleep(5)
  
  # Click on the case number
  case_details_page_xpath <- '//*[@id="ContentPlaceHolder1_GridViewSearResult_LinkButton1_0"]'
  case_details_page <- remDr$findElement(using = "xpath",value = case_details_page_xpath)
  case_details_page$clickElement()
  Sys.sleep(5)
  
  # Switch Windows
  web_page_id <- remDr$getWindowHandles()[[2]]
  remDr$switchToWindow(windowId = web_page_id)

  case_details_html <- remDr$getPageSource() %>% pluck(1)
  all_actions <- c()
  action_details <- c()
  list_of_actions <-
    case_details_html %>% read_html() %>% html_nodes(css = '#ContentPlaceHolder1_gridActionList') %>% html_table %>% pluck(1) 
  total_rows <- nrow(list_of_actions)
  
  # If number 2 is not present in the list of action numbers, then search for more pages
  if(total_rows > 10) {
    total_pages <-
      list_of_actions[nrow(list_of_actions), ] %>% unlist(use.names = FALSE) %>% na.omit() %>% as.vector() %>% length()
    for (j in 1:total_pages) {
      if(j > 1){
      actions_html <- remDr$getPageSource() %>% pluck(1)
      list_of_actions <- actions_html %>% read_html() %>% html_nodes(css = '#ContentPlaceHolder1_gridActionList') %>% html_table %>% pluck(1) 
      }
      total_rows <- nrow(list_of_actions) - 2
      list_of_actions_j <- list_of_actions[1:total_rows, 1:4]
      all_actions <-
        dplyr::bind_rows(all_actions, list_of_actions_j)
      
      # Click on Expand All Action List
      expand_action_xpath <- '//*[@id="ContentPlaceHolder1_lnkExpandAllAction"]'
      expand_action_xpath_element <- remDr$findElement(using = "xpath", value = expand_action_xpath)
      expand_action_xpath_element$clickElement()

      # Sleep
      Sys.sleep(20)
      
      # Fetch all action details
      action_details_html <- remDr$getPageSource() %>% pluck(1)
      action_details_page <- get_action_details(action_details_html, total_rows)
      action_details <- dplyr::bind_rows(action_details, action_details_page) 
      
      if (j < total_pages) {
        next_action_list_page_xpath <-
          glue::glue(
            '//*[@id="ContentPlaceHolder1_gridActionList"]/tbody/tr[12]/td/table/tbody/tr/td[{j+1}]'
          )
        next_action_list_page <-
          remDr$findElement(using = "xpath", value = next_action_list_page_xpath)
        next_action_list_page$clickElement()
        Sys.sleep(20)    
      }
    }
  } else {
    list_of_actions_j <- list_of_actions[1:total_rows, 1:4]
    all_actions <-
      dplyr::bind_rows(all_actions, list_of_actions_j)
    
    # Click on Expand All Action List
    expand_action_xpath <- '//*[@id="ContentPlaceHolder1_lnkExpandAllAction"]'
    expand_action_xpath_element <- remDr$findElement(using = "xpath", value = expand_action_xpath)
    expand_action_xpath_element$clickElement()
    
    # Sleep
    Sys.sleep(20)
    
    # Fetch all action details
    action_details_html <- remDr$getPageSource() %>% pluck(1)
    action_details_page <- get_action_details(action_details_html,total_rows)
    action_details <- dplyr::bind_rows(action_details, action_details_page) 
  }
  
  file_title_action_list <- glue::glue("data/nhrc_data_2020/case_actions/action_list/case_{i}_action_list.csv")
  readr::write_csv(all_actions, file_title_action_list)
  
  file_title_action_details <- glue::glue("data/nhrc_data_2020/case_actions/action_details/case_{i}_action_details.csv")
  readr::write_csv(action_details, file_title_action_details)
  
}
