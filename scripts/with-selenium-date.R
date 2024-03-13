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
library(webshot2)
library(magick)

# User Agent --------------------------------------------------------------
#Not needed for selenium. 
user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36"


# Declare selenium client and server details ------------------------------
rd <- rsDriver(browser = "firefox",chromever = NULL,port = free_port())
remDr <- rd$client

# Fetch case details from the case listing page ---------------------------
case_search_url <- "https://hrcnet.nic.in/HRCNet/public/SearchComplaint.aspx"
remDr$navigate(case_search_url)

# Fill Date
select_date <- remDr$findElement(using = "class","date_wid")
select_date$clickElement()
select_date$sendKeysToElement(list("02022020"))

#Solve Captcha
captcha_image <- remDr$findElement(using = "xpath",'//*[@id="captchaimgOffline"]')
# captcha_image$highlightElement()
captcha_image$screenshot(file = "captcha/xxx.gif")
# captcha_size <- captcha_image$getElementSize()

cw <- 180
ch <- 30
x_off <- 150
y_off <- 550
captcha_im <- image_read("captcha/xxx.gif")
cropped_im <- image_crop(captcha_im, geometry = geometry_area(cw,ch,x_off,y_off))
cropped_im <- magick::image_resize(image = cropped_im, geometry = 600)
# image_enhance(image = cropped_im)
image_write(cropped_im, path = "captcha/captcha.png")
eng <- tesseract::tesseract("eng")
text <- tesseract::ocr("captcha/captcha.png", engine = eng)


# Total Number of Records
total_records <- remDr$findElement(using = "id","ContentPlaceHolder1_lblSuccess")
total_records <- total_records$getElementText() %>% unlist()
total_records_number <-
  str_match(string = total_records, pattern = "[0-9].*") %>% as.numeric()

#Click on View More to get list of all pages
view_more <- remDr$findElement(using = "id","ContentPlaceHolder1_btnExpandGridViewSearResult")
view_more$clickElement()

# Find case details from first 20 pages
all_cases_list <- c()

# Fetch case list from the first page
case_meta_table <- remDr$findElement(using = "id", "ContentPlaceHolder1_GridViewSearResult")
case_meta_table <- case_meta_table$getPageSource()
case_meta_table <- read_html(x = case_meta_table %>% unlist()) %>% html_table() %>% pluck(1)
case_meta_table <- case_meta_table[1:20,2:7]
all_cases_list <- bind_rows(all_cases_list, case_meta_table)

page_num_xpath <- data.frame("page_num"=c(2:51),"xpath_num"=c(2:11,rep(3:12,4)))
  
# Fetch case list from other pages
for(page_num in 2:50){
  xpath_num <- page_num_xpath$xpath_num[page_num_xpath$page_num == page_num]
  next_page_xpath <- glue::glue('//*[@id="ContentPlaceHolder1_GridViewSearResult"]/tbody/tr[22]/td/table/tbody/tr/td[{xpath_num}]/a')
  next_page <- remDr$findElement(using = "xpath",value = next_page_xpath)
  next_page$clickElement()
  case_meta_table <- remDr$findElement(using = "id", "ContentPlaceHolder1_GridViewSearResult")
  case_meta_table <- case_meta_table$getPageSource()
  case_meta_table <- read_html(x = case_meta_table %>% unlist()) %>% html_table() %>% pluck(1)
  case_meta_table <- case_meta_table[1:20,2:7]
  all_cases_list <- bind_rows(all_cases_list, case_meta_table)
  
  # Save data every 10th page
  if (page_num %% 10 == 0) {
    file_title <- glue::glue("data/case_lists/cases_{page_num}.csv")
    readr::write_csv(all_cases_list, file_title,append = FALSE)
  }
  print(glue::glue("Total Cases Addded: {nrow(all_cases_list)}"))
}

