
pages_to_scrape <-  30000# Enter in multiples of 10

# This is replaced by creating a global variable - all_pages - to get the xpath of the next page
# page_num_xpath <-
#   data.frame("page_num" = c(c(1:10), c(11:pages_to_scrape)),
#              "xpath_num" = c(c(2:11), rep(c(3:12), length(
#                c(11:pages_to_scrape)
#              ) / 10)))

view_more_page_nums <- c(1,100,seq(150,30000,50))

from_date <- as.Date("01012020","%d%m%Y")
to_date <- as.Date("31122020","%d%m%Y")
dates_2020 <- seq.Date(from = from_date, to = to_date,by = 1)
dates_2020 <- format(dates_2020,"%d%m%Y")

# xpath values for the case_details page

xpath_df <-
  data.frame(
    "element_name" = c(
      "diary_number",
      "case_file_number",
      "registration_date",
      "action_table",
      "section",
      "language",
      "mode",
      "received_date",
      "complaint_date",
      "complainant_name",
      "complainant_address",
      "complainant_state",
      "complainant_district",
      "complainant_pincode",
      "incident_place",
      "incident_date",
      "incident_category",
      "incident_district",
      "incident_state",
      "incident_details",
      "victim_name",
      "gender",
      "religion",
      "caste",
      "address",
      "district",
      "state"
    ),
    "xpath_value" = c(
      '//*[@id="ContentPlaceHolder1_lblgetDiaryNo"]',
      '//*[@id="ContentPlaceHolder1_lblgetFileNo"]',
      '//*[@id="ContentPlaceHolder1_lblgeatComDate"]',
      '//*[@id="ContentPlaceHolder1_gridActionList"]/tbody',
      '//*[@id="ContentPlaceHolder1_lbl_Section"]',
      '//*[@id="ContentPlaceHolder1_lbl_Complaint_language"]',
      '//*[@id="ContentPlaceHolder1_lbl_Mode"]',
      '//*[@id="ContentPlaceHolder1_lbl_RecDate"]',
      '//*[@id="ContentPlaceHolder1_lbl_CompDate"]',
      '//*[@id="ContentPlaceHolder1_lblCompName"]',
      '//*[@id="ContentPlaceHolder1_lblComAddress"]',
      '//*[@id="ContentPlaceHolder1_lblCimState"]',
      '//*[@id="ContentPlaceHolder1_lblComDistrict"]',
      '//*[@id="ContentPlaceHolder1_lblComPincode"]',
      '//*[@id="ContentPlaceHolder1_lblIncPlace"]',
      '//*[@id="ContentPlaceHolder1_lblIncDate"]',
      '//*[@id="ContentPlaceHolder1_lblIncCat"]',
      '//*[@id="ContentPlaceHolder1_lblIncDis"]',
      '//*[@id="ContentPlaceHolder1_lblInsSt"]',
      '//*[@id="collapse3"]/div/div/table/tbody/tr[4]/td',
      '//*[@id="ContentPlaceHolder1_lblVicName"]' ,
      '//*[@id="ContentPlaceHolder1_lblVicGender"]',
      '//*[@id="ContentPlaceHolder1_lblVicReligion"]',
      '//*[@id="ContentPlaceHolder1_lblVicCast"]',
      '//*[@id="ContentPlaceHolder1_lblVicAddress"]',
      '//*[@id="ContentPlaceHolder1_lblVicDistrict"]',
      '//*[@id="ContentPlaceHolder1_lblVicState"]'
    )
  )

# Fetch case list table from the case listing page
get_case_table <- function() {
  case_meta_table <-
    remDr$findElement(using = "id", "ContentPlaceHolder1_GridViewSearResult")
  case_meta_table <- case_meta_table$getPageSource()
  case_meta_table <-
    read_html(x = case_meta_table %>% unlist()) %>% html_table() %>% pluck(1)
  all_pages <<- case_meta_table[nrow(case_meta_table),] %>% unlist()
  all_pages <<- all_pages[!is.na(all_pages)] %>% unname()
  case_meta_table <- case_meta_table[1:20, 2:7]
  case_meta_table$id <- 1:nrow(case_meta_table)
  return(case_meta_table)
}


get_case_details <- function(html_string, case_meta_element_xpath){
  meta_text <- html_string %>% html_nodes(xpath = case_meta_element) %>% html_text
  return(meta_text)
}


