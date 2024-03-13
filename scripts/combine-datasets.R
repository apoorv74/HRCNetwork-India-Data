
library(jsonlite)
library(dplyr)


file_list <- c("cases_2550","cases_2750","cases_4900")
file_path <- "data/case_details/"
file_ext <- ".json"
all_cases <- data.frame()
for(i in 1:length(file_list)){
  case_details_file_path <- glue::glue("{file_path}{file_list[i]}{file_ext}")
  case_details <-
    jsonlite::fromJSON(txt = case_details_file_path)  
  case_details$list_of_actions <- NULL
  case_details$incident_details <- NULL
  all_cases <- bind_rows(all_cases, case_details)
}

readr::write_csv(all_cases, "data/all_case_details.csv")
