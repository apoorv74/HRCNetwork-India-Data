
library(jsonlite)
library(dplyr)


file_list <- dir("data/case_details/")
file_path <- "data/case_details/"
all_case_details <- data.frame()
all_case_actions <- data.frame()
total_files <- length(file_list)
p <- progress::progress_bar$new(total = total_files)
for(i in 1:total_files){
  p$tick()
  case_details_file_path <- glue::glue("{file_path}{file_list[i]}")
  case_details <-
    jsonlite::fromJSON(txt = case_details_file_path)
  case_details <- unique(case_details)
  diary_numbers <- case_details$diary_number %>% unlist()
  if(!is.null(diary_numbers)){
  # Move the actiontable in a separate df
  for(j in 1:nrow(case_details)){
  case_action <- case_details$action_table[[j]] %>% bind_rows()
  case_action$diary_number <- case_details$diary_number[j]
  case_action$case_file_number <- case_details$case_file_number[j]
  all_case_actions <- bind_rows(all_case_actions, case_action)
  }
  
  case_details$action_table <- NULL
  case_details$incident_details <- NULL
  all_case_details <- bind_rows(all_case_details, case_details)
  
  }
}

all_case_details <- unique(all_case_details)
all_case_actions <- unique(all_case_actions)

readr::write_csv(all_case_details, "data/nhrc_data_2020/all_case_details.csv")
readr::write_csv(all_case_actions, "data/nhrc_data_2020/all_case_actions.csv")
