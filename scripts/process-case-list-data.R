library(dplyr)
library(readr)


# Read File ---------------------------------------------------------------
all_cases <- readr::read_csv("data/all_case_list.csv", col_types = cols())


# Format Date Cols --------------------------------------------------------
all_cases$`Incident Date` <- as.Date(x = all_cases$`Incident Date`, format="%d/%m/%Y")
all_cases$`Entry Date` <- as.Date(x = all_cases$`Entry Date`, format="%d/%m/%Y")


# Extract case type -------------------------------------------------------
case_type_pattern <- "[0-9].*\\/(.*?)\\/[0-9].*"
all_cases$case_type <- str_match(all_cases$`Diary Number`,case_type_pattern)[,2]

all_cases %>% 
  group_by(month = lubridate::floor_date(`Entry Date`, "month")) %>%
  summarize(summary_variable = n()) %>% kableExtra::kable(format = "rst")

readr::write_csv(all_cases, "data/all_case_list_formatted.csv")
