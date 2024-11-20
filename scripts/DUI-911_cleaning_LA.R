library(readxl)
library(tidyverse)
library(janitor)
library(plyr)
library(lubridate)
library(stringr)

#read in data
LA_all = read_excel("data/DUI_911_LA.xlsx", sheet = "All 23152") %>% janitor::row_to_names(row_number = 1)
LA_unit_assigned = read_excel("data/DUI_911_LA.xlsx", sheet = "23152 Unit Assigned")
LA_stop = read_excel("data/DUI_911_LA.xlsx", sheet = "23152 STOP")
LA_arrest = read_excel("data/DUI_911_LA.xlsx", sheet = "23152 1015")

#clean up data - keep incident number and date, add column for "all" , "unit assigned", "stop" and "arrest"
LA_all_clean <- LA_all %>% select(Master_Incident_Number, Response_Date, Time_First_Unit_Assigned) %>% mutate(All_Calls = "All Calls") %>% mutate(Response_Date = as.Date(as.numeric(Response_Date), origin = "1899-12-30"))

LA_unit_assigned_clean <- LA_unit_assigned %>% select(Master_Incident_Number, Response_Date) %>% mutate(Unit_Assigned = "Unit Assigned")

LA_stop_clean <- LA_stop %>% select(Master_Incident_Number, Response_Date) %>% mutate(Stop = "Stop")

LA_arrest_clean <- LA_arrest %>% select(Master_Incident_Number, Response_Date) %>% mutate(Arrest = "Arrest")

LA_all_info_clean <- join_all(list(LA_all_clean, LA_unit_assigned_clean, LA_stop_clean, LA_arrest_clean), by = 'Master_Incident_Number', type = 'full') %>% 
  filter(!is.na(All_Calls)) %>% 
  distinct(Master_Incident_Number, .keep_all = TRUE) %>% 
  mutate(year = year(Response_Date)) %>% 
  mutate(month = as.character(month(Response_Date))) %>%
  mutate(month = str_pad(month, width=2, pad="0")) %>%
  mutate(month_year = paste0(year, "-", month, "-01"))

LA_calls_by_year <- LA_all_info_clean %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(calls = n())

LA_assigned_by_year <- LA_all_info_clean %>% 
  filter(!is.na(Unit_Assigned)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(unit_assigned = n())

LA_stops_by_year <- LA_all_info_clean %>% 
  filter(!is.na(Stop)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(stops = n())

LA_arrests_by_year <- LA_all_info_clean %>% 
  filter(!is.na(Arrest)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(arrests = n())


LA_by_year <- join_all(list(LA_calls_by_year, LA_assigned_by_year, LA_stops_by_year, LA_arrests_by_year), by = 'year', type = 'full') %>% 
  adorn_totals("row") %>% 
  mutate(pct_calls_unit_assigned = (unit_assigned/calls)*100) %>% 
  mutate(pct_unit_assigned_arrest = (arrests/unit_assigned)*100) %>% 
  mutate(pct_calls_arrest = (arrests/calls)*100)


remove(LA_all, LA_unit_assigned, LA_stop, LA_arrest)
remove(LA_all_clean, LA_unit_assigned_clean, LA_stop_clean, LA_arrest_clean)
remove(LA_calls_by_year, LA_assigned_by_year, LA_stops_by_year, LA_arrests_by_year)


write.csv(LA_all_info_clean, "output/LA_all_info_clean.csv", row.names = FALSE)
write.csv(LA_by_year, "output/LA_by_year.csv", row.names = FALSE)

