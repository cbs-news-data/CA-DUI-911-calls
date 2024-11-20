library(readxl)
library(tidyverse)
library(janitor)
library(plyr)
library(lubridate)
library(stringr)

#read in data
SF_all = read_excel("data/DUI_911_SF.xlsx", sheet = "All 23152 SF County") %>% janitor::row_to_names(row_number = 1)
SF_unit_assigned = read_excel("data/DUI_911_SF.xlsx", sheet = "23152 Unit Assigned")
SF_stop = read_excel("data/DUI_911_SF.xlsx", sheet = "23152 STOP")
SF_arrest = read_excel("data/DUI_911_SF.xlsx", sheet = "23152 1015")

#clean up data - keep incident number and date, add column for "all" , "unit assigned", "stop" and "arrest"
SF_all_clean <- SF_all %>% 
  select(Master_Incident_Number, Response_Date, Time_First_Unit_Assigned) %>% 
  mutate(All_Calls = "All Calls") %>% 
  mutate(Response_Date = as.Date(as.numeric(Response_Date), origin = "1899-12-30")) %>% 
  mutate(Time_First_Unit_Assigned = as.Date(as.numeric(Time_First_Unit_Assigned), origin = "1899-12-30"))

SF_unit_assigned_clean <- SF_unit_assigned %>% select(Master_Incident_Number) %>% mutate(Unit_Assigned = "Unit Assigned")

SF_stop_clean <- SF_stop %>% select(Master_Incident_Number) %>% mutate(Stop = "Stop")

SF_arrest_clean <- SF_arrest %>% select(Master_Incident_Number) %>% mutate(Arrest = "Arrest")

SF_all_info_clean <- join_all(list(SF_all_clean, SF_unit_assigned_clean, SF_stop_clean, SF_arrest_clean), by = 'Master_Incident_Number', type = 'full') %>% 
  filter(!is.na(All_Calls)) %>% 
  distinct(Master_Incident_Number, .keep_all = TRUE) %>% 
  mutate(year = year(Response_Date))


SF_calls_by_year <- SF_all_info_clean %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(calls = n())

SF_assigned_by_year <- SF_all_info_clean %>% 
  filter(!is.na(Unit_Assigned)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(unit_assigned = n())

SF_stops_by_year <- SF_all_info_clean %>% 
  filter(!is.na(Stop)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(stops = n())

SF_arrests_by_year <- SF_all_info_clean %>% 
  filter(!is.na(Arrest)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(arrests = n())


SF_by_year <- join_all(list(SF_calls_by_year, SF_assigned_by_year, SF_stops_by_year, SF_arrests_by_year), by = 'year', type = 'full') %>% 
  adorn_totals("row") %>% 
  mutate(pct_calls_unit_assigned = (unit_assigned/calls)*100) %>% 
  mutate(pct_unit_assigned_arrest = (arrests/unit_assigned)*100) %>% 
  mutate(pct_calls_arrest = (arrests/calls)*100)


remove(SF_all, SF_unit_assigned, SF_stop, SF_arrest)
remove(SF_all_clean, SF_unit_assigned_clean, SF_stop_clean, SF_arrest_clean)
remove(SF_calls_by_year, SF_assigned_by_year, SF_stops_by_year, SF_arrests_by_year)


write.csv(SF_all_info_clean, "output/SF_all_info_clean.csv", row.names = FALSE)
write.csv(SF_by_year, "output/SF_by_year.csv", row.names = FALSE)