library(readxl)
library(tidyverse)
library(janitor)
library(plyr)
library(lubridate)
library(stringr)

#read in data
SAC_all = read_excel("data/DUI_911_SAC.xlsx", sheet = "23152 ALL") %>% janitor::row_to_names(row_number = 1)
SAC_stop = read_excel("data/DUI_911_SAC.xlsx", sheet = "23152 with STOP") %>% janitor::row_to_names(row_number = 1)
SAC_DUI_went_arrest = read_excel("data/DUI_911_SAC.xlsx", sheet = "23152 went 1015") %>% janitor::row_to_names(row_number = 1)
SAC_arrest_from_DUI = read_excel("data/DUI_911_SAC.xlsx", sheet = "1015 from 23152") %>% janitor::row_to_names(row_number = 1)


#clean up data - keep incident number and date, add column for "all" , "unit assigned", "stop" and "arrest"
SAC_all_clean <- SAC_all %>% 
  select(Master_Incident_Number2, Date, Time_First_Unit_Assigned) %>% 
  mutate(All_Calls = "All Calls") %>% 
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"))%>% 
  mutate(Time_First_Unit_Assigned = as.Date(as.numeric(Time_First_Unit_Assigned), origin = "1899-12-30")) %>% 
  mutate(Unit_Assigned = case_when(!is.na(Time_First_Unit_Assigned) == TRUE ~ "Unit Assigned"))


SAC_stop_clean <- SAC_stop %>% 
  select(Master_Incident_Number2) %>% mutate(Stop = "Stop")

SAC_DUI_went_arrest_clean <- SAC_DUI_went_arrest %>% select(Master_Incident_Number2) %>% mutate(Arrest1 = "Arrest")

SAC_arrest_from_DUI_clean <- SAC_arrest_from_DUI %>% select(Master_Incident_Number2) %>% mutate(Arrest2 = "Arrest")


SAC_all_info_clean <- join_all(list(SAC_all_clean, SAC_stop_clean, SAC_DUI_went_arrest_clean, SAC_arrest_from_DUI_clean), by = 'Master_Incident_Number2', type = 'full') %>% 
  mutate(Arrest = case_when(((Arrest1 == "Arrest") | (Arrest2 == "Arrest")) ~ "Arrest",
                            TRUE ~ NA)) %>% 
  select(-Arrest1, -Arrest2) %>% 
  filter(!is.na(All_Calls)) %>% 
  filter(!is.na(Master_Incident_Number2)) %>% 
  distinct(Master_Incident_Number2, .keep_all = TRUE) %>% 
  mutate(year = year(Date))


SAC_calls_by_year <- SAC_all_info_clean %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(calls = n())

SAC_assigned_by_year <- SAC_all_info_clean %>% 
  filter(!is.na(Unit_Assigned)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(unit_assigned = n())

SAC_stops_by_year <- SAC_all_info_clean %>% 
  filter(!is.na(Stop)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(stops = n())

SAC_arrests_by_year <- SAC_all_info_clean %>% 
  filter(!is.na(Arrest)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(arrests = n())


SAC_by_year <- join_all(list(SAC_calls_by_year, SAC_assigned_by_year, SAC_stops_by_year, SAC_arrests_by_year), by = 'year', type = 'full') %>% 
  adorn_totals("row") %>%
  mutate(pct_calls_unit_assigned = (unit_assigned/calls)*100) %>% 
  mutate(pct_unit_assigned_arrest = (arrests/unit_assigned)*100) %>% 
  mutate(pct_calls_arrest = (arrests/calls)*100)


remove(SAC_all, SAC_stop, SAC_DUI_went_arrest, SAC_arrest_from_DUI)
remove(SAC_all_clean, SAC_stop_clean, SAC_DUI_went_arrest_clean, SAC_arrest_from_DUI_clean)
remove(SAC_calls_by_year, SAC_assigned_by_year, SAC_stops_by_year, SAC_arrests_by_year)


write.csv(SAC_all_info_clean, "output/SAC_all_info_clean.csv", row.names = FALSE)
write.csv(SAC_by_year, "output/SAC_by_year.csv", row.names = FALSE)
