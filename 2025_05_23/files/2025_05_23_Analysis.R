rm(list = ls())
options(scipen = 999)
setwd("/Volumes/GoogleDrive-103007369919315814324/untitled folder/My Drive/NILnomics/Newsletters/2025_05_23")

#Load NILnomics-wide functions
source("/Volumes/GoogleDrive-103007369919315814324/untitled folder/My Drive/NILnomics/MFRS Cleaner/Utilities.R")

#Load Libraries
library(tidyverse)
library(ellmer)
library(pdftools)
library(readxl)
library(ggimage)#To have images as points on graph
library(gridExtra)#For ggplot footers
library(png)   # If your image is a .png file
library(kableExtra)
library(flextable)
library(formattable)
library(scales)
library(readr)
library(httr)
library(jsonlite)
library(openxlsx)
library("gitcreds")#Need to push to GitHub
library(tidyverse)
library(rvest)
library(purrr)
library(stringr)
library(tidyr)
library(jsonlite)
library(readxl)


#Set slug for each dataset####
NILnomics_Athletic_Participation_Data <- "nilnomics/ncaa-athlete-participation"
NILnomics_school_logo_and_other_data <- "nilnomics/ncaa-school-logo-and-other-data"
NILnomics_ncaa_financial_reporting_data <- "nilnomics/ncaa-financial-reporting-data"
NILnomics_ncaa_eada_data <- "nilnomics/ncaa-eada-data"
NILnomics_2024_25_ncaa_mens_hockey_results <- "nilnomics/2024-25-ncaa-mens-hockey-results"
NILnomics_house_settlement_roster_limits <- "nilnomics/house-settlement-roster-limits"
NILnomics_institution_name_iped_id_conference_bridge_file <- "nilnomics/institution-name-iped-id-conference-bridge-file"
NILnomics_Institutional_Revenues  <- "nilnomics/knight-newhouse-data"

#Use Kaggle CLI commands to download individual dataset
system(paste("kaggle datasets download -d", NILnomics_Athletic_Participation_Data, "--force"), intern = TRUE)
unzip("ncaa-athlete-participation.zip")
Athletic_Participation_Data <- readr::read_csv("Athletic Participation Data.csv")

#Use Kaggle CLI commands to download individual dataset
system(paste("kaggle datasets download -d", NILnomics_school_logo_and_other_data, "--force"), intern = TRUE)
unzip("ncaa-school-logo-and-other-data.zip")
School_Logos <- read_excel("School_Logos.xlsx")

#Use Kaggle CLI commands to download individual dataset
system(paste("kaggle datasets download -d", NILnomics_ncaa_financial_reporting_data, "--force"), intern = TRUE)
unzip("ncaa-financial-reporting-data.zip")
NCAA_Financial_Reports_Data <- readr::read_csv("NCAA Financial Reports Data - Items Disaggregated.csv")

#Use Kaggle CLI commands to download individual dataset
system(paste("kaggle datasets download -d", NILnomics_ncaa_eada_data, "--force"), intern = TRUE)
unzip("ncaa-eada-data.zip")
EADA_2023 <- read_excel("Combined_EADA.xlsx")

#Use Kaggle CLI commands to download individual dataset
system(paste("kaggle datasets download -d", NILnomics_2024_25_ncaa_mens_hockey_results, "--force"), intern = TRUE)
unzip("2024-25-ncaa-mens-hockey-results.zip")
Hockey_DF01 <- read_excel("2025 Mens Hockey Results.xlsx")

#Use Kaggle CLI commands to download individual dataset
system(paste("kaggle datasets download -d", NILnomics_house_settlement_roster_limits, "--force"), intern = TRUE)
unzip("house-settlement-roster-limits.zip")
Scholarship_Old_Limits <- read_excel("Scholarship_Old_Limits.xlsx")

#Use Kaggle CLI commands to download individual dataset
system(paste("kaggle datasets download -d", NILnomics_institution_name_iped_id_conference_bridge_file, "--force"), intern = TRUE)
unzip("institution-name-iped-id-conference-bridge-file.zip")
Institution_Name_UnitIDs <- read_excel("Institution Name UnitIDs.xlsx")

#Use Kaggle CLI commands to download individual dataset
system(paste("kaggle datasets download -d", NILnomics_Institutional_Revenues, "--force"), intern = TRUE)
unzip("knight-newhouse-data.zip")
Institutional_Revenues <- read_excel("Institutional_Revenues.xlsx")


#Ivy League Roster Sizes####
#All sport scrape - Harvard/Cornell 2022-23####
#Create DF that has a URL for each roster, the sport, sex, and school
team_urls <- tibble(
  url = c("https://gocrimson.com/sports/womens-basketball/roster/2022-23","https://gocrimson.com/sports/womens-fencing/roster/2022-23","https://gocrimson.com/sports/field-hockey/roster/2022","https://gocrimson.com/sports/womens-golf/roster/2022-23","https://gocrimson.com/sports/womens-ice-hockey/roster/2022-23","https://gocrimson.com/sports/womens-lacrosse/roster/2022","https://gocrimson.com/sports/womens-heavyweight-rowing/roster/2022-23","https://gocrimson.com/sports/womens-lightweight-rowing/roster/2022-23","https://gocrimson.com/sports/womens-soccer/roster/2022","https://gocrimson.com/sports/softball/roster/2022","https://gocrimson.com/sports/womens-swimming-and-diving/roster/2022-23", "https://gocrimson.com/sports/womens-tennis/roster/2022-23","https://gocrimson.com/sports/womens-volleyball/roster/2022","https://gocrimson.com/sports/womens-water-polo/roster/2022",
          "https://gocrimson.com/sports/mens-fencing/roster","https://gocrimson.com/sports/baseball/roster/2023","https://gocrimson.com/sports/mens-basketball/roster/2022-23","https://gocrimson.com/sports/cross-country/roster/2022","https://gocrimson.com/sports/football/roster/2023","https://gocrimson.com/sports/mens-golf/roster/2022-23","https://gocrimson.com/sports/mens-ice-hockey/roster/2022-23","https://gocrimson.com/sports/mens-lacrosse/roster/2023","https://gocrimson.com/sports/mens-heavyweight-rowing/roster/2022-23","https://gocrimson.com/sports/mens-lightweight-rowing/roster/2022-23","https://gocrimson.com/sports/alpine-skiing/roster/2022-23","https://gocrimson.com/sports/mens-soccer/roster/2022","https://gocrimson.com/sports/mens-swimming-and-diving/roster/2022-23","https://gocrimson.com/sports/mens-tennis/roster/2022-23","https://gocrimson.com/sports/mens-track-and-field/roster/2022-23","https://gocrimson.com/sports/mens-volleyball/roster/2023","https://gocrimson.com/sports/mens-water-polo/roster/2022","https://gocrimson.com/sports/wrestling/roster/2022-23",
          "https://cornellbigred.com/sports/womens-basketball/roster/2022-23","https://cornellbigred.com/sports/womens-cross-country/roster/2022-23","https://cornellbigred.com/sports/equestrian/roster/2022-23","https://cornellbigred.com/sports/field-hockey/roster/2022-23","https://cornellbigred.com/sports/womens-gymnastics/roster/2022-23","https://cornellbigred.com/sports/womens-ice-hockey/roster/2022-23","https://cornellbigred.com/sports/womens-lacrosse/roster/2022","https://cornellbigred.com/sports/womens-rowing/roster/2022-23","https://cornellbigred.com/sports/womens-soccer/roster/2022","https://cornellbigred.com/sports/softball/roster/2023","https://cornellbigred.com/sports/womens-swimming-and-diving/roster/2022-23","https://cornellbigred.com/sports/womens-tennis/roster/2022-23","https://cornellbigred.com/sports/womens-track-and-field/roster/2022-23","https://cornellbigred.com/sports/womens-volleyball/roster/2022",
          "https://cornellbigred.com/sports/baseball/roster/2023","https://cornellbigred.com/sports/mens-basketball/roster/2022-23","https://cornellbigred.com/sports/mens-cross-country/roster/2022-23","https://cornellbigred.com/sports/fencing/roster/2022-23","https://cornellbigred.com/sports/football/roster/2022-23","https://cornellbigred.com/sports/mens-golf/roster/2022-23","https://cornellbigred.com/sports/mens-ice-hockey/roster/2022-23","https://cornellbigred.com/sports/mens-lacrosse/roster/2022","https://cornellbigred.com/sports/rowing/roster/2022-23","https://cornellbigred.com/sports/mens-rowing/roster/2022-23","https://cornellbigred.com/sports/mens-soccer/roster/2022-23","https://cornellbigred.com/sports/mens-swimming-and-diving/roster/2022-23","https://cornellbigred.com/sports/mens-tennis/roster/2022-23","https://cornellbigred.com/sports/mens-track-and-field/roster/2022-23","https://cornellbigred.com/sports/wrestling/roster/2022-23"
  ),
  sport = c("Basketball","Fencing","Field Hockey","Golf","Ice Hockey", "Lacrosse","Heavyweight Rowing", "Lightweight Rowing", "Soccer", "Softball", "Swimming & Diving", "Tennis",  "Volleyball", "Water Polo",
            "Fencing","Baseball","Basketball", "Cross Country", "Football", "Golf", "Ice Hockey", "Lacrosse", "Heavyweight Rowing", "Lightweight Rowing", "Skiing", "Soccer", "Swimming & Diving", "Tennis", "Track & Field", "Volleyball", "Water Polo", "Wrestling",
            "Basketball", "Cross Country", "Equestrian","Field Hockey", "Gymnastics", "Ice Hockey", "Lacrosse", "Rowing", "Soccer", "Softball", "Swimming & Diving", "Tennis", "Track & Field", "Volleyball",
            "Baseball","Basketball", "Cross Country","Fencing", "Football", "Golf", "Ice Hockey", "Lacrosse", "Heavyweight Rowing", "Lightweight Rowing", "Soccer", "Swimming & Diving", "Tennis", "Track & Field", "Wrestling"
  ),
  sex = c("Women","Women", "Women", "Women", "Women", "Women", "Women","Women", "Women", "Women", "Women", "Women", "Women", "Women","Women",
          "Men","Men","Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men",
          "Women","Women","Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", 
          "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men","Men"
  ),
  school = c("Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University",
             "Harvard University","Harvard University","Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University",
             "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University",
             "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University"
  )
)

# Function to parse all table cells from one team page
scrape_team_roster <- function(url, sport, sex,school) {
  cat("Scraping:", sport, "|", sex,"|", school, "\n" )
  
  tryCatch({
    page <- read_html(url)
    rows <- page %>% html_elements("table tbody tr")
    
    if (length(rows) == 0) return(tibble())  # No data
    
    map_dfr(rows, function(row) {
      cells <- row %>% html_elements("td")
      if (length(cells) == 0) return(NULL)
      
      row_data <- cells %>%
        map_chr(~ html_text(.x, trim = TRUE)) %>%
        setNames(paste0("col", seq_along(.))) %>%
        as_tibble_row()
      
      row_data <- row_data %>%
        mutate(
          sport = sport,
          sex = sex,
          school = school
        )
      
      row_data
    })
  }, error = function(e) {
    message("Failed to scrape ", url)
    tibble()
  })
}


# Apply the scraper to each team in the list
all_rosters <- pmap_dfr(team_urls, scrape_team_roster)

# View the combined results
print(all_rosters)



#Cleanup
clean_roster <- function(roster_df) {
  roster_df %>%
    rowwise() %>%
    mutate(
      values = list(c_across(starts_with("col")))
    ) %>%
    mutate(
      name = values[which.max(str_detect(values, "^[A-Z][a-z]+\\s[A-Z][a-z]+"))],  # best guess at full name
      year = values[str_detect(values, "(?i) Fr.|SO.|JR.|SR.|So.|Jr.|Sr.|5th")][1],
      position = values[str_detect(values, "(?i)forward|defense|midfield|goalkeeper|sprinter|thrower|distance|hurdles|diver|freestyle|breaststroke|fly|back|IM")][1],
      hometown_school = values[str_detect(values, ",") & str_detect(values, "/")][1]
    ) %>%
    mutate(
      hometown = str_trim(str_extract(hometown_school, "^[^/]+")),
      high_school = str_trim(str_extract(hometown_school, "(?<=/ ).*$"))
    ) %>%
    select(-values, -hometown_school) %>%
    ungroup()
}

cleaned_rosters <- clean_roster(all_rosters)

#More cleanup
cleaned_rosters_final <- cleaned_rosters%>%
  filter((school == "Harvard University" & col1 != "") |
           (school == "Harvard University" & sport == "Fencing" & !is.na(col4)) |
           (school == "Cornell University" & !is.na(col4)))%>%
  #Assigning which sports use which of the columns to store athlete names
  mutate(name = case_when(school=="Harvard University" & sport %in% c("Skiing","Golf","Heavyweight Rowing","Lightweight Rowing", "Swimming & Diving","Tennis","Cross Country","Track & Field","Wrestling") ~ col1,
                          school=="Harvard University" & sport %in% c("Fencing","Basketball","Field Hockey","Ice Hockey", "Lacrosse", "Soccer","Softball","Volleyball","Water Polo","Baseball","Football") ~ col2,
                          school=="Cornell University" & sport %in% c("Fencing","Golf","Gymnastics", "Swimming & Diving","Track & Field") ~ col1,
                          school=="Cornell University" & sex == "Men" & sport %in% c("Lacrosse","Soccer") ~ col2,
                          school=="Cornell University" & sport %in% c("Basketball","Cross Country","Equestrian","Heavyweight Rowing","Ice Hockey","Lightweight Rowing","Rowing", "Volleyball","Tennis","Water Polo","Baseball","Football","Wrestling") ~ col2,
                          school=="Cornell University" & sport %in% c("Field Hockey","Lacrosse","Soccer","Softball") ~ col3,
                          T ~ name))%>%
  #Some pages have both Men/Women and so one of the sexes must be manually assigned
  mutate(sex = case_when(school == "Harvard University" & sport == "Track & Field" & name %in% c("Chrystal Aluya", "Mfoniso Andrew", "Fabiola Belibi", "Josefina Biernacki", "Victoria Bossong", "Anna Burt", "Alana Carroll", "Mackenzie Condon", "Zoe Cooper", "Cristina DeMeo", "Michaela Denson", "Jordyn Duby", "Chloe Fair", "Eden Finkelstein", "Eloise Freitag", "Cammy Garabian", "Makena Gates", "Jackie George", "Ella Gilson", "Isabelle Goldstein", "Izzy Goudros", "Katie Haag", "Mayi Hughes", "Marie Jensen", "Jada Johnson", "Emma Langis", "Shaked Leibovitz", "Molly Malague", "Katina Martin", "Tina Martin", "Ellaney Matarese", "Hannah McLaughlin", "Marianne Mihas", "Sarah Naticchia", "Egbe Ndip-Agbor", "Lily O'Donoghue-McDonald", "Ellie O'Hara", "Jacklynn Okereke", "Sarah Omoregie", "Adaji Osaro-Igwe", "Allaura Osborne", "Kristin Otervik", "Annelies Quinton", "Maia Ramsden", "Stephanie Ratcliffe", "Maya Rayle", "Anastasia Retsa", "Isabell Sagar", "Penelope Salmon", "Cara Salsberry", "Meaghan Toscano", "Estel Valeanu") ~ "Women",
                         school == "Harvard University" & sport == "Cross Country" & name %in% c("Zoe Cooper", "Cristina DeMeo", "Jordyn Duby", "Eloise Freitag", "Makena Gates", "Ella Gilson", "Isabelle Goldstein", "Katie Haag", "Marie Jensen", "Emma Langis", "Shaked Leibovitz", "Molly Malague", "Ellaney Matarese", "Marianne Mihas", "Sarah Naticchia", "Lily O’Donoghue-McDonald", "Kristin Otervik", "Annelies Quinton", "Maia Ramsden", "Maya Rayle", "Iz Sagar", "Penelope Salmon") ~ "Women",
                         school == "Harvard University" & sport == "Skiing" & name %in% c("Laura Appleby", "Quincy Donley", "Annabel Hagen", "Elsie Halvorsen", "Madeline Kitch", "Sydney Mason", "Hayden McJunkin", "Emma Ryan", "Olyvia Snyder", "Amelia Tucker", "Tali Wong") ~ "Women",
                         T ~ sex),
         sport = case_when(sport %in% c("Heavyweight Rowing","Lightweight Rowing") ~ "Rowing",
                           T ~ sport))%>%
  filter(!(sport == "Rowing" & sex == "Men"))%>%
  select(name,school,sex,sport)%>%
  distinct()

#Calculate lost roster spots overall by school and overall by school/sex/sport
Roster_Calc <- cleaned_rosters_final%>%
  group_by(school,sport,sex)%>%
  summarise(n=n())%>%
  left_join(Scholarship_Old_Limits, by = c("sport" = "Sport","sex" = "Sex"))%>%
  mutate(`Lost Roster Spots` = case_when(`New limit` < n ~ n - `New limit`,
                                         T ~ 0))

Roster_Calc_Totals <- cleaned_rosters_final%>%
  group_by(school,sport,sex)%>%
  summarise(n=n())%>%
  left_join(Scholarship_Old_Limits, by = c("sport" = "Sport","sex" = "Sex"))%>%
  mutate(`Lost Roster Spots` = case_when(`New limit` < n ~ n - `New limit`,
                                         T ~ 0))%>%
  group_by(school)%>%
  summarise(Total = sum(`Lost Roster Spots`,na.rm=T))

#Princeton Scrape 2022-2023####
Princeton_URLs <- tibble(
  url = c("https://goprincetontigers.com/sports/womens-basketball/roster/2022-23","https://goprincetontigers.com/sports/womens-cross-country/roster/2022","https://goprincetontigers.com/sports/womens-fencing/roster/2022-23","https://goprincetontigers.com/sports/field-hockey/roster/2022","https://goprincetontigers.com/sports/womens-golf/roster/2022-23","https://goprincetontigers.com/sports/womens-ice-hockey/roster/2022-23","https://goprincetontigers.com/sports/womens-lacrosse/roster/2023","https://goprincetontigers.com/sports/womens-rowing/roster/2023","https://goprincetontigers.com/sports/womens-soccer/roster/2022","https://goprincetontigers.com/sports/softball/roster/2023","https://goprincetontigers.com/sports/womens-swimming-and-diving/roster/2023-24","https://goprincetontigers.com/sports/womens-tennis/roster/2022-23","https://goprincetontigers.com/sports/womens-track-and-field/roster/2022-23","https://goprincetontigers.com/sports/womens-volleyball/roster/2023", "https://goprincetontigers.com/sports/womens-water-polo/roster/2023",
          "https://goprincetontigers.com/sports/baseball/roster/2023","https://goprincetontigers.com/sports/mens-basketball/roster/2022-23","https://goprincetontigers.com/sports/mens-cross-country/roster/2022","https://goprincetontigers.com/sports/mens-fencing/roster/2022-23","https://goprincetontigers.com/sports/football/roster/2022","https://goprincetontigers.com/sports/mens-golf/roster/2022-23","https://goprincetontigers.com/sports/mens-ice-hockey/roster/2022-23","https://goprincetontigers.com/sports/mens-lacrosse/roster/2023","https://goprincetontigers.com/sports/mens-soccer/roster/2022","https://goprincetontigers.com/sports/mens-swimming-and-diving/roster/2023-24","https://goprincetontigers.com/sports/mens-tennis/roster/2022-23","https://goprincetontigers.com/sports/mens-track-and-field/roster/2023-24","https://goprincetontigers.com/sports/mens-volleyball/roster/2023","https://goprincetontigers.com/sports/mens-water-polo/roster/2023","https://goprincetontigers.com/sports/wrestling/roster/2022-23",
          "https://yalebulldogs.com/sports/womens-basketball/roster/2022-23?view=2","https://yalebulldogs.com/sports/womens-cross-country/roster/2022?view=2","https://yalebulldogs.com/sports/womens-fencing/roster/2022-23?view=2","https://yalebulldogs.com/sports/field-hockey/roster/2023?view=2","https://yalebulldogs.com/sports/womens-golf/roster/2022-23?view=2","https://yalebulldogs.com/sports/womens-gymnastics/roster/2023?view=2","https://yalebulldogs.com/sports/womens-ice-hockey/roster/2022-23?view=2","https://yalebulldogs.com/sports/womens-lacrosse/roster/2023?view=2","https://yalebulldogs.com/sports/womens-crew/roster/2022-23?view=2","https://yalebulldogs.com/sports/womens-soccer/roster/2022?view=2","https://yalebulldogs.com/sports/softball/roster/2023?view=2","https://yalebulldogs.com/sports/womens-swimming-and-diving/roster/2022-23?view=2","https://yalebulldogs.com/sports/womens-tennis/roster/2022-23?view=2","https://yalebulldogs.com/sports/womens-track-and-field/roster/2022-23?view=2","https://yalebulldogs.com/sports/womens-volleyball/roster/2022?view=2",
          "https://yalebulldogs.com/sports/baseball/roster/2023?view=2","https://yalebulldogs.com/sports/mens-basketball/roster/2022-23?view=2","https://yalebulldogs.com/sports/mens-cross-country/roster/2022?view=2","https://yalebulldogs.com/sports/mens-fencing/roster/2022-23?view=2","https://yalebulldogs.com/sports/football/roster/2022-23?view=2","https://yalebulldogs.com/sports/mens-golf/roster/2022-23?view=2","https://yalebulldogs.com/sports/mens-ice-hockey/roster/2022-23?view=2","https://yalebulldogs.com/sports/mens-lacrosse/roster/2023?view=2","https://yalebulldogs.com/sports/mens-soccer/roster/2022?view=2","https://yalebulldogs.com/sports/mens-swimming-and-diving/roster/2022-23?view=2","https://yalebulldogs.com/sports/mens-tennis/roster/2022-23?view=2","https://yalebulldogs.com/sports/mens-track-and-field/roster/2022-23?view=2",
          "https://dartmouthsports.com/sports/womens-basketball/roster/2022-23","https://dartmouthsports.com/sports/womens-cross-country/roster/2022","https://dartmouthsports.com/sports/equestrian/roster/2022-23","https://dartmouthsports.com/sports/field-hockey/roster/2022","https://dartmouthsports.com/sports/womens-golf/roster/2022-23","https://dartmouthsports.com/sports/womens-ice-hockey/roster/2022-23","https://dartmouthsports.com/sports/womens-lacrosse/roster/2023","https://dartmouthsports.com/sports/womens-rowing/roster/2022-23","https://dartmouthsports.com/sports/alpine-skiing/roster/2022-23","https://dartmouthsports.com/sports/womens-soccer/roster/2022","https://dartmouthsports.com/sports/softball/roster/2023","https://dartmouthsports.com/sports/womens-swimming-and-diving/roster/2022-23","https://dartmouthsports.com/sports/womens-tennis/roster/2022-23","https://dartmouthsports.com/sports/womens-track-and-field/roster/2022-23","https://dartmouthsports.com/sports/womens-volleyball/roster/2023",
          "https://dartmouthsports.com/sports/baseball/roster/2023","https://dartmouthsports.com/sports/mens-basketball/roster/2022-23","https://dartmouthsports.com/sports/mens-cross-country/roster/2022","https://dartmouthsports.com/sports/football/roster/2022","https://dartmouthsports.com/sports/mens-golf/roster/2022-23","https://dartmouthsports.com/sports/mens-lacrosse/roster/2023","https://dartmouthsports.com/sports/alpine-skiing/roster/2022-23","https://dartmouthsports.com/sports/mens-soccer/roster/2022","https://dartmouthsports.com/sports/mens-swimming-and-diving/roster/2022-23","https://dartmouthsports.com/sports/mens-tennis/roster/2022-23","https://dartmouthsports.com/sports/mens-track-and-field/roster/2022-23"
          ),
  sport = c("Basketball", "Cross Country", "Fencing", "Field Hockey", "Golf", "Ice Hockey", "Lacrosse", "Rowing", "Soccer", "Softball", "Swimming & Diving", "Tennis", "Track & Field", "Volleyball", "Water Polo",
            "Baseball", "Basketball", "Cross Country", "Fencing", "Football", "Golf", "Ice Hockey", "Lacrosse", "Soccer", "Swimming & Diving", "Tennis", "Track & Field", "Volleyball", "Water Polo", "Wrestling",
            "Basketball", "Cross Country", "Fencing", "Field Hockey", "Golf", "Gymnastics", "Ice Hockey", "Lacrosse", "Rowing", "Soccer", "Softball", "Swimming & Diving", "Tennis", "Track & Field", "Volleyball",
            "Baseball", "Basketball", "Cross Country", "Fencing", "Football", "Golf", "Ice Hockey", "Lacrosse", "Soccer", "Swimming & Diving", "Tennis", "Track & Field",
            "Basketball", "Cross Country", "Equestrian", "Field Hockey", "Golf", "Ice Hockey", "Lacrosse", "Rowing", "Skiing", "Soccer", "Softball", "Swimming & Diving", "Tennis", "Track & Field", "Volleyball",
            "Baseball", "Basketball", "Cross Country", "Football", "Golf", "Lacrosse", "Skiing", "Soccer", "Swimming & Diving", "Tennis", "Track & Field"
            ),
  sex = c("Women","Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women",
          "Men","Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men",
          "Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women",
          "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men",
          "Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women",
          "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men"
  ),
  school = c("Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University",
             "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University",
             "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University",
             "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University",
             "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", 
             "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College"
             )
)

#Count number of rosters
URL_Length <- nrow(Princeton_URLs)

#Create blank DF for roster data to be added to
Final_Roster <- data.frame()

while(URL_Length > 0){
  
  #Pull first row from current roster
  url <-Princeton_URLs[[1]][URL_Length]
  
  #Scan row
  page <- read_html(url)
  
  # Step 2: extract all scripts
  scripts <- page %>% html_elements("script") %>% html_text()
  
  #Extract the __INITIAL_STATE__ JSON string (single quoted)
  raw_data <- case_when(Princeton_URLs[[4]][URL_Length] == "Princeton University" ~ scripts[str_detect(scripts, "window.__INITIAL_STATE__")][1],
                        Princeton_URLs[[4]][URL_Length] %in% c("Dartmouth College", "Yale University") ~ scripts[str_detect(scripts, "\\{\"@type\"")][1])
  
  
  #Pull each line that matches athlete's format (should be ignoring coaches/staff)
  Roster_Length <- if (Princeton_URLs[[4]][URL_Length] == "Princeton University") {
    Roster_Length <- str_extract_all(raw_data, '"firstName":"[^"]+","lastName":"[^"]+","hometown":"[^"]+","highSchool":"[^"]+","previousSchool":')[[1]]
  } else if (Princeton_URLs[[4]][URL_Length] %in% c("Dartmouth College", "Yale University") ) {
    Roster_Length <- str_extract_all(raw_data, '"name":"[^"]+","gender":"[^"]+","url":"[^"]+"')[[1]]
  } else {
    Roster_Length <- character(0)  # or NULL, depending on your preference
  }
  
  #Create loop variable for the current roster
  n <- length(Roster_Length)
  
  #Create list for each field
  Last_Name_List <- list()
  First_Name_List <- list()
  Whole_Name_List <- list()
  High_School_List <- list()
  Home_Town_List <- list()
  
  while(n>0){
    
    # Match values safely — default to NA if no match
    last_name <- str_match(Roster_Length, '\\"lastName\\":\\"([^"]+?)\\",\\"hometown\\"')[,2]
    first_name <- str_match(Roster_Length, '\\"firstName\\":\\"([^"]+?)\\",\\"lastName\\"')[,2]
    full_name <- str_match(Roster_Length, '\\"name\\":\\"([^"]+?)\\",\\"gender\\"')[,2]
    high_school <- str_match(Roster_Length, '\\"highSchool\\":\\"([^"]+?)\\",\\"previousSchool\\"')[,2]
    hometown <- str_match(Roster_Length, '\\"hometown\\":\\"([^"]+?)\\",\\"highSchool\\"')[,2]
    
    # Append to lists (use NA if NULL)
    Last_Name_List <- append(Last_Name_List, list(ifelse(is.na(last_name), NA, last_name)))
    First_Name_List <- append(First_Name_List, list(ifelse(is.na(first_name), NA, first_name)))
    Whole_Name_List <- append(Whole_Name_List, list(ifelse(is.na(full_name), NA, full_name)))
    High_School_List <- append(High_School_List, list(ifelse(is.na(high_school), NA, high_school)))
    Home_Town_List <- append(Home_Town_List, list(ifelse(is.na(hometown), NA, hometown)))
    
    #Update loop
    n=n-1
  }
  
  #Create the new DF
  Final_Roster_Temp <- data.frame(
    FirstName = unlist(First_Name_List),
    LastName = unlist(Last_Name_List),
    Whole_Name = unlist(Whole_Name_List),
    HighSchool = unlist(High_School_List),
    HomeTown = unlist(Home_Town_List),
    sport = Princeton_URLs$sport[URL_Length],
    sex = Princeton_URLs$sex[URL_Length],
    school = Princeton_URLs$school[URL_Length],
    stringsAsFactors = FALSE
  )
  
  #Create/append to current DF of all athletes
  Final_Roster <- bind_rows(Final_Roster, Final_Roster_Temp)
  
  #Update URL loop
  URL_Length <- URL_Length - 1
}

Final_Roster <- bind_rows(Final_Roster,cleaned_rosters_final)

Roster_Calc_Princeton <- Final_Roster%>%
  mutate(sport = case_when(sport %in% c("Heavyweight Rowing","Lightweight Rowing") ~ "Rowing",
                           T ~ sport))%>%
  filter(!(sport == "Rowing" & sex == "Men"))%>%
  distinct()%>%
  group_by(school,sport,sex)%>%
  summarise(n=n())%>%
  left_join(Scholarship_Old_Limits, by = c("sport" = "Sport","sex" = "Sex"))%>%
  mutate(`Lost Roster Spots` = case_when(`New limit` < n ~ n - `New limit`,
                                         T ~ 0),
         `New Roster Spots` = case_when(`New limit` > n ~ `New limit` - n,
                                      T ~ 0))

Total_Roster_Changes <- Roster_Calc_Princeton%>%
  group_by(school)%>%
  summarise(Lost = sum(`Lost Roster Spots`,na.rm=T),New = sum(`New Roster Spots`, na.rm=T))


#Graph#####
# Create a combined sport/sex column
Roster_Calc <- Roster_Calc %>%
  mutate(sport_sex = paste(sport, sex, sep = " - "))%>%
  mutate(sport_sex = if_else(
    sport_sex == "Swimming & Diving - Women",
    "Swimming &\nDiving - Women",
    sport_sex
  ))

# Extract unique new limits per sport/sex
limits <- Roster_Calc %>%
  select(sport_sex, `New limit`) %>%
  distinct()

# Plot
ggplot(Roster_Calc, aes(x = school, y = n, fill = school)) +
  geom_col(color="black") +
  geom_hline(data = limits, aes(yintercept = `New limit`), linetype = "dashed", color = "red") +
  facet_wrap(~ sport_sex, scales = "free_y") +
  labs(
    x = NULL,
    y = NULL,
    title = "Number of Athletes per Sport/Sex by Institution vs. House Settlement Roster Limits",
    legend = "Institution"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    legend.position = "right",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )+
  scale_fill_manual(
    name = "Institutions",
    values = c(
    "Cornell University" = "#B31B1B",
    "Harvard University" = "#A51C30",
    "Yale University" = "#00356B",
    "Princeton University" = "#F58025",
    "Dartmouth College" = "#00693E"
  )) 







#Hockey ticket sales vs FBS football####
Hockey_Football_Ticket_Sales <- NCAA_Financial_Reports_Data%>%
  filter(`Fiscal Year` == 2023 & Item == 'Ticket Sales' & Sport %in% c('Football','Ice Hockey') & Men > 2838181)%>%
  filter(!is.na(unitid))%>%#####UPDATE LATER
  left_join(School_Logos,by=c("unitid" = "unitid")) %>%
  arrange(desc(Men))%>%
  mutate(id = row_number()) %>%
  mutate(id = fct_reorder(as.character(id), Men, .desc = TRUE))%>%
  mutate(Men = Men/1000000)


ggplot(Hockey_Football_Ticket_Sales, aes(x = id, y = Men, fill = Sport))+
  geom_bar(stat="identity") + 
  geom_text(aes(label = dollar(Men,accuracy=1)),vjust=2,size=2.5,color="white")+
  scale_x_discrete(labels = Hockey_Football_Ticket_Sales$abbreviation) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 20)),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y='Total Ticket Sales (Millions)', title = "Top 50 Institutions in Total Ticket Sales, Football/Ice Hockey") +
  scale_fill_manual(values = c("#042a32","#189339"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 66), labels = function(x) paste0("$", round(x)))



#To commit changes to GitHub:####
#Stage, commit, comment, push
#Always run in terminal to push update: git push origin main


