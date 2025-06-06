

#Save this for a later newsletter - going to need to make sure I have as many FRS docs as possible

rm(list = ls())
options(scipen = 999)
setwd("/Volumes/GoogleDrive-103007369919315814324/untitled folder/My Drive/NILnomics/Newsletters") #Update this each week

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
library(gitcreds)#Need to push to GitHub
library(tidyverse)
library(rvest)
library(purrr)
library(stringr)
library(tidyr)
library(jsonlite)
library(officer)
library(readxl)
library(DT)
library(knitr)
library(kableExtra)


#Load Kaggle data####
NILnomics_school_logo_and_other_data <- "nilnomics/ncaa-school-logo-and-other-data"
NILnomics_arenas <- "nilnomics/College-Sports-Arenas"
NILnomics_ncaa_financial_reporting_data <- "nilnomics/ncaa-financial-reporting-data"

#Use Kaggle CLI commands to download individual dataset
system(paste("kaggle datasets download -d", NILnomics_school_logo_and_other_data, "--force"), intern = TRUE)
unzip("ncaa-school-logo-and-other-data.zip")
School_Logos <- read_excel("School_Logos.xlsx")

#Use Kaggle CLI commands to download individual dataset
system(paste("kaggle datasets download -d", NILnomics_arenas, "--force"), intern = TRUE)
unzip("College-Sports-Arenas.zip")
Arenas <- read_excel("Arenas.xlsx")

#Use Kaggle CLI commands to download individual dataset
system(paste("kaggle datasets download -d", NILnomics_ncaa_financial_reporting_data, "--force"), intern = TRUE)
unzip("ncaa-financial-reporting-data.zip")
NCAA_Financial_Reports_Data <- readr::read_csv("NCAA Financial Reports Data - Items Disaggregated.csv")

Ticket_Sales_All_Sports <- NCAA_Financial_Reports_Data%>%
  filter(Item == 'Ticket Sales')%>%
  pivot_longer(c(Men, Women, MenOrWomen),names_to = "Gender", values_to = "Ticket Revenue")%>%
  select(unitid, IPEDS_Name, `Fiscal Year`, Sport, Gender, `Ticket Revenue`)%>%
  filter(!is.na(`Ticket Revenue`) & !Sport %in% c('Sport', 'Subtotal All Teams', 'Revenue Not Related to Specific Teams', 'Total Revenue') & `Ticket Revenue` > 0)

Ticket_Sales_All_Sports_Final <- Ticket_Sales_All_Sports%>%
  filter(!Sport %in% c('Acrobatics and Tumbling','Fencing','Skiing','Bowling','Golf','Equestrian','Rifle','Rowing','Water Polo','Wrestling','Tumbling'))%>% #filter out sports without ticket revenue in every year
  group_by(`Fiscal Year`, Sport, Gender)%>%
  summarise(Highest = max(`Ticket Revenue`, na.rm=T))%>%
  inner_join(Ticket_Sales_All_Sports,by=c('Sport' = 'Sport', 'Gender' = 'Gender', 'Fiscal Year' = 'Fiscal Year', 'Highest' = 'Ticket Revenue'))%>%
  filter(!is.na(`Fiscal Year`) & Gender %in% c('Men', 'Women') & !Sport == 'Others')%>%
  left_join(School_Logos, by=c('unitid' = 'unitid'))%>%
  select(Sport, Gender, `Fiscal Year`, Highest, logo)%>%
  mutate(across(where(is.factor), as.character))%>%
  filter(!Sport == 'Volleyball')

Ticket_Sales_All_Logo <- Ticket_Sales_All_Sports_Final%>%
  select(Sport, Gender, `Fiscal Year`, logo)%>%
  pivot_wider(names_from = `Fiscal Year`, values_from = logo)

Ticket_Sales_All_Sales <- Ticket_Sales_All_Sports_Final%>%
  select(Sport, Gender, `Fiscal Year`, Highest)%>%
  pivot_wider(names_from = `Fiscal Year`, values_from = Highest)%>%
  mutate(
    across(
      .cols = c(`2017`:`2024`),
      .fns = ~ dollar(.x / 1e6, accuracy = 0.1),
      .names = "{.col}"
    )
  )


Ticket_Sales_All_Logo2 <- Ticket_Sales_All_Logo
for (col in year_cols) {
  Ticket_Sales_All_Logo2[[col]] <- sprintf('<img src="%s" height="35" alt="%s"/>', Ticket_Sales_All_Logo2[[col]], col)
}

kable(Ticket_Sales_All_Logo2, escape = FALSE, format = "html") %>%
  add_header_above(c("Top Ticket Revenue by Sport/Gender, 2017 - 2024" = ncol(Ticket_Sales_All_Logo2))) %>%  # adjust numbers based on your df
  kable_styling(font_size = 20, full_width = TRUE, position = "left")


#Start of focus on big sports
Logo_Sales_Tickets_DF <- rbind(Ticket_Sales_All_Logo2,Ticket_Sales_All_Sales)%>%
  filter(Sport %in% c('Football','Basketball') | (Sport == 'Ice Hockey' & Gender == 'Men'))%>%
  arrange(Sport,Gender)%>% 
  group_by(Sport, Gender) %>%      # Group by these columns
  mutate(row_id = row_number()) %>% # Then number within group
  ungroup()


# Create a group identifier every 2 rows
Logo_Sales_Tickets_DF <- Logo_Sales_Tickets_DF %>%
  mutate(group_id = as.integer(factor(paste(Sport, Gender))))%>%
  group_by(group_id) %>%
  mutate(row_type = ifelse(row_number() == 1, "Logo", "Dollar")) %>%
  ungroup()

Logo_Sales_Tickets_DF %>%
  mutate(
    row_type = recode(row_type,
                      "Logo" = "Team",
                      "Dollar" = "Ticket Revenue ($ Millions)"),
    across(c(Sport, Gender), as.character)
  ) %>%
  arrange(Sport, Gender, row_type) %>%  # 🔑 sort to enable collapsing
  select(row_type, Sport, Gender, `2017`:`2024`) %>%
  kable("html", escape = FALSE, col.names = c("Type", "Sport", "Gender", 2017:2024)) %>%
  kable_styling(full_width = TRUE) %>%
  collapse_rows(columns = 2:3, valign = "top", row_group_label_position = "stack")%>%
  row_spec(seq(1, nrow(Logo_Sales_Tickets_DF), by = 2), 
           extra_css = "border-bottom: none;")


