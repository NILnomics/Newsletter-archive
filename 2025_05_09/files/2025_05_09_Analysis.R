rm(list = ls())
options(scipen = 999)

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

getwd()


# Specify the dataset slug (replace with your own dataset slug)
NILnomics_Athletic_Participation_Data <- "nilnomics/ncaa-athlete-participation"
NILnomics_school_logo_and_other_data <- "nilnomics/ncaa-school-logo-and-other-data"
NILnomics_ncaa_financial_reporting_data <- "nilnomics/ncaa-financial-reporting-data"
NILnomics_ncaa_eada_data <- "nilnomics/ncaa-eada-data"
NILnomics_2024_25_ncaa_mens_hockey_results <- "nilnomics/2024-25-ncaa-mens-hockey-results"
NILnomics_house_settlement_roster_limits <- "nilnomics/house-settlement-roster-limits"
NILnomics_institution_name_iped_id_conference_bridge_file <- "nilnomics/institution-name-iped-id-conference-bridge-file"



# Run the Kaggle CLI command to download the dataset
system(paste("kaggle datasets download -d", NILnomics_Athletic_Participation_Data, "--force"), intern = TRUE)
unzip("ncaa-athlete-participation.zip")
Athletic_Participation_Data <- readr::read_csv("Athletic Participation Data.csv")


# Run the Kaggle CLI command to download the dataset
system(paste("kaggle datasets download -d", NILnomics_school_logo_and_other_data, "--force"), intern = TRUE)
unzip("ncaa-school-logo-and-other-data.zip")
School_Logos <- read_excel("School_Logos.xlsx")


# Run the Kaggle CLI command to download the dataset
system(paste("kaggle datasets download -d", NILnomics_ncaa_financial_reporting_data, "--force"), intern = TRUE)
unzip("ncaa-financial-reporting-data.zip")
NCAA_Financial_Reports_Data <- readr::read_csv("NCAA Financial Reports Data - Items Disaggregated.csv")


# Run the Kaggle CLI command to download the dataset
system(paste("kaggle datasets download -d", NILnomics_ncaa_eada_data, "--force"), intern = TRUE)
unzip("ncaa-eada-data.zip")
EADA_2023 <- read_excel("EADA_2023.xlsx")%>%
  filter(ClassificationCode == 1)

# Run the Kaggle CLI command to download the dataset
system(paste("kaggle datasets download -d", NILnomics_2024_25_ncaa_mens_hockey_results, "--force"), intern = TRUE)
unzip("2024-25-ncaa-mens-hockey-results.zip")
Hockey_DF01 <- read_excel("2025 Mens Hockey Results.xlsx")

# Run the Kaggle CLI command to download the dataset
system(paste("kaggle datasets download -d", NILnomics_house_settlement_roster_limits, "--force"), intern = TRUE)
unzip("house-settlement-roster-limits.zip")
Scholarship_Old_Limits <- read_excel("Scholarship_Old_Limits.xlsx")

# Run the Kaggle CLI command to download the dataset
system(paste("kaggle datasets download -d", NILnomics_institution_name_iped_id_conference_bridge_file, "--force"), intern = TRUE)
unzip("institution-name-iped-id-conference-bridge-file.zip")
Institution_Name_UnitIDs <- read_excel("Institution Name UnitIDs.xlsx")

#Load Data

#Top 10 revenue earners####
EADA_2023_TopRevenue01 <- EADA_2023%>%
  select(unitid,OPEID,institution_name, GRND_TOTAL_REVENUE)%>%
  arrange(desc(GRND_TOTAL_REVENUE))

Top10FINALDF <- top_n(EADA_2023_TopRevenue01,10,wt=GRND_TOTAL_REVENUE)%>%
  left_join(School_Logos, by = c('unitid' = 'unitid'))%>%
  rename(Conference = conference,
         School = school,
         Revenue = GRND_TOTAL_REVENUE)

Top10FINALDF%>%
  select(School, Conference,Revenue)%>%
  mutate(
    Team = "",
    Rank = row.names(.),
    Revenue = color_bar("lightgreen")(currency(Revenue,digits=0)))%>%
  select(Rank, Team, School,Conference,Revenue)%>%
  kable("html", escape = F) %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(5, width = "3cm") %>%
  add_header_above(c("Fiscal Year 2023" = 5))%>%
  add_header_above(c("Top 10 Athletic Department Revenue" = 5),font_size=25)%>%
  column_spec(2, image = spec_image(Top10FINALDF$logo, 140, 100))

#Participant and House analysis####
#Using EADA data
#Make a men/women DF separately to label the sex, rejoin
#Swimming and Swimming & Diving are listed separately, although very few schools have Swimming & Diving. Regardless, adding them up to combine them then filtering out the separate parts
EADA_2023_01Men <- EADA_2023%>%
  select(unitid,OPEID,institution_name,PARTIC_MEN_Baseball,PARTIC_MEN_Bskball,PARTIC_MEN_Fencing,PARTIC_MEN_FldHcky,PARTIC_MEN_Football,PARTIC_MEN_Golf,PARTIC_MEN_Gymn,PARTIC_MEN_IceHcky,PARTIC_MEN_Lacrsse,PARTIC_MEN_Rifle,PARTIC_MEN_Rowing,PARTIC_MEN_Skiing,PARTIC_MEN_Soccer,PARTIC_MEN_Softball,PARTIC_MEN_SwimDivng,PARTIC_MEN_Swimming,PARTIC_MEN_Tennis,PARTIC_MEN_TrkFldIn,PARTIC_MEN_TrkFldOut,PARTIC_MEN_XCountry,PARTIC_MEN_Vollball,PARTIC_MEN_WaterPolo,PARTIC_MEN_Wrestling,PARTIC_MEN_BchVoll,PARTIC_MEN_Bowling,PARTIC_MEN_Eqstrian)%>%
  rename('Baseball' = PARTIC_MEN_Baseball, 'Basketball' = PARTIC_MEN_Bskball,'Beach Volleyball' = PARTIC_MEN_BchVoll,'Bowling' = PARTIC_MEN_Bowling,'Cross Country' = PARTIC_MEN_XCountry,'Equestrian' = PARTIC_MEN_Eqstrian,'Fencing' = PARTIC_MEN_Fencing,'Field Hockey' = PARTIC_MEN_FldHcky,'Football' = PARTIC_MEN_Football,'Golf' = PARTIC_MEN_Golf,'Gymnastics' = PARTIC_MEN_Gymn,'Ice Hockey' = PARTIC_MEN_IceHcky,'Rifle' = PARTIC_MEN_Rifle,'Rowing' = PARTIC_MEN_Rowing,'Skiing' = PARTIC_MEN_Skiing,'Soccer' = PARTIC_MEN_Soccer,'Softball' = PARTIC_MEN_Softball,'Swim1' = PARTIC_MEN_SwimDivng,'Swim2' = PARTIC_MEN_Swimming,'Tennis' = PARTIC_MEN_Tennis,'Track and Field, X-Country Indoor' = PARTIC_MEN_TrkFldIn, 'Track and Field, X-Country Outdoor' = PARTIC_MEN_TrkFldOut,'Volleyball' = PARTIC_MEN_Vollball,'Water Polo' = PARTIC_MEN_WaterPolo,'Wrestling' = PARTIC_MEN_Wrestling,'Lacrosse' = PARTIC_MEN_Lacrsse)%>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  mutate('Swimming' = Swim1 + Swim2)%>%
  pivot_longer(!c(unitid,OPEID, institution_name),names_to='Sport',values_to='Participants')%>%
  mutate('Sex' = case_when(Sport == 'Lacrosse' ~ 'Men',
                           T ~ 'Both'),
         'Sex Original' = 'Men')

EADA_2023_01Women <- EADA_2023%>%
  select(unitid,OPEID,institution_name,PARTIC_WOMEN_Baseball,PARTIC_WOMEN_Bskball,PARTIC_WOMEN_Fencing,PARTIC_WOMEN_FldHcky,PARTIC_WOMEN_Football,PARTIC_WOMEN_Golf,PARTIC_WOMEN_Gymn,PARTIC_WOMEN_IceHcky,PARTIC_WOMEN_Lacrsse,PARTIC_WOMEN_Rifle,PARTIC_WOMEN_Rowing,PARTIC_WOMEN_Skiing,PARTIC_WOMEN_Soccer,PARTIC_WOMEN_Softball,PARTIC_WOMEN_SwimDivng,PARTIC_WOMEN_Swimming,PARTIC_WOMEN_Tennis,PARTIC_WOMEN_TrkFldIn,PARTIC_WOMEN_TrkFldOut,PARTIC_WOMEN_XCountry,PARTIC_WOMEN_Vollball,PARTIC_WOMEN_WaterPolo,PARTIC_WOMEN_Wrestling,PARTIC_WOMEN_BchVoll,PARTIC_WOMEN_Bowling,PARTIC_WOMEN_Eqstrian)%>%
  rename('Baseball' = PARTIC_WOMEN_Baseball, 'Basketball' = PARTIC_WOMEN_Bskball,'Beach Volleyball' = PARTIC_WOMEN_BchVoll,'Bowling' = PARTIC_WOMEN_Bowling,'Cross Country' = PARTIC_WOMEN_XCountry,'Equestrian' = PARTIC_WOMEN_Eqstrian,'Fencing' = PARTIC_WOMEN_Fencing,'Field Hockey' = PARTIC_WOMEN_FldHcky,'Football' = PARTIC_WOMEN_Football,'Golf' = PARTIC_WOMEN_Golf,'Gymnastics' = PARTIC_WOMEN_Gymn,'Ice Hockey' = PARTIC_WOMEN_IceHcky,'Rifle' = PARTIC_WOMEN_Rifle,'Rowing' = PARTIC_WOMEN_Rowing,'Skiing' = PARTIC_WOMEN_Skiing,'Soccer' = PARTIC_WOMEN_Soccer,'Softball' = PARTIC_WOMEN_Softball,'Swim1' = PARTIC_WOMEN_SwimDivng,'Swim2' = PARTIC_WOMEN_Swimming,'Tennis' = PARTIC_WOMEN_Tennis,'Track and Field, X-Country Indoor' = PARTIC_WOMEN_TrkFldIn, 'Track and Field, X-Country Outdoor' = PARTIC_WOMEN_TrkFldOut,'Volleyball' = PARTIC_WOMEN_Vollball,'Water Polo' = PARTIC_WOMEN_WaterPolo,'Wrestling' = PARTIC_WOMEN_Wrestling,'Lacrosse' = PARTIC_WOMEN_Lacrsse)%>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  mutate('Swimming' = Swim1 + Swim2)%>%
  pivot_longer(!c(unitid,OPEID, institution_name),names_to='Sport',values_to='Participants')%>%
  mutate('Sex' = case_when(Sport == 'Lacrosse' ~ 'Women',
                           T ~ 'Both'),
         'Sex Original' = 'Women')

EADA_2023_01 <- rbind(EADA_2023_01Women,EADA_2023_01Men)%>%
  left_join(Scholarship_Old_Limits,by=c('Sport' = 'Sport', 'Sex' = 'Sex'))%>%
  left_join(Institution_Name_UnitIDs,by=c('unitid' = 'unitid'))%>%
  filter(!Sport %in% c('Swim1','Swim2'))%>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  mutate('Lost Sports' = case_when(Participants > `Roster Limit` ~ Participants - `Roster Limit`,
                                 T ~ 0))

EADA_2023_01_Sport <- EADA_2023_01%>%
  group_by(Sport,Level)%>%
  summarise('Lost Roster Spots' = sum(`Lost Sports`, na.rm = T))

EADA_2023_01_SportTotal <- EADA_2023_01%>%
  group_by(Sport)%>%
  summarise('Total Lost Roster Spots' = sum(`Lost Sports`, na.rm = T))%>%
  left_join(EADA_2023_01_Sport,by=c('Sport'='Sport'))%>%
  mutate(LabelSpots = case_when(`Total Lost Roster Spots` > 382 ~ `Lost Roster Spots`,
                                T ~ as.numeric(NA)))

EADA_2023_01_Sex <- EADA_2023_01%>%
  group_by(`Sex Original`)%>%
  summarise('Lost Roster Spots' = sum(`Lost Sports`, na.rm = T))

EADA_2023_01_Institution <- EADA_2023_01%>%
  group_by(institution_name)%>%
  summarise('Lost Roster Spots' = sum(`Lost Sports`, na.rm = T))

EADA_2023_01_Total <- EADA_2023_01%>%
  group_by(Level)%>%
  summarise('Lost Roster Spots' = sum(`Lost Sports`, na.rm = T))

ggplot(EADA_2023_01_SportTotal%>%filter(`Lost Roster Spots` > 0), aes(x = fct_reorder(str_wrap(Sport,20),`Total Lost Roster Spots`), y = `Lost Roster Spots`, fill = Level))+
  geom_col(color="black")+
  geom_text(aes(label=LabelSpots, group=Level),position = position_stack(vjust = 0.5), size=3.5,color="white")+
  coord_flip()+
  labs(x="Sport",y="Total Number of Roster Spots", title="Number of Athletes Losing Roster Spots by Sport")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 20)))+
  scale_fill_manual(values = c("#042a32","#189339"))

ggplot(EADA_2023_01_Sex, aes(x="", y=`Lost Roster Spots`, fill=`Sex Original`))+
  geom_bar(width = 1, stat = "identity",color="black")+
  geom_text(aes(label = `Lost Roster Spots`),
            position = position_stack(vjust = 0.5),color="white") +
  coord_polar("y", start=0)+
  theme_minimal()+
  guides(fill = guide_legend(title = "Sex"))+
  labs(title="Number of Athletes Losing Roster Spots by Sex")+
  theme(axis.text.x=element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"))+
  scale_fill_manual(values = c("#189339",
                               "#042a32"))

ggplot(EADA_2023_01_Institution%>%filter(`Lost Roster Spots` > 71), aes(x = fct_reorder(institution_name,`Lost Roster Spots`), y = `Lost Roster Spots`))+
  geom_col(color="black",fill="#189339")+
  geom_text(aes(label=`Lost Roster Spots`),position = position_stack(vjust = 0.5), size=3.5,color="white")+
  coord_flip()+
  labs(x="Institution",y="Total Number of Roster Spots", title=str_wrap("Number of Athletes Losing Roster Spots by Institution",35))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 20)))

  
#Hockey####
HOCKEY_MFRS_DATA01 <- NCAA_Financial_Reports_Data%>%
  filter(`Fiscal Year` == 2023 & RE_Cat == 'Revenue' & Sport == 'Ice Hockey')%>%
  replace_na(list(Men=0,Women=0,MenOrWomen=0))%>%
  mutate(Total = Men + Women + MenOrWomen)%>%
  group_by(IPEDS_Name)%>%
  summarise(`Total Revenue` = sum(Total,na.rm=T))%>%
  left_join(Institution_Name_UnitIDs, by=c('IPEDS_Name'='Institution'))%>%
  arrange(desc(`Total Revenue`))%>%
  inner_join(Hockey_DF01, by=c('IPEDS_Name' = 'IPEDS_Name'))%>%
  left_join(School_Logos, by = c('unitid.x' = 'unitid'))%>%
  mutate(`Frozen Four Tournament:` = case_when(`Frozen Four` == 1 ~ "Made Frozen Four",
                                               T ~ "Didn't Make Frozen Four"),
         Revenue = currency(`Total Revenue`/1000000,digits=0))

ggplot(HOCKEY_MFRS_DATA01)+
  geom_point(aes(x=Revenue,y=PctOverall,colour=`Frozen Four Tournament:`),size=15,shape=1,stroke=2)+
  geom_image(
    aes(
      x = Revenue, y = PctOverall,
      image = logo
    ),
    size = 0.25, by = "width") +
  labs(y = "Win %",title="2024-2025 NCAA DI Hockey Team Win Percentage by Team Revenue",x="Total Revenue (Millions) FY 2023", fill="Frozen Four")+
  guides(fill = guide_legend(reverse=TRUE))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        plot.margin = margin(t=5,l=5,r=5,b = 20),
        legend.position = "top")+
  scale_y_continuous(labels = scales::percent,limits=c(0,1))+
  scale_x_continuous(labels = scales::dollar_format(prefix="$", suffix = "M"))



