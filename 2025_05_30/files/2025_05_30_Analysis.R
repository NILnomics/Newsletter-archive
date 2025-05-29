rm(list = ls())
options(scipen = 999)
setwd("/Volumes/GoogleDrive-103007369919315814324/untitled folder/My Drive/NILnomics/Newsletters/2025_05_30")

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


#Load Kaggle data####
NILnomics_school_logo_and_other_data <- "nilnomics/ncaa-school-logo-and-other-data"
NILnomics_arenas <- "nilnomics/College-Sports-Arenas"

#Use Kaggle CLI commands to download individual dataset
system(paste("kaggle datasets download -d", NILnomics_school_logo_and_other_data, "--force"), intern = TRUE)
unzip("ncaa-school-logo-and-other-data.zip")
School_Logos <- read_excel("School_Logos.xlsx")

#Use Kaggle CLI commands to download individual dataset
system(paste("kaggle datasets download -d", NILnomics_arenas, "--force"), intern = TRUE)
unzip("College-Sports-Arenas.zip")
Arenas <- read_excel("Arenas.xlsx")

#Grid View schools scrape####
School_URLs <- tibble(
  url = c("https://gocrimson.com/sports/womens-basketball/roster?view=2","https://gocrimson.com/sports/womens-fencing/roster?view=2","https://gocrimson.com/sports/field-hockey/roster?view=2","https://gocrimson.com/sports/womens-golf/roster?view=2","https://gocrimson.com/sports/womens-ice-hockey/roster?view=2","https://gocrimson.com/sports/womens-lacrosse/roster?view=2","https://gocrimson.com/sports/womens-heavyweight-rowing/roster?view=2","https://gocrimson.com/sports/womens-lightweight-rowing/roster?view=2","https://gocrimson.com/sports/womens-soccer/roster?view=2","https://gocrimson.com/sports/softball/roster?view=2","https://gocrimson.com/sports/womens-swimming-and-diving/roster?view=2", "https://gocrimson.com/sports/womens-tennis/roster?view=2","https://gocrimson.com/sports/womens-volleyball/roster?view=2","https://gocrimson.com/sports/womens-water-polo/roster?view=2",
          "https://gocrimson.com/sports/mens-fencing/roster?view=2","https://gocrimson.com/sports/baseball/roster?view=2","https://gocrimson.com/sports/mens-basketball/roster?view=2","https://gocrimson.com/sports/cross-country/roster?view=2","https://gocrimson.com/sports/football/roster?view=2","https://gocrimson.com/sports/mens-golf/roster?view=2","https://gocrimson.com/sports/mens-ice-hockey/roster?view=2","https://gocrimson.com/sports/mens-lacrosse/roster?view=2","https://gocrimson.com/sports/mens-heavyweight-rowing/roster?view=2","https://gocrimson.com/sports/mens-lightweight-rowing/roster?view=2","https://gocrimson.com/sports/alpine-skiing/roster?view=2","https://gocrimson.com/sports/mens-soccer/roster?view=2","https://gocrimson.com/sports/mens-swimming-and-diving/roster?view=2","https://gocrimson.com/sports/mens-tennis/roster?view=2","https://gocrimson.com/sports/mens-track-and-field/roster?view=2","https://gocrimson.com/sports/mens-volleyball/roster?view=2","https://gocrimson.com/sports/mens-water-polo/roster?view=2","https://gocrimson.com/sports/wrestling/roster?view=2",
           "https://cornellbigred.com/sports/womens-basketball/roster/2025-26","https://cornellbigred.com/sports/womens-cross-country/roster/2024","https://cornellbigred.com/sports/equestrian/roster/2025-26","https://cornellbigred.com/sports/field-hockey/roster/2025","https://cornellbigred.com/sports/womens-gymnastics/roster/2024-25","https://cornellbigred.com/sports/womens-ice-hockey/roster/2025-26","https://cornellbigred.com/sports/womens-lacrosse/roster/2025","https://cornellbigred.com/sports/womens-rowing/roster/2024-25","https://cornellbigred.com/sports/womens-soccer/roster/2024","https://cornellbigred.com/sports/softball/roster/2025","https://cornellbigred.com/sports/womens-swimming-and-diving/roster/2025-26","https://cornellbigred.com/sports/womens-tennis/roster/2024-25","https://cornellbigred.com/sports/womens-track-and-field/roster/2024-25","https://cornellbigred.com/sports/womens-volleyball/roster/2024",
           "https://cornellbigred.com/sports/baseball/roster/2025","https://cornellbigred.com/sports/mens-basketball/roster/2025-26","https://cornellbigred.com/sports/mens-cross-country/roster/2024","https://cornellbigred.com/sports/football/roster/2024","https://cornellbigred.com/sports/mens-golf/roster/2024-25","https://cornellbigred.com/sports/mens-ice-hockey/roster/2024-25","https://cornellbigred.com/sports/mens-lacrosse/roster/2025","https://cornellbigred.com/sports/mens-soccer/roster/2025","https://cornellbigred.com/sports/mens-swimming-and-diving/roster/2025-26","https://cornellbigred.com/sports/mens-tennis/roster/2024-25","https://cornellbigred.com/sports/mens-track-and-field/roster/2024-25","https://cornellbigred.com/sports/wrestling/roster/2024-25",
           "https://yalebulldogs.com/sports/womens-basketball/roster/2024-25?view=2","https://yalebulldogs.com/sports/womens-cross-country/roster/2024?view=2","https://yalebulldogs.com/sports/womens-fencing/roster/2024-25?view=2","https://yalebulldogs.com/sports/field-hockey/roster/2024?view=2","https://yalebulldogs.com/sports/womens-golf/roster/2024-25?view=2","https://yalebulldogs.com/sports/womens-gymnastics/roster/2025?view=2","https://yalebulldogs.com/sports/womens-ice-hockey/roster/2024-25?view=2","https://yalebulldogs.com/sports/womens-lacrosse/roster/2025?view=2","https://yalebulldogs.com/sports/womens-crew/roster/2024-25?view=2","https://yalebulldogs.com/sports/womens-soccer/roster/2024?view=2","https://yalebulldogs.com/sports/softball/roster/2025?view=2","https://yalebulldogs.com/sports/womens-swimming-and-diving/roster/2024-25?view=2","https://yalebulldogs.com/sports/womens-tennis/roster/2024-25?view=2","https://yalebulldogs.com/sports/womens-track-and-field/roster/2024-25?view=2","https://yalebulldogs.com/sports/womens-volleyball/roster/2024?view=2",
           "https://yalebulldogs.com/sports/baseball/roster/2025?view=2","https://yalebulldogs.com/sports/mens-basketball/roster/2024-25?view=2","https://yalebulldogs.com/sports/mens-cross-country/roster/2024?view=2","https://yalebulldogs.com/sports/mens-fencing/roster/2024-25?view=2","https://yalebulldogs.com/sports/football/roster/2024?view=2","https://yalebulldogs.com/sports/mens-golf/roster/2024-25?view=2","https://yalebulldogs.com/sports/mens-ice-hockey/roster/2024-25?view=2","https://yalebulldogs.com/sports/mens-lacrosse/roster/2025?view=2","https://yalebulldogs.com/sports/mens-soccer/roster/2024?view=2","https://yalebulldogs.com/sports/mens-swimming-and-diving/roster/2024-25?view=2","https://yalebulldogs.com/sports/mens-tennis/roster/2024-25?view=2","https://yalebulldogs.com/sports/mens-track-and-field/roster/2024-25?view=2",
           "https://dartmouthsports.com/sports/womens-basketball/roster/2024-25?view=2","https://dartmouthsports.com/sports/womens-cross-country/roster/2024?view=2","https://dartmouthsports.com/sports/equestrian/roster/2024-25?view=2","https://dartmouthsports.com/sports/field-hockey/roster/2024?view=2","https://dartmouthsports.com/sports/womens-golf/roster/2024-25?view=2","https://dartmouthsports.com/sports/womens-ice-hockey/roster/2024-25?view=2","https://dartmouthsports.com/sports/womens-lacrosse/roster/2025?view=2","https://dartmouthsports.com/sports/womens-rowing/roster/2024-25?view=2","https://dartmouthsports.com/sports/alpine-skiing/roster/2024-25?view=2","https://dartmouthsports.com/sports/womens-soccer/roster/2024?view=2","https://dartmouthsports.com/sports/softball/roster/2025?view=2","https://dartmouthsports.com/sports/womens-swimming-and-diving/roster/2024-25?view=2","https://dartmouthsports.com/sports/womens-tennis/roster/2024-25?view=2","https://dartmouthsports.com/sports/womens-track-and-field/roster/2024-25?view=2","https://dartmouthsports.com/sports/womens-volleyball/roster/2025?view=2",
           "https://dartmouthsports.com/sports/baseball/roster/2025?view=2","https://dartmouthsports.com/sports/mens-basketball/roster/2024-25?view=2","https://dartmouthsports.com/sports/mens-cross-country/roster/2024?view=2","https://dartmouthsports.com/sports/football/roster/2024?view=2","https://dartmouthsports.com/sports/mens-golf/roster/2024-25?view=2","https://dartmouthsports.com/sports/mens-ice-hockey/roster/2024-25?view=2","https://dartmouthsports.com/sports/mens-lacrosse/roster/2025?view=2","https://dartmouthsports.com/sports/alpine-skiing/roster/2024-25?view=2","https://dartmouthsports.com/sports/mens-soccer/roster/2024?view=2","https://dartmouthsports.com/sports/mens-swimming-and-diving/roster/2024-25?view=2","https://dartmouthsports.com/sports/mens-tennis/roster/2024-25?view=2","https://dartmouthsports.com/sports/mens-track-and-field/roster/2024-25?view=2",
          "https://gocolumbialions.com/sports/womens-basketball/roster/2024-25?view=2","https://gocolumbialions.com/sports/cross-country/roster/2024?view=2","https://gocolumbialions.com/sports/fencing/roster/2024-25?view=2","https://gocolumbialions.com/sports/field-hockey/roster/2025?view=2","https://gocolumbialions.com/sports/womens-golf/roster/2024-25?view=2","https://gocolumbialions.com/sports/womens-lacrosse/roster/2025?view=2","https://gocolumbialions.com/sports/womens-rowing/roster/2024-25?view=2","https://gocolumbialions.com/sports/womens-soccer/roster/2024-25?view=2","https://gocolumbialions.com/sports/softball/roster/2025?view=2","https://gocolumbialions.com/sports/womens-swimming-and-diving/roster/2024-25?view=2","https://gocolumbialions.com/sports/womens-tennis/roster/2024-25?view=2","https://gocolumbialions.com/sports/track-and-field/roster/2024-25?view=2","https://gocolumbialions.com/sports/womens-volleyball/roster/2024?view=2",
          "https://gocolumbialions.com/sports/baseball/roster/2025?view=2","https://gocolumbialions.com/sports/mens-basketball/roster/2025-26?view=2","https://gocolumbialions.com/sports/cross-country/roster/2024?view=2","https://gocolumbialions.com/sports/fencing/roster/2024-25?view=2","https://gocolumbialions.com/sports/football/roster/2025?view=2","https://gocolumbialions.com/sports/mens-golf/roster/2024-25?view=2","https://gocolumbialions.com/sports/mens-soccer/roster/2024?view=2","https://gocolumbialions.com/sports/mens-swimming-and-diving/roster/2024-25?view=2","https://gocolumbialions.com/sports/mens-tennis/roster/2024-25?view=2","https://gocolumbialions.com/sports/track-and-field/roster/2024-25?view=2","https://gocolumbialions.com/sports/wrestling/roster/2024-25?view=2",
          "https://gopsusports.com/sports/womens-basketball/roster/season/2024-25?view=table","https://gopsusports.com/sports/cross-country/roster/season/2024?view=table","https://gopsusports.com/sports/fencing/roster/season/2024-25?view=table","https://gopsusports.com/sports/field-hockey/roster/season/2024?view=table","https://gopsusports.com/sports/womens-golf/roster/season/2024-25?view=table","https://gopsusports.com/sports/womens-gymnastics/roster/season/2025?view=table","https://gopsusports.com/sports/womens-ice-hockey/roster/season/2024-25?view=table","https://gopsusports.com/sports/womens-lacrosse/roster/season/2025?view=table","https://gopsusports.com/sports/womens-soccer/roster/season/2025?view=table","https://gopsusports.com/sports/softball/roster/season/2025?view=table","https://gopsusports.com/sports/womens-swimming-and-diving/roster/season/2024-25?view=table","https://gopsusports.com/sports/womens-tennis/roster/season/2024-25?view=table","https://gopsusports.com/sports/track-field/roster/season/2025?view=table","https://gopsusports.com/sports/womens-volleyball/roster/season/2024?view=table",
          "https://gopsusports.com/sports/baseball/roster/season/2025?view=table","https://gopsusports.com/sports/mens-basketball/roster/season/2024-25?view=table","https://gopsusports.com/sports/cross-country/roster/season/2024?view=table","https://gopsusports.com/sports/fencing/roster/season/2024-25?view=table","https://gopsusports.com/sports/football/roster/season/2025?view=table","https://gopsusports.com/sports/mens-golf/roster/season/2024-25?view=table","https://gopsusports.com/sports/mens-gymnastics/roster/season/2025?view=table","https://gopsusports.com/sports/mens-ice-hockey/roster/season/2024-25?view=table","https://gopsusports.com/sports/mens-lacrosse/roster/season/2025?view=table","https://gopsusports.com/sports/mens-soccer/roster/season/2024?view=table","https://gopsusports.com/sports/mens-swimming-and-diving/roster/season/2024-25?view=table","https://gopsusports.com/sports/mens-tennis/roster/season/2024-25?view=table","https://gopsusports.com/sports/track-field/roster/season/2025?view=table","https://gopsusports.com/sports/mens-volleyball/roster/season/2025?view=table","https://gopsusports.com/sports/wrestling/roster/season/2024-25?view=table"
          ),
  sport = c("Basketball","Fencing","Field Hockey","Golf","Ice Hockey", "Lacrosse","Heavyweight Rowing", "Lightweight Rowing", "Soccer", "Softball", "Swimming & Diving", "Tennis",  "Volleyball", "Water Polo",
            "Fencing","Baseball","Basketball", "Cross Country", "Football", "Golf", "Ice Hockey", "Lacrosse", "Heavyweight Rowing", "Lightweight Rowing", "Skiing", "Soccer", "Swimming & Diving", "Tennis", "Track & Field", "Volleyball", "Water Polo", "Wrestling",          
             "Basketball", "Cross Country", "Equestrian","Field Hockey", "Gymnastics", "Ice Hockey", "Lacrosse", "Rowing", "Soccer", "Softball", "Swimming & Diving", "Tennis", "Track & Field", "Volleyball",
             "Baseball","Basketball", "Cross Country",  "Football", "Golf", "Ice Hockey", "Lacrosse",  "Soccer", "Swimming & Diving", "Tennis", "Track & Field", "Wrestling",
             "Basketball", "Cross Country", "Fencing", "Field Hockey", "Golf", "Gymnastics", "Ice Hockey", "Lacrosse", "Rowing", "Soccer", "Softball", "Swimming & Diving", "Tennis", "Track & Field", "Volleyball",
             "Baseball", "Basketball", "Cross Country", "Fencing", "Football", "Golf", "Ice Hockey", "Lacrosse", "Soccer", "Swimming & Diving", "Tennis", "Track & Field",
             "Basketball", "Cross Country", "Equestrian", "Field Hockey", "Golf", "Ice Hockey", "Lacrosse", "Rowing", "Skiing", "Soccer", "Softball", "Swimming & Diving", "Tennis", "Track & Field", "Volleyball",
             "Baseball", "Basketball", "Cross Country", "Football", "Golf", "Ice Hockey","Lacrosse", "Skiing", "Soccer", "Swimming & Diving", "Tennis", "Track & Field",
            "Basketball", "Cross Country", "Fencing", "Field Hockey", "Golf", "Lacrosse", "Rowing", "Soccer", "Softball", "Swimming & Diving", "Tennis", "Track & Field", "Volleyball",
            "Baseball", "Basketball", "Cross Country", "Fencing", "Football", "Golf", "Soccer", "Swimming & Diving", "Tennis", "Track & Field", "Wrestling",
            "Basketball", "Cross Country", "Fencing", "Field Hockey", "Golf", "Gymnastics", "Ice Hockey", "Lacrosse", "Soccer", "Softball", "Swimming & Diving", "Tennis", "Track & Field", "Volleyball",
            "Baseball", "Basketball", "Cross Country", "Fencing","Football", "Golf", "Gymnastics", "Ice Hockey", "Lacrosse", "Soccer", "Swimming & Diving", "Tennis", "Track & Field", "Volleyball", "Wrestling"
            ),
  sex = c("Women","Women", "Women", "Women", "Women", "Women", "Women","Women", "Women", "Women", "Women", "Women", "Women", "Women","Women",
          "Men","Men","Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men",
           "Women","Women","Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", 
           "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men",
          "Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women",
           "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men",
           "Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women",
           "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men",
          "Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women",
          "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men",
          "Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women",
          "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men","Men", "Men", "Men", "Men", "Men", "Men", "Men"
  ),
  school = c("Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University",
             "Harvard University","Harvard University","Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University", "Harvard University",            
             "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University",
              "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University", "Cornell University",
               "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University",
              "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University", "Yale University",
              "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College",
              "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College","Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College", "Dartmouth College",
              "Columbia University", "Columbia University", "Columbia University", "Columbia University", "Columbia University", "Columbia University", "Columbia University", "Columbia University", "Columbia University", "Columbia University", "Columbia University", "Columbia University", "Columbia University",
             "Columbia University", "Columbia University", "Columbia University", "Columbia University", "Columbia University", "Columbia University", "Columbia University", "Columbia University", "Columbia University", "Columbia University", "Columbia University",
             "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University",
             "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University", "Penn State University"
             )
)

#Count number of rosters
URL_Length <- nrow(School_URLs)

#Create blank DF for roster data to be added to
Final_Roster_Grid_Schools <- data.frame()

while(URL_Length > 0){
  
  #Pull first row from current roster
  url <-School_URLs[[1]][URL_Length]
  
  #Scan row
  page <- read_html(url)
  
  
  #TEST
  tables <- page %>% html_table(fill = TRUE)
  if (School_URLs[[4]][URL_Length] == "Penn State University"){roster_table <- tables[[1]]}else{roster_table <- tables[[3]]}
  #roster_table <- tables[[3]]
  roster_table <- roster_table %>%
    mutate(sport =School_URLs[[2]][URL_Length],
           sex = School_URLs[[3]][URL_Length],
           school = School_URLs[[4]][URL_Length])
  
  if ("No." %in% names(roster_table)) {
    roster_table$`No.` <- as.character(roster_table$`No.`)
  }
  if ("# Jersey Number" %in% names(roster_table)) {
    roster_table$`# Jersey Number` <- as.character(roster_table$`# Jersey Number`)
  }
  
Final_Roster_Grid_Schools <- bind_rows(Final_Roster_Grid_Schools,roster_table)
  
  #Update URL loop
  URL_Length <- URL_Length - 1
}

#Cleanup
Final_Roster_Grid_Schools_Final <- Final_Roster_Grid_Schools%>%
  select(Name, Hometown, sport, sex, school, `Full Name`, `Hometown / High School`, `Hometown / Previous School`, `Hometown / Previous School(s)`, `Hometown / High School / Club`,`Hometown / Previous Team`,`Hometown/High School`, `Hometown / Last School`)%>%
  mutate(`Full Name` = case_when(!is.na(`Full Name`) ~ `Full Name`, T ~ Name),
         `Hometown/HighSchool` = case_when(!is.na(`Hometown / High School`) ~ `Hometown / High School`,
                                         !is.na(`Hometown / Previous School`) ~ `Hometown / Previous School`,
                                         !is.na(`Hometown / Previous School(s)`) ~ `Hometown / Previous School(s)`,
                                         !is.na(`Hometown / High School / Club`) ~ `Hometown / High School / Club`,
                                         !is.na(`Hometown / Previous Team`) ~ `Hometown / Previous Team`,
                                         !is.na(`Hometown/High School`) ~ `Hometown/High School`,
                                         T ~ `Hometown / Last School`))%>%
  select(`Full Name`, Hometown,`Hometown/HighSchool`, sport, sex, school)%>%
  separate(`Hometown/HighSchool`, into = c("Hometown2", "High_School"), sep = " / ")%>%
  mutate(Hometown = case_when(!is.na(Hometown) ~ str_extract(Hometown, "(?<=,\\s).*"),
                              !is.na(Hometown2) ~ str_extract(Hometown2, "(?<=,\\s).*"),
                              T ~ NA),
         Citizen = case_when(Hometown %in% c("Mich.","Ariz.","Pa.","Idaho","Utah","N.J.","Minn.","Ind.","Calif.","Mo.","Va.","Wis.","N.Y.","Missouri","Illinois","Md.","Ill.","Ky.","Texas","Conn.","R.I.","W.Va.","Ohio","Mass.","Fla.","Wisc.","Ore.","Colo.","Ga.","Del.","Wash.","D.C.","N.C.","Ct.","Ala.","Iowa","Ark.","Tenn.","Miss.","N.H.","Neb.","PA","Maine","Pa","Va","PA.","La.","Hawaii","Mont.","Calif. ","Kan.","Okla.","W. Va.","La","Nev.","Florida","Tenn. /","Fla. /","NY","Wisconsin","Penn.","Ga. ","Col.","Tenn. ","MA","N.M.","Indiana","NC","Wyo.","New York","Vt.","Alaska","S.C.","Ida.","Tex.","S.D.","Kentucky","VT","El Salvador","Fla","Ari.","Ga","N.Y","Beach, Va.","Me.","Ia.","OH","Md. ","N.J","Calif. /","Mass. /","N.Y. /","Va.  /","Conn. /","Mary.","Alabama","MI","Georgia","CA","TX","Kansas","WVa.","Ind. /","Dela. /","Tex. /","Ga. /","N.C. /","Va. /","Texas /","Colo. /","Ohio /","Wash. /","Vt. /","NV","N.J. /","Col","Ohi.","N.D.","Calif","Vir.","Miss,","IL","FL","CT","DE","OK","VA","MD","Oh.","Or.","IA","Tennessee","Colorado","NJ.","CT.","North Carolina","LA","NY, U.S.","NJ","WA","MN") ~ 1, 
                             Hometown %in% c("Nigeria","NSW, Australia","Quebec, Canada","Canada","Australia","Sweden","United Kingdom","Japan","South Africa","Wisc.","Brazil","Chile","Ireland","Bermuda","RM (Chile)","N.Z.","Croatia","Scotland","Entre Ríos, Argentina","Kenya","England","Ontario","Alberta","Russia","Auckland, New Zealand","Silesian","Switzerland","Finland","Germany","Bolivia","Hong Kong","Kuwait","China","Egypt", "Alberta, Canada", "Spain","Romania","Bulgaria", "Slovakia", "Estonia", "Nova Scotia","Austria","Latvia","Thailand","Turkiye","France","The Netherlands", "South Korea", "Belgium","Republic of Korea","Turkey","British Columbia","Czech Republic","Ontario, Canada","Serbia", "Saskatchewan", "British Columbia, Canada","Philippines","Algeria","B.C.","New South Wales","Greece","Portugal","Denmark", "Jamaica", "Peru /", "Israel", "Quebec", "Athens, Greece","Cyprus","Norway","Iceland","Dominican Republic","Ghana","Costa Rica", "Alta.","YT","Ont.", "Hungary","Manitoba", "Mozambique", "Northern Ireland", "Mexico", "New Zealand", "Malaysia", "Netherlands","Argentina","Uruguay","U.K.", "Greece /", "Sweden /", "Congo", "Croatia /", "Slovenia","Paraguay","Denmark /","Sask.","Ontario /","Que.", "Ont. /", "Alb. /", "Surrey, England","Norfolk, England","London, England","Essex, England","Western Cape, South Africa","Lithuania", "U.S.V.I.","Czechia","Zimbabwe","India", "Italy", "Québec", "Puerto Rico","American Samoa", "Suriname","Balearic Islands, Spain","Gauteng, South Africa", "Victoria, Australia", "Indonesia","Colombia","Aus.", "Peru", "ON","QC","Prince Edward Island","Suffolk, England","Kent (U.K.)") ~ 0,
                                             T ~ as.numeric(NA)))%>%
  select(`Full Name`, sport, sex, school, Citizen)
                             
#Non-Grid schools scrape####
Non_Grid_School_URLs <- tibble(
  url = c("https://goprincetontigers.com/sports/womens-basketball/roster/2022-23","https://goprincetontigers.com/sports/womens-cross-country/roster/2022","https://goprincetontigers.com/sports/womens-fencing/roster/2022-23","https://goprincetontigers.com/sports/field-hockey/roster/2022","https://goprincetontigers.com/sports/womens-golf/roster/2022-23","https://goprincetontigers.com/sports/womens-ice-hockey/roster/2022-23","https://goprincetontigers.com/sports/womens-lacrosse/roster/2023","https://goprincetontigers.com/sports/womens-rowing/roster/2023","https://goprincetontigers.com/sports/womens-soccer/roster/2022","https://goprincetontigers.com/sports/softball/roster/2023","https://goprincetontigers.com/sports/womens-swimming-and-diving/roster/2023-24","https://goprincetontigers.com/sports/womens-tennis/roster/2022-23","https://goprincetontigers.com/sports/womens-track-and-field/roster/2022-23","https://goprincetontigers.com/sports/womens-volleyball/roster/2023", "https://goprincetontigers.com/sports/womens-water-polo/roster/2023",
          "https://goprincetontigers.com/sports/baseball/roster/2023","https://goprincetontigers.com/sports/mens-basketball/roster/2022-23","https://goprincetontigers.com/sports/mens-cross-country/roster/2022","https://goprincetontigers.com/sports/mens-fencing/roster/2022-23","https://goprincetontigers.com/sports/football/roster/2022","https://goprincetontigers.com/sports/mens-golf/roster/2022-23","https://goprincetontigers.com/sports/mens-ice-hockey/roster/2022-23","https://goprincetontigers.com/sports/mens-lacrosse/roster/2023","https://goprincetontigers.com/sports/mens-soccer/roster/2022","https://goprincetontigers.com/sports/mens-swimming-and-diving/roster/2023-24","https://goprincetontigers.com/sports/mens-tennis/roster/2022-23","https://goprincetontigers.com/sports/mens-track-and-field/roster/2023-24","https://goprincetontigers.com/sports/mens-volleyball/roster/2023","https://goprincetontigers.com/sports/mens-water-polo/roster/2023","https://goprincetontigers.com/sports/wrestling/roster/2022-23",
         "https://brownbears.com/sports/womens-basketball/roster/2024-25","https://brownbears.com/sports/womens-crew/roster/2024-25","https://brownbears.com/sports/womens-cross-country/roster/2024","https://brownbears.com/sports/equestrian/roster/2024-25","https://brownbears.com/sports/fencing/roster/2024-25","https://brownbears.com/sports/womens-gymnastics/roster/2025","https://brownbears.com/sports/womens-ice-hockey/roster/2024-25","https://brownbears.com/sports/womens-lacrosse/roster/2025","https://brownbears.com/sports/womens-soccer/roster/2025","https://brownbears.com/sports/softball/roster/2025","https://brownbears.com/sports/womens-swimming-and-diving/roster/2024-25","https://brownbears.com/sports/womens-tennis/roster/2024-25","https://brownbears.com/sports/womens-track-and-field/roster/2024-25","https://brownbears.com/sports/womens-volleyball/roster/2024","https://brownbears.com/sports/womens-water-polo/roster/2025",
         "https://brownbears.com/sports/baseball/roster/2026","https://brownbears.com/sports/mens-basketball/roster/2024-25","https://brownbears.com/sports/mens-cross-country/roster/2024","https://brownbears.com/sports/football/roster/2024","https://brownbears.com/sports/mens-ice-hockey/roster/2024-25","https://brownbears.com/sports/mens-lacrosse/roster/2025","https://brownbears.com/sports/mens-soccer/roster/2024","https://brownbears.com/sports/mens-swimming-and-diving/roster/2024-25","https://brownbears.com/sports/mens-tennis/roster/2024-25","https://brownbears.com/sports/mens-track-and-field/roster/2024-25","https://brownbears.com/sports/mens-water-polo/roster/2024","https://brownbears.com/sports/wrestling/roster/2024-25"
          ),
  sport = c("Basketball", "Cross Country", "Fencing", "Field Hockey", "Golf", "Ice Hockey", "Lacrosse", "Rowing", "Soccer", "Softball", "Swimming & Diving", "Tennis", "Track & Field", "Volleyball", "Water Polo",
            "Baseball", "Basketball", "Cross Country", "Fencing", "Football", "Golf", "Ice Hockey", "Lacrosse", "Soccer", "Swimming & Diving", "Tennis", "Track & Field", "Volleyball", "Water Polo", "Wrestling",
            "Basketball", "Rowing", "Cross Country", "Equestrian", "Fencing", "Gymnastics", "Ice Hockey", "Lacrosse", "Soccer", "Softball", "Swimming & Diving", "Tennis", "Track & Field", "Volleyball", "Water Polo",
            "Baseball", "Basketball", "Cross Country", "Football", "Ice Hockey", "Lacrosse", "Soccer", "Swimming & Diving", "Tennis", "Track & Field", "Water Polo", "Wrestling"
                     ),
  sex = c("Women","Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women", "Women",
          "Men","Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men",
           "Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women","Women",
           "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men", "Men"
          
          ),
  school = c("Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University",
             "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University", "Princeton University",
             "Brown University", "Brown University", "Brown University", "Brown University", "Brown University", "Brown University", "Brown University", "Brown University", "Brown University", "Brown University", "Brown University", "Brown University", "Brown University", "Brown University", "Brown University",
             "Brown University","Brown University","Brown University","Brown University","Brown University","Brown University","Brown University","Brown University","Brown University","Brown University","Brown University","Brown University"
            )
)

#Count number of rosters
URL_Length <- nrow(Non_Grid_School_URLs)

#Create blank DF for roster data to be added to
Final_Roster_Non_Grid_Schools <- data.frame()

while(URL_Length > 0){
  
  #Pull first row from current roster
  url <-Non_Grid_School_URLs[[1]][URL_Length]
  
  #Scan row
  page <- read_html(url)
  
  # Step 2: extract all scripts
  scripts <- page %>% html_elements("script") %>% html_text()
  
  #Extract the __INITIAL_STATE__ JSON string (single quoted)
  raw_data <- case_when(Non_Grid_School_URLs[[4]][URL_Length] %in% c("Princeton University","Brown University") ~ scripts[str_detect(scripts, "window.__INITIAL_STATE__")][1],
                        Non_Grid_School_URLs[[4]][URL_Length] %in% c("Dartmouth College", "Yale University") ~ scripts[str_detect(scripts, "\\{\"@type\"")][1])
  
  #"firstName\":\"Sabrina\",\"lastName\":\"Shipley\",\"hometown\":\"Miami, Fla.\",\"highSchool\":\"Ransom Everglades\",\"previousSchool\
  # See #6?
  
  
  
  #Pull each line that matches athlete's format (should be ignoring coaches/staff)
  Roster_Length <- if (Non_Grid_School_URLs[[4]][URL_Length] %in% c("Princeton University","Brown University")) {
    Roster_Length <- str_extract_all(raw_data, '"firstName":"[^"]+","lastName":"[^"]+","hometown":"[^"]+","highSchool":"[^"]+","previousSchool":')[[1]]
  } else if (Non_Grid_School_URLs[[4]][URL_Length] %in% c("Dartmouth College", "Yale University") ) {
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
  Final_Roster_Non_Grid_Schools_Temp <- data.frame(
    FirstName = unlist(First_Name_List),
    LastName = unlist(Last_Name_List),
    Whole_Name = unlist(Whole_Name_List),
    HighSchool = unlist(High_School_List),
    HomeTown = unlist(Home_Town_List),
    sport = Non_Grid_School_URLs$sport[URL_Length],
    sex = Non_Grid_School_URLs$sex[URL_Length],
    school = Non_Grid_School_URLs$school[URL_Length],
    stringsAsFactors = FALSE
  )
  
  #Create/append to current DF of all athletes
  Final_Roster_Non_Grid_Schools <- bind_rows(Final_Roster_Non_Grid_Schools, Final_Roster_Non_Grid_Schools_Temp)
  
  #Update URL loop
  URL_Length <- URL_Length - 1
}

#Generating lots of duplicates. Clean up
Final_Roster_Non_Grid_Schools_Final <- Final_Roster_Non_Grid_Schools%>%
  distinct()%>%
  mutate(Hometown = case_when(!is.na(HomeTown) ~ str_extract(HomeTown, "(?<=,\\s).*"), T ~ NA),
         Citizen = case_when(Hometown %in% c("Mass.","N.Y.","N.J.","PA","Pa.","Ill.","Ct.","R.I.","OH","Fla.","Wisc.","Ohio","Calif","Calif.","Wash.","Conn.","N.C.","Vt.","Ga.","Texas","N.H.","N.M.","Mo.","Maine","S.C.","Md.","Va.","Ore.","Wis.","Colo.","Mich.","Del.","Iowa","Tenn.","Ariz.","Idaho","Minn.","W.Va.","Nev.","Ind.","Kansas","D.C.","Fl.","P.A.","CO","MA","C.O.","Alaska","Maryland","Hawaii","Calif. ","Utah","Az.","N.Z.","Mont.","C.T.","Ark.","Penn.","Kan.","N.Y,","La.","Nebraska","Denmark","Brazil","Ireland","Que.","N.B.","Miss.","Ky.","Okla.","Neb.","Ala.","NJ") ~ 1,
                             Hometown %in% c("Côte D\'Ivoire","Croatia","Heves, Hungary","England","Puerto Rico","Hungary","Greece","P.R.","Ghana","Vietnam","Taiwan","Mexico","Germany","Fyn, Denmark", "Alta.","Ont.","Russia","B.C.","Guinea","New Zealand","Poland","Istanbul","British Columbia, Canada","Côte D'Ivoire","Ontario","Nigeria","British Columbia","Sask.","Qué.","Australia","Romania","UAE","South Korea","Alberta, Canada","China","Turkey","United Kingdom","South Africa ","Canada","Czech Republic","Scotland","Alb.","Serbia","South Africa","Italy","U.K.","Quebec","Hong Kong","Finland","Alberta","South Australia","Netherlands","Israel","Ukraine","Sweden","Zimbabwe","Great Britain","Oxfordshire, England","Eng.","New South Wales, Australia","N.L.","Kent, England","The Netherlands","Limburg, Belgium","West Sussex, England","Devon, England","Victoria, Australia") ~ 0,
                             T ~ as.numeric(NA)),
         `Full Name` = str_c(FirstName, LastName, sep = " "))%>%
  select(`Full Name`, sport, sex, school, Citizen)

#Combine grid/non-grid schools
Final_Roster_All_Schools <- bind_rows(Final_Roster_Grid_Schools_Final, Final_Roster_Non_Grid_Schools_Final)%>%
  filter(!(sport == "Heavyweight Rowing" & sex == "Men"))%>%
  mutate(sport = case_when(sport %in% c('Heavyweight Rowing', 'Lightweight Rowing') ~ "Rowing", T ~ sport))


#Graph#####
#Schools Overall####
School_Totals <- Final_Roster_All_Schools%>%
  group_by(school)%>%
  summarise(Citizens = sum(Citizen,na.rm=T),NonCitizens = sum(Citizen==0,na.rm=T))

# Reshape data
School_Totals_long <- School_Totals %>%
  pivot_longer(cols = c(Citizens, NonCitizens),
               names_to = "Citizenship",
               values_to = "Count") %>%
  mutate(
    # Explicitly set Citizenship factor levels: Citizens first, NonCitizens second
    Citizenship = factor(Citizenship, levels = c("Citizens", "NonCitizens")),
    school = factor(school, levels = unique(school)),
    fill_key = paste0(school, "_", Citizenship)
  )

# Verify factor levels of Citizenship (must be Citizens, then NonCitizens)
print(levels(School_Totals_long$Citizenship))
# Verify levels of fill_key - must be ordered accordingly next

# Step 4: Define base colors for each school
base_colors <- c(
  "Brown University" = "#4E3629",     # Brown
  "Columbia University" = "#003865",  # Steel Blue
  "Cornell University" = "#B31B1B",   # Firebrick
  "Dartmouth College" = "#00693E",    # Dark Green
  "Harvard University" = "#A51C30",   # Crimson
  "Penn State University" = "#001E44",# PSU Blue
  "Princeton University" = "#F58025", # Dark Orange
  "Yale University" = "#00356B"       # Yale Blue
)

# Set factor levels of fill_key explicitly: all Citizens first, then all NonCitizens
fill_levels <- c(
  paste0(names(base_colors), "_Citizens"),
  paste0(names(base_colors), "_NonCitizens")
)

School_Totals_long$fill_key <- factor(School_Totals_long$fill_key, levels = fill_levels)

# Verify factor levels for fill_key:
print(levels(School_Totals_long$fill_key))

# Create colors vector with faded NonCitizens
fill_colors <- c(
  setNames(base_colors, paste0(names(base_colors), "_Citizens")),
  setNames(sapply(base_colors, alpha, 0.4), paste0(names(base_colors), "_NonCitizens"))
)

# Plot
ggplot(School_Totals_long, aes(x = school, y = Count, fill = fill_key)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), color="black")+
  geom_text(aes(label = Count),
            position = position_stack(vjust = 0.5, reverse=T), # centers text inside each stacked segment
            size = 6, color = "white") +           # adjust size and color as needed
  scale_fill_manual(values = fill_colors) +
  labs(title = "Citizenship Breakdown by School",
       x = "School", y = "Number of Students") +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Number of Student Athletes",
    title = "Number of Student Athletes by School/Citizenship Status",
    legend = NULL
  ) +
  scale_y_continuous(expand = c(0, 0))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5))

#Schools by Sport####
Sports_Totals <- Final_Roster_All_Schools%>%
  group_by(school,sport)%>%
  summarise(Citizens = sum(Citizen,na.rm=T),NonCitizens = sum(Citizen==0,na.rm=T))%>%
  mutate(Percent = case_when(NonCitizens == 0 ~ 0, is.na(NonCitizens) ~ 0,T ~NonCitizens/(Citizens+NonCitizens)))


# Reshape
Sports_Totals_long <- Sports_Totals %>%
  pivot_longer(cols = c(Citizens, NonCitizens),
               names_to = "Citizenship",
               values_to = "Count") %>%
  mutate(
    Citizenship = factor(Citizenship, levels = c("Citizens", "NonCitizens")),
    school = factor(school, levels = unique(school)),
    fill_key = paste0(school, "_", Citizenship)
  )

# Define your colors again, same as before

base_colors <- c(
  "Brown University" = "#4E3629",     # Brown
  "Columbia University" = "#003865",  # Steel Blue
  "Cornell University" = "#B31B1B",   # Firebrick
  "Dartmouth College" = "#00693E",    # Dark Green
  "Harvard University" = "#A51C30",   # Crimson
  "Penn State University" = "#001E44",# PSU Blue
  "Princeton University" = "#F58025", # Dark Orange
  "Yale University" = "#00356B"       # Yale Blue
)

fill_levels <- c(
  paste0(names(base_colors), "_Citizens"),
  paste0(names(base_colors), "_NonCitizens")
)

Sports_Totals_long$fill_key <- factor(Sports_Totals_long$fill_key, levels = fill_levels)

fill_colors <- c(
  setNames(base_colors, paste0(names(base_colors), "_Citizens")),
  setNames(sapply(base_colors, alpha, 0.4), paste0(names(base_colors), "_NonCitizens"))
)

# Plot with facet_wrap by sport

ggplot(Sports_Totals_long, aes(x = school, y = Count, fill = fill_key)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), color="black") +
  scale_fill_manual(values = fill_colors) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = NULL,
    y = "Number of Student Athletes",
    title = "Number of Student Athletes by School/Citizenship Status",
    legend = NULL
  ) +
  theme_minimal()+
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  ) +
  facet_wrap(~ sport)


#Hockey####
Arenas_Final <- Arenas%>%
  left_join(School_Logos,by=c("unitid" = "unitid"))


Arenas_Final %>%
  mutate(abbreviation = fct_reorder(abbreviation, Capacity, .desc = TRUE)) %>%
  ggplot(aes(x = abbreviation, y = Capacity, fill = color)) +
  geom_bar(stat = "identity", color="black") +
  #coord_flip() +
  scale_fill_identity() +  
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Hockey Arena Capacity by School",
    x = NULL,
    y = "Capacity"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
       # axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5))



#To commit changes to GitHub:####
#Stage, commit, comment, push
#Always run in terminal to push update: git push origin main



