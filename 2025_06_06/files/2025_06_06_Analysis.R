rm(list = ls())
options(scipen = 999)
setwd("/Volumes/GoogleDrive-103007369919315814324/untitled folder/My Drive/NILnomics/Newsletters/2025_06_06") #Update this each week

#Load NILnomics-wide functions
source("/Volumes/GoogleDrive-103007369919315814324/untitled folder/My Drive/NILnomics/MFRS Cleaner/Utilities.R")

#Load Libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(forcats)
library(patchwork)

#Load Kaggle data####
NILnomics_school_logo_and_other_data <- "nilnomics/ncaa-school-logo-and-other-data"
NILnomics_arenas <- "nilnomics/College-Sports-Arenas"
NILnomics_ncaa_EADA_data <- "nilnomics/ncaa-eada-data"
NILnomics_college_bb_sb_playoff <- "nilnomics/college-baseballsoftball-playoff-data"

#Use Kaggle CLI commands to download individual dataset
system(paste("kaggle datasets download -d", NILnomics_school_logo_and_other_data, "--force"), intern = TRUE)
unzip("ncaa-school-logo-and-other-data.zip")
School_Logos <- read_excel("School_Logos.xlsx")

#Use Kaggle CLI commands to download individual dataset
system(paste("kaggle datasets download -d", NILnomics_arenas, "--force"), intern = TRUE)
unzip("College-Sports-Arenas.zip")
Arenas <- read_excel("Arenas.xlsx")

#Use Kaggle CLI commands to download individual dataset
system(paste("kaggle datasets download -d", NILnomics_ncaa_EADA_data, "--force"), intern = TRUE)
unzip("ncaa-eada-data.zip")
EADA_DF <- read_excel("Combined_EADA.xlsx")

#Use Kaggle CLI commands to download individual dataset
system(paste("kaggle datasets download -d", NILnomics_college_bb_sb_playoff, "--force"), intern = TRUE)
unzip("college-baseballsoftball-playoff-data.zip")
CollegeWS_SB_Data <- read_excel("CollegeWS_SB_Data.xlsx")

label_gap <- 0.1  # small space between text and bar

#BB/SB budget %
EADA_DF02 <- EADA_DF%>%
  filter(Year == '2024')%>%
  select(unitid,TOTAL_EXPENSE_ALL_Baseball, TOTAL_EXPENSE_ALL_Softball, GRND_TOTAL_REVENUE)%>%
  inner_join(CollegeWS_SB_Data, by = c('unitid' = 'unitid'))%>%
  mutate(Baseball_Percent = case_when(MWS == 1 ~ TOTAL_EXPENSE_ALL_Baseball/GRND_TOTAL_REVENUE, T ~ 0),
         Softball_Percent = case_when(WCWS == 1 ~ TOTAL_EXPENSE_ALL_Softball/GRND_TOTAL_REVENUE, T ~ 0))%>%
  mutate(Name = fct_reorder(Name, Baseball_Percent, .desc = FALSE))

# Top plot: Baseball (MWS)
p1 <- EADA_DF02 %>%
  ggplot(aes(x = Baseball_Percent, y = Name)) +
  geom_col(fill = "#1f77b4", size = 0.1) +
  geom_text(aes(label = scales::percent(Baseball_Percent, accuracy = .1)),
           hjust = -0.1, color = "white", size = 3.2) +
  #scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_x_reverse(expand = expansion(mult = c(0.1, 0)), labels = label_percent(scale = 100.1)) +
  
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_blank()
  )

# Bottom plot: Softball (WCWS)
p2 <- EADA_DF02 %>%
  ggplot(aes(x = Softball_Percent, y = Name)) +
  geom_col(fill = "#ff7f0e", size = 0.1) +
  geom_text(aes(label = scales::percent(Softball_Percent, accuracy = .1)),
            hjust = 1.1, color = "white", size = 3.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)), labels = label_percent(scale = 100.1)) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_blank()
  )

# Center labels
p_labels <- EADA_DF02 %>%
  ggplot(aes(y = Name, x = 1)) +
  geom_text(aes(label = Name), size = 3.5, hjust = 0.5) +
  scale_x_continuous(limits = c(0, 2)) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# Combine with patchwork
final_plot <- p1 + p_labels + p2 + 
  plot_layout(ncol = 3, widths = c(1, 0.4, 1))

  
#Hockey Chart
Arenas_Hockey_DF <- Arenas%>%
  left_join(School_Logos, by = c('unitid' = 'unitid'))

custom_colors <- c(
  "#636363",
  "#e7298a",  # Pink (Set1)
  #"#66a61e",  # Green (Dark2)
 # "#a6761d",  # Brown (Dark2)
  "#e31a1c",  # Red (Set1)
  "#6a3d9a",  # Dark purple (Set1)
  "#1f78b4",  # Blue (Set1)
  "#ff7f00",  # Orange (Set1)
  "#b15928"   # Dark brown (Set1)
)

# Step 1: Sort abbreviations by Conference and Capacity
Arenas_Hockey_DF <- Arenas_Hockey_DF %>%
  group_by(`Men's Conference`) %>%
  mutate(avg_capacity = mean(Capacity)) %>%
  ungroup() %>%
  arrange(desc(avg_capacity), desc(Capacity)) %>%
  mutate(abbreviation = factor(abbreviation, levels = abbreviation))

# Step 2: Create the plot
ggplot(Arenas_Hockey_DF, aes(x = abbreviation, y = Capacity, fill = `Men's Conference`)) +
  geom_col() +
  geom_text(aes(label = Capacity),angle=90, vjust = 0.5, size = 3, color = "white", position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, title="NCAA Men's Division I Ice Hockey Arena Capacities by Conference/Institution") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = custom_colors)+
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.grid = element_blank()
    
  )


#To commit changes to GitHub:####
#Stage, commit, comment, push
#Always run in terminal to push update: git push origin main