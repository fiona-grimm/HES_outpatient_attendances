#===============================================================================
# Purpose: Visualisation of NHS Outpatient Appointment data
# Author: Fiona Grimm
# Date: 07/2019
#===============================================================================

# 1. Import packages ----

library(tidyverse)
library(readxl)
library(plotly)
library(htmltools)

# Source plotly credentials 
source('plotly.R')

# 2. Downloading HES outpatient data from NHS Digital ----

filename <- "hosp-epis-stat-outp-rep-tabs-2017-18-tab.xlsx"
url <- "https://files.digital.nhs.uk/0D/0C3CF4/hosp-epis-stat-outp-rep-tabs-2017-18-tab.xlsx"


download.file(url, destfile = filename, mode = "wb")

# 3. Colours for graphs  ----

# Colours
my_dark_grey <- '#524c48'
my_light_grey <- '#e2dfd8'
my_lighter_grey <- '#eeede8'
my_blue <- '#005078'
my_lighter_blue <- '#AAD4E6'
my_red <- '#dd0031'
my_light_red <- '#F2A1A3'

# 4. Importing, wrangle and plot attendance types  ----

attendance_counts <- read_xlsx(path = filename, sheet = 'Summary Report 3', skip = 3, n_max = 11) %>% 
  select(1:7) 

attendance_counts_long <- attendance_counts %>% 
  # Combine hospital and patient cancellations
  mutate(Cancelled = `Patient cancellations` + `Hospital cancellations`) %>% 
  rename(Missed = `Did not attends (DNAs)`, Attended = Attendances) %>% 
  select(-`Patient cancellations`, -`Hospital cancellations`) %>% 
  gather(-Year, key = 'Attendance type', value = 'Count') %>% 
  group_by(Year) %>% 
  mutate(pct = 100*(Count / Count[`Attendance type` == 'Total']),
         Attendance_type = fct_relevel(`Attendance type`, c('Unknown', # first factor level ends up at the top
                                                            'Missed',
                                                            'Cancelled',
                                                            'Attended'))) %>% 
  filter(`Attendance type` != 'Total')


# text argument defines custom tooltip to be displayed when hovering over bars
attendance_plot <- attendance_counts_long %>% 
  ggplot(aes(x = Year, y = Count/1000000, group = Attendance_type, fill = Attendance_type, text = paste(Attendance_type, '<br>Year:', Year, '<br>Appointments:', Count,  '<br>Percent:', round(pct,1), '%'))) +
  geom_bar(stat = 'identity', position = position_stack()) +
  theme(axis.line = element_blank(),
        axis.text = element_text(colour = my_dark_grey),
        axis.text.x = element_text(angle = -45, vjust = -2, hjust = 0.7, colour = my_dark_grey),
        axis.text.y = element_text(colour = my_dark_grey),
        axis.ticks = element_blank(),
        axis.title = element_text(colour = my_dark_grey),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour = my_lighter_grey),
        plot.caption = element_text(colour = my_lighter_blue, hjust = 0),
        plot.title = element_text(colour = my_blue),
        plot.subtitle = element_text(colour = my_blue),
        legend.background = element_rect(fill=NA),
        legend.justification= c(1,0),
        legend.text = element_text(colour = my_dark_grey), 
        legend.title = element_blank(),
        legend.position = 'top',
        legend.spacing.x = unit(5, 'pt'),
        legend.spacing.y = unit(15, 'pt')) + 
  ylab('Number of appointments [millions]') +
  scale_fill_manual(values = rev(c(my_blue, my_lighter_blue, my_red, my_light_grey))) +
  guides(fill = guide_legend(ncol = 3, 
                             byrow = FALSE, 
                             label.hjust = 0,
                             keyheight = 0.1,
                             reverse = TRUE))

# Save locally
ggsave('OP_attend_count.png', attendance_plot, device='png',  width = 7, height = 5)

# Convert to interactive plot
attendance_plot_inter <- ggplotly(attendance_plot, tooltip = c('text')) %>% 
  config(displayModeBar = F) %>% 
  layout(xaxis=list(fixedrange=TRUE)) %>% 
  layout(yaxis=list(fixedrange=TRUE))

# Publish graph
api_create(attendance_plot_inter, filename = "HES_outpatient_attendance")

# 4. Importing, wrangle and plot attendance by gender and age  ----

attendance_sex_age <- read_xlsx(path = filename, sheet = 'Summary Report 8', skip = 3, n_max = 19) %>% 
  select(1:4)

age_bins <- str_c(c("0 - 4", 
              "5 - 9", "10 - 14",
              "15 - 19", "20 - 24",
              "25 - 29", "30 - 34",
              "35 - 39", "40 - 44",
              "45 - 49", "50 - 54",
              "55 - 59", "60 - 64",
              "65 - 69", "70 - 74",
              "75 - 79", "80 - 84",
              "85 - 89", "90+"), ' years')

attendance_sex_age_long <- attendance_sex_age %>% 
  # Combine hospital and patient cancellations
  rename(Maternity = `Female (maternity)`) %>% 
  gather(-`Age (yrs)`, key = 'Sex', value = 'Count') %>% 
  mutate(Maternity = ifelse(Sex == 'Maternity', 'maternity', 'other'),
         Sex = ifelse(Maternity == 'maternity', 'Female', Sex),
         `Age (yrs)` = str_c(`Age (yrs)`, ' years')) %>% 
  spread(key = 'Maternity', value = 'Count') %>% 
  gather(-`Age (yrs)`, - Sex, key = 'Type', value = 'Count') %>% 
  mutate(pct = (Count / sum(Count, na.rm = TRUE))*100,
         `Age (yrs)` = factor(`Age (yrs)`, levels = age_bins))
  

# text argument defines custom tooltip to be displayed when hovering over bars
sex_age_plot <- attendance_sex_age_long %>% 
  ggplot(aes(x = `Age (yrs)`, y = Count/1000000, group = Type, fill = Type, text = paste('<br>Appointments:', Count,  '<br>Percent:', round(pct,2), '%'))) +
  geom_bar(stat = 'identity', position = position_stack()) +
  facet_grid(Sex ~ .) + 
  theme(axis.line = element_blank(),
        axis.text = element_text(colour = my_dark_grey),
        axis.text.x = element_text(angle = -45, vjust = -3, hjust = 0.8, colour = my_dark_grey),
        axis.text.y = element_text(colour = my_dark_grey),
        axis.ticks = element_blank(),
        axis.title = element_text(colour = my_dark_grey),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour = my_lighter_grey),
        plot.caption = element_text(colour = my_lighter_blue, hjust = 0),
        plot.title = element_text(colour = my_blue),
        plot.subtitle = element_text(colour = my_blue),
        legend.background = element_rect(fill=NA),
        legend.justification= c(1,0),
        legend.text = element_text(colour = my_dark_grey), 
        legend.title = element_blank(),
        legend.position = 'top',
        legend.spacing.x = unit(5, 'pt'),
        legend.spacing.y = unit(15, 'pt')) + 
  ylab('Number of appointments [millions]') +
  xlab('Age') +
  scale_fill_manual(values = rev(c(my_red, my_lighter_blue))) +
  guides(fill = guide_legend(ncol = 1, 
                             byrow = FALSE, 
                             label.hjust = 0,
                             keyheight = 0.1,
                             reverse = TRUE))

# Save locally
ggsave('OP_sex_age_count.png', sex_age_plot, device='png',  width = 7, height = 5)


# Convert to interactive plot
sex_age_plot_inter <- ggplotly(sex_age_plot, tooltip = c('text')) %>% 
  config(displayModeBar = F) %>% 
  layout(xaxis=list(fixedrange=TRUE)) %>% 
  layout(yaxis=list(fixedrange=TRUE))

# Publish graph
api_create(sex_age_plot_inter, filename = "HES_outpatient_sex_age")
