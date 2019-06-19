#===============================================================================
# Analysis of NHS winter sitrep data 2018/2019
# Purpose: import, clean and generate STP map of bed occupancy by month
# Authour: Fiona Grimm
# Date: 03/2019
#===============================================================================

# 1. Import packages ----

library(tidyverse)
library(readxl)
library(plotly)
library(htmltools)


# 2. Downloading HES outpatient data from NHS Digital ----

filename <- "hosp-epis-stat-outp-rep-tabs-2017-18-tab.xlsx"
url <- "https://files.digital.nhs.uk/0D/0C3CF4/hosp-epis-stat-outp-rep-tabs-2017-18-tab.xlsx"


download.file(url, destfile = filename, mode = "wb")


# 3. Importing data from R-unfriendly spreadsheets ----

attendance_counts <- read_xlsx(path = filename, sheet = 'Summary Report 3', skip = 3, n_max = 11) %>% 
  select(1:7) 

# 4. Wrangle ----

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

# 5. Plot ----

# Colours
my_dark_grey <- '#524c48'
my_light_grey <- '#e2dfd8'
my_lighter_grey <- '#eeede8'
my_blue <- '#005078'
my_lighter_blue <- '#7fbfda'
my_light_blue <- '#53a9cd'
my_red <- '#dd0031'

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
  labs(title = 'NHS hospital outpatient appointments') +
  ylab('Number of appointments [millions]') +
  scale_fill_manual(values = rev(c(my_blue, my_light_blue, my_red, my_light_grey))) +
  guides(fill = guide_legend(ncol = 3, 
                             byrow = FALSE, 
                             label.hjust = 0,
                             keyheight = 0.1,
                             reverse = TRUE))

# Save locally
ggsave('OP_attend_count.pdf', attendance_plot, device='pdf',  width = 7, height = 5)

# Convert to interactive plot
attendance_plot_inter <- ggplotly(attendance_plot, tooltip = c('text')) %>% 
  config(displayModeBar = F) %>% 
  layout(xaxis=list(fixedrange=TRUE)) %>% 
  layout(yaxis=list(fixedrange=TRUE))

# Source plotly credentials 
source('plotly.R')

# Publish graph
api_create(attendance_plot_inter, filename = "HES_outpatient_attendance")

