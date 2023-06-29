# ----- Load libraries ---- #

library(tidyverse)  # Load the tidyverse package for data manipulation and visualization
library(lubridate)  # Load the lubridate package for working with dates
library(gganimate)  # Load the gganimate package for creating animated plots
library(magick)     # Load the magick package for image rendering and manipulation
library(svglite)    # Load the svglite package for creating vector graphics
library(extrafont)  # Load the extrafont package for using custom fonts
library(janitor)    # Load the janitor package for data cleaning functions
library(Cairo)      # Load the Cairo package for high-quality graphics
library(ggtext)     # Load the ggtext package for enhanced text formatting

# ----- This section prepares and cleans our dataframe ---- #

# Read the data and clean column names
ex <- read.csv('new data//CLEAN3.csv', header = TRUE)
ex <- clean_names(ex)

# Convert the date column to a date object using lubridate
ex$date <- mdy(ex$rate_date)

# Trim leading and trailing spaces in the currency column
ex$currency <- trimws(ex$currency)

# Select necessary columns
df_clean <- ex %>% select(date, currency, central_rate)

# Define the title for the plot
title <- "<br> <span style='color:#45bf5f;'>Naira</span> <span style='color: black;'>vs</span> <span style='color: black;'>the</span> <span style='color: #2A0944;'>US Dollar<span><span style='color: black;'>,</span> <span style='color: #1f7a8c;'>Euro<span><span style='color: black;'>,</span> <span style='color: black;'>and<span> 
<br> <span style='color: #FEC260;'>Pound<span> <span style='color: black;'>since</span> <span style='color: black;'>2001.<span>"

# Set the order of the currency factor levels
df_clean$currency <- factor(df_clean$currency, levels = c("US DOLLAR", "EURO", "POUND"))

# Create the plot
a <- ggplot(data = df_clean, aes(x = date, y = central_rate, color = currency)) +
  geom_line(linewidth = 1.5, lineend = "butt", linejoin = "round") +
  scale_color_manual(values = c('#2A0944', '#1f7a8c', '#FEC260'),
                     name = '') +
  transition_reveal(date) +
  ggtitle(title) + 
  scale_y_continuous(breaks = seq(0,1000,200)) +
  #ylim(c(0,1000)) +
  facet_wrap(~currency, ncol = 1) + 
  scale_x_date(date_labels = "%Y", date_breaks = '3 years') +
  xlab('
       
       Year
       
       
       ') + ylab("Exchange Rate (Naira)
       
                     ") +
  labs(subtitle = "
       
       Exchange rate as at : {format(frame_along, '%b - %Y')}
       
       ",
       caption = '@DOh_Bams | R {tidyverse, gganimate} | Data: Central Bank of Nigeria') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
    panel.grid.major.y = element_line(colour = 'grey90',
                                      linewidth = 0.5),
    strip.text = element_blank(),
    text = element_text(family = "Tw Cen MT"),
    legend.box.background = element_blank(),
    legend.position = 'bottom',
    legend.text = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 24),
    plot.caption = element_text(size = 10, hjust = 0.5, vjust = -15),
    panel.background = element_blank(),
    plot.title = element_markdown(size = 28, face = 'bold'),
    plot.subtitle = element_text(size = 24),
    plot.margin = margin(1.5, 1.5, 1.5, 1.5, 'cm'),
    panel.spacing = unit(4, "lines"))

# Create the animated plot
a_anim <- animate(a, height = 20, width = 12, device = 'svglite', renderer = magick_renderer(), fps = 10, duration= 25, start_pause = 10, end_pause = 30)