library(shiny)
library(shinydashboard)
library(tidyverse)
library(RColorBrewer)
library(DT)
library(config)
library(RPostgreSQL)
library(plotly)
library(powerMediation)
library(bayesAB)
library(ggplot2)
library(ggthemes)

## READ FUNCTIONS ---
walk(list.files('functions', full.names = T), source)

purple <- as.character("#4E1FE0")
red <- as.character("#FC125F")
green <- as.character('#27EB94')

theme <- theme_minimal() +
  theme(
    text = element_text(size = 10, color = "#666666", family = "Arial Rounded MT Bold"),
    axis.text = element_text(size = rel(0.8), color = "#666666"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
    axis.ticks = element_line(colour = "#e8e8e8"),
    legend.key = element_blank(),
    legend.position = "none",
    panel.grid.major = element_line(color = "#e8e8e8", size = 0.2),
    # panel.grid.minor = element_line(color = "#e8e8e8", size = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
theme_set(theme)
                  
#### LOAD NON REACTIVE DATA --------
df_daily <- f_generate_data() 

df_summary <- df_daily %>%
  group_by(test_name, variation) %>%
  summarize(
    trials = sum(trials, na.rm = T),
    successes = sum(successes, na.rm = T), 
    failure = trials - successes,
    rate = successes/trials
  )

lift_slider <- sliderInput(
  "lift", 
  label = "Desired Lift",
  min = 0, max = 15, value = 5, step = 1
)

alpha = numericInput(
  'alpha_select', 
  'Alpha', 
  value = 20
  )

beta = numericInput(
  'beta_select', 
  'Beta', 
  value = 80
)

df_tests <- df_daily %>%
  select(test_name, start_date, end_date) %>%
  unique()

test_select <- selectInput(
  "selected_test",
  label = 'Select Test',
  choices = df_tests$test_name,
  selected = 'Test A',
  width = "150px",
  size = NULL
)