library(shiny)
library(shinydashboard)
library(tidyverse)
library(RColorBrewer)
library(DT)
library(GoNoodleR)
library(config)
library(RPostgreSQL)
library(plotly)
library(powerMediation)
library(bayesAB)

## READ FUNCTIONS ---
walk(list.files('functions', full.names = T), source)

gonoodle_theme()

purple <- as.character("#4E1FE0")
red <- as.character("#FC125F")
green <- as.character('#27EB94')

#### LOAD NON REACTIVE DATA --------
df_daily <- f_generate_data() 

df_summary <- df_daily %>%
  group_by(test_name, variation) %>%
  summarize(trials = sum(trials, na.rm = T),
            successes = sum(successes, na.rm = T), 
            failure = trials - successes,
            rate = successes/trials)

lift_slider <- sliderInput("lift", label = "Desired Lift",
                           min = 0, max = 15, value = 5, step = 1)

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