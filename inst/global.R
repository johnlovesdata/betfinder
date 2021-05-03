# options
options(scipen = 999)

# libraries
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(reactable)

# load data
search_props_raw <- readRDS("props.rds") %>%
  filter(!is.na(tidyopp))
