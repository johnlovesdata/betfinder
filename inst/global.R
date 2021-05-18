# options
options(scipen = 999)

# libraries
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(reactable)
library(R.utils)

# load data
search_props_raw <- readRDS("props.rds")
