startTime <- Sys.time()
library(tidyverse)
load_all()

source('./inst/get_gambling_stuff_data_LOCAL.R')
props_list <- get_all_props()

source('./inst/output_dashboard_data.R')
endTime <- Sys.time()

endTime-startTime
