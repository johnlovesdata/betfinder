startTime <- Sys.time()
devtools::load_all()
data_list <- get_data_for_dashboard('server')
props_list <- get_all_props()
output_data_for_dashboard(data_list, props_list)
endTime <- Sys.time()
endTime-startTime
