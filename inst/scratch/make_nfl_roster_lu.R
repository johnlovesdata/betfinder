nfl_roster = data.frame(
  name = unique(nfl_raw$full_name),
  aliases = NA,
  stringsAsFactors = FALSE
)

lower = tolower(nfl_roster$name)
upper = toupper(nfl_roster$name)
hacky = hacky_tidyup_player_names(nfl_roster$name)

output_list = as.list(rep(NA_character_, nrow(nfl_roster)))

for (i in 1:length(output_list)) {
  output_list[[i]] <- c(lower[[i]], upper[[i]], hacky[[i]])
}

nfl_roster$aliases = output_l
write_json(nfl_roster, './inst/lu/nfl/player/lu.json')
