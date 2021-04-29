load_all()
# get props ----

# draftkings
dk_ftts <- try(get_props('dk', 'nba', 'ftts'))
dk_fpts <- try(get_props('dk', 'nba', 'fpts'))
dk_points_ou <- try(get_props('dk', 'nba', 'player points ou'))
dk_rebounds_ou <- try(get_props('dk', 'nba', 'player rebounds ou'))
dk_assists_ou <- try(get_props('dk', 'nba', 'player assists ou'))
dk_threes_ou <- try(get_props('dk', 'nba', 'player 3pts ou'))

# fanduel
fd_ftts <- try(get_props('fd', 'nba', 'ftts'))
fd_fpts <- try(get_props('fd', 'nba', 'fpts'))
fd_points_alt <- try(get_props('fd', 'nba', 'player points alt'))
fd_rebounds_alt <- try(get_props('fd', 'nba', 'player rebounds alt'))
fd_assists_alt <- try(get_props('fd', 'nba', 'player assists alt'))
# fd_threes_alt <- try(get_props('fd', 'nba', 'player 3pts alt')) # does fd ever even do this?
fd_points_ou <- try(get_props('fd', 'nba', 'player points ou'))
fd_rebounds_ou <- try(get_props('fd', 'nba', 'player rebounds ou'))
fd_assists_ou <- try(get_props('fd', 'nba', 'player assists ou'))
fd_threes_ou <- try(get_props('fd', 'nba', 'player 3pts ou'))
fd_points_tiers <- try(get_props('fd', 'nba', 'player points tiers'))
fd_rebounds_tiers <- try(get_props('fd', 'nba', 'player rebounds tiers'))
fd_assists_tiers <- try(get_props('fd', 'nba', 'player assists tiers'))
fd_threes_tiers <- try(get_props('fd', 'nba', 'player 3pts tiers'))

# pointsbet
pb_ftts <- try(get_props('pb', 'nba', 'ftts'))
pb_fpts <- try(get_props('pb', 'nba', 'fpts'))
pb_points_alt <- try(get_props('pb', 'nba', 'player points alt'))
pb_rebounds_alt <- try(get_props('pb', 'nba', 'player rebounds alt'))
pb_threes_alt <- try(get_props('pb', 'nba', 'player 3pts alt'))
pb_assists_alt <- try(get_props('pb', 'nba', 'player assists alt'))
pb_points_ou <- try(get_props('pb', 'nba', 'player points ou'))
pb_rebounds_ou <- try(get_props('pb', 'nba', 'player rebounds ou'))
pb_assists_ou <- try(get_props('pb', 'nba', 'player assists ou'))
pb_threes_ou <- try(get_props('pb', 'nba', 'player 3pts ou'))
pb_points_tiers <- try(get_props('pb', 'nba', 'player points tiers'))
pb_rebounds_tiers <- try(get_props('pb', 'nba', 'player rebounds tiers'))
pb_assists_tiers <- try(get_props('pb', 'nba', 'player assists tiers'))
pb_threes_tiers <- try(get_props('pb', 'nba', 'player 3pts tiers'))
