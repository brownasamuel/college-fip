library(baseballr)
library(tidyverse)

teams = load_ncaa_baseball_teams()
d1_2023_ids = teams %>%
  filter(year == 2023 & division == 1) %>%
  pull(team_id)

scrape_team_hss = function(team_id) {
  hitters = ncaa_team_player_stats(team_id, year = 2023, type = "batting") %>%
    filter(!is.na(player_id))
  
  ## Creating desirable advanced statistics for hitters: 
  
  hitters_adv = hitters %>%
    mutate(across(BA:SB, function(x) ifelse(is.na(x), 0, x))) %>%
    mutate(PA = AB + BB + HBP + SF + SH, 
           K_Rate = K / PA, 
           BB_Rate = BB / PA, 
           HR_Rate = HR / PA, 
           OPS = OBPct + SlgPct, 
           ISO = SlgPct - BA) %>%
    filter(PA > 0)
  
  hitters_final = hitters_adv %>%
    select(team_name, conference, team_id, conference_id, division, player_id, player_name, year = Yr, position = Pos,
           PA, AVG = BA, OBP = OBPct, SLG = SlgPct, OPS, ISO, HR, K_Rate, BB_Rate, HR_Rate)
  
  return(hitters_final)
}

hss = map_dfr(.x = d1_2023_ids, .f = ~ scrape_team_hss(.x))

write_csv(hss, "HSS-D1-2023.csv")
