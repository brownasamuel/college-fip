library(baseballr)
library(tidyverse)

teams = load_ncaa_baseball_teams()
d1_2023_ids = teams %>%
  filter(year == 2023 & division == 1) %>%
  pull(team_id)

scrape_team_pss = function(team_id) {
  pitchers = ncaa_team_player_stats(team_id, year = 2023, type = "pitching") %>%
    filter(!is.na(player_id))
  
  ## Creating desirable advanced statistics for pitchers:
  
  pitchers_adv = pitchers %>%
    filter(IP > 0) %>%
    mutate(across(App:KL, function(x) ifelse(is.na(x), 0, x))) %>%
    mutate(Outs = IP * 3) %>%
    mutate(Outs = ifelse(Outs %% 1 != 0, round(Outs) + 1, Outs)) %>%
    mutate(K_Rate = SO / BF,
           BB_Rate = BB / BF,
           K_Minus_BB = K_Rate - BB_Rate, 
           K_Div_BB = K_Rate / BB_Rate,
           FIP_No_Constant = ((13 * `HR-A`) + (3 * (BB + HB)) - (2 * SO)) / (Outs / 3),
           IP_New = Outs / 3)
  
  pitchers_final = pitchers_adv %>%
    select(team_name, conference, team_id, conference_id, division, player_id, player_name, year = Yr, position = Pos,
           IP = IP_New, ER, HR_Allowed = `HR-A`, TBF = BF, BB, K = SO, K_Rate, BB_Rate, K_Minus_BB, K_Div_BB, FIP_No_Constant, GO, FO)

  return(pitchers_final)
}

pss = map_dfr(.x = d1_2023_ids, .f = ~ scrape_team_pss(.x))

write_csv(pss, "PSS-D1-2023.csv")
