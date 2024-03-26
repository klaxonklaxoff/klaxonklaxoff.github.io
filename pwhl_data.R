# Load libraries ----
library(fastRhockey)
library(tidyverse)

# Get data ----
teams <- 
  pwhl_teams()

# standing <- 
#   pwhl_standings() %>% 
#   mutate(across(c(games_played:penalty_kill_pct), as.numeric))

schedule <- 
  pwhl_schedule(season = 2023) %>% 
  select(-c(home_team_id, away_team_id, venue_url)) %>% 
  mutate(game_date = as.Date(strptime(sub(".*, ", "", game_date), "%b %d"))) %>% 
  filter(game_date <= Sys.Date())

stats_goalie <- 
  pwhl_stats(season = 2023) %>% 
  select(-minutes) %>% 
  mutate(across(c(games_played:so_save_percentage), as.numeric))

# stats_player <- 
  # do.call(rbind, lapply(c(unique(teams$team_label)), function(x) {
  #   pwhl_stats(season = 2023, position = "skater", team = x)
  # }))
  # pwhl_stats(position = "skater", team = "BOS", season = 2023)

players <- 
  do.call(rbind, lapply(c(unique(teams$team_label)), function(x) {
    pwhl_team_roster(season = 2023, team = x)
  }))

w_v_l <-
  standing %>%
  select(team, wins, losses) %>%
  mutate(
    team = sub("PWHL ", "", team),
    losses = losses * -1) %>%
  pivot_longer(
    cols = c("wins", "losses"),
    names_to = "w_l",
    values_to = "score"
  )

rm(teams)

write.csv(schedule, "./data/schedule.csv")
write.csv(stats_goalie, "./data/stats_goalie.csv")
write.csv(players, "./data/players.csv")
write.csv(w_v_l, "./data/w_v_l.csv")
