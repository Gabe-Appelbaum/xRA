library(tidyverse)
library(nflreadr)

#functions
run_value_calculation <- function(mlb){
  # start by tagging each event as its own thing
  mlb <- mlb %>%
    mutate(rv_event = 
             case_when(bb_type == "ground_ball" & launch_speed < 95.0 & events != "sac_bunt" ~ "soft_gb",
                       bb_type == "ground_ball" & launch_speed >= 95.0 ~ "hard_gb",
                       bb_type == "fly_ball" & launch_speed < 95.0 ~ "soft_fb",
                       bb_type == "fly_ball" & launch_speed >= 95.0 ~ "hard_fb",
                       bb_type == "line_drive" & launch_speed < 95.0 & events != "sac_bunt" ~ "soft_ld",
                       bb_type == "line_drive" & launch_speed >= 95.0 ~ "hard_ld",
                       bb_type == "popup" & events != "sac_bunt" ~ "popup",
                       description %in% c("swinging_strike_blocked", "called_strike", 
                                          "swinging_strike", "unknown_strike") & strikes != 2 ~ "nd_strike",
                       description %in% c("foul", "foul_tip") & strikes != 2 ~ "nd_strike",
                       events  %in% c("strikeout", "strikeout_double_play") ~ "so", 
                       description %in% c("ball", "blocked_ball", "hit_by_pitch") & balls != 3 ~ "nd_ball",
                       events  == "walk" ~ "ubb"
             )            
    )
  
  # next group by events and calculate mean
  run_values <- mlb %>%
    drop_na(rv_event) %>% # drop the rows that dont have an event
    drop_na(delta_run_exp) %>% # drop the rows that don't have a run expectancy
    group_by(rv_event) %>%
    summarise_at(vars(delta_run_exp), list(mean = mean)) %>%
    mutate(mean = round(mean, 3)) %>%
    as.data.frame()
  
  return(run_values)
}

metric_calculation <- function(mlb, pa_bb_so, run_values){
  # start by tagging each event as its own thing
  mlb <- mlb %>%
    mutate(rv_event = 
             case_when(bb_type == "ground_ball" & launch_speed < 95.0 & events != "sac_bunt" ~ "soft_gb",
                       bb_type == "ground_ball" & launch_speed >= 95.0 ~ "hard_gb",
                       bb_type == "fly_ball" & launch_speed < 95.0 ~ "soft_fb",
                       bb_type == "fly_ball" & launch_speed >= 95.0 ~ "hard_fb",
                       bb_type == "line_drive" & launch_speed < 95.0 & events != "sac_bunt" ~ "soft_ld",
                       bb_type == "line_drive" & launch_speed >= 95.0 ~ "hard_ld",
                       bb_type == "popup" & events != "sac_bunt" ~ "popup",
                       description %in% c("swinging_strike_blocked", "called_strike", 
                                          "swinging_strike", "unknown_strike") & strikes != 2 ~ "nd_strike",
                       description %in% c("foul", "foul_tip") & strikes != 2 ~ "nd_strike",
                       events  %in% c("strikeout", "strikeout_double_play") ~ "so", 
                       description %in% c("ball", "blocked_ball", "hit_by_pitch") & balls != 3 ~ "nd_ball",
                       events  == "walk" ~ "ubb"
             )            
    )
  
  # now lets calculate how many times each event happened for each player
  player_events <- mlb %>% 
    group_by(player_name, batter) %>%
    summarise(soft_gb = length(which(rv_event == "soft_gb")),
              hard_gb = length(which(rv_event == "hard_gb")),
              soft_fb = length(which(rv_event == "soft_fb")),
              hard_fb = length(which(rv_event == "hard_fb")),
              soft_ld = length(which(rv_event == "soft_ld")),
              hard_ld = length(which(rv_event == "hard_ld")),
              popup = length(which(rv_event == "popup")),
              nd_strike = length(which(rv_event == "nd_strike")),
              nd_ball = length(which(rv_event == "nd_ball")),
    )
  
  #clean players name and then manually add Jr's and II's back on, also makes some other minor changes
  #this will need to be updated for 22 and 21 outlier names
  player_events$player_name <- clean_player_names(player_events$player_name)
  player_events <- player_events %>%
    mutate(
      player_name = case_when(
        player_name %in% c("Ronald Acuña", "LaMonte Wade", "Bobby Witt", "Vladimir Guerrero", "Fernando Tatis",
                           "Lourdes Gurriel", "Jazz Chisholm", "Jackie Bradley") ~ paste0(player_name, " Jr."),# Add "Jr." to specified names
        player_name %in% c("Michael Harris", "Cedric Mullins") ~ paste0(player_name, " II"),
        substr(player_name, 1, 2) == "JD" ~ str_replace(player_name, "JD", "J.D."),
        substr(player_name, 1, 2) == "JP" ~ str_replace(player_name, "JP", "J.P."),
        substr(player_name, 1, 2) == "TJ" ~ str_replace(player_name, "TJ", "T.J."),
        substr(player_name, 1, 2) == "JT" ~ str_replace(player_name, "JT", "J.T."),
        substr(player_name, 1, 2) == "JJ" ~ str_replace(player_name, "JJ", "J.J."),
        substr(player_name, 1, 2) == "AJ" ~ str_replace(player_name, "AJ", "A.J."),
        TRUE ~ player_name
      ),
      player_name = chartr("áéíóúñÁÉÍÓÚÑ", "aeiounAEIOUN", player_name),
      player_name = ifelse(player_name == "Ryan OHearn", "Ryan O'Hearn", player_name),
      player_name = ifelse(player_name == "KeBryan Hayes", "Ke'Bryan Hayes", player_name),
      player_name = ifelse(player_name == "Tyler ONeill", "Tyler O'Neill", player_name), 
      player_name = ifelse(player_name == "Travis dArnaud", "Travis d'Arnaud", player_name),
      player_name = ifelse(player_name == "Ha-Seong Kim", "Ha-seong Kim", player_name),
      player_name = ifelse(player_name == "Ji Man Choi", "Ji-Man Choi", player_name),
      player_name = ifelse(player_name == "Michael A Taylor", "Michael A. Taylor", player_name),
      player_name = ifelse(player_name == "CJ Cron", "C.J. Cron", player_name),
      player_name = ifelse(player_name == "DJ Stewart", "D.J. Stewart", player_name)
    )
  
  #join with PA, BB, SO
  player_events <- left_join(pa_bb_so, player_events, by = join_by(Name == player_name))
  
  # calculate the metric itself
  player_events <- player_events %>% #next step is multiplying by the mean run value
    mutate(xRuns = (
      (soft_gb * run_values[9,2]) + (hard_gb * run_values[2,2]) + 
        (soft_fb * run_values[8,2]) + (hard_fb * run_values[1,2]) +
        (soft_ld * run_values[10,2]) + (hard_ld * run_values[3,2]) + 
        (popup * run_values[6,2]) +
        (nd_strike * run_values[5,2]) + (SO * run_values[7,2]) +
        (nd_ball * run_values[4,2]) + (BB * run_values[11,2])
      )
      )
  
  player_events$xRA <- player_events$xRuns / player_events$PA
  player_events$xRA_100 <- player_events$xRA * 100
  
  return(player_events)
}

league_xra_calc <- function(mlb, pa_bb_so, run_values){
  # start by tagging each event as its own thing
  mlb <- mlb %>%
    mutate(rv_event = 
             case_when(bb_type == "ground_ball" & launch_speed < 95.0 & events != "sac_bunt" ~ "soft_gb",
                       bb_type == "ground_ball" & launch_speed >= 95.0 ~ "hard_gb",
                       bb_type == "fly_ball" & launch_speed < 95.0 ~ "soft_fb",
                       bb_type == "fly_ball" & launch_speed >= 95.0 ~ "hard_fb",
                       bb_type == "line_drive" & launch_speed < 95.0 & events != "sac_bunt" ~ "soft_ld",
                       bb_type == "line_drive" & launch_speed >= 95.0 ~ "hard_ld",
                       bb_type == "popup" & events != "sac_bunt" ~ "popup",
                       description %in% c("swinging_strike_blocked", "called_strike", 
                                          "swinging_strike", "unknown_strike") & strikes != 2 ~ "nd_strike",
                       description %in% c("foul", "foul_tip") & strikes != 2 ~ "nd_strike",
                       events  %in% c("strikeout", "strikeout_double_play") ~ "so", 
                       description %in% c("ball", "blocked_ball", "hit_by_pitch") & balls != 3 ~ "nd_ball",
                       events  == "walk" ~ "ubb"
             )            
    )
  
  # now lets calculate how many times each event happened across the league
  player_events <- mlb %>% 
    summarise(soft_gb = length(which(rv_event == "soft_gb")),
              hard_gb = length(which(rv_event == "hard_gb")),
              soft_fb = length(which(rv_event == "soft_fb")),
              hard_fb = length(which(rv_event == "hard_fb")),
              soft_ld = length(which(rv_event == "soft_ld")),
              hard_ld = length(which(rv_event == "hard_ld")),
              popup = length(which(rv_event == "popup")),
              nd_strike = length(which(rv_event == "nd_strike")),
              nd_ball = length(which(rv_event == "nd_ball")),
    )
  
  #join with PA, BB, SO
  player_events <- cbind(player_events, pa_bb_so)
  
  # calculate the metric itself
  player_events <- player_events %>% #next step is multiplying by the mean run value
    mutate(xRuns = (
      (soft_gb * run_values[9,2]) + (hard_gb * run_values[2,2]) + 
        (soft_fb * run_values[8,2]) + (hard_fb * run_values[1,2]) +
        (soft_ld * run_values[10,2]) + (hard_ld * run_values[3,2]) + 
        (popup * run_values[6,2]) +
        (nd_strike * run_values[5,2]) + (so * run_values[7,2]) +
        (nd_ball * run_values[4,2]) + (bb * run_values[11,2])
    )
    )
  
  player_events$xRA <- player_events$xRuns / player_events$pa
  player_events$xRA_100 <- player_events$xRA * 100
  
  return(player_events)
}

# read in the main 3 years
MLB23 <- read_csv("data/MLB 23.csv") %>% select(-rv_event) # drop this column so it matches
MLB22 <- read_csv("data/MLB 22.csv")
MLB21 <- read_csv("data/MLB 21.csv")


# calculate xRA for each of the three years
{
xRA23 <- metric_calculation(MLB23, 
                            read_csv("data/pa_bb_so_2023.csv") %>% 
                              select(Name, PA, SO, BB), 
                            run_value_calculation(rbind(MLB22, MLB21, read_csv("data/MLB 20.csv")))
                            ) %>%
  select(Name, batter, xRA_100)

xRA22 <- metric_calculation(MLB22, 
                            read_csv("data/pa_bb_so_2022.csv") %>% 
                              select(Name, PA, SO, BB),
                            run_value_calculation(rbind(MLB21, read_csv("data/MLB 20.csv"), read_csv("data/MLB 19.csv")))
                            ) %>%
  select(Name, batter, xRA_100)

xRA21 <- metric_calculation(MLB21, read_csv("data/pa_bb_so_2021.csv") %>% 
                              select(Name, PA, SO, BB),
                            run_value_calculation(
                              rbind(
                                read_csv("data/MLB 20.csv"),
                                read_csv("data/MLB 19.csv"),
                                read_csv("data/MLB 18.csv")))
                            ) %>%
  select(Name, batter, xRA_100)

write_csv(xRA23, "data/player xRA 2023.csv")
write_csv(xRA22, "data/player xRA 2022.csv")
write_csv(xRA21, "data/player xRA 2021.csv")
}

# league xRA
lg_xRA23 <- league_xra_calc(MLB23,
                            read_csv("data/lg_pa_bb_so.csv") %>% filter(year == 2023),
                            run_value_calculation(rbind(MLB22, MLB21, read_csv("data/MLB 20.csv")))
                            )

# calculate run values across the six year period and each of the three year periods
{
mlb <- rbind(
  read_csv("data/MLB 18.csv"),
  read_csv("data/MLB 19.csv"),
  read_csv("data/MLB 20.csv"),
  read_csv("data/MLB 21.csv"),
  read_csv("data/MLB 22.csv"),
  read_csv("data/MLB 23.csv") %>% select(-rv_event)
)

full_rv <- run_value_calculation(mlb)
write_csv(full_rv, "data/six year rv.csv")

rv2021 <- run_value_calculation(rbind(
  read_csv("data/MLB 20.csv"),
  read_csv("data/MLB 19.csv"),
  read_csv("data/MLB 18.csv"))
  )
write_csv(rv2021, "data/rv 21")

rv2022 <- run_value_calculation(rbind(
  read_csv("data/MLB 21.csv"),
  read_csv("data/MLB 20.csv"),
  read_csv("data/MLB 19.csv"))
)
write_csv(rv2022, "data/rv 22")

rv2023 <- run_value_calculation(rbind(
  read_csv("data/MLB 22.csv"),
  read_csv("data/MLB 21.csv"),
  read_csv("data/MLB 20.csv"))
)
write_csv(rv2023, "data/rv 23")
}