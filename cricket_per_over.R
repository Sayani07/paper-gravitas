cricket_per_over <- cricketdata %>% 
  group_by(match_id, batting_team, bowling_team,  inning, over) %>%
  summarise(runs_per_over = sum(total_runs)) 

cricket_tsibble <- cricket_per_over %>% 
  ungroup() %>% 
  mutate(data_index = row_number()) %>% 
  as_tsibble(index = data_index)


harmony(cricket_tsibble, lowest_unit = "over", highest_unit = "match", hierarchy_tbl =  hierarchy_model)
cricket_per_over %>% 
  
  
  cricket_tsibble %>% granplot("inning_match", "over_inning",
                               hierarchy_model,
                               response = "runs_per_over",
                               plot_type = "quantile",
                               quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9)
                               
                               
  )