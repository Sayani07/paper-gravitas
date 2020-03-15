harmony_data <- smart_meter10 %>% harmony(
  ugran = "month",
  filter_out = c("hhour", "fortnight")
) 

for(i in 1:nrow(harmony_data))
{
  
 data_new <- create_gran(smart_meter10, harmony_data[1,1]$facet_variable)%>% create_gran(harmony_data[1,2]$x_variable)
 
data_new$hour_day = as.numeric(data_new$hour_day)
density(data_new$hour_day)
data_new$day_week = as.numeric(data_new$day_week)
density(data_new$day_week)

data_new[-c(1:2)]  %>% 
  pivot_wider(names_from = day_week,
              values_from = general_supply_kwh,
              values_fn = list(general_supply_kwh = list)) %>% 
  map(unlist) 


%>% map_dfr(unlist)
  
    
}
    