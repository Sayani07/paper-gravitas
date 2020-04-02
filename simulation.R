
library(gravitas)
library(tsibbledata)
library(dplyr)
library(ggplot2)


cust50 <- sm_cust50 %>%  distinct(customer_id)

VIC <- sm_cust50 %>% 
  filter(customer_id==cust50$customer_id[2])

VIC %>%
  #create_gran("quarter_year") %>%
  #filter(month_year %in% c("Jan", "Mar", "Dec")) %>% 
  #create_gran("day_week") %>%
  #filter(day_week %in% c("Sat", "Sun", "Mon")) %>%
  prob_plot("quarter_year", "wknd_wday",
            response = "general_supply_kwh",
            plot_type = "lv",
            symmetric = FALSE
  ) +
  ylab("Electricity Demand [KWh]") +
  xlab("weekend/weekday") + 
  ggtitle("(a) Letter value plot with quarter-of-year facet")+
  scale_fill_brewer(palette = "Dark2") 



VIC %>%
  #create_gran("month_year") %>%
  #filter(month_year %in% c("Jan", "Mar", "Dec")) %>% 
  #create_gran("day_week") %>%
  #filter(day_week %in% c("Sat", "Sun", "Mon")) %>%
  prob_plot("wknd_wday",
            "quarter_year",
            response = "general_supply_kwh",
            plot_type = "lv",
            symmetric = FALSE
  ) +
  ylab("") +
    xlab("quarter-of-year")  + 
  ggtitle("(b) Letter value plot with weekend/weekday facet") +
  scale_fill_brewer(palette = "Paired") 




VIC %>%
  #create_gran("month_year") %>%
  #filter(month_year %in% c("Jan", "Mar", "Dec")) %>% 
  #create_gran("day_week") %>%
  #filter(day_week %in% c("Sat", "Sun", "Mon")) %>%
  prob_plot("quarter_year",
            "month_year",
            response = "general_supply_kwh",
            plot_type = "lv",
            k = 3, 
            symmetric = FALSE
  ) +
  ylab("") +
  xlab("day_month")  + 
  ggtitle("(c) Letter value plot across quarters & months") 

