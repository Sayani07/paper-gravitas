## ---- load
library(bookdown)
library(rticles)
library(knitr)
library(tidyverse)
library(tsibble)
library(gravitas)
library(ggpubr)
library(kableExtra)
library(tibble)

## ---- linear-time
knitr::include_graphics("Figs/linear-ex.png")

## ---- circular-dow
knitr::include_graphics("Figs/circular-ex.png")

## ----aperiodic-example
knitr::include_graphics("Figs/aperiodic-ex3.png")

## ----tab-mayan
table_mayan <- tibble(`linear (G)` = c("kin", "uinal", "tun", "katun", "baktun"),
                      `single-order-up cyclic (C)` = c("kin-of-uinal","uinal-of-tun"," tun-of-katun", "katun-of-baktun", 1),
                      `period length/conversion operator (K)`= c(20, 18, 20, 20, 1))
kable(table_mayan,
      format = "latex",
      booktabs = TRUE,
      align = "l",
      caption = "Hierarchy table for Mayan calendar with circular single-order-up granularities.") %>%
  kable_styling()

##----tab-gregorian
table_greg <- tibble(`linear (G)` = c("minute", "hour", "day", "month", "year"),
                     `single-order-up cyclic (C)` = c("minute-of-hour","hour-of-day","day-of-month", "month-of-year", 1),
                     `period length/conversion operator (K)` = c(60, 24, "k(day, month)", 12, 1))

kable(table_greg,
      format = "latex",
      booktabs = TRUE,
      caption = "Hierarchy table for the Gregorian calendar with both circular and quasi-circular single-order-up granularities.") %>%
  #row_spec(0, bold = TRUE) %>%
  kable_styling()

## ----data-structure
include_graphics("Figs/data-struc-diffcol.png")

## ----allFig
load("data/sm_cust50.rds")
cust50 <- sm_cust50 %>%  distinct(customer_id)

VIC <- sm_cust50 %>% as_tsibble(regular = FALSE) %>% 
  filter(customer_id==cust50$customer_id[2])

scene1 <-  VIC %>%
  prob_plot("quarter_year", "wknd_wday",
            response = "general_supply_kwh",
            plot_type = "lv",
            symmetric = FALSE
  ) +
  ylab("") +
  #xlab("weekend/weekday") +
  scale_fill_brewer(palette = "Dark2") + theme(legend.position = "right") + ggtitle("") + scale_y_log10() + xlab("")+ 
  theme_minimal()


scene2 <- VIC %>%
  prob_plot("wknd_wday",
            "quarter_year",
            response = "general_supply_kwh",
            plot_type = "lv",
            symmetric = FALSE
  ) +
  ylab("") +
  xlab("quarters of the year")  +
  scale_fill_brewer(palette = "Paired") + theme(legend.position = "right") + ggtitle("") + scale_y_log10() + theme_minimal()




scene3 <- VIC %>%
  prob_plot("quarter_year",
            "month_year",
            response = "general_supply_kwh",
            plot_type = "lv",
            symmetric = FALSE
  ) +
  ylab("") +
  xlab("months of the year") + theme(legend.position = "right") + scale_fill_brewer(palette = "Set2") + ggtitle("") + scale_y_log10()+ theme_minimal()

gg_fig <- ggarrange(scene3,
                    ggarrange(scene1, scene2, ncol = 2, labels = c("b", "c")),
                    nrow = 2, labels = "a")
#label.y = "electricity demand [KWh]"\


ggpubr::annotate_figure(gg_fig,
                        left = text_grob("electricity demand [KWh]",  rot = 90)) + theme_minimal()


## ----search
smart_meter %>%
  search_gran(filter_out = c("semester", 
                             "quarter",
                             "fortnight"))

## ----search_gran_limit2
smart_meter10 %>% search_gran(
  highest_unit = "month",
  filter_out = c("hhour", "fortnight"))%>%
  knitr::kable(format = "latex",
               booktabs = TRUE) %>%
  #row_spec(0, bold = TRUE)%>%
  kable_styling()

## ----harmony-tab
sm <- smart_meter10 %>%
  filter(customer_id %in% c(10017936))

harmonies <- sm %>%
  harmony(ugran = "month",
          filter_in = "wknd_wday", 
          filter_out = c("hhour", "fortnight")) %>%
  rename(`facet variable` = facet_variable,
         `x-axis variable` = x_variable,
         `facet levels` = facet_levels,
         `x-axis levels` = x_levels) 


knitr::kable(harmonies,
             format = "latex",
             booktabs = TRUE,
             caption = "Harmonies with a pair of cyclic granularity one placed on facet and the other on x-axis. Out of 42 possible combinations of cyclic granularities, only 16 are harmony pairs.") %>%
  #row_spec(0, bold = TRUE) %>%
  kable_styling()

## ----bothcust
cust2_quantile <- smart_meter10 %>%
  filter(customer_id %in% c(10017936)) %>%
  prob_plot("wknd_wday",
            "hour_day",
            response = "general_supply_kwh",
            plot_type = "quantile",
            symmetric = TRUE,
            quantile_prob = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)
  ) +
  scale_y_sqrt() +
  ylab("electricity demand [KWh]") + xlab("hours of the day") + ggtitle("") + ylab("")+ 
  theme_minimal()

cust2_violin <- smart_meter10 %>%
  filter(customer_id %in% c(10017936)) %>%
  prob_plot("wknd_wday",
            "hour_day",
            response = "general_supply_kwh",
            plot_type = "violin"
  ) +
  scale_y_sqrt() +
  ylab("") + xlab("hours of the day") + ggtitle("") +
  scale_x_discrete( breaks = seq(0, 23, 5))+ 
  theme_minimal()

cust2_box <- smart_meter10 %>%
  filter(customer_id %in% c(10017936)) %>%
  prob_plot("hour_day",
            "wknd_wday",
            response = "general_supply_kwh",
            plot_type = "boxplot"
  ) +
  scale_y_sqrt() +
  xlab("") +
  ggtitle("") + ylab("") +
  scale_x_discrete(labels = c('wday','wend')) +
  ggplot2::theme(axis.text.x = element_text(size = 7))+ 
  theme_minimal()


gg_fig <- ggarrange(cust2_box,
                    ggarrange(cust2_quantile, cust2_violin, nrow = 2, labels = c("b", "c")),
                    ncol = 2, labels = "a")

ggpubr::annotate_figure(gg_fig,
                        left = text_grob("electricity demand [KWh]",  rot = 90))

## ----hierarchy-cric
hierarchy_model <- tibble::tibble(
  `linear (G)` = c("over", "inning", "match", "season"),
  `single-order-up cyclic (C)` = c("over-of-inning", "inning-of-match", "match-of-season", 1),
  `period length/conversion operator (K)` = c(20, 2, "k(match, season)", 1)
)

knitr::kable(hierarchy_model,
             format = "latex",
             booktabs = TRUE,
             caption = "Hierarchy table for cricket where overs are nested within an innings, innings nested within a match and matches within a season.") %>%
  #row_spec(0, bold = TRUE)%>%
  kable_styling()

## ----cricex
cricket_tsibble <- cricket %>%
  mutate(data_index = row_number()) %>%
  as_tsibble(index = data_index)

hierarchy_model <- tibble::tibble(
  units = c("index", "over", "inning", "match"),
  convert_fct = c(1, 20, 2, 1)
)

cricket_tsibble %>%
  filter(batting_team %in% c(
    "Mumbai Indians",
    "Chennai Super Kings"
  )) %>%
  mutate(inning = paste0("innings: ", inning)) %>%
  prob_plot("inning",
            "over",
            response = "runs_per_over",
            hierarchy_model,
            plot_type = "lv"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  #ggtitle("(a) Runs per over across over faceted by inning") +
  theme(legend.position = "right") +
  ggtitle("a") +
  ylab("runs per over") +
  xlab("overs of the innings") +
  theme(plot.title = element_text(face = "bold")) +
  ggplot2::theme(
    strip.text = ggplot2::element_text(
      size = 10,
      margin = ggplot2::margin(b=0, t=0)
    )
  ) +  theme_minimal() 
#geom_smooth(aes( x = over,
#                               y=runs_per_over), method = lm, #formula = y ~ splines::bs(x, 3), se = FALSE)

cricket_all <- read_csv("data-raw/deliveries_all.csv")
matches_all <- read_csv("data-raw/matches_all.csv")

cricket_season <- cricket_all %>% left_join(matches_all, by = c("match_id" = "id"))

# cricket_per_over <- cricket_season %>%
#   group_by(season,
#            match_id,
#            batting_team,
#            bowling_team,
#            inning,
#            over) %>%
#   summarise(runs_per_over = sum(total_runs),
#             run_rate = sum(total_runs)/length(total_runs))
#
# cricket_tsibble_all <- cricket_per_over %>%
#   ungroup() %>%
#   mutate(data_index = row_number()) %>%
#   as_tsibble(index = data_index)

cricket_dot_field <- cricket_season %>%
  mutate(
    fielding_proxy = if_else(dismissal_kind %in%
                               c("caught", "caught and bowled"), 1, 0),
    dot_ball_proxy = if_else(total_runs == 0, 1, 0),
    wicket_proxy = if_else(is.na(dismissal_kind), 0, 1)
  ) %>%
  group_by(
    season,
    match_id,
    batting_team,
    bowling_team,
    inning,
    over
  ) %>%
  summarise(
    runs_per_over = sum(total_runs),
    run_rate = sum(total_runs)*6 / length(total_runs),
    fielding_wckts = sum(fielding_proxy),
    dot_balls = sum(dot_ball_proxy)
  ) %>%
  mutate(diff_run_rate = c(0, diff(run_rate)))

cricket_tsibble <- cricket_dot_field %>%
  ungroup() %>%
  mutate(data_index = row_number()) %>%
  as_tsibble(index = data_index)

cricket_data <- cricket_tsibble %>%
  mutate(
    field = if_else(fielding_wckts == 0, "0", "1+"),
    dot = if_else(dot_balls == 0, "no dot balls", ">0 dot balls"),
    lag_field = lag(field),
    lag_dot = lag(dot)
  ) %>%
  filter(lag_field != 0, lag_dot != 0) 

cricket_data$lag_field <- factor(cricket_data$field, levels = c("0", "1+"))

# filter(fielding_wckts %in% c(0,1)) %>%
#
cricket_data %>%
  filter(over!=1) %>%
  prob_plot("over", "lag_field",
            hierarchy_model,
            response = "run_rate",
            plot_type = "quantile",
            symmetric = FALSE,
            quantile_prob = c(0.25, 0.5, 0.75)) +
  #ggtitle("(b) Runs per over across overs faceted by number of wickets in previous over") +
  ylab("runs per over")  +
  xlab("number of wickets in previous over") +
  ggtitle("b") +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.ticks = element_blank(), legend.background = element_blank(),
        legend.key = element_blank(), panel.background = element_blank(), strip.background = element_blank(),
        plot.background = element_blank(), complete = TRUE, panel.grid.major = element_line(colour = "#E0E0E0"),
        panel.border = element_rect(colour = "#E0E0E0", fill = NA))

