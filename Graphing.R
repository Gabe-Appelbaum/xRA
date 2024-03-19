library(tidyverse)
library(mlbplotR)
library(ggimage)
library(plotly)
library(gt)
library(gtExtras)
library(ggtext)
library(gridExtra)
library(grid)
library(scales)
library(patchwork)

# bar charts of run value section ----

# read in the run values first
# this function reads in and retags rv in a way that is graph friendly
read_rv <- function(rv_year){
  read_csv(rv_year) %>%
    mutate(rv_event = factor(rv_event, levels = rv_event[order(mean)]),
           rv_event = recode(rv_event, 
                             popup = "Pop-up", 
                             so = "Strike 3",
                             soft_fb = "Soft Fly Ball",
                             soft_gb = "Soft Ground ball",
                             nd_strike = "Non Decisive Strike",
                             hard_gb = "Hard Ground Ball",
                             nd_ball = "Non Decisive Ball",
                             soft_ld = "Soft Line Drive",
                             ubb = "Ball 4",
                             hard_ld = "Hard Line Drive",
                             hard_fb = "Hard Fly Ball"))
}

rv <- read_rv("data/rv 18_23.csv")

rv21 <- read_rv("data/rv 21.csv")

rv22 <- read_rv("data/rv 22.csv")

rv23 <- read_rv("data/rv 23.csv")

# here's a bar chart for the full 6 year period
rv18_23_bars <- ggplot(rv, aes(y = rv_event, x = mean, fill = mean)) +
  geom_col(show.legend = FALSE) + 
  scale_fill_gradient(low = "skyblue", high = "blue") +
  geom_label(aes(label = mean), 
             fill = "white", 
             label.padding = unit(.18, "lines"),
             size = 4) +
  theme_classic() + 
  labs(title = "Value of events from 2018-2023", 
       y = "", 
       x = "Mean Run Value", 
       fill = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        text = element_text(size = 15),
        axis.title.x = element_text(size = 15))

rv18_23_bars

# here's a function for making a chart for each individual block
rv_bar_charts <- function(rv, year){
  ggplot(rv, aes(mean, rv_event, fill = mean)) +
    geom_col(show.legend = FALSE) + 
    scale_fill_gradient(low = "skyblue", high = "blue") +
    geom_label(aes(label = mean), 
               fill = "white", 
               label.padding = unit(.18, "lines"),
               size = 4) +
    theme_classic() + 
    labs(title = paste("Value of events in", year), 
         y = "", 
         x = "Mean Run Value", 
         fill = "") +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          text = element_text(size = 15),
          axis.title.x = element_text(size = 15))
}

rv21_bars <- rv_bar_charts(rv21, 2021)
rv22_bars <- rv_bar_charts(rv22, 2022)
rv23_bars <- rv_bar_charts(rv23, 2023)

ggsave("plots/rv18_23bars.jpg", plot = rv18_23_bars, width = 9, height = 9, units = "in", device = "jpeg")

ggsave("plots/rv23_bars.jpg", plot = rv23_bars, width = 9, height = 9, units = "in", device = "jpeg")

grid.arrange(rv18_23_bars, rv23_bars, ncol=2)
ggsave("plots/Double bar charts.png", plot = grid.arrange(rv18_23_bars, rv23_bars, ncol=2), width = 14, height = 5)
#----

# reading in xRA and calculating xRA+ ----

# this is a function that reads in the xRA, calculates xRA+, and sorts the data
read_xRA <- function(xRA) {
  read_csv(xRA) %>%
    arrange(desc(xRA_100)) %>%
    mutate("xRA+" = round(100 + 10 * scale(xRA_100)),
           xRA_100 = round(xRA_100, 2),
           "MLB Rank" = row_number())
}

xRA21 <- read_xRA("data/player xRA 2021.csv")
xRA22 <- read_xRA("data/player xRA 2022.csv")
xRA23 <- read_xRA("data/player xRA 2023.csv")
#----

# headshot leaderboards ----

# load in headshots
headshot <- load_headshots() %>%
  select(savant_id, espn_headshot)

# function to create leaderboards
headshot_leaders <- function(xRA, headshot, num1, num2){

  xRA <- left_join(xRA, headshot, by = join_by(batter == savant_id))
  
  xRA %>%
    select("MLB Rank", "Headshot" = espn_headshot, "Player" = Name, 'xRA/100 PA' = xRA_100, 'xRA+' = 'xRA+') %>%
    arrange(desc(`xRA/100 PA`)) %>%
    slice(num1:num2) %>%
    gt() %>%  
    gt_theme_excel() %>%
    gt_img_rows(columns = "Headshot", height = 100) %>%
    cols_align(align = "center") %>%
    cols_width(
      everything() ~ px(150)
    ) %>%
    tab_header(
      title = "Expected Run Average in 2022"
    ) %>%
    tab_footnote(
      footnote = "Data: baseballr and Fangraphs"
    ) %>%
    tab_footnote(
      footnote = "xRA+ is calculated by setting the mean to 100 and a standard deviation to 10, so an xRA+ of 130 is 3 standard deviations above the mean"
    ) %>%
    tab_options(
      heading.title.font.size = px(38),
      column_labels.font.size = px(21),
      table.font.size = px(20)
    )
}

# top 10 and next 10 for 2023, the numbers are what rank of players to get
headshots_23_10 <- headshot_leaders(xRA23, headshot, 1, 10)
gtsave(headshots_23_10,"plots/top_10_xRA_with_headshots_2023.png", expand = 50)

headshots_23_next10 <- headshot_leaders(xRA23, headshot, 11, 20)
gtsave(headshots_23_next10,"plots/numbers 11_20_xRA_with_headshots_2023.png", expand = 50)

headshots_22 <- headshot_leaders(xRA22, headshot, 1, 10)
gtsave(headshots_22, "plots/xRA_with_headshots_2022.png", expand = 50)
#----

# xRA year to year scatterplots ----

xRA_scatter <- function(xRA_pred, xRA_target, pred_yr, targ_yr, corr){
  ggplot(inner_join(xRA_pred, xRA_target, by = join_by(batter)), aes(xRA_100.x,xRA_100.y)) +
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) +
    theme_classic() + 
    labs(x = paste(pred_yr, "xRA per 100 PA"), 
         y = paste(targ_yr, "xRA per 100 PA"),
         caption = paste("The r-squared between", pred_yr, "and", targ_yr, "xRA is", corr)) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    coord_fixed() +
    ylim(-5, 10) +
    xlim(-5, 10)
}

xRA_22_23 <- xRA_scatter(xRA22, xRA23, 2022, 2023, 0.485)
xRA_21_22 <- xRA_scatter(xRA21, xRA22, 2021, 2022, 0.487)
yr_to_yr_sticky <- grid.arrange(xRA_22_23, xRA_21_22, ncol=2, top=textGrob('Testing xRA "stickiness" by checking year to year r-squared'))
ggsave("plots/yr to yr stickiness.png", plot = yr_to_yr_sticky, width = 14, height = 5)
#----

# predictiveness with other metrics ----

# this section reads in xRA again (very dumb and inefficient) and merges it with other stats
# to create a correlation matrix year to year
{
xRA_23 <- read_csv("data/player xRA 2023.csv") %>%
  select(Name, 'xRA_23' = xRA_100)
xRA_22 <- read_csv("data/player xRA 2022.csv") %>%
  select(Name, 'xRA_22' = xRA_100)
xRA_21 <- read_csv("data/player xRA 2021.csv") %>%
  select(Name, 'xRA_21' = xRA_100)

stats_23 <- read_csv("data/xwOBA_wRC+_2023.csv") %>%
  rename('xwOBA_23' = xwOBA, 'wRC+_23' = 'wRC+', 'wOBA_23' = wOBA)
stats_22 <- read_csv("data/xwOBA_wRC+_2022.csv") %>%
  rename('xwOBA_22' = xwOBA, 'wRC+_22' = 'wRC+', 'wOBA_22' = wOBA)
stats_21 <- read_csv("data/xwOBA_wRC+_2021.csv") %>%
  rename('xwOBA_21' = xwOBA, 'wRC+_21' = 'wRC+', 'wOBA_21' = wOBA)

predict_23 <- {list(xRA_23, xRA_22, stats_23, stats_22) %>%
    reduce(inner_join) %>%
    select(-Name) %>%
    cor() %>%
    as.data.frame() %>%
    select(xRA_23, xwOBA_23, 'wRC+_23', wOBA_23) %>%
    slice(2,6:8) %>%as
    rownames_to_column("predictor") %>%
    rename("xRA 23" = xRA_23, "xwOBA 23" = xwOBA_23,"wRC+ 23" = `wRC+_23`, "wOBA 23" = "wOBA_23") %>%
    mutate(predictor = recode(predictor, xRA_22 = "xRA 22", xwOBA_22 = "xwOBA 22", `wRC+_22` = "wRC+ 22", wOBA_22 = "wOBA 22")) %>%
    mutate(across(where(is.numeric), round, 2))}

predict_22 <- {list(xRA_22, xRA_21, stats_22, stats_21) %>%
    reduce(inner_join) %>%
    select(-Name) %>%
    cor() %>%
    as.data.frame() %>%
    select(xRA_22, xwOBA_22, 'wRC+_22', wOBA_22) %>%
    slice(2,6:8) %>%
    rownames_to_column("predictor") %>%
    rename("xRA 22" = xRA_22, "xwOBA 22" = xwOBA_22,"wRC+ 22" = `wRC+_22`, "wOBA 22" = "wOBA_22") %>%
    mutate(predictor = recode(predictor, xRA_21 = "xRA 21", xwOBA_21 = "xwOBA 21", `wRC+_21` = "wRC+ 21", wOBA_21 = "wOBA 21")) %>%
    mutate(across(where(is.numeric), round, 2))}
}

predict_table <- function(table, yr1, yr2){
  table %>% 
    gt() %>%
    cols_align(align = "center") %>%
    cols_width(
      everything() ~ px(150)
    ) %>%
    tab_header(
      title = paste(yr1, "-", yr2)
    ) %>%
    tab_footnote(
      footnote = "Data: baseballr and Fangraphs"
    ) %>%
    tab_options(
      heading.title.font.size = px(38),
      column_labels.font.size = px(21),
      table.font.size = px(20)
    ) %>% 
    tab_style(
      style = cell_fill(color = '#90EE90'),
      locations = cells_body(
        columns = c(2:3), 
        rows = c(1:2)
      )
    )
}

pred_21_22 <- predict_table(predict_22, 2021, 2022)
pred_22_23 <- predict_table(predict_23, 2022, 2023)

pred_gr <- gt_group(pred_21_22, pred_22_23)
gtsave(pred_gr,
       "plots/predict groups.png", 
       expand = 50)
# ----