library(tidyverse)
library(nflfastR)
library(vip)
library(ggimage)
library(ggplot2)
library(gt)
library(nflreadr)
library(dplyr)
library(nflplotR)
library(ggrepel)


train_data <- data.frame()
for (year in 2007:2021) {
  pbp_load <- load_pbp(year)
  add_train <- calculate_player_stats(pbp_load, weekly = FALSE) 
  add_train <- add_train %>%
    filter(position == "QB") %>%
    filter(attempts >= 100) %>%
    mutate(cmppct = completions/attempts * 100, tdpct = passing_tds/attempts * 100, intpct = interceptions/attempts * 100, skpct = sacks/(attempts + sacks) * 100, epaatt = passing_epa/attempts, anyatt = (passing_yards - sack_yards + 20 * passing_tds - 45 * interceptions)/(attempts + sacks), passrtg = ((completions/attempts - 0.3) * 5 + (passing_yards/attempts - 3) * 0.25 + (passing_tds/attempts) * 20 + 2.375 - (interceptions/attempts * 25))/6 * 100) %>%
    select(name = player_display_name, team = recent_team, cmppct, tdpct, intpct, skpct, pacr, adjepacpoe = dakota, epaatt, anyatt, passrtg)  
  add_train <- mutate(add_train, year = year)
  train_data <- bind_rows(add_train, train_data)
}

write_csv(train_data, "train_data.csv")

# manually input mvp binary variable, 1 for mvp in specific year 0 for not

train_data <- read_csv("train_data.csv")

# only realization now that pacr was only measured in my data starting from 2007 so the train data will change to 2007 to 2021

train_data <- train_data %>%
  filter(year != 2012)

train_data[3:11] <- as.data.frame(scale(train_data[,c(3:11)]))

train_data <- 

train_reg <- glm(mvp ~ cmppct + tdpct + intpct + skpct + pacr + adjepacpoe + epaatt + anyatt + passrtg, data = train_data, family = binomial)

train_data <- train_data %>%
  mutate(prediction = predict(train_reg, train_data, type = "response")) %>%
  group_by(year) %>%
  mutate(mvp_prob = prediction/sum(prediction)) %>%
  mutate(mvp_won = ifelse(mvp == 1, "WON", "")) %>%
  arrange(-mvp_prob) %>%
  ungroup()

test_pbp <- load_pbp(2022)
test_data <- calculate_player_stats(test_pbp, weekly = FALSE) 

test_data <- test_data %>%
  filter(position == "QB") %>%
  filter(attempts >= 100) %>%
  mutate(cmppct = completions/attempts * 100, tdpct = passing_tds/attempts * 100, intpct = interceptions/attempts * 100, skpct = sacks/(attempts + sacks) * 100, epaatt = passing_epa/attempts, anyatt = (passing_yards - sack_yards + 20 * passing_tds - 45 * interceptions)/(attempts + sacks), passrtg = ((completions/attempts - 0.3) * 5 + (passing_yards/attempts - 3) * 0.25 + (passing_tds/attempts) * 20 + 2.375 - (interceptions/attempts * 25))/6 * 100) %>%
  select(name = player_display_name, team = recent_team, cmppct, tdpct, intpct, skpct, pacr, adjepacpoe = dakota, epaatt, anyatt, passrtg)  
test_data <- mutate(test_data, year = 2022)

test_data <- test_data %>%
  mutate(mvp = ifelse(name == "Patrick Mahomes", 1, 0))

test_data[3:11] <- as.data.frame(scale(test_data[,c(3:11)]))

test_data <- test_data %>%
  mutate(prediction = predict(train_reg, test_data, type = "response")) %>%
  group_by(year) %>%
  mutate(mvp_prob = prediction/sum(prediction)) %>%
  mutate(mvp_won = ifelse(mvp == 1, "WON", "")) %>%
  arrange(-mvp_prob) %>%
  ungroup()

train_data <- train_data %>%
  mutate(mvp_prob = round(mvp_prob, 3)) %>%
  select(name, team, year, mvp_prob, mvp_won)

test_data <- test_data %>%
  mutate(mvp_prob = round(mvp_prob, 3)) %>%
  select(name, team, year, mvp_prob, mvp_won)

summary(train_reg)


t2007 = train_data %>% filter(year == 2007) %>% filter(row_number() <= 6)
t2008 = train_data %>% filter(year == 2008) %>% filter(row_number() <= 6)
t2009 = train_data %>% filter(year == 2009) %>% filter(row_number() <= 6)
t2010 = train_data %>% filter(year == 2010) %>% filter(row_number() <= 6)
t2011 = train_data %>% filter(year == 2011) %>% filter(row_number() <= 6)
t2013 = train_data %>% filter(year == 2013) %>% filter(row_number() <= 6)
t2014 = train_data %>% filter(year == 2014) %>% filter(row_number() <= 6)
t2015 = train_data %>% filter(year == 2015) %>% filter(row_number() <= 6)
t2016 = train_data %>% filter(year == 2016) %>% filter(row_number() <= 6)
t2017 = train_data %>% filter(year == 2017) %>% filter(row_number() <= 6)
t2018 = train_data %>% filter(year == 2018) %>% filter(row_number() <= 6)
t2019 = train_data %>% filter(year == 2019) %>% filter(row_number() <= 6)
t2020 = train_data %>% filter(year == 2020) %>% filter(row_number() <= 6)
t2021 = train_data %>% filter(year == 2021) %>% filter(row_number() <= 6)
t2022 = test_data %>% filter(year == 2022) %>% filter(row_number() <= 6)


t2007 %>% gt() %>% # change the t value to the corresponding table above to output gt table and for 2022, add the title and subtitle
  cols_align(
    align = "center",
    columns = c(name, team, year, mvp_prob, mvp_won)
  ) %>%
  data_color(
    columns = mvp_prob,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    name = md("**Player**"),
    team = md("**Team**"),
    year = md("**Season**"),
    mvp_prob = md("**MVP Probability**"),
    mvp_won = md("**MVP Result**")
  ) 


