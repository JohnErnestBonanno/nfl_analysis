library(tidyverse)
library(nflverse)

## Core Components of tidyverse

# dplyr: “dplyr provides a grammar of data manipulation, providing a consistent set of verbs that solve the most common data manipulation challenges.”

# tidyr: “tidyr provides a set of functions that help you get to tidy data. Tidy data is data with a consistent form: in brief, every variable goes in a column, and every column is a variable.”

# readr: “readr provides a fast and friendly way to read rectangular data (like csv, tsv, and fwf). It is deigned to flexibly parse many types of data found in the wild, while still cleanly failing when data unexpectedly changes.”

# purrr: “purrr enhances R’s functional programming (FP) toolkit by providing a complete and consistent set of tools for working with functions and vectors. Once you master the basic concepts, purrr allows you to replace many for loops with code that is easier to write and more expressive.”

# tibble: “tibble is a modern re-imagining of the data frame, keeping what time has proven to be effective, and throwing out what it has not. Tibbles are data.frames that are lazy and surly; they do less and complain more forcing you to confront problems earlier, typically leading to cleaner, more expressive code.”

# stringr: “stringr provides a cohesive set of functions designed to make working with strings as easy as possible. It is built on top of stringi, which uses the ICU C library to provide fast, correct implementations of common string manipulations.”

# forcats: “forcats provides a suite of useful tools that solve common problems with factors. R uses factors to handle categorical variables, variables that have a fixed and known set of possible values.”

# ggplot2: “ggplot2 is a system for declaratively creating graphics, based on The Grammar of Graphics. You provide the data, tell ggplot2 how to map the variables to aesthetics, what graphical primitives to use, and it takes care of the details”



pbp <- nflreadr::load_pbp(2022) %>%
  filter(posteam == "PHI" & rush == 1) %>%
  group_by(rusher) %>%
  summarize(success_rate = mean(success))


pbp

# 2.5 Working With NFL Data and the dplyr Verbs

# 2.5.1 NFL Data and the filter() verb


pbp <- nflreadr::load_pbp(2022)
ne_offense <- pbp |> filter(posteam == "NE")

ne_offense

colnames(ne_offense)

afc_east <- pbp %>%
  filter(posteam %in% c("NE", "MIA", "NYJ", "BUF"))

afc_east

library(vroom)

time_in_pocket <- vroom("http://nfl-book.bradcongelio.com/qb-tip")

time_in_pocket <- time_in_pocket |>
  arrange(avg_time_to_throw) |>
  mutate(player = factor(player, levels = player))

fig <- ggplot(data = time_in_pocket, aes(x = player, y = avg_time_to_throw)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(fig)

time_in_pocket_no_afcn <- time_in_pocket %>%
  filter(!team_name %in% c("PIT", "BAL", "CIN", "CLE"))

time_in_pocket_no_afcn %>%
  summarize(avg_time_to_throw_no_afc = mean(avg_time_to_throw))

time_in_pocket %>%
  filter(team_name %in% c("PIT", "BAL", "CIN", "CLE")) %>%
  summarize(avg_time_to_throw_afc = mean(avg_time_to_throw))

# Multiple Filters

multiple_filters <- pbp %>%
  filter(down == 2 & play_type == "pass" &
           qb_hit == 1 &
           complete_pass == 1 & 
           air_yards >= ydstogo)

multiple_filters

total_yards <- pbp %>%
  filter(season_type == "REG" & !is.na(down)) %>% #Remove 2 point conversions
  summarize(yards = sum(yards_gained, na.rm = TRUE))

total_yards

total_yards_by_down <- pbp %>%
  filter(season_type == "REG" & !is.na(down)) %>% 
  group_by(down) %>%
  summarize(yards = sum(yards_gained, na.rm = TRUE))

total_yards_by_down

yrd_per_down = ggplot(total_yards_by_down,
                      aes(x=down,y=yards)) +
  geom_col() + 
  labs(title = "Yards Per Down: NFL 2022 Regular Season")
  
yrd_per_down

#Group_BY

rushing_success_grouped <- pbp %>%
  filter(play_type == "run" & play == 1 #Actually happens - no penality
         & !is.na(down)
         & season_type == "REG"
         ) %>%
  group_by(posteam) %>%
  summarize(
    success_rate = mean(success),
    total_yards = sum(yards_gained)
  ) %>% 
  arrange(-success_rate)

rushing_success_grouped %>%
  slice(1:10)

rushing_yrds <- ggplot(rushing_success_grouped,
                       aes(x = reorder(posteam, -total_yards), y = total_yards)) +
  geom_col() +
  labs(title = "Rushing Yards Per Team: 2022 Reg Season",
       x = "Team",
       y = "Total Yards")


# ggplot notes:
#   * order x_axis via reorder(x_axis,y=what you want to order)
#   * use labs for editting

rushing_yrds
  
running_back_success_min <- pbp %>%
  filter(play_type == "run" & play == 1 &
           !is.na(down) &
           !is.na(rusher)) %>%
  group_by(rusher) %>%
  summarize(success_rate = mean(success),
            n_rushes = n()) %>%
  filter(n_rushes >= 200) %>%
  arrange(-success_rate)

running_back_success_min %>%
  slice(1:10)

#Load Multiple Seasons

seasons_2010_2022 <- nflreadr::load_pbp(2010:2022)

yac_seasons <- seasons_2010_2022 %>%
  filter(season_type == "REG" &
           !is.na(posteam) &
           !is.na(yards_after_catch)) %>%
  group_by(season, posteam) %>%
  summarize(total_yac = sum(yards_after_catch, na.rm = TRUE)) %>%
  arrange(-total_yac)

yac_seasons

#Summarize Verb

team_aggressiveness <- pbp %>%
  filter(play_type == "pass" &
           down == 3 &
           ydstogo <= 5 &
           play == 1) %>%
  group_by(posteam) %>%
  summarize(total = n(),
            aggressiveness = sum(air_yards >= ydstogo, na.rm = TRUE),
            percentage = aggressiveness / total) %>%
  arrange(-percentage)

team_aggressiveness %>%
  slice(1:10)

#Mutate

#completion percent over expected
airyards_cpoe_mutate <- pbp %>%
  filter(!is.na(cpoe)) %>%
  mutate(
    ay_distance = case_when(
      air_yards < 0 ~ "Negative",
      air_yards >= 0 & air_yards < 10 ~ "Short",
      air_yards >= 10 & air_yards < 20 ~ "Medium",
      air_yards >= 20 ~ "Deep")) %>%
  group_by(passer, ay_distance) %>%
  summarize(avg_cpoe = mean(cpoe))

airyards_cpoe_mutate

airyards_cpoe_mutate %>%
  filter(ay_distance == "Medium") %>%
  arrange(-avg_cpoe) %>%
  slice(1:10)

airyards_cpoe_mutate %>%
  ungroup() %>%
  filter(ay_distance == "Medium") %>%
  arrange(desc(avg_cpoe)) %>%
  slice_head(n = 10)

#2.6 Core Skills for Tidy Data

# Boom vs. Bust:
#   * Boom%: correlates to any pass attempt by the quarterback that resulted in an expected points add (EPA) of at least 1
#   * Bust%: any pass attempt that resulted in an EPA of at least -1 

sis_data <- vroom("http://nfl-book.bradcongelio.com/sis-bb")

sis_data <- sis_data %>%
  janitor::clean_names() %>%
  select(player, team, att, boom_percent, bust_percent) %>%
  na.omit(sis_data)
  #filter(!is.na(team))

sis_data <- sis_data %>%
  mutate(
    boom_percent = readr::parse_number(boom_percent) / 100,
    bust_percent = readr::parse_number(bust_percent) / 100
  )

sis_data <- sis_data %>%
  filter(att >= 200) %>%
  mutate(difference = boom_percent - bust_percent)

view(sis_data)
glimpse(sis_data)

# Best Practice on data summary. Use both:
#   * view() - scrollable spreadsheet
#   * glimpse() - sample values w/ data_type


sis_data <- sis_data %>%
  mutate(team = case_when(
    player == "Baker Mayfield" ~ "Rams",
    TRUE ~ team))

sis_data %>%
  filter(player == "Baker Mayfield")

#Merging Data Sets

teams <- nflreadr::load_teams(current = TRUE) %>%
  select(team_abbr, team_nick, team_name, team_color, team_color2)

glimpse(teams)

sis_data <- sis_data %>%
  left_join(teams, by = c("team" = "team_nick"))

glimpse(sis_data)
