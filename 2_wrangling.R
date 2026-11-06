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
