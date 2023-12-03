# Advent of Code - Day 2-------------------------------------------------------

#=============================================================================#
# Library Load-in--------------------------------------------------------------
#=============================================================================#
library(tidyverse)
library(here)

#=============================================================================#
# Data Load-in-----------------------------------------------------------------
#=============================================================================#
df_input <- 
  read_csv(
    here(
      "scripts",
      "2023",
      "inputs",
      "input-02.csv"
    ),
    show_col_types = FALSE
  )

#=============================================================================#
# Part 1 Solution--------------------------------------------------------------
#=============================================================================#
df_input |>
  rowwise() |>
  # Pull out the max numbers for each color in each game
  mutate(
    game_id = str_extract(input, "Game \\d+"),
    game_id = str_replace_all(game_id, "[:alpha:]", ""),
    red_ns = str_extract_all(input, " \\d+ red"),
    red_max = max(as.numeric(str_replace_all(red_ns, "[:alpha:]", ""))),
    blue_ns = str_extract_all(input, " \\d+ blue"),
    blue_max = max(as.numeric(str_replace_all(blue_ns, "[:alpha:]", ""))),
    green_ns = str_extract_all(input, " \\d+ green"),
    green_max = max(as.numeric(str_replace_all(green_ns, "[:alpha:]", "")))
    ) |>
      separate_longer_delim(input, ";") |>
  select(game_id, ends_with("_max")) |>
  mutate(across(everything(), ~as.numeric(.))) |>
  ungroup() |>
  # Filter for the max number of colors asked for
  filter(red_max <= 12 &
         blue_max <= 14 &
         green_max <= 13) |>
  # Pull the unique rows as the manip caused dupes
  distinct() |>
  # Add it up
  summarize(game_id = sum(game_id)) |>
  # Spit it out
  pull()

#=============================================================================#
# Part 2 Solution--------------------------------------------------------------
#=============================================================================#
df_input |>
  rowwise() |>
  # Same as above
  mutate(
    game_id = str_extract(input, "Game \\d+"),
    game_id = str_replace_all(game_id, "[:alpha:]", ""),
    red_ns = str_extract_all(input, " \\d+ red"),
    red_max = max(as.numeric(str_replace_all(red_ns, "[:alpha:]", ""))),
    blue_ns = str_extract_all(input, " \\d+ blue"),
    blue_max = max(as.numeric(str_replace_all(blue_ns, "[:alpha:]", ""))),
    green_ns = str_extract_all(input, " \\d+ green"),
    green_max = max(as.numeric(str_replace_all(green_ns, "[:alpha:]", "")))
  ) |>
  separate_longer_delim(input, ";") |>
  select(game_id, ends_with("_max")) |>
  mutate(across(everything(), ~as.numeric(.))) |>
  ungroup() |>
  # Pull the uniques as manip caused dupes
  distinct() |>
  # Calc the powers
  mutate(cubed_total = red_max * blue_max * green_max) |>
  # Add it up
  summarize(cubed_total = sum(cubed_total)) |>
  # Spit it out
  pull()