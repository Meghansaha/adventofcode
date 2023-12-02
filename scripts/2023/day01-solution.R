# Advent of Code - Day 1-------------------------------------------------------

#=============================================================================#
# Library Load-in--------------------------------------------------------------
#=============================================================================#
library(tidyverse)
library(here)
library(glue)
library(stringi)

#=============================================================================#
# Data Load-in-----------------------------------------------------------------
#=============================================================================#
df_input <-
  read_csv(
    here(
      "scripts",
      "2023",
      "inputs",
      "input-01.csv"
    ),
    show_col_types = FALSE
  )

#=============================================================================#
# Part 1 Solution--------------------------------------------------------------
#=============================================================================#
df_input |>
  mutate(
    pulled_number = str_replace_all(input, "[:alpha:]", ""),
    first_number = str_extract(pulled_number, "^.{1}"),
    last_number = str_extract(pulled_number, ".{1}$"),
    final_number = glue("{first_number}{last_number}") |> as.numeric()
  ) |>
  summarize(final_number = sum(final_number)) |>
  pull()

#=============================================================================#
# Part 2 Solution--------------------------------------------------------------
#=============================================================================#
# Create a vector of number maps-----------------------------------------------
vec_digits <-
  c(
    "one" = "1",
    "two" = "2",
    "three" = "3",
    "four" = "4",
    "five" = "5",
    "six" = "6",
    "seven" = "7",
    "eight" = "8",
    "nine" = "9"
  ) 

# Create a "reversed" vector of number maps------------------------------------
rev_vec_digits <-
  c(
    "eno" = "1",
    "owt" = "2",
    "eerht" = "3",
    "ruof" = "4",
    "evif" = "5",
    "xis" = "6",
    "neves" = "7",
    "thgie" = "8",
    "enin" = "9" 
  )

# Create a custom function to find the "first" string match-------------------- 
str_first <- function(string, pattern){
  pattern[as_tibble(str_locate(string, pattern)) |>
            mutate(index = row_number()) |>
            filter(!is.na(start)) |>
            filter(start == min(start)) |>
            pull(index)]
}

# Throw it all together and hope that it works---------------------------------
df_input |>
  # Need this for the custom fx
  rowwise() |>
  # Need to flag if an observation has a "string" number-----------------------
  mutate(
  number_string = any(str_detect(input, c(names(vec_digits), names(rev_vec_digits))))
  ) |>
  # Apply the custom fx only to rows that have the string numbers--------------
  mutate(
  ## Grab the first string number----
    first_string_number = case_when(
    !number_string ~ NA_character_,
    number_string ~ list(str_first(input, names(vec_digits))) |> as.character()),
  ## Grab the last string number by reversing it----
    last_string_number = case_when(
    !number_string ~ NA_character_,
    number_string ~ list(str_first(stri_reverse(input), names(rev_vec_digits))) |> as.character()),
  # Pull the actual numeric digits from the string----
    pulled_digits = str_replace_all(input, "[:alpha:]", ""),
  # Pull the first actual digit----
    first_digit = str_extract(pulled_digits, "^.{1}"),
  # Pull the last actual digit----
    last_digit = str_extract(pulled_digits, ".{1}$"),
  # Finally, calc the "real" numbers by finding the first matches---- 
  ## Forwards---
    first_real_number = str_first(input, c(first_string_number, first_digit)),
  ## And reversed---
    last_real_number = str_first(stri_reverse(input), c(last_string_number, last_digit)),
  # Retrieve the "actual" numbers from the digit vecs-----
    first_real_number = case_when(
    first_real_number %in% names(vec_digits) ~ vec_digits[first_real_number],
    .default = first_real_number),
    last_real_number = case_when(
    last_real_number %in% names(rev_vec_digits) ~ vec_digits[as.numeric(rev_vec_digits[last_real_number])],
    .default = last_real_number),
  # Glue it together----
    final_number = glue("{first_real_number}{last_real_number}") |> as.numeric()
  ) |>
    ungroup() |>
  # Add it up----
    summarize(final_number = sum(final_number)) |>
  # spit it out----
    pull()
