library(countrycode)
library(dplyr)
library(readr)
library(readxl)

# Load functions ----------------------------------------------------------

fix_varnames <- function(df){
  colnames(df) <- colnames(df) |> 
    tolower() |> 
    gsub(pattern = "\\s", replacement = "_")
  df
}

percent_as_numeric <- function(var) {
  var |> 
    gsub(pattern = "\\s%", replacement = "") |> 
    as.numeric()
}

# Load data ---------------------------------------------------------------

idea_raw <- read_xls("data-raw/2021-12-07_idea/idea_export_40_61a8e9f34bb55.xls")


# Transform data ----------------------------------------------------------

idea <- idea_raw |> 
  fix_varnames() |> 
  filter(election_type == "Parliamentary") |> 
  group_by(country) |> 
  slice_max(year) |> 
  mutate(
    across(.cols = c(voter_turnout, 
                     vap_turnout, 
                     invalid_votes),
           .fns = percent_as_numeric),
    across(.cols = c(total_vote, 
                     registration, 
                     voting_age_population, 
                     population),
           .fns = parse_number)
  ) |> 
  mutate(
    ccode = countrycode(country,
                        origin = "country.name",
                        destination = "iso3c")
  )

vdem_raw <- readRDS("data-raw/2021-11-23_issp/V-Dem-CY-Core-v11.1.rds")
table(vdem_raw$v2x_regime)
