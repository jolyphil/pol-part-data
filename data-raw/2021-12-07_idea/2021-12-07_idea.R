library(countrycode)
library(dplyr)
library(readr)
library(readxl)
library(wbstats)

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
  select(country, year, voter_turnout, compulsory_voting, population) |> 
  group_by(country) |> 
  filter(year >= 2010 & year <= 2020) |> 
  slice_max(year) |> 
  ungroup() |> 
  mutate(voter_turnout = percent_as_numeric(voter_turnout),
         population = parse_number(population) / 1000,
         ccode = countrycode(country,
                             origin = "country.name",
                             destination = "iso3c"),
         compulsory_voting = factor(compulsory_voting)
  ) |> 
  rename(turnout = voter_turnout) |> 
  relocate(ccode, .after = country)

# Import World Bank Data --------------------------------------------------

# NY.GDP.PCAP.PP.KD: GDP per capita, PPP (constant 2017 international $) 

wb_raw <- wb_data(indicator = c("NY.GDP.PCAP.PP.KD", "NY.GDP.MKTP.KD.ZG"), 
                  start_date = 2010,
                  end_date = 2020)

wb <- wb_raw %>%
  select(iso2c, date, NY.GDP.PCAP.PP.KD, NY.GDP.MKTP.KD.ZG) |> 
  rename(ccode = iso2c, 
         year = date,
         gdp_cap = NY.GDP.PCAP.PP.KD, 
         gdp_growth = NY.GDP.MKTP.KD.ZG) |> 
  mutate(ccode = countrycode(ccode,
                             origin = "iso2c",
                             destination = "iso3c"), 
         gdp_cap = gdp_cap / 1000)

# V-Dem Data --------------------------------------------------------------
  
vdem_raw <- readRDS("data-raw/2021-12-07_idea/V-Dem-CY-Full+Others-v11.1.rds")
  
vdem <- vdem_raw |> 
  select(country_text_id, 
         year, 
         v2x_polyarchy, 
         v2x_corr, 
         v2x_regime, 
         v2elparlel)  |> 
  rename(ccode = country_text_id,
         dem_index = v2x_polyarchy,
         corruption = v2x_corr,
         regime = v2x_regime,
         elect_system = v2elparlel) |> 
  mutate(regime = case_when(regime == 0 ~ "Closed autocracy",
                            regime == 1 ~ "Electoral autocracy",
                            regime == 2 ~ "Electoral democracy",
                            regime == 3 ~ "Liberal democracy"),
         regime = factor(regime, levels = c("Closed autocracy",
                                            "Electoral autocracy",
                                            "Electoral democracy",
                                            "Liberal democracy")),
         elect_system = case_when(elect_system == 0 ~ "Majoritarian",
                                  elect_system == 1 ~ "Proportional",
                                  elect_system == 2 ~ "Mixed",
                                  elect_system == 3 ~ "Other"),
         elect_system = factor(elect_system, levels = c("Majoritarian",
                                                        "Proportional",
                                                        "Mixed",
                                                        "Other")))


# Merge datasets ----------------------------------------------------------

main <- idea |> 
  left_join(vdem, by = c("ccode", "year")) |> 
  filter(regime == "Electoral democracy" | regime == "Liberal democracy") |> 
  select(-c(regime)) |> 
  left_join(wb, by = c("ccode", "year"))


# Save dataset ------------------------------------------------------------

saveRDS(main, "data/2021-12-07_idea.rds")
