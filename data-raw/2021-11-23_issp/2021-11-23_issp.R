library(countrycode) # Convert country codes
library(dplyr) # Data wrangling
library(haven) # Import stata files
library(magrittr) # Pipe
library(wbstats) # Import World Bank Data

# Load functions ---------------------------------------------------------------

# Create function to recode political participation items
# Recode items as binary (0/1)

recode_polpart <- function(var){
  newvar <- NA_real_
  newvar[var == 1] <- 1 # Done in the last year = Done
  newvar[var %in% c(2, 3, 4)] <- 0 # Other = Not done
  newvar
}

# Political participation battery in the ISSP ----------------------------------

# V17 - Q13 Political actions: sign a petition
# V18 - Q14 Political actions: boycott certain products
# V19 - Q15 Political actions: take part in a demonstration
# V20 - Q16 Political actions: attend political meeting or rally
# V21 - Q17 Political actions: contact a politician
# V22 - Q18 Political actions: donate money or raise funds
# V23 - Q19 Political actions: contact media
# V24 - Q20 Political actions: express views on the internet

# Other variables --------------------------------------------------------------

# V49 - Q45 Agree: most of the time we can trust people in government
# C_ALPHAN: country code
# DATEYR: year of the survey
# WEIGHT: weight

issp_raw <- read_dta("data-raw/2021-11-23_issp/ZA6670_v2-0-0.dta") 

issp <- issp_raw %>%
  mutate(
    across(.cols = V17:V24,
           .fns = recode_polpart)
  ) %>%
  rename(petition = V17,
         boycott = V18,
         demo = V19,
         meeting = V20,
         contpol = V21,
         donate = V22,
         contmed = V23,
         expint = V24, 
         ccode = C_ALPHAN,
         year = DATEYR) %>%
  mutate(weight = as.numeric(WEIGHT),
         ccode = case_when(ccode == "GB-GBN" ~ "GB",
                           TRUE ~ as.character(ccode)),
         country = countrycode(ccode,
                               origin = "iso2c",
                               destination = "country.name"),
         trust_gov = case_when(
           V49 %in% c(1, 2)    ~ 1, # (Strongly) agree
           V49 %in% c(3, 4, 5) ~ 0  # Other
         )) %>%
  group_by(country) %>%
  mutate(year = round(mean(year))) %>%
  summarize(
    ccode = first(ccode),
    year = first(year),
    across(
    .cols = c(petition,
              boycott,
              demo,
              meeting,
              contpol,
              donate,
              contmed,
              expint,
              trust_gov),
    .fns = ~ weighted.mean(x = .x, w = weight, na.rm = TRUE) * 100)
  )


# Import World Bank Data --------------------------------------------------

# NY.GDP.PCAP.PP.KD: GDP per capita, PPP (constant 2017 international $) 

wb_raw <- wb_data(indicator = c("NY.GDP.PCAP.PP.KD"), 
              start_date = 2013,
              end_date = 2016)

wb <- wb_raw %>%
  select(iso2c, date, NY.GDP.PCAP.PP.KD) %>%
  rename(ccode = iso2c, 
         year = date,
         gdp = NY.GDP.PCAP.PP.KD)


# Import V-Dem Data -------------------------------------------------------

# v2x_polyarchy: Electoral democracy index
# v2x_corr: Corruption index

vdem_raw <- readRDS("data-raw/2021-11-23_issp/V-Dem-CY-Core-v11.1.rds")

vdem <- vdem_raw %>%
  select(country_text_id, year, v2x_polyarchy, v2x_corr) %>%
  rename(ccode = country_text_id,
         dem_index = v2x_polyarchy,
         corruption = v2x_corr) %>%
  mutate(ccode = countrycode(ccode,
                             origin = "iso3c",
                             destination = "iso2c"))

# Merge datasets ----------------------------------------------------------

master <- issp %>%
  left_join(wb, by = c("ccode", "year")) %>%
  left_join(vdem, by = c("ccode", "year"))


# Add variable labels -----------------------------------------------------

attr(master$country, "label") <- "Country"
attr(master$ccode, "label") <- "Country code, ISO2C"
attr(master$year, "label") <- "Year"
attr(master$petition, "label") <- "Sign petition, country mean"
attr(master$boycott, "label") <- "Boycott certain products, country mean"
attr(master$demo, "label") <- "Take part in a demonstration, country mean"
attr(master$meeting, "label") <- "Attend political meeting, country mean"
attr(master$contpol, "label") <- "Contact a politician, country mean"
attr(master$donate, "label") <- "Donate money or raise funds, country mean"
attr(master$contmed, "label") <- "Contact media, country mean"
attr(master$expint, "label") <- "Express views on the internet, country mean"
attr(master$trust_gov, "label") <- "Trust in government"
attr(master$dem_index, "label") <- "Electoral democracy index"
attr(master$corruption, "label") <- "Corruption index"


# Save dataset ------------------------------------------------------------

saveRDS(master, "data/2021-11-23_issp.rds")
