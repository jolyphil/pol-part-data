library(countrycode) # Convert country codes
library(dplyr) # Data wrangling
library(haven) # Import stata files
library(magrittr) # Pipe
library(wbstats) # Import World Bank Data

# Load functions ---------------------------------------------------------------

# Create function to recode political participation items
# Recode items as binary (0/1)

recode_polpart_binary <- function(var){
  newvar <- NA_real_
  newvar[var == 1] <- 1 # Done in the last year = Done
  newvar[var %in% c(2, 3, 4)] <- 0 # Other = Not done
  newvar
}

recode_polpart_factor <- function(var){
  newvar <- as.character(NA)
  newvar[var == 0] <- "No"
  newvar[var == 1] <- "Yes"
  newvar <- factor(newvar)
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

# ------------------------------------------------------------------------------


issp_raw <- read_dta("data-raw/2021-11-23_issp/ZA6670_v2-0-0.dta") 

issp <- issp_raw %>%
  mutate(
    across(.cols = V17:V24,
           .fns = recode_polpart_binary),
    polpart = V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24,
    across(.cols = V17:V24,
           .fns = recode_polpart_factor),
  ) %>%
  rename(
    ccode = C_ALPHAN,
    year = DATEYR,
    petition = V17,
    boycott = V18,
    demo = V19,
    meeting = V20,
    contpol = V21,
    donate = V22,
    contmed = V23,
    expint = V24
    ) %>%
  mutate(
    ccode = case_when(ccode == "GB-GBN" ~ "GB",
                           TRUE ~ as.character(ccode)),
         country = countrycode(ccode,
                               origin = "iso2c",
                               destination = "country.name"),
    lrscale = case_when(V48 %in% c(98, 99) ~ NA_real_,
                        TRUE ~ as.numeric(V48)),
    satis_demo = case_when(V62 %in% 0:10 ~ as.numeric(V62)),
    gender = case_when(SEX == 1 ~ "Male",
                       SEX == 2 ~ "Female"),
    gender = factor(gender, levels = c("Male", "Female")),
    age = case_when(AGE == 999 ~ NA_real_,
                    TRUE ~ as.numeric(AGE)),
    edu = case_when(DEGREE %in% c(0,1,2) ~ "Low",
                    DEGREE %in% c(3,4) ~ "Middle",
                    DEGREE %in% c(5,6) ~ "High"),
    edu = factor(edu, levels = c("Low", "Middle", "High")),
    union = case_when(UNION %in% c(1, 2) ~ "Yes",
                      UNION %in% c(0, 3) ~ "No"),
    union = factor(union),
    unemp = case_when(MAINSTAT == 2 ~ "Unemployed",
                      MAINSTAT %in% c(1, 3:9) ~ "Other"),
    unemp = factor(unemp),
    postcommunist = if_else(country %in% c("Croatia",
                                           "Czechia",
                                           "Georgia",
                                           "Hungary",
                                           "Lithuania",
                                           "Poland",
                                           "Russia",
                                           "Slovakia",
                                           "Slovenia"),
                            "yes",
                            "no"),
    postcommunist = factor(postcommunist)
    ) %>%
  select(
    country,
    ccode, 
    year,
    polpart, 
    petition,
    boycott,
    demo,
    meeting,
    contpol,
    donate,
    contmed,
    expint,
    lrscale,
    satis_demo,
    gender,
    age,
    edu,
    unemp,
    union,
    postcommunist
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
         gdp = NY.GDP.PCAP.PP.KD) %>%
  mutate(gdp = gdp / 1000)


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

main <- issp %>%
  left_join(wb, by = c("ccode", "year")) %>%
  left_join(vdem, by = c("ccode", "year"))


# Add variable labels -----------------------------------------------------

attr(main$country, "label") <- "Country"
attr(main$ccode, "label") <- "Country code, ISO2C"
attr(main$year, "label") <- "Year"
attr(main$polpart, "label") <- "Political participation index"
attr(main$petition, "label") <- "Signed a petition"
attr(main$boycott, "label") <- "Boycotted certain products"
attr(main$demo, "label") <- "Took part in a demonstration"
attr(main$meeting, "label") <- "Attended political meeting"
attr(main$contpol, "label") <- "Contacted a politician"
attr(main$donate, "label") <- "Donated money or raised funds"
attr(main$contmed, "label") <- "Contacted media"
attr(main$expint, "label") <- "Expressed views on the internet"
attr(main$lrscale, "label") <- "Left-right self-placement"
attr(main$satis_demo, "label") <- "Satisfaction with democracy"
attr(main$gender, "label") <- "Gender"
attr(main$age, "label") <- "Age"
attr(main$edu, "label") <- "Level of education"
attr(main$unemp, "label") <- "Unemployed"
attr(main$union, "label") <- "Trade union membership, currently or previously"
attr(main$postcommunist, "label") <- "Postcommunist country"
attr(main$gdp, "label") <- "GDP per capita, PPP (1000$)"
attr(main$dem_index, "label") <- "Electoral democracy index"
attr(main$corruption, "label") <- "Corruption index"


# Save dataset ------------------------------------------------------------

saveRDS(main, "data/2022-01-25_issp.rds")
