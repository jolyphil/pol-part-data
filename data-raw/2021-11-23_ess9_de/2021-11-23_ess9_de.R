library(dplyr)
library(haven)
library(magrittr)

# Load functions ---------------------------------------------------------------

recode_polpart <- function(var){
  newvar <- as.character(NA)
  newvar[var == 1] <- "Yes"
  newvar[var == 2] <- "No"
  newvar <- factor(newvar)
  newvar
}

# Load data --------------------------------------------------------------------

ess <- read_dta("data-raw/2021-11-16_ess9_de/ESS9DE.dta", 
               encoding = "latin1")

# Political participation battery in the ESS -----------------------------------
# vote     Voted last national election
# contplt  Contacted politician or government official last 12 months
# wrkprty  Worked in political party or action group last 12 months
# wrkorg   Worked in another organisation or association last 12 months
# badge    Worn or displayed campaign badge/sticker last 12 months
# sgnptit  Signed petition last 12 months
# pbldmn   Taken part in lawful public demonstration last 12 months
# bctprd   Boycotted certain products last 12 months
# pstplonl Posted or shared anything about politics online last 12 months

# Other variables --------------------------------------------------------------
# gndr     Gender
# agea     Age of respondent, calculated
# netustm  Internet use, how much time on typical day, in minutes
# hinctnta

# Coding of 'region' -----------------------------------------------------------
# DE1	Baden-Württemberg
# DE2	Bayern
# DE3	Berlin
# DE4	Brandenburg
# DE5	Bremen
# DE6	Hamburg
# DE7	Hessen
# DE8	Mecklenburg-Vorpommern
# DE9	Niedersachsen
# DEA	Nordrhein-Westfalen
# DEB	Rheinland-Pfalz
# DEC	Saarland
# DED	Sachsen
# DEE	Sachsen-Anhalt
# DEF	Schleswig-Holstein
# DEG	Thüringen

ess <- ess %>%
  mutate(
    across(.cols = c(contplt,
                     wrkprty,
                     wrkorg,
                     badge,
                     sgnptit,
                     pbldmn,
                     bctprd,
                     pstplonl),
           .fns = recode_polpart)
  ) %>%
  mutate(vote = case_when(vote == 1 ~ "Yes",
                          vote == 2 ~ "No",
                          vote == 3 ~ "Not eligible"),
         vote = factor(vote, levels = c("No", "Yes", "Not eligible")),
         gndr = case_when(gndr == 1 ~ "Male",
                          gndr == 2 ~ "Female"),
         gndr = factor(gndr, levels = c("Male", "Female")),
         agea = as.numeric(agea),
         netustm = if_else(netustm > quantile(netustm, 
                                              probs = 0.99,
                                              na.rm = TRUE),
                           NA_real_, # Exclude values > 99th percentile
                           as.numeric(netustm)),
         lrscale = as.numeric(lrscale),
         region_de = if_else(
           region %in% c("DE3", "DE4", "DE8", "DED", "DEE", "DEG"), 
           "East", 
           "West"),
         region_de = factor(region_de, levels = c("West", "East")),
         hinctnta = as.numeric(hinctnta),
         edu = case_when(eisced %in% c(1:2) ~ "Low", 
                         eisced %in% c(3:4) ~ "Middle",
                         eisced %in% c(5:7) ~ "High"),
         edu = ordered(edu, 
                       levels = c("Low", "Middle", "High"))
  ) %>%
  select(
    gndr,
    agea,
    region_de, 
    netustm,
    lrscale,
    vote,
    contplt,
    wrkprty,
    wrkorg,
    badge,
    sgnptit,
    pbldmn,
    bctprd,
    pstplonl,
    hinctnta,
    edu
  )




# Save data --------------------------------------------------------------------

saveRDS(ess, file = "data/2021-11-23_ess9_de.rds")
