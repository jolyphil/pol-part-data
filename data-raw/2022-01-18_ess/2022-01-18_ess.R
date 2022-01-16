library(haven)

# Load data --------------------------------------------------------------------

ess <- read_dta("data-raw/2022-01-18_ess/ESS9e03_1.dta", 
                encoding = "latin1")

# Save data --------------------------------------------------------------------

saveRDS(ess, file = "data/2022-01-18_ess.rds")
