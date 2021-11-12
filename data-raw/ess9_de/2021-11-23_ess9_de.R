library(haven)
ess <- read_dta("data-raw/ess9_de/ESS9DE.dta", 
               encoding = "latin1")
saveRDS(ess, file = "data/2021-11-23_ess9_de.rds")
