library(dplyr)
library(readxl)

# Load functions ----------------------------------------------------------

state_code <- function(state_name) {
  state_code <- state.abb[match(state_name,state.name)]
  state_code[state_name == "District of Columbia"] <- "DC"
  state_code[state_name == "Puerto Rico"] <- "PR"
  state_code
}

reshape_census_data <- function(indicator, filepath) {
  x <- readLines(filepath)
  state <- x[1:length(x)%%2 == 1] |> 
    state_code()
  values <- x[1:length(x)%%2 == 0] |> 
    gsub(pattern = "[^0-9|.]", replacement = "") |> 
    as.numeric()
  df <- data.frame(state, values)
  colnames(df) <- c("state", indicator)
  df
}


# Load CCC data -----------------------------------------------------------

ccc_raw <- read.csv("data-raw/2022-01-11_ccc/ccc_compiled.csv")

ccc <- ccc_raw %>%
  filter(date >= "2019-01-01" & date <= "2019-12-31") %>%
  group_by(state) %>%
  summarize(n_events = n(),
            n_people = sum(size_mean, na.rm = TRUE)) %>%
  filter(!is.na(state) & !(state %in% c("DC", "PR", "GU")))

# Load US Census data -----------------------------------------------------

population <- reshape_census_data(indicator = "population",
                                  filepath = "data-raw/2022-01-11_ccc/census_population.txt")
education <- reshape_census_data(indicator = "ba_degree",
                                  filepath = "data-raw/2022-01-11_ccc/census_education.txt")
income <- reshape_census_data(indicator = "med_income",
                                 filepath = "data-raw/2022-01-11_ccc/census_income.txt") |> 
  mutate(med_income = med_income / 1000)
employment <- reshape_census_data(indicator = "employ",
                              filepath = "data-raw/2022-01-11_ccc/census_employment.txt")


# Load election data ------------------------------------------------------

election <- read_excel("data-raw/2022-01-11_ccc/federalelections2016.xlsx", 
                       sheet = 3, 
                       range = "A4:G55") |> 
  select(c(1, 4, 5, 7))

colnames(election) <- c("state", "trump", "clinton", "total")

election <- election |> 
  mutate(trump = (trump / total) * 100,
         clinton = (clinton / total) * 100,
         trump = round(trump, digits = 1),
         clinton = round(clinton, digits = 1)) |> 
  select(-c(total))

# Join datasets -----------------------------------------------------------

main <- ccc |> 
  left_join(population, by = "state") |> 
  left_join(education, by = "state") |> 
  left_join(income, by = "state") |> 
  left_join(employment, by = "state") |> 
  left_join(election, by = "state") |> 
  mutate(protest = n_people / (population / 1000),
         protest = log(protest),
         protest = (protest - mean(protest)) / sd(protest),
         n_people = n_people / 1000,
         population = population / 1000000) |> 
  relocate(protest, .after = state)


# Add variable labels -----------------------------------------------------

attr(main$state, "label") <- "State"
attr(main$protest, "label") <- "Standardized protest level"
attr(main$n_events, "label") <- "Number of protest events"
attr(main$n_people, "label") <- "Number of participants, in thousands"
attr(main$population, "label") <- "Population, in millions"
attr(main$ba_degree, "label") <- "University degree, pop. 18-24 years, in percent"
attr(main$med_income, "label") <- "Median household income, in thousands of dollars"
attr(main$employ, "label") <- "Labor force participation rate, in percent"
attr(main$trump, "label") <- "Vote for Trump 2016, in percent"
attr(main$clinton, "label") <- "Vote for Clinton 2016, in percent"


# Save dataset ------------------------------------------------------------

saveRDS(main, file = "data/2022-01-11_ccc.rds")
