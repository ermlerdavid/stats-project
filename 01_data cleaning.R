library(tidyverse)

setwd("~/Documents/GitHub/stats-project")

# Process OWID CO2 data
co2 <- read_csv("co2_pc.csv") %>%
  filter(Year == 2016) %>%
  distinct(Code, .keep_all = TRUE) %>%  # <--- This removes duplicate countries
  select(Code, co2_pc = 4)

# Process OWID renewable energy data
renew <- read_csv("renewable_share.csv") %>%
  filter(Year == 2016) %>%
  distinct(Code, .keep_all = TRUE) %>%
  select(Code, renew_share = 4)

# Process World Bank GDP data (skip 4 metadata rows)
gdp <- read_csv("gdp_pc.csv", skip = 4) %>%
  select(Code = `Country Code`, gdp_pc = `2016`)

# Process World Bank industry share data (skip 4 metadata rows)
industry <- read_csv("industry_share.csv", skip = 4) %>%
  select(Code = `Country Code`, ind_share = `2016`)

# Merge all datasets together
q1_data <- co2 %>%
  inner_join(renew, by = "Code") %>%
  inner_join(gdp, by = "Code") %>%
  inner_join(industry, by = "Code") %>%
  drop_na()

# Create logged and squared variables for the EKC regression
q1_data <- q1_data %>%
  mutate(
    ln_co2 = log(co2_pc),
    ln_gdp = log(gdp_pc),
    ln_gdp_squared = ln_gdp^2
  )

# Check the number of countries in the final dataset
print(paste("Number of countries in final dataset:", nrow(q1_data)))

# Save the cleaned dataset
write_csv(q1_data, "cleaned_data_2016.csv")