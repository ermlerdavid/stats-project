install.packages ("lmtest")
install.packages("sandwich")

library(tidyverse)
library(lmtest)
library(sandwich)

setwd("~/Documents/GitHub/stats-project")

# Load your cleaned data from Step 1
data <- read_csv("cleaned_data_2016.csv")

# Model 1: Baseline (Simple linear relationship)
model1 <- lm(ln_co2 ~ ln_gdp, data = data)
summary(model1)

# Model 2: The EKC Test (Adding the squared term)
model2 <- lm(ln_co2 ~ ln_gdp + ln_gdp_squared, data = data)
summary(model2)

# Model 3: Full model with controls (Renewable + Industry)
model3 <- lm(ln_co2 ~ ln_gdp + ln_gdp_squared + renew_share + ind_share, data = data)
summary(model3)

# Model 3 with Robust Standard Errors (Crucial for correct inference)
coeftest(model3, vcov = vcovHC(model3, type = "HC1"))

# Calculate the turning point from Model 3
b1 <- coef(model3)["ln_gdp"]
b2 <- coef(model3)["ln_gdp_squared"]
turning_point <- exp(-b1 / (2 * b2))
print(paste("Turning point (USD GDP per capita):", round(turning_point, 2)))

library(ggplot2)

ggplot(data, aes(x = ln_gdp, y = ln_co2)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "darkred", size = 1.5) +
  labs(
    title = "Environmental Kuznets Curve (2016)",
    subtitle = "Relationship between Log GDP per Capita and Log CO2 Emissions per Capita",
    x = "Log of GDP per Capita",
    y = "Log of CO2 Emissions per Capita"
  ) +
  theme_minimal()

ggsave("ekc_plot.png", width = 8, height = 6, dpi = 300)

install.packages("stargazer")
library(stargazer)

stargazer(model1, model2, model3, 
          type = "text",
          title = "Regression Results for the EKC (2016)",
          column.labels = c("Baseline", "EKC", "With Controls"),
          covariate.labels = c("Log GDP per Capita", "Log GDP Squared", "Renewable Share", "Industry Share"),
          out = "regression_table.txt")
