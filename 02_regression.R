install.packages ("lmtest")
install.packages("sandwich")
install.packages("car")
install.packages("stargazer")

library(stargazer)
library(tidyverse)
library(lmtest)
library(sandwich)
library(car)
library(ggplot2)


setwd("~/Documents/GitHub/stats-project")
sink("R_output_log.txt")

# Load your cleaned data from Step 1
data <- read_csv("cleaned_data_2016.csv")

# 1. Summary Statistics Table
summary_vars <- data %>%
  select(co2_pc, gdp_pc, renew_share, ind_share, ln_co2, ln_gdp, ln_gdp_squared)

stargazer(as.data.frame(summary_vars), 
          type = "text", 
          title = "Summary Statistics", 
          digits = 2, 
          out = "summary_stats.txt")

# 2. Histograms to justify log transformation
ggplot(data, aes(x = co2_pc)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Histogram of Raw CO2 per Capita", x = "CO2 per Capita", y = "Frequency") +
  theme_minimal()
ggsave("hist_raw_co2.png", width = 6, height = 4, dpi = 300)

ggplot(data, aes(x = ln_co2)) +
  geom_histogram(fill = "darkgreen", bins = 30) +
  labs(title = "Histogram of Logged CO2 per Capita", x = "ln(CO2 per Capita)", y = "Frequency") +
  theme_minimal()
ggsave("hist_log_co2.png", width = 6, height = 4, dpi = 300)

# 3. EKC Scatter Plot with Quadratic Fit
ggplot(data, aes(x = ln_gdp, y = ln_co2)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "darkred", linewidth = 1.5) +
  labs(
    title = "Environmental Kuznets Curve (2016)",
    subtitle = "Relationship between Log GDP per Capita and Log CO2 Emissions per Capita",
    x = "Log of GDP per Capita",
    y = "Log of CO2 Emissions per Capita"
  ) +
  theme_minimal()
ggsave("ekc_plot.png", width = 8, height = 6, dpi = 300)


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

# plotting EKC from model 3
ggplot(data, aes(x = ln_gdp, y = ln_co2)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "darkred", linewidth = 1.5) +
  labs(
    title = "Environmental Kuznets Curve (2016)",
    subtitle = "Relationship between Log GDP per Capita and Log CO2 Emissions per Capita",
    x = "Log of GDP per Capita",
    y = "Log of CO2 Emissions per Capita"
  ) +
  theme_minimal()

ggsave("ekc_plot.png", width = 8, height = 6, dpi = 300)

stargazer(model1, model2, model3, 
          type = "text",
          title = "Regression Results for the EKC (2016)",
          column.labels = c("Baseline", "EKC", "With Controls"),
          covariate.labels = c("Log GDP per Capita", "Log GDP Squared", "Renewable Share", "Industry Share"),
          out = "regression_table.txt")


# Model 4 with OECD Dummy
# Create the dummy variable for OECD (Global North proxy)
oecd_countries <- c("AUT", "AUS", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", "EST", 
                    "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", 
                    "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", 
                    "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")

data <- data %>%
  mutate(global_north = ifelse(Code %in% oecd_countries, 1, 0))


model4 <- lm(ln_co2 ~ ln_gdp * global_north + ln_gdp_squared * global_north + renew_share + ind_share, data = data)
summary(model4)

# Tests and Residuals
# 1. Breusch-Pagan Test for Heteroskedasticity
bptest(model3)

# 2. VIF for Multicollinearity
vif(model3)

# 3. Diagnostic Plots (2x2 grid)
png("residual_plots.png", width = 800, height = 800)
par(mfrow = c(2, 2))
plot(model3)
par(mfrow = c(1, 1))
dev.off()

robust_se_list <- list(
  sqrt(diag(vcovHC(model1, type = "HC1"))),
  sqrt(diag(vcovHC(model2, type = "HC1"))),
  sqrt(diag(vcovHC(model3, type = "HC1"))),
  sqrt(diag(vcovHC(model4, type = "HC1")))
)

# Regression Table
stargazer(model1, model2, model3, model4, 
          type = "text",
          title = "Regression Results for the EKC (2016)",
          column.labels = c("Baseline", "EKC Only", "With Controls", "Global North Int."),
          covariate.labels = c("Log GDP", "Log GDP Squared", "Renewable Share", "Industry Share", 
                               "Global North", "Log GDP x North", "Log GDP Sq x North"),
          se = robust_se_list, 
          out = "regression_table.txt")


sink()
