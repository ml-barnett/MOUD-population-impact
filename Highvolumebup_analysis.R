if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(fixest)) install.packages("fixest", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if (!require(haven)) install.packages("haven", dependencies = TRUE)

library(dplyr)
library(fixest)
library(ggplot2)
library(haven)

df <- read_dta("/Users/davidzhu/Downloads/moud_analytic_data_05292025.dta")
head(df)

# Aggregate county-year claims
county_year <- df %>%
  group_by(county_fips, year) %>%
  summarize(Tot_Clms = sum(Tot_Clms, na.rm = TRUE), .groups = "drop")

# Year-to-year changes
county_year <- county_year %>%
  arrange(county_fips, year) %>%
  group_by(county_fips) %>%
  mutate(
    lag_Clms = lag(Tot_Clms),
    abs_change = Tot_Clms - lag_Clms,
    pct_change = 100 * (Tot_Clms - lag_Clms) / lag_Clms
  ) %>%
  ungroup()

summary(county_year$abs_change)
summary(county_year$pct_change)

# Large absolute changes
thresholds <- quantile(county_year$abs_change, probs = c(0.05, 0.95), na.rm = TRUE)
county_year <- county_year %>%
  mutate(large_change_flag = abs_change <= thresholds[1] | abs_change >= thresholds[2])

# Entry/exit events of top 5% prescribers
entry_exit_events <- df %>%
  filter(top5_flag == 1) %>%
  mutate(
    entry_event = year == firstyear,
    exit_event = year == lastyear & lastyear != 2023
  ) %>%
  group_by(county_fips, year) %>%
  summarize(
    entry_top5 = any(entry_event, na.rm = TRUE),
    exit_top5 = any(exit_event, na.rm = TRUE),
    .groups = "drop"
  )

# Merge with county-year data
county_year_joined <- county_year %>%
  left_join(entry_exit_events, by = c("county_fips", "year")) %>%
  mutate(
    entry_top5 = ifelse(is.na(entry_top5), FALSE, entry_top5),
    exit_top5 = ifelse(is.na(exit_top5), FALSE, exit_top5)
  )

# Distributions
hist(county_year_joined$abs_change, breaks = 100,
     main = "Absolute Changes in County-Level Buprenorphine",
     xlab = "Change in # Claims", xlim = c(-1000, 1000),
     col = "gray", xaxt = "n")
axis(1, at = seq(-1000, 1000, by = 100))

filtered_pct <- county_year_joined$pct_change[
  county_year_joined$pct_change >= -200 & county_year_joined$pct_change <= 1000 &
    !is.na(county_year_joined$pct_change)
]
hist(filtered_pct, breaks = 100,
     main = "Percent Changes in County-Level Buprenorphine",
     xlab = "% Change in Claims", xlim = c(-200, 1000),
     col = "gray", xaxt = "n")
axis(1, at = seq(-200, 1000, by = 100))

# Fixed-effects logistic regression (county + year)
model1 <- feglm(large_change_flag ~ entry_top5 + exit_top5 | county_fips + year,
                data = county_year_joined, family = "binomial")
summary(model1)

# Odds ratios and 95% CI's
coef_est <- coef(model1)
se_est <- sqrt(diag(vcov(model1)))

odds_ratios <- exp(coef_est)
lower_ci <- exp(coef_est - 1.96 * se_est)
upper_ci <- exp(coef_est + 1.96 * se_est)

or_table <- data.frame(
  Predictor = names(coef_est),
  Odds_Ratio = odds_ratios,
  CI_Lower = lower_ci,
  CI_Upper = upper_ci
)
print(or_table, row.names = FALSE)

write.csv(county_year_joined, "county_year_analysis_final.csv", row.names = FALSE)





## Sensitivity analysis (excluding the bottom 25% of bup claims, assuming that these are more likely to correspond to small counties)

county_avg <- county_year_joined %>%
  group_by(county_fips) %>%
  summarize(mean_claims = mean(Tot_Clms, na.rm = TRUE), .groups = "drop")

county_year_sens <- county_year_joined %>%
  left_join(county_avg, by = "county_fips")

q25 <- quantile(county_year_sens$mean_claims, 0.25, na.rm = TRUE)

county_year_filtered <- county_year_sens %>%
  filter(mean_claims > q25) %>%
  filter(!is.na(abs_change), !is.na(pct_change))

model_sens <- feglm(large_change_flag ~ entry_top5 + exit_top5 | county_fips + year,
                    data = county_year_filtered, family = "binomial")

print("Original Model:")
summary(model1)

print("Filtered Model (Excluding Bottom 25% Small Counties):")
summary(model_sens)

get_or_table <- function(model) {
  coef_est <- coef(model)
  se_est <- sqrt(diag(vcov(model)))
  data.frame(
    Predictor = names(coef_est),
    Odds_Ratio = exp(coef_est),
    CI_Lower = exp(coef_est - 1.96 * se_est),
    CI_Upper = exp(coef_est + 1.96 * se_est)
  )
}

print("Odds Ratios (Original Model):")
print(get_or_table(model1), row.names = FALSE)

print("Odds Ratios (Filtered Model):")
print(get_or_table(model_sens), row.names = FALSE)

summary(county_year_filtered$Tot_Clms)
summary(county_year_filtered$abs_change)
summary(county_year_filtered$pct_change)

hist(county_year_filtered$abs_change, breaks = 100,
     main = "Absolute Changes (Filtered Data)",
     xlab = "Change in # Claims", xlim = c(-1000, 1000),
     col = "gray", xaxt = "n")
axis(1, at = seq(-1000, 1000, by = 100))

filtered_pct_sens <- county_year_filtered$pct_change[
  county_year_filtered$pct_change >= -200 & county_year_filtered$pct_change <= 1000
]

hist(filtered_pct_sens, breaks = 100,
     main = "Percent Changes (Filtered Data)",
     xlab = "% Change in Claims", xlim = c(-200, 1000),
     col = "gray", xaxt = "n")
axis(1, at = seq(-200, 1000, by = 100))

