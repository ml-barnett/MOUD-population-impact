if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(fixest)) install.packages("fixest", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if (!require(haven)) install.packages("haven", dependencies = TRUE)
if (!require(stringr)) install.packages("stringr", dependencies = TRUE)
if (!require(readr)) install.packages("readr", dependencies = TRUE)
if (!require(writexl)) install.packages("writexl", dependencies = TRUE)
if (!require(tidyr)) install.packages("tidyr", dependencies = TRUE)
if (!require(purrr)) install.packages("purrr", dependencies = TRUE)


library(dplyr)
library(fixest)
library(ggplot2)
library(haven)
library(stringr)
library(readr)
library(writexl)
library(tidyr)
library(purrr)

df <- read_dta("/Users/davidzhu/Downloads/moud_analytic_data_05292025.dta")
head(df)

# Counts
firstyear_counts <- df %>% count(firstyear, name = "n_firstyear")
lastyear_counts <- df %>% count(lastyear, name = "n_lastyear")
year_counts <- df %>% count(year, name = "n_year")
full_year_summary <- full_join(firstyear_counts, lastyear_counts, 
                               by = c("firstyear" = "lastyear")) %>%
  full_join(year_counts, by = c("firstyear" = "year")) %>%
  rename(year = firstyear) %>%
  arrange(year)
print(full_year_summary)

# County-year claims
df <- df %>% rename(fips = county_fips)
county_year <- df %>%
  group_by(fips, year) %>%
  summarize(Tot_Clms = sum(Tot_Clms, na.rm = TRUE), .groups = "drop")
county_year <- county_year %>%
  arrange(fips, year) %>%
  group_by(fips) %>%
  mutate(
    lag_Clms = lag(Tot_Clms),
    abs_change = Tot_Clms - lag_Clms,
    pct_change = 100 * (Tot_Clms - lag_Clms) / lag_Clms
  ) %>%
  ungroup()
summary(county_year$abs_change)
summary(county_year$pct_change)
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
  group_by(fips, year) %>%
  summarize(
    entry_top5 = any(entry_event, na.rm = TRUE),
    exit_top5 = any(exit_event, na.rm = TRUE),
    .groups = "drop"
  )
county_year_joined <- county_year %>%
  left_join(entry_exit_events, by = c("fips", "year")) %>%
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
model1 <- feglm(large_change_flag ~ entry_top5 + exit_top5 | fips + year,
                data = county_year_joined, family = "binomial")
summary(model1)
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

#### PHASE 1 — Entry Groups

entry_years <- df %>%
  filter(top5_flag == 1) %>%
  group_by(fips) %>%
  summarize(first_entry_year = min(firstyear, na.rm = TRUE), .groups = "drop")

entry_years <- entry_years %>%
  mutate(entry_group = case_when(
    first_entry_year %in% 2013:2015 ~ "a. 2013-2015",
    first_entry_year == 2016 ~ "b. 2016",
    first_entry_year == 2017 ~ "c. 2017",
    first_entry_year == 2018 ~ "d. 2018",
    first_entry_year == 2019 ~ "e. 2019",
    first_entry_year == 2020 ~ "f. 2020",
    first_entry_year == 2021 ~ "g. 2021",
    first_entry_year %in% 2022:2023 ~ "h. 2022-2023",
    TRUE ~ NA_character_
  ))

all_counties <- df %>% distinct(fips)
entry_years <- full_join(all_counties, entry_years, by = "fips") %>%
  mutate(entry_group = ifelse(is.na(entry_group), "i. Never", entry_group))

entry_group_counts <- entry_years %>%
  count(entry_group, name = "N_counties") %>%
  arrange(entry_group)

print(entry_group_counts)

df_grouped <- df %>%
  left_join(entry_years, by = "fips")

avg_by_group_year <- df_grouped %>%
  group_by(entry_group, year) %>%
  summarize(mean_claims = mean(Tot_Clms, na.rm = TRUE), .groups = "drop")

ggplot(avg_by_group_year, aes(x = year, y = mean_claims, color = entry_group)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(2012, 2023, by = 1)) +
  labs(title = "Average Buprenorphine Claims per County by Entry Group",
       x = "Year",
       y = "Average # of Claims",
       color = "Entry Group") +
  theme_minimal()

#### PHASE 2 — Event-Time Analysis by Top 5% Prescriber Entry

entry_lookup <- df %>%
  filter(top5_flag == 1) %>%
  mutate(fips = as.character(fips)) %>%
  group_by(fips) %>%
  summarize(first_entry_year = min(firstyear, na.rm = TRUE), .groups = "drop")

df_grouped <- df_grouped %>%
  mutate(fips = as.character(fips))

if ("first_entry_year" %in% names(df_grouped)) {
  df_grouped <- df_grouped %>% select(-first_entry_year)
}

df_filtered <- df_grouped %>%
  filter(!entry_group %in% c("a. 2013-2015", "h. 2022-2023")) %>%
  left_join(entry_lookup, by = "fips") %>%
  mutate(event_time = year - first_entry_year)

df_event_window <- df_filtered %>%
  filter(event_time >= -3 & event_time <= 3)

event_trends <- df_event_window %>%
  group_by(entry_group, event_time) %>%
  summarize(
    mean_claims = mean(Tot_Clms, na.rm = TRUE),
    se = sd(Tot_Clms, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    lower = mean_claims - 1.96 * se,
    upper = mean_claims + 1.96 * se
  )

ggplot(event_trends, aes(x = event_time, y = mean_claims)) +
  geom_line(color = "steelblue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~entry_group, scales = "free_y") +
  labs(
    title = "Buprenorphine Claims by Years from Entry of Top 5% Prescriber",
    x = "Years from Entry",
    y = "Average Number of Claims"
  ) +
  theme_minimal()

#### PHASE 3 — Exit Groups

exit_years <- df %>%
  filter(top5_flag == 1, lastyear != 2023) %>%
  group_by(fips) %>%
  summarize(last_exit_year = max(lastyear, na.rm = TRUE), .groups = "drop") %>%
  mutate(fips = as.character(fips))

exit_years <- exit_years %>%
  mutate(exit_group = case_when(
    last_exit_year %in% 2013:2015 ~ "a. 2013-2015",
    last_exit_year == 2016 ~ "b. 2016",
    last_exit_year == 2017 ~ "c. 2017",
    last_exit_year == 2018 ~ "d. 2018",
    last_exit_year == 2019 ~ "e. 2019",
    last_exit_year == 2020 ~ "f. 2020",
    last_exit_year == 2021 ~ "g. 2021",
    last_exit_year == 2022 ~ "h. 2022",
    TRUE ~ NA_character_
  ))

all_counties <- df %>% distinct(fips) %>%
  mutate(fips = as.character(fips))

exit_years <- full_join(all_counties, exit_years, by = "fips") %>%
  mutate(exit_group = ifelse(is.na(exit_group), "i. Never or Still Active", exit_group))

exit_group_counts <- exit_years %>%
  count(exit_group, name = "N_counties") %>%
  arrange(exit_group)

print(exit_group_counts)

df_exit_grouped <- df %>%
  mutate(fips = as.character(fips)) %>%
  left_join(exit_years, by = "fips")

avg_by_exit_group_year <- df_exit_grouped %>%
  group_by(exit_group, year) %>%
  summarize(mean_claims = mean(Tot_Clms, na.rm = TRUE), .groups = "drop")

ggplot(avg_by_exit_group_year, aes(x = year, y = mean_claims, color = exit_group)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(2012, 2023, by = 1)) +
  labs(
    title = "Average Buprenorphine Claims per County by Exit Group",
    x = "Year",
    y = "Average # of Claims",
    color = "Exit Group"
  ) +
  theme_minimal()

#### PHASE 4 — Event-Time Analysis by Top 5% Prescriber Exit

exit_lookup <- df %>%
  filter(top5_flag == 1, lastyear != 2023) %>%
  mutate(fips = as.character(fips)) %>%
  group_by(fips) %>%
  summarize(last_exit_year = max(lastyear, na.rm = TRUE), .groups = "drop")

df_grouped <- df_grouped %>%
  mutate(fips = as.character(fips))

if ("last_exit_year" %in% names(df_grouped)) {
  df_grouped <- df_grouped %>% select(-last_exit_year)
}

df_exit_filtered <- df_grouped %>%
  left_join(exit_lookup, by = "fips") %>%
  filter(!is.na(last_exit_year)) %>%
  mutate(event_time = year - last_exit_year)

df_exit_window <- df_exit_filtered %>%
  filter(event_time >= -3 & event_time <= 3)

df_exit_window <- df_exit_window %>%
  mutate(exit_group = case_when(
    last_exit_year == 2016 ~ "b. 2016",
    last_exit_year == 2017 ~ "c. 2017",
    last_exit_year == 2018 ~ "d. 2018",
    last_exit_year == 2019 ~ "e. 2019",
    last_exit_year == 2020 ~ "f. 2020",
    last_exit_year == 2021 ~ "g. 2021",
    last_exit_year == 2022 ~ "h. 2022",
    TRUE ~ "Other"
  )) %>%
  filter(exit_group != "Other")

exit_event_trends <- df_exit_window %>%
  group_by(exit_group, event_time) %>%
  summarize(
    mean_claims = mean(Tot_Clms, na.rm = TRUE),
    se = sd(Tot_Clms, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    lower = mean_claims - 1.96 * se,
    upper = mean_claims + 1.96 * se
  )

ggplot(exit_event_trends, aes(x = event_time, y = mean_claims)) +
  geom_line(color = "darkred", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "salmon", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~exit_group, scales = "free_y") +
  labs(
    title = "Buprenorphine Claims by Years from Exit of Top 5% Prescriber",
    x = "Years from Exit",
    y = "Average Number of Claims"
  ) +
  theme_minimal()

# Bar plots (showing the same thing as the event-time  analysis in phase 2 and 4)

ggplot(event_trends, aes(x = factor(event_time), y = mean_claims)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_vline(xintercept = which(levels(factor(event_trends$event_time)) == "0"),
             linetype = "dashed") +
  facet_wrap(~entry_group, scales = "free_y") +
  labs(
    title = "Buprenorphine Claims by Years from Entry of Top 5% Prescriber",
    x = "Years from Entry",
    y = "Average Number of Claims"
  ) +
  theme_minimal()

ggplot(exit_event_trends, aes(x = factor(event_time), y = mean_claims)) +
  geom_bar(stat = "identity", fill = "darkred", alpha = 0.8) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_vline(xintercept = which(levels(factor(exit_event_trends$event_time)) == "0"),
             linetype = "dashed") +
  facet_wrap(~exit_group, scales = "free_y") +
  labs(
    title = "Buprenorphine Claims by Years from Exit of Top 5% Prescriber",
    x = "Years from Exit",
    y = "Average Number of Claims"
  ) +
  theme_minimal()

# Event-time analysis with raw counts (dots for individual counties are superimposed)

ggplot() +
  geom_jitter(data = df_event_window, aes(x = event_time, y = Tot_Clms),
              width = 0.2, height = 0, alpha = 0.3, color = "steelblue") +
  geom_line(data = event_trends, aes(x = event_time, y = mean_claims),
            color = "steelblue", size = 1) +
  geom_ribbon(data = event_trends, aes(x = event_time, ymin = lower, ymax = upper),
              fill = "lightblue", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~entry_group, scales = "free_y") +
  labs(title = "Buprenorphine Claims by Years from Entry of Top 5% Prescriber (with Raw Points)",
       x = "Years from Entry",
       y = "Number of Claims") +
  theme_minimal()

ggplot() +
  geom_jitter(data = df_exit_window, aes(x = event_time, y = Tot_Clms),
              width = 0.2, height = 0, alpha = 0.3, color = "darkred") +
  geom_line(data = exit_event_trends, aes(x = event_time, y = mean_claims),
            color = "darkred", size = 1) +
  geom_ribbon(data = exit_event_trends, aes(x = event_time, ymin = lower, ymax = upper),
              fill = "salmon", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~exit_group, scales = "free_y") +
  labs(title = "Buprenorphine Claims by Years from Exit of Top 5% Prescriber (with Raw Points)",
       x = "Years from Exit",
       y = "Number of Claims") +
  theme_minimal()









#### ACS data
clean_fips <- function(fips_col) {
  str_sub(fips_col, -5)
}

demographic_data <- read.csv("/Users/davidzhu/Downloads/Demographic_data.csv") %>%
  mutate(fips = clean_fips(fips)) %>%
  select(
    fips,
    sex_male              = DP05_0002E,
    sex_female            = DP05_0003E,
    race_white            = DP05_0037E,
    race_black            = DP05_0038E,
    race_aian             = DP05_0039E,
    race_asian            = DP05_0047E,
    race_nhpi             = DP05_0055E,
    race_other            = DP05_0060E,
    ethnicity_hispanic    = DP05_0076E,
    ethnicity_nonhispanic = DP05_0081E
  ) %>%
  mutate(across(-fips, as.numeric))

social_data <- read.csv("/Users/davidzhu/Downloads/Social_data.csv") %>%
  mutate(fips = clean_fips(fips)) %>%
  select(
    fips,
    educ_less_9th        = DP02_0060E,
    educ_9_12_nodegree   = DP02_0061E,
    educ_highschool      = DP02_0062E,
    educ_some_college    = DP02_0063E,
    educ_associates      = DP02_0064E,
    educ_bachelors       = DP02_0065E,
    educ_grad_prof       = DP02_0066E,
    has_disability       = DP02_0072E
  ) %>%
  mutate(across(-fips, as.numeric))

economic_data <- read.csv("/Users/davidzhu/Downloads/Economic_data.csv") %>%
  mutate(fips = clean_fips(fips)) %>%
  select(
    fips,
    employed                    = DP03_0004E,
    unemployed                  = DP03_0005E,
    unemployment_rate           = DP03_0009E,
    retirement_income           = DP03_0068E,
    social_security_income      = DP03_0070E,
    public_assistance_income    = DP03_0072E,
    food_stamps                 = DP03_0074E,
    income_lt_10k               = DP03_0076E,
    income_10k_14k              = DP03_0077E,
    income_15k_24k              = DP03_0078E,
    income_25k_34k              = DP03_0079E,
    income_35k_49k              = DP03_0080E,
    income_50k_74k              = DP03_0081E,
    income_75k_99k              = DP03_0082E,
    income_100k_149k            = DP03_0083E,
    income_150k_199k            = DP03_0084E,
    income_200k_plus            = DP03_0085E,
    median_income               = DP03_0086E,
    mean_income                 = DP03_0087E,
    insurance_any               = DP03_0096E,
    insurance_private           = DP03_0097E,
    insurance_public            = DP03_0098E,
    insurance_none              = DP03_0099E
  ) %>%
  mutate(across(-fips, as.numeric))

# Overdose
clean_overdose_fips <- function(fips) {
  str_pad(as.character(fips), width = 5, side = "left", pad = "0")
}
overdose_all <- tibble()
for (yr in 2013:2023) {
  file_path <- paste0("/Users/davidzhu/Downloads/Overdose_", yr, ".csv")
  overdose <- read_csv(file_path, show_col_types = FALSE) %>%
    mutate(
      year = yr,
      fips = clean_overdose_fips(fips)
    ) %>%
    select(
      fips,
      year,
      overdose_deaths = Deaths,
      crude_rate = `Crude Rate`
    )
  overdose_all <- bind_rows(overdose_all, overdose)
}

final_df <- df %>%
  mutate(fips = clean_overdose_fips(fips)) %>%
  left_join(demographic_data, by = "fips") %>%
  left_join(social_data, by = "fips") %>%
  left_join(economic_data, by = "fips") %>%
  left_join(overdose_all, by = c("fips", "year")) %>%
  rename(
    `Drug overdose deaths` = overdose_deaths,
    `Crude drug overdose death rates per 100,000` = crude_rate
  )

glimpse(final_df)
write_xlsx(final_df, "/Users/davidzhu/Downloads/moud_claims_with_covariates.xlsx")

# Merge entry/exit flags into final_df
final_df <- final_df %>%
  left_join(entry_exit_events %>%
              mutate(fips = as.character(fips)),
            by = c("fips", "year")) %>%
  mutate(
    entry_top5 = ifelse(is.na(entry_top5), FALSE, entry_top5),
    exit_top5 = ifelse(is.na(exit_top5), FALSE, exit_top5)
  )

# County-years for overdose data
county_year_overdose <- final_df %>%
  group_by(fips, year) %>%
  summarize(
    overdose_rate = mean(`Crude drug overdose death rates per 100,000`, na.rm = TRUE),
    entry_top5 = any(entry_top5, na.rm = TRUE),
    exit_top5 = any(exit_top5, na.rm = TRUE),
    .groups = "drop"
  )

# Fixed effects model — overdose rate outcome
model_od <- feols(overdose_rate ~ entry_top5 + exit_top5 | fips + year,
                  data = county_year_overdose)
summary(model_od)

#### PHASE 1 — Entry Groups (Overdose)

df_grouped_overdose <- final_df %>%
  left_join(entry_years %>% mutate(fips = as.character(fips)), by = "fips")

avg_by_group_year_od <- df_grouped_overdose %>%
  group_by(entry_group, year) %>%
  summarize(mean_od_rate = mean(`Crude drug overdose death rates per 100,000`, na.rm = TRUE),
            .groups = "drop")

ggplot(avg_by_group_year_od, aes(x = year, y = mean_od_rate, color = entry_group)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(2012, 2023, by = 1)) +
  labs(title = "Average Overdose Death Rates per County by Entry Group",
       x = "Year",
       y = "Crude Overdose Death Rate per 100,000",
       color = "Entry Group") +
  theme_minimal()

#### PHASE 2 — Event-Time Analysis by Top 5% Prescriber Entry (Overdose)

event_trends_od <- df_event_window %>%
  left_join(final_df %>% select(fips, year, `Crude drug overdose death rates per 100,000`), 
            by = c("fips", "year")) %>%
  group_by(entry_group, event_time) %>%
  summarize(
    mean_od_rate = mean(`Crude drug overdose death rates per 100,000`, na.rm = TRUE),
    se = sd(`Crude drug overdose death rates per 100,000`, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    lower = mean_od_rate - 1.96 * se,
    upper = mean_od_rate + 1.96 * se
  )

ggplot(event_trends_od, aes(x = event_time, y = mean_od_rate)) +
  geom_line(color = "steelblue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~entry_group, scales = "free_y") +
  labs(
    title = "Overdose Death Rates by Years from Entry of Top 5% Prescriber",
    x = "Years from Entry",
    y = "Crude Overdose Death Rate per 100,000"
  ) +
  theme_minimal()

#### PHASE 3 — Exit Groups (Overdose)

df_exit_grouped_overdose <- final_df %>%
  left_join(exit_years %>% mutate(fips = as.character(fips)), by = "fips")

avg_by_exit_group_year_od <- df_exit_grouped_overdose %>%
  group_by(exit_group, year) %>%
  summarize(mean_od_rate = mean(`Crude drug overdose death rates per 100,000`, na.rm = TRUE),
            .groups = "drop")

ggplot(avg_by_exit_group_year_od, aes(x = year, y = mean_od_rate, color = exit_group)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(2012, 2023, by = 1)) +
  labs(title = "Average Overdose Death Rates per County by Exit Group",
       x = "Year",
       y = "Crude Overdose Death Rate per 100,000",
       color = "Exit Group") +
  theme_minimal()

#### PHASE 4 — Event-Time Analysis by Top 5% Prescriber Exit (Overdose)

exit_event_trends_od <- df_exit_window %>%
  left_join(final_df %>% select(fips, year, `Crude drug overdose death rates per 100,000`), 
            by = c("fips", "year")) %>%
  group_by(exit_group, event_time) %>%
  summarize(
    mean_od_rate = mean(`Crude drug overdose death rates per 100,000`, na.rm = TRUE),
    se = sd(`Crude drug overdose death rates per 100,000`, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    lower = mean_od_rate - 1.96 * se,
    upper = mean_od_rate + 1.96 * se
  )

ggplot(exit_event_trends_od, aes(x = event_time, y = mean_od_rate)) +
  geom_line(color = "darkred", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "salmon", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~exit_group, scales = "free_y") +
  labs(
    title = "Overdose Death Rates by Years from Exit of Top 5% Prescriber",
    x = "Years from Exit",
    y = "Crude Overdose Death Rate per 100,000"
  ) +
  theme_minimal()

# Bar plots (showing the same thing as the event-time  analysis in phase 2 and 4)
ggplot(event_trends_od, aes(x = factor(event_time), y = mean_od_rate)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_vline(xintercept = which(levels(factor(event_trends_od$event_time)) == "0"),
             linetype = "dashed") +
  facet_wrap(~entry_group, scales = "free_y") +
  labs(
    title = "Overdose Death Rates by Years from Entry of Top 5% Prescriber",
    x = "Years from Entry",
    y = "Crude Overdose Death Rate per 100,000"
  ) +
  theme_minimal()

ggplot(exit_event_trends_od, aes(x = factor(event_time), y = mean_od_rate)) +
  geom_bar(stat = "identity", fill = "darkred", alpha = 0.8) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_vline(xintercept = which(levels(factor(exit_event_trends_od$event_time)) == "0"),
             linetype = "dashed") +
  facet_wrap(~exit_group, scales = "free_y") +
  labs(
    title = "Overdose Death Rates by Years from Exit of Top 5% Prescriber",
    x = "Years from Exit",
    y = "Crude Overdose Death Rate per 100,000"
  ) +
  theme_minimal()

## Heatmaps (county-level distribution of top 5% prescribers overall, by entry, and by exit)

if (!require(usmap)) install.packages("usmap", dependencies = TRUE)
library(usmap)
library(ggplot2)

top5_counties <- df %>%
  group_by(fips) %>%
  summarize(has_top5 = any(top5_flag == 1), .groups = "drop") %>%
  mutate(has_top5 = ifelse(has_top5, "Yes", "No"))
top5_counties <- top5_counties %>%
  mutate(fips = str_pad(as.character(fips), width = 5, side = "left", pad = "0"))
top5_counties_any <- df %>%
  group_by(fips) %>%
  summarize(has_top5_any = any(top5_flag == 1), .groups = "drop") %>%
  mutate(has_top5_any = ifelse(has_top5_any, "Yes", "No")) %>%
  mutate(fips = str_pad(as.character(fips), width = 5, side = "left", pad = "0"))
plot_usmap(regions = "counties", data = top5_counties_any, values = "has_top5_any") +
  scale_fill_manual(values = c("Yes" = "#008080", "No" = "beige")) +
  labs(
    title = "Counties Top 5% Buprenorphine Prescriber in Any Year",
    fill = "Top 5% Prescriber"
  ) +
  theme(legend.position = "right")


top5_counties_entry <- entry_years %>%
  mutate(has_entry = ifelse(entry_group != "i. Never", "Yes", "No")) %>%
  select(fips, has_entry) %>%
  mutate(fips = str_pad(as.character(fips), width = 5, side = "left", pad = "0"))
plot_usmap(regions = "counties", data = top5_counties_entry, values = "has_entry") +
  scale_fill_manual(values = c("Yes" = "steelblue", "No" = "beige")) +
  labs(
    title = "Counties with Entry of Top 5% Buprenorphine Prescriber",
    fill = "Entry of Top 5% Prescriber"
  ) +
  theme(legend.position = "right")

top5_counties_exit <- exit_years %>%
  mutate(has_exit = ifelse(exit_group != "i. Never or Still Active", "Yes", "No")) %>%
  select(fips, has_exit) %>%
  mutate(fips = str_pad(as.character(fips), width = 5, side = "left", pad = "0"))
plot_usmap(regions = "counties", data = top5_counties_exit, values = "has_exit") +
  scale_fill_manual(values = c("Yes" = "salmon", "No" = "beige")) +
  labs(
    title = "Counties with Exit of Top 5% Buprenorphine Prescriber",
    fill = "Exit of Top 5% Prescriber"
  ) +
  theme(legend.position = "right")





## Fixed effects models and average treatment effects (ATE)

# Entry (bup claims)

df_event_window_clean <- df_event_window %>%
  group_by(fips) %>%
  mutate(claims_m1 = Tot_Clms[event_time == -1][1]) %>%
  ungroup() %>%
  filter(!is.na(Tot_Clms), !is.na(claims_m1), claims_m1 > 10)

model_event_entry <- feols(Tot_Clms ~ i(event_time, ref = -1) | fips + year, data = df_event_window_clean)

event_coef_entry <- broom::tidy(model_event_entry) %>%
  filter(grepl("event_time", term)) %>%
  mutate(event_time = as.numeric(str_extract(term, "-?\\d+"))) %>%
  arrange(event_time) %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )

event_coef_entry_display <- event_coef_entry %>%
  select(event_time, estimate, lower, upper) %>%
  mutate(
    estimate = round(estimate, 2),
    lower = round(lower, 2),
    upper = round(upper, 2)
  )

print(event_coef_entry_display)

ggplot(event_coef_entry, aes(x = event_time, y = estimate)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
  labs(
    title = "Effect of Top 5% Prescriber Entry on Buprenorphine Claims",
    x = "Years from Entry",
    y = "Change in Claims vs. Year -1"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(event_coef_entry$event_time), max(event_coef_entry$event_time), by = 1))

# ATE post-entry (years 0 to +2)
post_entry_years <- c(0, 1, 2)

post_entry_effects <- event_coef_entry %>%
  filter(event_time %in% post_entry_years)

ATE_post_entry <- mean(post_entry_effects$estimate)
ATE_CI_lower_entry <- mean(post_entry_effects$lower)
ATE_CI_upper_entry <- mean(post_entry_effects$upper)
cat("ATE post-entry (years 0 to +2):", round(ATE_post_entry, 2), "\n")
cat("95% CI:", round(ATE_CI_lower_entry, 2), "to", round(ATE_CI_upper_entry, 2), "\n")

# Exit (bup claims)
df_exit_window_clean <- df_exit_window %>%
  group_by(fips) %>%
  mutate(claims_m1 = Tot_Clms[event_time == -1][1]) %>%
  ungroup() %>%
  filter(!is.na(Tot_Clms), !is.na(claims_m1), claims_m1 > 10)

model_event_exit <- feols(Tot_Clms ~ i(event_time, ref = -1) | fips + year, data = df_exit_window_clean)

event_coef_exit <- broom::tidy(model_event_exit) %>%
  filter(grepl("event_time", term)) %>%
  mutate(event_time = as.numeric(str_extract(term, "-?\\d+"))) %>%
  arrange(event_time) %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )

event_coef_exit_display <- event_coef_exit %>%
  select(event_time, estimate, lower, upper) %>%
  mutate(
    estimate = round(estimate, 2),
    lower = round(lower, 2),
    upper = round(upper, 2)
  )

print(event_coef_exit_display)

ggplot(event_coef_exit, aes(x = event_time, y = estimate)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "salmon", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
  labs(
    title = "Effect of Top 5% Prescriber Exit on Buprenorphine Claims (Cleaned Data)",
    x = "Years from Exit",
    y = "Change in Claims vs. Year -1"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(event_coef_exit$event_time), max(event_coef_exit$event_time), by = 1))

# ATE post-exit (years 0 to +2)
post_exit_years <- c(0, 1, 2)

post_exit_effects <- event_coef_exit %>%
  filter(event_time %in% post_exit_years)

ATE_post_exit <- mean(post_exit_effects$estimate)
ATE_CI_lower_exit <- mean(post_exit_effects$lower)
ATE_CI_upper_exit <- mean(post_exit_effects$upper)
cat("ATE post-exit (years 0 to +2):", round(ATE_post_exit, 2), "\n")
cat("95% CI:", round(ATE_CI_lower_exit, 2), "to", round(ATE_CI_upper_exit, 2), "\n")


# Entry (overdose)

df_event_window_od_clean <- df_event_window %>%
  left_join(final_df %>% select(fips, year, overdose_rate = `Crude drug overdose death rates per 100,000`), 
            by = c("fips", "year")) %>%
  mutate(overdose_rate = as.numeric(overdose_rate)) %>%
  group_by(fips) %>%
  mutate(rate_m1 = overdose_rate[event_time == -1][1]) %>%
  ungroup() %>%
  filter(!is.na(overdose_rate), !is.na(rate_m1), rate_m1 > 0)

model_event_entry_od <- feols(overdose_rate ~ i(event_time, ref = -1) | fips + year, data = df_event_window_od_clean)

event_coef_entry_od <- broom::tidy(model_event_entry_od) %>%
  filter(grepl("event_time", term)) %>%
  mutate(event_time = as.numeric(str_extract(term, "-?\\d+"))) %>%
  arrange(event_time) %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )

event_coef_entry_od_display <- event_coef_entry_od %>%
  select(event_time, estimate, lower, upper) %>%
  mutate(
    estimate = round(estimate, 2),
    lower = round(lower, 2),
    upper = round(upper, 2)
  )

print(event_coef_entry_od_display)

ggplot(event_coef_entry_od, aes(x = event_time, y = estimate)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
  labs(
    title = "Effect of Top 5% Prescriber Entry on Overdose Death Rate",
    x = "Years from Entry",
    y = "Change in Overdose Rate vs. Year -1"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(event_coef_entry_od$event_time), max(event_coef_entry_od$event_time), by = 1))

# ATE post-entry (years 0 to +2)
post_entry_years <- c(0, 1, 2)

post_entry_effects_od <- event_coef_entry_od %>%
  filter(event_time %in% post_entry_years)

ATE_post_entry_od <- mean(post_entry_effects_od$estimate)
ATE_CI_lower_entry_od <- mean(post_entry_effects_od$lower)
ATE_CI_upper_entry_od <- mean(post_entry_effects_od$upper)
cat("ATE post-entry (years 0 to +2):", round(ATE_post_entry_od, 2), "\n")
cat("95% CI:", round(ATE_CI_lower_entry_od, 2), "to", round(ATE_CI_upper_entry_od, 2), "\n")

# Exit (overdose)
df_exit_window_od_clean <- df_exit_window %>%
  left_join(final_df %>% select(fips, year, overdose_rate = `Crude drug overdose death rates per 100,000`), 
            by = c("fips", "year")) %>%
  mutate(overdose_rate = as.numeric(overdose_rate)) %>%
  group_by(fips) %>%
  mutate(rate_m1 = overdose_rate[event_time == -1][1]) %>%
  ungroup() %>%
  filter(!is.na(overdose_rate), !is.na(rate_m1), rate_m1 > 0)

model_event_exit_od <- feols(overdose_rate ~ i(event_time, ref = -1) | fips + year, data = df_exit_window_od_clean)

event_coef_exit_od <- broom::tidy(model_event_exit_od) %>%
  filter(grepl("event_time", term)) %>%
  mutate(event_time = as.numeric(str_extract(term, "-?\\d+"))) %>%
  arrange(event_time) %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )

event_coef_exit_od_display <- event_coef_exit_od %>%
  select(event_time, estimate, lower, upper) %>%
  mutate(
    estimate = round(estimate, 2),
    lower = round(lower, 2),
    upper = round(upper, 2)
  )
print(event_coef_exit_od_display)

ggplot(event_coef_exit_od, aes(x = event_time, y = estimate)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "salmon", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
  labs(
    title = "Effect of Top 5% Prescriber Exit on Overdose Death Rate",
    x = "Years from Exit",
    y = "Change in Overdose Rate vs. Year -1"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(event_coef_exit_od$event_time), max(event_coef_exit_od$event_time), by = 1))

# ATE post-exit (years 0 to +2)
post_exit_years <- c(0, 1, 2)

post_exit_effects_od <- event_coef_exit_od %>%
  filter(event_time %in% post_exit_years)

ATE_post_exit_od <- mean(post_exit_effects_od$estimate)
ATE_CI_lower_exit_od <- mean(post_exit_effects_od$lower)
ATE_CI_upper_exit_od <- mean(post_exit_effects_od$upper)
cat("ATE post-exit (years 0 to +2):", round(ATE_post_exit_od, 2), "\n")
cat("95% CI:", round(ATE_CI_lower_exit_od, 2), "to", round(ATE_CI_upper_exit_od, 2), "\n")



