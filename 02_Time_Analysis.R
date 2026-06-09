## CS251 || Dr. Ahreum Ju
## Seattle City University
## Authors: Faith Faith, Terrance, Mohammad
## Team Project || Team One
## 06-10-2026

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Script 2: Temporal Analysis Trends
# Purpose: Analyze crime patterns over time (2020-2024)
# Includes COVID-19 period comparison as specified in abstract
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# Set display options to show all columns
options(tibble.width = Inf)

# Load cleaned data from Script 1
crime_data <- readRDS("data/crime_cleaned.rds")

cat(" **~~ Temporal Analysis starting.... **~~\n")
cat(
  "Analyzing data from", min(crime_data$date_occurred),
  "to", max(crime_data$date_occurred), "\n"
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 2A ~ Annual crime totals from 2020-2024
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

annual_totals <- crime_data |>
  group_by(occurrence_year) |>
  summarise(
    total_crimes = n(),
    avg_daily = round(n() / 365, 1)
  ) |>
  arrange(occurrence_year)

cat("\n **~~ Annual Crime Totals ~~**\n")
print(annual_totals)

write.csv(annual_totals, "outputs/tables/annual_crime_totals.csv",
  row.names = FALSE
)

annual_plot <- ggplot(
  annual_totals,
  aes(x = factor(occurrence_year), y = total_crimes)
) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = comma(total_crimes)), vjust = -0.5, size = 4) +
  labs(
    title = "Annual Crime Totals in Los Angeles (2020-2024)",
    subtitle = "Total reported incidents by year",
    x = "Year",
    y = "Total Number of Crimes"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("outputs/figures/annual_crime_totals.png", annual_plot,
  width = 8, height = 6, dpi = 300
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 2B ~ Monthly trends for time series analysis
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

monthly_trends <- crime_data |>
  group_by(occurrence_year, occurrence_month) |>
  summarise(total_crimes = n(), .groups = "drop") |>
  mutate(
    date = make_date(occurrence_year, occurrence_month, 1),
    year_month = format(date, "%Y-%m")
  ) |>
  arrange(date)

monthly_plot <- ggplot(monthly_trends, aes(x = date, y = total_crimes)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 1.5) +
  geom_smooth(
    method = "loess", se = TRUE, color = "gray50",
    linetype = "dashed"
  ) +
  labs(
    title = "Monthly Crime Trends in Los Angeles (2020-2024)",
    subtitle = "Five-year analysis period including COVID-19 pandemic",
    x = "Date",
    y = "Total Crimes Reported"
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("outputs/figures/monthly_crime_trends.png", monthly_plot,
  width = 12, height = 6, dpi = 300
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 2C ~ COVID-19 period comparison (Key research question)
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

crime_data <- crime_data |>
  mutate(
    covid_period = case_when(
      date_occurred < as.Date("2020-03-11") ~
        "Pre-COVID (Jan-Mar 2020)",
      date_occurred < as.Date("2021-03-11") ~
        "Year 1: Peak COVID (Mar 2020-Mar 2021)",
      date_occurred < as.Date("2022-03-11") ~
        "Year 2: Mid COVID (Mar 2021-Mar 2022)",
      date_occurred < as.Date("2023-05-05") ~
        "Year 3: Late COVID (Mar 2022-May 2023)",
      TRUE ~ "Post-COVID (May 2023-Dec 2024)"
    ),
    covid_period = factor(covid_period, levels = c(
      "Pre-COVID (Jan-Mar 2020)",
      "Year 1: Peak COVID (Mar 2020-Mar 2021)",
      "Year 2: Mid COVID (Mar 2021-Mar 2022)",
      "Year 3: Late COVID (Mar 2022-May 2023)",
      "Post-COVID (May 2023-Dec 2024)"
    ))
  )

covid_comparison <- crime_data |>
  group_by(covid_period) |>
  summarise(
    total_crimes = n(),
    days_in_period = n_distinct(date_occurred),
    avg_daily_crimes = round(total_crimes / days_in_period, 1)
  ) |>
  mutate(
    baseline_avg = avg_daily_crimes[
      covid_period == "Pre-COVID (Jan-Mar 2020)"
    ],
    percent_change = round(
      ((avg_daily_crimes - baseline_avg) / baseline_avg) * 100, 1
    )
  )

cat("\n **~~ Crime Trend by COVID Period ~~**\n")
print(as.data.frame(covid_comparison))

write.csv(covid_comparison, "outputs/tables/covid_period_comparison.csv",
  row.names = FALSE
)

covid_plot <- ggplot(
  covid_comparison,
  aes(
    x = covid_period, y = avg_daily_crimes,
    fill = covid_period
  )
) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = paste0(
      avg_daily_crimes, " (",
      percent_change, "%)"
    )),
    vjust = -0.5, size = 3.5
  ) +
  scale_fill_viridis_d() +
  labs(
    title = "Daily Crime Rates Before, During, and After COVID-19",
    subtitle = "Average crimes per day with % change from pre-COVID baseline",
    x = "Time Period",
    y = "Average Daily Crimes"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("outputs/figures/covid_period_comparison.png", covid_plot,
  width = 12, height = 6, dpi = 300
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 2D ~ Pattern analysis of which days have the most crime
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

dow_patterns <- crime_data |>
  group_by(day_of_week) |>
  summarise(
    total_crimes = n(),
    percentage = round(100 * n() / nrow(crime_data), 1)
  ) |>
  arrange(desc(total_crimes))

cat("\n **~~ Crime by Day of Week ~~** \n")
print(dow_patterns)

dow_plot <- ggplot(
  dow_patterns,
  aes(
    x = reorder(day_of_week, -total_crimes),
    y = total_crimes, fill = day_of_week
  )
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentage, "%")),
    vjust = -0.5, size = 3.5
  ) +
  scale_fill_viridis_d() +
  labs(
    title = "Crime by Day of Week: Los Angeles (2020-2024)",
    subtitle = paste("Total incidents:", comma(nrow(crime_data))),
    x = "Day of Week",
    y = "Total Incidents"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("outputs/figures/day_of_week_patterns.png", dow_plot,
  width = 10, height = 6, dpi = 300
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 2E ~ Reporting Delay Analysis
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

reporting_summary <- crime_data |>
  filter(!is.na(reporting_delay), reporting_delay >= 0) |>
  summarise(
    mean_delay = round(mean(reporting_delay), 1),
    median_delay = round(median(reporting_delay), 1),
    max_delay = max(reporting_delay),
    pct_reported_same_day = round(100 * sum(reporting_delay == 0) / n(), 1)
  )

cat("\n **~~ Reporting Delay Statistics ~~**\n")
print(reporting_summary)

delay_histogram <- ggplot(
  crime_data |> filter(!is.na(reporting_delay), reporting_delay <= 30),
  aes(x = reporting_delay)
) +
  geom_histogram(
    binwidth = 1, fill = "steelblue",
    color = "black", alpha = 0.7
  ) +
  labs(
    title = "Crime Reporting Delays in Los Angeles",
    subtitle = "Days between occurrence and police report (capped at 30 days)",
    x = "Reporting Delay (Days)",
    y = "Number of Incidents"
  ) +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("outputs/figures/reporting_delays.png", delay_histogram,
  width = 10, height = 6, dpi = 300
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 2F ~ Saving results for later analysis
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

saveRDS(crime_data, "data/crime_with_covid_periods.rds")

cat("\n **~~ Temporal Analysis Complete ~~** \n")
cat("Generated files:\n")
cat("  - outputs/figures/annual_crime_totals.png\n")
cat("  - outputs/figures/monthly_crime_trends.png\n")
cat("  - outputs/figures/covid_period_comparison.png\n")
cat("  - outputs/figures/day_of_week_patterns.png\n")
cat("  - outputs/figures/reporting_delays.png\n")
cat("  - outputs/tables/annual_crime_totals.csv\n")
cat("  - outputs/tables/covid_period_comparison.csv\n")
