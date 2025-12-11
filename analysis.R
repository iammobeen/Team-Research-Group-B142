# ============================================
# Marriage Rate Analysis (1990-2016)
# 7COM1079 - Team Research Group B142
# University of Hertfordshire
# ============================================
# Background Research:
# - Smock & Schwartz (2020): Demographic trends in U.S. family formation
# - Tach & Eads (2015): Economic impacts of union dissolution
# - Bloome & Ang (2020): Marriage trends 1985-2015
# ============================================

# Load dataset
df_states <- read.csv("state_marriage_rates.csv", stringsAsFactors = FALSE)

# Remove empty columns
df_states <- df_states[, colSums(is.na(df_states) | df_states == "") < nrow(df_states)]

# Convert wide to long
years <- names(df_states)[-1]

Year <- c()
State <- c()
Rate <- c()

for (yr in years) {
  numeric_year <- as.numeric(gsub("X", "", yr))  # remove "X1990"
  Year  <- c(Year, rep(numeric_year, nrow(df_states)))
  State <- c(State, df_states$State)
  Rate  <- c(Rate, as.numeric(df_states[[yr]]))
}

df_long <- data.frame(Year = Year, State = State, Rate = Rate)
df_long <- na.omit(df_long)

# ===========================
# SCATTER PLOT
# ===========================
plot(df_long$Year, df_long$Rate,
     main = "Scatter Plot of Marriage Rates Over Time (1990–2016)",
     xlab = "Year",
     ylab = "Marriage Rate (per 1,000)",
     pch = 19, col = "blue")

# ===========================
# HISTOGRAM
# ===========================
hist(df_long$Rate,
     main = "Histogram of Marriage Rates (1990–2016)",
     xlab = "Marriage Rate (per 1,000)",
     col = "lightblue",
     border = "black")

# ===========================
# PEARSON CORRELATION
# ===========================
correlation_result <- cor.test(df_long$Year, df_long$Rate, method = "pearson")

cat("\n========== CORRELATION ANALYSIS ==========\n")
cat("Pearson r:", correlation_result$estimate, "\n")
cat("t-statistic:", correlation_result$statistic, "\n")
cat("p-value:", correlation_result$p.value, "\n")

if (correlation_result$p.value < 0.05) {
  cat("\nResult: Reject H0, significant correlation exists\n")
} else {
  cat("\nResult: Fail to reject H0, no significant correlation\n")
}
