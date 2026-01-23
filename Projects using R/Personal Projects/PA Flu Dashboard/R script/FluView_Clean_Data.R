# Install packages
install.packages(c("epidatr", "dplyr", "ggplot2", "MMWRweek"))
library(epidatr)
library(dplyr)
library(ggplot2)
library(MMWRweek)

getwd()

# Keep the relevant variables for the dashboard
pa_clean <- flu_lab %>%
  filter(region == "pa") %>%
  transmute(
    week_start = as.Date(epiweek),         
    total_specimens = total_specimens,
    total_a = total_a,
    total_b = total_b,
    total_positive = total_a + total_b,     # Create a total confirmed flu cases variable
    percent_positive = percent_positive     
  ) %>%
  arrange(week_start)

# Plot 
ggplot(pa_clean, aes(x = week_start, y = total_positive)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Weekly laboratory-confirmed influenza positives, Pennsylvania",
    subtitle = "October-December 2025",
    x = "Week ending",
    y = "Influenza-positive specimens (A & B)"
  )

# Save the subset of the dataset
write.csv(
  pa_clean, file = "~/Downloads/processed_pa_flu_late_2025.csv",
  row.names = FALSE)