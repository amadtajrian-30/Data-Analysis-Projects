# Lab confirmed influenza cases (October - December 2025) Dashboard
# Tajrian Amad

# Install packages
install.packages(c("epidatr", "dplyr", "ggplot2", "MMWRweek"))
library(epidatr)
library(dplyr)
library(ggplot2)
library(MMWRweek)

# Pull lab confirmed flu data from Delphi EpiData API - focused on PA state
late_2025 <- epirange(202540, 202552)

flu_lab <- pub_fluview_clinical(
  regions  = c("pa"),
  epiweeks = late_2025
)

# Create a total confirmed flu cases variable
pa <- pa %>%
  mutate(
    total_positive = total_a + total_b
  )

# Recalculate percent positive - serve as a secondary metric for contextual interpretation
pa <- pa %>%
  mutate(
    pct_pos_calc = 100 * total_positive / total_specimens
  )

# Plot of confirmed flu cases (count)
ggplot(pa, aes(x = epiweek, y = total_positive)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Weekly laboratory-confirmed influenza cases, Pennsylvania",
    subtitle = "CDC FluView virologic surveillance, late 2025",
    x = "Week ending",
    y = "Confirmed influenza-positive specimens"
  )




