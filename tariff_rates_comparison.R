# Author: Scottie Szewczyk
# Tidy Tuesday: US Agricultural Tariffs (April 28, 2026)
# Source: https://github.com/rfordatascience/tidytuesday/tree/main/data/2026/2026-04-28
# Created with the assistance of Claude Sonnet 4.6 through Posit Assistant in RStudio.

# NOTE: The data preparation steps were created using similar code to the "Code for the plot"
# section in the Tidy Tuesday GitHub page for April 29, 2026.


library(conflicted)
conflicted::conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(lubridate)
library(ggtext)

# ── Load data ─────────────────────────────────────────────────────────────────
# Load tidytuesdayR data for April 28, 2026.
 tuesdata <- tidytuesdayR::tt_load('2026-04-28')
 agreements <- tuesdata$agreements
 tariff_agricultural <- tuesdata$tariff_agricultural


# ── Data preparation ──────────────────────────────────────────────────────────

tariff_with_years <- tariff_agricultural |>
  mutate(
    begin_effective_year = year(begin_effective_date),
    end_effective_year   = year(end_effective_date)
  )

# Agreements used:
#   canada = NAFTA rates for Canada (data available 1994-1999)
#   mexico = NAFTA rates for Mexico (data available 1994-2024)
#   usmca  = USMCA rates (2020-2024, not split by country in the source data)
# Note: usmca+ ("Plus rate") is excluded to keep rates comparable across agreements.
relevant_agreements <- c("canada", "mexico", "usmca")

# Create all hts8-agreement-year combinations for 1994-2024
tariff_hts_nafta <- tariff_agricultural |>
  filter(agreement %in% relevant_agreements) |>
  distinct(hts8, agreement)

tariff_years_nafta <- tariff_hts_nafta |>
  crossing(year = 1994:2024)

# Join to get the rate in effect for each hts8-agreement-year combination
tariff_rates_nafta <- tariff_years_nafta |>
  left_join(
    tariff_with_years |> filter(agreement %in% relevant_agreements),
    join_by(
      hts8,
      agreement,
      year >= begin_effective_year,
      year <= end_effective_year
    )
  ) |>
  filter(!is.na(ad_val_rate)) |>
  distinct(hts8, agreement, year, ad_val_rate)

# Assign HTS section from first 2 digits of hts8
tariff_rates_nafta_section <- tariff_rates_nafta |>
  mutate(
    hts_chapter = as.integer(substr(hts8, 1, 2)),
    section = case_when(
      hts_chapter <= 5  ~ "I. Live Animals & Animal Products",
      hts_chapter <= 14 ~ "II. Vegetable Products",
      hts_chapter <= 15 ~ "III. Animal/Vegetable Fats & Oils",
      hts_chapter <= 24 ~ "IV. Prepared Foodstuffs",
      TRUE ~ "Other"
    )
  )

# USMCA has no country-level split in the source data; duplicate rows for each country
nafta_canada <- tariff_rates_nafta_section |>
  filter(agreement == "canada") |>
  mutate(country = "Canada")

nafta_mexico <- tariff_rates_nafta_section |>
  filter(agreement == "mexico") |>
  mutate(country = "Mexico")

usmca_canada <- tariff_rates_nafta_section |>
  filter(agreement == "usmca") |>
  mutate(country = "Canada")

usmca_mexico <- tariff_rates_nafta_section |>
  filter(agreement == "usmca") |>
  mutate(country = "Mexico")

tariff_combined <- bind_rows(nafta_canada, nafta_mexico, usmca_canada, usmca_mexico)

# ── Summarise mean rates ──────────────────────────────────────────────────────

# Note: unlike the MFN example (which filters ad_val_rate > 0), zero rates are
# included here (ad_val_rate >= 0). Under NAFTA/USMCA, most products are
# duty-free; excluding zeros would leave almost no Mexico NAFTA data after 2002
# and would misrepresent the overall tariff burden. Values outside [0, 1) are
# excluded as likely non-ad-valorem or erroneous entries.

section_colors <- c(
  "I. Live Animals & Animal Products"   = "#D55E00",
  "II. Vegetable Products"              = "#009E73",
  "III. Animal/Vegetable Fats & Oils"   = "#CC79A7",
  "IV. Prepared Foodstuffs"             = "#0072B2"
)

section_label_levels <- c(
  "<span style='color:#D55E00'>**I. Live Animals & Animal Products**</span>",
  "<span style='color:#009E73'>**II. Vegetable Products**</span>",
  "<span style='color:#CC79A7'>**III. Animal/Vegetable Fats & Oils**</span>",
  "<span style='color:#0072B2'>**IV. Prepared Foodstuffs**</span>"
)

mean_rates <- tariff_combined |>
  filter(ad_val_rate >= 0, ad_val_rate < 1, section != "Other") |>
  group_by(country, year, section) |>
  summarise(
    avg_rate   = mean(ad_val_rate, na.rm = TRUE),
    n_products = n(),
    .groups = "drop"
  ) |>
  filter(n_products >= 10) |>
  mutate(
    section_label = case_when(
      section == "I. Live Animals & Animal Products" ~
        "<span style='color:#D55E00'>**I. Live Animals & Animal Products**</span>",
      section == "II. Vegetable Products" ~
        "<span style='color:#009E73'>**II. Vegetable Products**</span>",
      section == "III. Animal/Vegetable Fats & Oils" ~
        "<span style='color:#CC79A7'>**III. Animal/Vegetable Fats & Oils**</span>",
      section == "IV. Prepared Foodstuffs" ~
        "<span style='color:#0072B2'>**IV. Prepared Foodstuffs**</span>"
    ),
    section_label = factor(section_label, levels = section_label_levels)
  )

# Shared y-axis limits across Canada and Mexico, with top padding for peak labels
pad_limits <- function(lim, top_frac = 0.14, bot_frac = 0.02) {
  span <- diff(lim)
  c(lim[1] - span * bot_frac, lim[2] + span * top_frac)
}

shared_y_limits <- pad_limits(range(mean_rates$avg_rate * 100, na.rm = TRUE))

# Year of the highest avg tariff rate per section per country
max_rate_points <- mean_rates |>
  group_by(country, section, section_label) |>
  slice_max(avg_rate, n = 1, with_ties = FALSE) |>
  ungroup()

# ── Plot helper ───────────────────────────────────────────────────────────────

nafta_start <- 1994
usmca_start <- 2020

make_tariff_plot <- function(ctry, title_text, subtitle_text) {
  plot_data <- mean_rates      |> filter(country == ctry)
  max_data  <- max_rate_points |> filter(country == ctry)

  ggplot(plot_data, aes(x = year, y = avg_rate * 100, color = section)) +
    geom_vline(xintercept = nafta_start,
               linetype = "dotted", color = "red", linewidth = 0.8) +
    geom_vline(xintercept = usmca_start,
               linetype = "dotted", color = "red", linewidth = 0.8) +
    annotate("text", x = nafta_start + 0.3, y = Inf,
             label = "NAFTA\n(Jan 1994)", hjust = 0, vjust = 1.3,
             size = 2.8, color = "red") +
    annotate("text", x = usmca_start + 0.3, y = Inf,
             label = "USMCA\n(Jul 2020)", hjust = 0, vjust = 1.3,
             size = 2.8, color = "red") +
    geom_line(linewidth = 1.2, show.legend = FALSE) +
    geom_point(size = 1.5, alpha = 0.7, show.legend = FALSE) +
    geom_text(
      data = max_data,
      aes(label = year),
      vjust = -0.8, size = 2.5, show.legend = FALSE
    ) +
    scale_color_manual(values = section_colors) +
    facet_wrap(~section_label, ncol = 2) +
    scale_x_continuous(breaks = seq(1994, 2024, by = 6)) +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      limits = shared_y_limits,
      expand = expansion(0)
    ) +
    labs(
      title    = title_text,
      subtitle = subtitle_text,
      x        = "Year",
      y        = "Average Tariff Rate",
      caption  = paste0(
        "Red dotted lines mark agreement start dates (NAFTA: Jan 1994, USMCA: Jul 2020).\n",
        "Rates expressed as mean ad valorem rate across all products in each section ",
        "(including zero-rated).\nYear labels mark the peak average rate for each section."
      )
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_markdown(size = 14),
      plot.subtitle = element_text(size = 10, color = "grey30"),
      plot.caption  = element_text(size = 8, color = "grey40"),
      strip.text    = element_markdown(size = 9)
    )
}

# ── Plots ─────────────────────────────────────────────────────────────────────

plot_canada <- make_tariff_plot(
  "Canada",
  paste0(
    "Canada: Mean Tariff Rates Under ",
    "<span style='color:grey40'>**NAFTA**</span>",
    " and ",
    "<span style='color:grey40'>**USMCA**</span>"
  ),
  paste0(
    "Average ad valorem rates by HTS section (1994-2024).\n",
    "Note: no Canada-specific NAFTA data for 2000-2019; lines break across this gap."
  )
)

plot_mexico <- make_tariff_plot(
  "Mexico",
  paste0(
    "Mexico: Mean Tariff Rates Under ",
    "<span style='color:grey40'>**NAFTA**</span>",
    " and ",
    "<span style='color:grey40'>**USMCA**</span>"
  ),
  "Average ad valorem rates by HTS section (1994-2024)."
)

plot_canada
plot_mexico

# ── Detail tables ─────────────────────────────────────────────────────────────

fmt_table <- function(ctry) {
  mean_rates |>
    filter(country == ctry) |>
    select(year, section, avg_rate, n_products) |>
    mutate(avg_rate_pct = round(avg_rate * 100, 3)) |>
    select(-avg_rate) |>
    rename(
      Year           = year,
      Section        = section,
      `Avg Rate (%)` = avg_rate_pct,
      `N Products`   = n_products
    ) |>
    arrange(Section, Year)
}

cat("---- Canada ----\n")
print(fmt_table("Canada"), n = Inf)

cat("\n---- Mexico ----\n")
print(fmt_table("Mexico"), n = Inf)
