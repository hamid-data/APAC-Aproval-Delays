# ==============================================================================
# Regulatory approval delays: pembrolizumab and trastuzumab deruxtecan (Td-XD)
# FDA approval vs. local approval in Asia-Pacific (APAC) countries
# ==============================================================================
# Assumes the approvals dataset is already loaded in R as a data frame called
# `data` (one row per drug x indication x country).

library(dplyr)
library(stringr)
library(janitor)
library(ggplot2)
library(gt)

# ==============================================================================
# 1. DATA PREPARATION
# ==============================================================================

# Tidy column names, then give the long ones shorter working names
data <- data %>%
  clean_names() %>%
  rename(
    drug            = systemic_therapy_generic_name,
    indication      = indication_e_g_1l_2l_maintenance,
    curative_intent = curative_intent_y_n,
    esmo_guideline  = in_esmo_guideline_y_n,
    fda_date        = international_regulatory_approval_fda_dd_mm_yyyy,
    local_date      = local_regulatory_approval_date_dd_mm_yyyy,
    mcbs_category   = mcbs_high_low_high_benefit_a_b_or_4_5_low_benefit_c_or_1_3,
    mcbs_evidence   = value_level_of_evidence_i_v_grade_of_evidence_a_e_esmo_mcbs_magnitude_of_clinical_benefit_scale_v2_0_5_1_who_essential_medicines_2023_list
  ) %>%
  mutate(
    fda_date   = as.Date(fda_date),
    local_date = as.Date(local_date)
  )

# Manual correction: one South Korea pembrolizumab (breast) approval date was
# implausible (delay below -60 months in the source data), so it is replaced.
data <- data %>%
  mutate(
    local_date = case_when(
      country == "S. Korea" &
        drug == "Pembrolizumab" &
        cancer_type == "Breast" &
        fda_to_local_delay_months < -60 ~ as.Date("2021-07-23"),
      TRUE ~ local_date
    )
  )

# Delay = local approval - FDA approval, in months (30.44 days per month).
# Negative values mean the local approval came before the FDA approval.
data <- data %>%
  mutate(
    fda_to_local_delay_days   = as.numeric(local_date - fda_date),
    fda_to_local_delay_months = fda_to_local_delay_days / 30.44,

    curative_group = case_when(
      grepl("^Y", curative_intent) ~ "Curative",
      TRUE                         ~ "Non-curative"
    ),
    mcbs_group = case_when(
      mcbs_category == "High" ~ "High MCBS",
      mcbs_category == "Low"  ~ "Low MCBS",
      TRUE ~ NA_character_
    ),
    esmo_group = case_when(
      esmo_guideline == "Y" ~ "ESMO: Yes",
      esmo_guideline == "N" ~ "ESMO: No",
      TRUE ~ NA_character_
    ),

    # ESMO-MCBS score (numeric). Two versions: extracted from the text, and a
    # direct numeric conversion of the same column.
    mcbs_score        = as.numeric(str_extract(mcbs_evidence, "(?<=MCBS\\s)[0-5]")),
    mcbs_score_direct = suppressWarnings(as.numeric(mcbs_evidence))
  )

# Indications with an FDA date and a local date (delay can be calculated)
analysis_data <- data %>%
  filter(!is.na(fda_to_local_delay_months))

summary(analysis_data$fda_to_local_delay_months)

# Coverage check: how many rows have an FDA date / a calculable delay
data %>%
  group_by(drug, country) %>%
  summarise(
    total_rows      = n(),
    rows_with_fda   = sum(!is.na(fda_date)),
    rows_with_delay = sum(!is.na(fda_to_local_delay_months)),
    .groups = "drop"
  )

# ==============================================================================
# 2. DELAY SUMMARY TABLES (median, IQR, min, max)
# ==============================================================================

summarise_delay <- function(df, ...) {
  df %>%
    group_by(...) %>%
    summarise(
      n            = n(),
      median_delay = median(fda_to_local_delay_months),
      q1           = quantile(fda_to_local_delay_months, 0.25),
      q3           = quantile(fda_to_local_delay_months, 0.75),
      min_delay    = min(fda_to_local_delay_months),
      max_delay    = max(fda_to_local_delay_months),
      .groups = "drop"
    ) %>%
    mutate(
      median_iqr = paste0(round(median_delay, 1), " (",
                          round(q1, 1), "–", round(q3, 1), ")")
    )
}

delay_table <- function(summ, title, subtitle = NULL, extra = NULL, extra_label = NULL) {
  tab <- summ %>%
    select(drug, country, all_of(extra), n, median_iqr, min_delay, max_delay) %>%
    gt(groupname_col = "drug") %>%
    cols_label(
      country    = "Country",
      n          = "N",
      median_iqr = "Median (IQR), months",
      min_delay  = "Minimum",
      max_delay  = "Maximum"
    ) %>%
    fmt_number(columns = c(min_delay, max_delay), decimals = 1) %>%
    tab_header(title = md(paste0("**", title, "**")), subtitle = subtitle) %>%
    tab_options(table.font.size = 12, heading.title.font.size = 14)
  if (!is.null(extra)) {
    tab <- tab %>% cols_label(.list = setNames(as.list(extra_label), extra))
  }
  tab
}

# Table 1: by drug and country
summary_drug_country <- summarise_delay(analysis_data, drug, country)
table1 <- delay_table(summary_drug_country,
                      title    = "Approval Delays by Drug and Country",
                      subtitle = "Median (IQR) months from FDA approval")
table1

# Table 2: by drug, country and cancer type
summary_cancer <- summarise_delay(analysis_data, drug, country, cancer_type)
table2 <- delay_table(summary_cancer,
                      title = "Approval Delays by Drug, Country and Cancer Type",
                      extra = "cancer_type", extra_label = "Cancer Type")
table2

# Table 3: by drug, country and indication
summary_indication <- summarise_delay(analysis_data, drug, country, indication)
table3 <- delay_table(summary_indication,
                      title    = "Approval Delays by Drug, Country and Indication",
                      subtitle = "Median (IQR), minimum and maximum months from FDA approval",
                      extra = "indication", extra_label = "Indication")
table3

# Table 4: by drug, country and curative intent
summary_curative <- summarise_delay(analysis_data, drug, country, curative_group)
table4 <- delay_table(summary_curative,
                      title    = "Approval Delays by Drug, Country and Curative Intent",
                      subtitle = "Median (IQR), minimum and maximum months from FDA approval",
                      extra = "curative_group", extra_label = "Curative Intent")
table4

# Indications without an FDA approval date (excluded from delay analyses)
no_fda <- data %>%
  filter(is.na(fda_date)) %>%
  mutate(
    local_approval_clean = ifelse(
      is.na(local_date),
      "Not approved",
      format(local_date, "%d %b %Y")
    )
  ) %>%
  select(drug, country, cancer_type, indication, local_approval_clean)

no_fda %>% count(drug, country)

table_no_fda <- no_fda %>%
  gt(groupname_col = "drug") %>%
  cols_label(
    country              = "Country",
    cancer_type          = "Cancer Type",
    indication           = "Indication",
    local_approval_clean = "Local Approval Date"
  ) %>%
  tab_header(
    title    = md("**Indications Without FDA Approval Anchor**"),
    subtitle = "Excluded from FDA-anchored delay analyses"
  ) %>%
  tab_source_note(
    source_note = md("*FDA approval date unavailable; these indications were excluded from delay analyses.*")
  ) %>%
  tab_options(table.font.size = 12, heading.title.font.size = 14)
table_no_fda

# ==============================================================================
# 3. SUBSETS USED FOR THE FIGURES
# ==============================================================================

# Pembrolizumab and Td-XD indications with both dates available
pembro_dates <- data %>%
  filter(drug == "Pembrolizumab", !is.na(fda_date), !is.na(local_date))

tdxd_dates <- data %>%
  filter(drug == "Td-XD", !is.na(fda_date), !is.na(local_date))

# Sample size per country (pembrolizumab)
pembro_dates %>% count(country, name = "n_delay")

# ==============================================================================
# 4. FIGURES
# ==============================================================================

# ---- Figure 1: delay distribution by country and drug ------------------------
ggplot(analysis_data,
       aes(x = reorder(country, fda_to_local_delay_months, median),
           y = fda_to_local_delay_months,
           fill = drug)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  coord_flip() +
  theme_bw() +
  labs(
    title = "Distribution of Approval Delays by Country",
    x = "Country",
    y = "Delay (Months)",
    fill = "Drug"
  )

# ---- Swimlane timelines (FDA approval = black point, local = coloured) -------
plot_timeline <- function(df, ycol, title, subtitle = NULL, facet = NULL) {
  p <- ggplot(df, aes(y = .data[[ycol]], color = country)) +
    geom_segment(aes(x = fda_date, xend = local_date, yend = .data[[ycol]]),
                 linewidth = 0.9) +
    geom_point(aes(x = fda_date), color = "black", size = 2) +
    geom_point(aes(x = local_date), size = 2.5) +
    labs(title = title, subtitle = subtitle,
         x = "Approval Date", y = "Tumour Type", color = "Country") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_blank(),
          plot.title = element_text(face = "bold", size = 14))
  if (!is.null(facet)) p <- p + facet_wrap(vars(.data[[facet]]))
  p
}

# Pembrolizumab: by tumour type, curative intent, ESMO-MCBS category
plot_timeline(pembro_dates, "cancer_type",
              "Pembrolizumab: FDA vs APAC Approval Timeline by Tumour Type")

plot_timeline(pembro_dates, "cancer_type",
              "Pembrolizumab: FDA to APAC Approval Timeline",
              "Stratified by Curative vs Non-Curative Indications",
              facet = "curative_group")

plot_timeline(filter(pembro_dates, !is.na(mcbs_group)), "cancer_type",
              "Pembrolizumab: FDA to APAC Approval Timeline",
              "Stratified by ESMO-MCBS Clinical Benefit Category",
              facet = "mcbs_group")

# Td-XD: by tumour type, curative intent, ESMO-MCBS category
plot_timeline(tdxd_dates, "cancer_type",
              "Td-XD: FDA to APAC Approval Timeline",
              "Stratified by Tumour Type")

plot_timeline(tdxd_dates, "cancer_type",
              "Td-XD: FDA to APAC Approval Timeline",
              "Stratified by Curative Intent",
              facet = "curative_group")

plot_timeline(filter(tdxd_dates, !is.na(mcbs_group)), "cancer_type",
              "Td-XD: FDA to APAC Approval Timeline",
              "Stratified by ESMO-MCBS Clinical Benefit",
              facet = "mcbs_group")

# ---- Delay by ESMO-MCBS category and ESMO guideline inclusion ----------------
plot_delay_box <- function(df, drug_name, xcol, title, xlab) {
  df %>%
    filter(drug == drug_name) %>%
    ggplot(aes(x = .data[[xcol]], y = fda_to_local_delay_months, fill = .data[[xcol]])) +
    geom_boxplot(alpha = 0.7) +
    labs(title = title, x = xlab, y = "Delay (Months)") +
    theme_minimal() +
    theme(legend.position = "none")
}

plot_delay_box(analysis_data, "Pembrolizumab", "mcbs_category",
               "Pembrolizumab: Approval Delay by MCBS Category", "MCBS Group")
plot_delay_box(analysis_data, "Td-XD", "mcbs_category",
               "Td-XD: Approval Delay by MCBS Category", "MCBS Group")
plot_delay_box(analysis_data, "Pembrolizumab", "esmo_guideline",
               "Pembrolizumab: Approval Delay by ESMO Guideline Inclusion", "ESMO Guideline")
plot_delay_box(analysis_data, "Td-XD", "esmo_guideline",
               "Td-XD: Approval Delay by ESMO Guideline Inclusion", "ESMO Guideline")

# ---- ESMO-MCBS score vs delay -------------------------------------------------
table(analysis_data$mcbs_score, useNA = "ifany")

# All indications
ggplot(analysis_data, aes(x = mcbs_score, y = fda_to_local_delay_months)) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.6, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_bw() +
  labs(
    title = "Association Between ESMO-MCBS Score and Approval Delay",
    subtitle = "Higher clinical benefit vs FDA-to-local approval delay",
    x = "ESMO-MCBS Score (1–5)",
    y = "Delay (Months)"
  )

# By drug
ggplot(analysis_data,
       aes(x = mcbs_score_direct, y = fda_to_local_delay_months, color = drug)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Association Between ESMO-MCBS Score and Approval Delay by Drug",
    x = "ESMO-MCBS Score (1–5)",
    y = "Delay (Months)",
    color = "Drug"
  ) +
  theme_minimal()

# ---- Median delay by country --------------------------------------------------
plot_median_country <- function(df, drug_name, fill_colour, title) {
  med <- df %>%
    filter(drug == drug_name, !is.na(fda_to_local_delay_months)) %>%
    group_by(country) %>%
    summarise(median_delay = median(fda_to_local_delay_months, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(median_delay)
  print(med)
  ggplot(med, aes(x = reorder(country, median_delay), y = median_delay)) +
    geom_col(fill = fill_colour) +
    coord_flip() +
    labs(title = title, x = "Country", y = "Median Delay (Months)") +
    theme_minimal(base_size = 14)
}

plot_median_country(data, "Pembrolizumab", "#E64B35",
                    "Pembrolizumab: Median FDA-to-Local Approval Delay by Country")
plot_median_country(data, "Td-XD", "#4DBBD5",
                    "Td-XD: Median FDA-to-Local Approval Delay by Country")

# ==============================================================================
# 5. EXPLORATORY TESTS (pembrolizumab, delay vs MCBS / ESMO guideline / score)
# ==============================================================================
pembro_data <- analysis_data %>% filter(drug == "Pembrolizumab")

wilcox.test(fda_to_local_delay_months ~ mcbs_group, data = pembro_data)
wilcox.test(fda_to_local_delay_months ~ esmo_group, data = pembro_data)

cor.test(analysis_data$mcbs_score,
         analysis_data$fda_to_local_delay_months,
         method = "spearman")
