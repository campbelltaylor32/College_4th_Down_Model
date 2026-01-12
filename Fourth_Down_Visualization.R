################################################################################
# Fourth Down Decision Analysis - Coach Aggression Visualization
# Author: CFB Analytics Project
# Description: Creates professional gt tables with team logos and coach images
#              for displaying coach aggression rankings
################################################################################

# ============================================================================
# SETUP & LIBRARIES
# ============================================================================

# Install packages if needed
packages_needed <- c("tidyverse", "gt", "gtExtras", "cfbfastR", "cfbplotR", 
                     "rvest", "httr", "glue", "scales", "webshot2")

for(pkg in packages_needed) {
  if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

library(tidyverse)
library(gt)
library(gtExtras)
library(cfbfastR)
library(cfbplotR)
library(scales)
library(glue)

Sys.setenv(CFBD_API_KEY = "+7pFEyFsISDn4fKPCGPyI9K/7tylHLRlY1bqQbT9QFHE2t7h0e9J1k47PMOGWDll")


# ============================================================================
# LOAD DATA
# ============================================================================

cat("=== LOADING DATA ===\n")

# Load coach aggression rankings from Python output
coach_aggression <- read_csv("coach_aggression_rankings.csv")
coach_aggression <- coach_aggression %>% 
  filter(last_year == 2025)
cat(paste0("Loaded ", nrow(coach_aggression), " coaches\n"))

# ============================================================================
# GET TEAM LOGOS
# ============================================================================

cat("\n=== FETCHING TEAM LOGOS ===\n")

# Get team info with logos from cfbfastR
team_info <- cfbd_team_info() %>%
  select(school, logo, color, alt_color, abbreviation) %>%
  rename(team = school) %>%
  distinct()

# Create manual mapping for common team name variations
team_name_mapping <- tribble(
  ~primary_team, ~standardized_team,
  "Miami", "Miami",
  "Miami (FL)", "Miami",
  "Miami (OH)", "Miami (OH)",
  "USC", "USC",
  "Southern California", "USC",
  "LSU", "LSU",
  "Louisiana State", "LSU",
  "Ole Miss", "Ole Miss",
  "Mississippi", "Ole Miss",
  "Pitt", "Pittsburgh",
  "Pittsburgh", "Pittsburgh",
  "NC State", "NC State",
  "North Carolina State", "NC State",
  "SMU", "SMU",
  "Southern Methodist", "SMU"
)

# Merge team logos with coach data
coach_data <- coach_aggression %>%
  left_join(team_name_mapping, by = "primary_team") %>%
  mutate(standardized_team = ifelse(is.na(standardized_team), primary_team, standardized_team)) %>%
  left_join(team_info, by = c("standardized_team" = "team"))

# Fill missing logos with placeholder
coach_data <- coach_data %>%
  mutate(
    logo = ifelse(is.na(logo), 
                  "https://a.espncdn.com/i/teamlogos/ncaa/500/default.png", 
                  logo),
    color = ifelse(is.na(color), "#333333", color)
  )

# ============================================================================
# PREPARE DATA FOR TABLE
# ============================================================================

cat("\n=== PREPARING TABLE DATA ===\n")

# Select and format columns for the table
table_data <- coach_data %>%
  arrange(aggression_rank) %>%
  mutate(
    # Format percentages
    actual_go_pct = actual_go_rate * 100,
    expected_go_pct = expected_go_rate * 100,
    aoe_pct = aggression_over_expected * 100,
    
    # Create year range
    years_active = paste0(first_year, "-", last_year),
    
    # Format AOE with sign
    aoe_display = sprintf("%+.1f%%", aoe_pct),
    
    # Determine if aggressive or conservative
    style_type = case_when(
      aggression_over_expected > 0.02 ~ "aggressive",
      aggression_over_expected < -0.02 ~ "conservative",
      TRUE ~ "neutral"
    )
  ) %>%
  select(
    aggression_rank,
    logo,
    coach_name,
    primary_team,
    years_active,
    total_opportunities,
    total_go_for_it,
    actual_go_pct,
    expected_go_pct,
    aoe_pct,
    aoe_display,
    style_type,
    color
  )

# ============================================================================
# CREATE TOP 25 AGGRESSIVE COACHES TABLE
# ============================================================================

cat("\n=== CREATING TOP 25 AGGRESSIVE COACHES TABLE ===\n")

top_25_aggressive <- table_data %>%
  head(25) %>%
  select(-style_type, -color)

gt_aggressive <- top_25_aggressive %>%
  gt() %>%
  
  # Title and subtitle
  tab_header(
    title = md("**Most Aggressive 4th Down Coaches**"),
    subtitle = md("*College Football 2015-2025 | Minimum 50 Fourth Down Opportunities*")
  ) %>%
  
  # Column labels
  cols_label(
    aggression_rank = "Rank",
    logo = "",
    coach_name = "Coach",
    primary_team = "Team",
    years_active = "Years",
    total_opportunities = "4th Downs",
    total_go_for_it = "Went For It",
    actual_go_pct = "Actual %",
    expected_go_pct = "Expected %",
    aoe_pct = "AOE",
    aoe_display = ""
  ) %>%
  
  # Format the logo column as images
  gt_img_rows(columns = logo, height = 30) %>%
  
  # Format numeric columns
  fmt_number(
    columns = c(actual_go_pct, expected_go_pct),
    decimals = 1,
    pattern = "{x}%"
  ) %>%
  
  fmt_number(
    columns = aoe_pct,
    decimals = 1,
    pattern = "{x}%",
    force_sign = TRUE
  ) %>%
  
  # Hide the text AOE display column (we formatted aoe_pct instead)
  cols_hide(columns = aoe_display) %>%
  
  # Color the AOE column
  data_color(
    columns = aoe_pct,
    palette = c("#FFFFFF", "#2E7D32"),
    domain = c(0, max(top_25_aggressive$aoe_pct) * 1.1)
  ) %>%
  
  # Add bar visualization for go-for-it rate
  gt_plt_bar_pct(
    column = actual_go_pct,
    scaled = TRUE,
    fill = "#1976D2",
    background = "#E3F2FD"
  ) %>%
  
  # Style the table
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  
  tab_style(
    style = list(
      cell_text(weight = "bold", size = px(14))
    ),
    locations = cells_body(columns = coach_name)
  ) %>%
  
  tab_style(
    style = list(
      cell_text(weight = "bold", color = "#2E7D32")
    ),
    locations = cells_body(columns = aoe_pct)
  ) %>%
  
  # Alternating row colors
  opt_row_striping(row_striping = TRUE) %>%
  
  # Table options
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(20),
    heading.subtitle.font.size = px(14),
    column_labels.font.weight = "bold",
    table.border.top.style = "solid",
    table.border.top.width = px(3),
    table.border.top.color = "#1976D2",
    heading.border.bottom.style = "solid",
    heading.border.bottom.width = px(2),
    heading.border.bottom.color = "#1976D2"
  )
  
  # Add source note
  gt_aggressive <- gt_aggressive %>%
  tab_source_note(
    source_note = md("**Data:** cfbfastR | **Model:** XGBoost/LightGBM Classification | **AOE** = Actual Go Rate - Expected Go Rate")
  ) %>%
  
  # Footer
  tab_footnote(
    footnote = "Aggression Over Expectation (AOE) measures how often a coach goes for it compared to model prediction",
    locations = cells_column_labels(columns = aoe_pct)
  )

# Save the table
gtsave(gt_aggressive, "top_25_aggressive_coaches.png", expand = 20)
gtsave(gt_aggressive, "top_25_aggressive_coaches.html")

cat("Saved: top_25_aggressive_coaches.png\n")
cat("Saved: top_25_aggressive_coaches.html\n")

# ============================================================================
# CREATE TOP 25 CONSERVATIVE COACHES TABLE
# ============================================================================

cat("\n=== CREATING TOP 25 CONSERVATIVE COACHES TABLE ===\n")

top_25_conservative <- table_data %>%
  arrange(desc(aggression_rank)) %>%
  head(25) %>%
  mutate(conservative_rank = row_number()) %>%
  select(-style_type, -color, -aggression_rank) %>%
  relocate(conservative_rank, .before = everything())

gt_conservative <- top_25_conservative %>%
  gt() %>%
  
  # Title and subtitle
  tab_header(
    title = md("**🏈 Most Conservative 4th Down Coaches**"),
    subtitle = md("*College Football 2015-2025 | Minimum 50 Fourth Down Opportunities*")
  ) %>%
  
  # Column labels
  cols_label(
    conservative_rank = "Rank",
    logo = "",
    coach_name = "Coach",
    primary_team = "Team",
    years_active = "Years",
    total_opportunities = "4th Downs",
    total_go_for_it = "Went For It",
    actual_go_pct = "Actual %",
    expected_go_pct = "Expected %",
    aoe_pct = "AOE",
    aoe_display = ""
  ) %>%
  
  # Format the logo column as images
  gt_img_rows(columns = logo, height = 30) %>%
  
  # Format numeric columns
  fmt_number(
    columns = c(actual_go_pct, expected_go_pct),
    decimals = 1,
    pattern = "{x}%"
  ) %>%
  
  fmt_number(
    columns = aoe_pct,
    decimals = 1,
    pattern = "{x}%",
    force_sign = TRUE
  ) %>%
  
  # Hide the text AOE display column
  cols_hide(columns = aoe_display) %>%
  
  # Color the AOE column (red for conservative)
  data_color(
    columns = aoe_pct,
    palette = c("#C62828", "#FFFFFF"),
    domain = c(min(top_25_conservative$aoe_pct) * 1.1, 0)
  ) %>%
  
  # Style the table
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  
  tab_style(
    style = list(
      cell_text(weight = "bold", size = px(14))
    ),
    locations = cells_body(columns = coach_name)
  ) %>%
  
  tab_style(
    style = list(
      cell_text(weight = "bold", color = "#C62828")
    ),
    locations = cells_body(columns = aoe_pct)
  ) %>%
  
  # Alternating row colors
  opt_row_striping(row_striping = TRUE) %>%
  
  # Table options
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(20),
    heading.subtitle.font.size = px(14),
    column_labels.font.weight = "bold",
    table.border.top.style = "solid",
    table.border.top.width = px(3),
    table.border.top.color = "#C62828",
    heading.border.bottom.style = "solid",
    heading.border.bottom.width = px(2),
    heading.border.bottom.color = "#C62828"
  ) %>%
  
  # Add source note
  tab_source_note(
    source_note = md("**Data:** cfbfastR | **Model:** XGBoost/LightGBM Classification | **AOE** = Actual Go Rate - Expected Go Rate")
  )

# Save the table
gtsave(gt_conservative, "top_25_conservative_coaches.png", expand = 20)
gtsave(gt_conservative, "top_25_conservative_coaches.html")

cat("Saved: top_25_conservative_coaches.png\n")
cat("Saved: top_25_conservative_coaches.html\n")

# ============================================================================
# CREATE FULL RANKINGS TABLE (HTML ONLY - TOO LARGE FOR PNG)
# ============================================================================

cat("\n=== CREATING FULL RANKINGS TABLE ===\n")

full_rankings <- table_data %>%
  select(-style_type, -color)

gt_full <- full_rankings %>%
  gt() %>%
  
  # Title and subtitle
  tab_header(
    title = md("**🏈 Complete Coach 4th Down Aggression Rankings**"),
    subtitle = md("*College Football 2015-2025 | Minimum 50 Fourth Down Opportunities*")
  ) %>%
  
  # Column labels
  cols_label(
    aggression_rank = "Rank",
    logo = "",
    coach_name = "Coach",
    primary_team = "Team",
    years_active = "Years",
    total_opportunities = "4th Downs",
    total_go_for_it = "Went For It",
    actual_go_pct = "Actual %",
    expected_go_pct = "Expected %",
    aoe_pct = "AOE",
    aoe_display = ""
  ) %>%
  
  # Format the logo column as images
  gt_img_rows(columns = logo, height = 25) %>%
  
  # Format numeric columns
  fmt_number(
    columns = c(actual_go_pct, expected_go_pct),
    decimals = 1,
    pattern = "{x}%"
  ) %>%
  
  fmt_number(
    columns = aoe_pct,
    decimals = 1,
    pattern = "{x}%",
    force_sign = TRUE
  ) %>%
  
  # Hide the text AOE display column
  cols_hide(columns = aoe_display) %>%
  
  # Color the AOE column (diverging palette)
  data_color(
    columns = aoe_pct,
    palette = c("#C62828", "#FFFFFF", "#2E7D32"),
    domain = c(-15, 0, 15)
  ) %>%
  
  # Style the table
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = coach_name)
  ) %>%
  
  # Alternating row colors
  opt_row_striping(row_striping = TRUE) %>%
  
  # Table options
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(18),
    heading.subtitle.font.size = px(12),
    column_labels.font.weight = "bold"
  ) %>%
  
  # Add source note
  tab_source_note(
    source_note = md("**Data:** cfbfastR | **AOE** = Aggression Over Expectation (Actual - Expected Go Rate)")
  )

# Save as HTML only (full table too large for PNG)
gtsave(gt_full, "full_coach_rankings.html")
cat("Saved: full_coach_rankings.html\n")

# ============================================================================
# CREATE SUMMARY STATISTICS TABLE
# ============================================================================

cat("\n=== CREATING SUMMARY STATISTICS TABLE ===\n")

summary_stats <- table_data %>%
  summarise(
    total_coaches = n(),
    total_opportunities = sum(total_opportunities),
    total_go_for_it = sum(total_go_for_it),
    avg_go_rate = mean(actual_go_pct),
    median_go_rate = median(actual_go_pct),
    avg_aoe = mean(aoe_pct),
    sd_aoe = sd(aoe_pct),
    most_aggressive = coach_name[which.max(aoe_pct)],
    most_conservative = coach_name[which.min(aoe_pct)]
  )

gt_summary <- summary_stats %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Value") %>%
  mutate(
    Metric = case_when(
      Metric == "total_coaches" ~ "Total Coaches Analyzed",
      Metric == "total_opportunities" ~ "Total 4th Down Plays",
      Metric == "total_go_for_it" ~ "Total Go-For-It Attempts",
      Metric == "avg_go_rate" ~ "Average Go-For-It Rate",
      Metric == "median_go_rate" ~ "Median Go-For-It Rate",
      Metric == "avg_aoe" ~ "Average AOE",
      Metric == "sd_aoe" ~ "Standard Deviation AOE",
      Metric == "most_aggressive" ~ "Most Aggressive Coach",
      Metric == "most_conservative" ~ "Most Conservative Coach",
      TRUE ~ Metric
    ),
    Value = case_when(
      grepl("Rate|AOE|Deviation", Metric) & !grepl("Coach", Metric) ~ 
        paste0(round(as.numeric(Value), 2), "%"),
      grepl("Total", Metric) & !grepl("Coach", Metric) ~ 
        format(as.numeric(Value), big.mark = ","),
      TRUE ~ as.character(Value)
    )
  ) %>%
  gt() %>%
  tab_header(
    title = md("**📊 Fourth Down Analysis Summary**")
  ) %>%
  cols_label(
    Metric = "Statistic",
    Value = "Value"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Metric)
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(18)
  )

gtsave(gt_summary, "summary_statistics.png", expand = 20)
gtsave(gt_summary, "summary_statistics.html")

cat("Saved: summary_statistics.png\n")
cat("Saved: summary_statistics.html\n")

# ============================================================================
# CREATE CONFERENCE BREAKDOWN TABLE
# ============================================================================

cat("\n=== CREATING CONFERENCE ANALYSIS ===\n")

# Get conference info
team_conferences <- cfbd_team_info() %>%
  select(school, conference) %>%
  rename(primary_team = school)

# Merge with coach data
coach_conf <- coach_data %>%
  left_join(team_conferences, by = "primary_team") %>%
  filter(!is.na(conference))

# Conference summary
conf_summary <- coach_conf %>%
  group_by(conference) %>%
  summarise(
    n_coaches = n(),
    avg_opportunities = mean(total_opportunities),
    avg_go_rate = mean(actual_go_rate) * 100,
    avg_aoe = mean(aggression_over_expected) * 100,
    most_aggressive = coach_name[which.max(aggression_over_expected)],
    .groups = "drop"
  ) %>%
  filter(n_coaches >= 3) %>%
  arrange(desc(avg_aoe))

gt_conference <- conf_summary %>%
  gt() %>%
  tab_header(
    title = md("**🏈 Fourth Down Aggression by Conference**"),
    subtitle = md("*Average metrics for coaches in each conference*")
  ) %>%
  cols_label(
    conference = "Conference",
    n_coaches = "Coaches",
    avg_opportunities = "Avg 4th Downs",
    avg_go_rate = "Avg Go Rate",
    avg_aoe = "Avg AOE",
    most_aggressive = "Most Aggressive"
  ) %>%
  fmt_number(
    columns = c(avg_go_rate, avg_aoe),
    decimals = 1,
    pattern = "{x}%"
  ) %>%
  fmt_number(
    columns = avg_opportunities,
    decimals = 0
  ) %>%
  data_color(
    columns = avg_aoe,
    palette = c("#C62828", "#FFFFFF", "#2E7D32"),
    domain = c(-5, 0, 5)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(18)
  )

gtsave(gt_conference, "conference_breakdown.png", expand = 20)
gtsave(gt_conference, "conference_breakdown.html")

cat("Saved: conference_breakdown.png\n")
cat("Saved: conference_breakdown.html\n")

# ============================================================================
# EXPORT CLEAN CSV FOR BLOG
# ============================================================================

cat("\n=== EXPORTING CLEAN DATA FOR BLOG ===\n")

blog_data <- table_data %>%
  select(
    Rank = aggression_rank,
    Coach = coach_name,
    Team = primary_team,
    Years = years_active,
    `4th Down Opps` = total_opportunities,
    `Went For It` = total_go_for_it,
    `Actual Go %` = actual_go_pct,
    `Expected Go %` = expected_go_pct,
    `AOE %` = aoe_pct
  ) %>%
  mutate(
    `Actual Go %` = round(`Actual Go %`, 1),
    `Expected Go %` = round(`Expected Go %`, 1),
    `AOE %` = round(`AOE %`, 2)
  )

write_csv(blog_data, "coach_aggression_for_blog.csv")
cat("Saved: coach_aggression_for_blog.csv\n")

# ============================================================================
# COMPLETE
# ============================================================================

cat("\n" + paste(rep("=", 60), collapse = "") + "\n")
cat("VISUALIZATION PIPELINE COMPLETE\n")
cat(paste(rep("=", 60), collapse = "") + "\n")

cat("\nOutput files created:\n")
cat("  - top_25_aggressive_coaches.png/html\n")
cat("  - top_25_conservative_coaches.png/html\n")
cat("  - full_coach_rankings.html\n")
cat("  - summary_statistics.png/html\n")
cat("  - conference_breakdown.png/html\n")
cat("  - coach_aggression_for_blog.csv\n")