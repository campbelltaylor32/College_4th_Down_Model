################################################################################
# Fourth Down Decision Analysis - Data Acquisition & Feature Engineering
# Author: Campbell Taylor
# Description: Acquires play-by-play data from cfbfastR, engineers features for
#              modeling 4th down go-for-it decisions, and exports clean CSV
# Note: Memory-efficient version - processes PBP one year at a time
################################################################################

# ============================================================================
# SETUP & LIBRARIES
# ============================================================================

require(tidyverse)
require(cfbfastR)
require(lubridate)

# Set API Key
Sys.setenv(CFBD_API_KEY = "+7pFEyFsISDn4fKPCGPyI9K/7tylHLRlY1bqQbT9QFHE2t7h0e9J1k47PMOGWDll")

# Define years - aligned to 2015-2025 for PBP data consistency
years_download <- seq(2015, 2025)
weeks <- seq(1, 14)

# Define play types for classification (used later in loop)
punt_plays <- c("Punt", "Punt Return Touchdown", "Blocked Punt", 
                "Blocked Punt Touchdown", "Punt Touchdown")
fg_plays <- c("Field Goal Good", "Field Goal Missed", "Blocked Field Goal",
              "Blocked Field Goal Touchdown", "Field Goal Attempt")
penalty_plays <- c("Penalty")

# ============================================================================
# SECTION 1: GAME INFORMATION
# ============================================================================

cat("=== DOWNLOADING GAME INFORMATION ===\n")

tot_games <- data.frame()
for(i in seq_along(years_download)){
  cat(paste0("Season: ", years_download[i], "...\n"))
  for(j in seq_along(weeks)){
    tmp_games <- tryCatch(
      cfbd_game_info(year = years_download[i], week = weeks[j]),
      error = function(e) data.frame()
    )
    if(nrow(tmp_games) > 0){
      tot_games <- rbind(tot_games, tmp_games)
    }
  }
}

# Clean Game Info
tot_games <- tot_games %>%
  filter(completed == TRUE) %>%
  select(
    game_id, season, week, season_type, start_date,
    neutral_site, conference_game, venue_id,
    home_id, home_team, home_conference,
    home_points, away_id, away_team, away_conference,
    away_points
  ) %>%
  arrange(season, week) %>%
  distinct()

cat(paste0("Total games downloaded: ", nrow(tot_games), "\n"))

# ============================================================================
# SECTION 2: BETTING DATA
# ============================================================================

cat("\n=== DOWNLOADING BETTING DATA ===\n")

tot_betting <- data.frame()
for(i in seq_along(years_download)){
  cat(paste0("Season: ", years_download[i], "...\n"))
  for(j in seq_along(weeks)){
    tmp_betting <- tryCatch(
      cfbd_betting_lines(year = years_download[i], week = weeks[j]),
      error = function(e) data.frame()
    )
    if(nrow(tmp_betting) > 0){
      tot_betting <- rbind(tot_betting, tmp_betting)
    }
  }
}

# Clean Betting Data - take one spread. Don't need multiple providers
tot_betting <- tot_betting %>%
  select(game_id, home_team, away_team, spread, formatted_spread) %>%
  group_by(game_id) %>%
  slice_max(n = 1, order_by = spread, with_ties = FALSE) %>%
  ungroup() %>%
  distinct()

# Merge game info with betting
fin_data <- merge(tot_games, tot_betting, by = c("game_id", "home_team", "away_team"), all.x = TRUE)

fin_data <- fin_data %>%
  rowwise() %>%
  mutate(
    home_favored = ifelse(
      !is.na(formatted_spread) & grepl(home_team, formatted_spread, fixed = TRUE),
      1, 0
    ),
    away_favored = ifelse(home_favored == 0 & !is.na(spread), 1, 0)
  ) %>%
  ungroup()

# Final game-level data
game_data <- fin_data %>%
  select(
    game_id, season, week, home_team, away_team, neutral_site,
    conference_game, spread, formatted_spread, home_favored, away_favored,
    home_points, away_points
  )

cat(paste0("Games with betting data: ", sum(!is.na(game_data$spread)), "\n"))

# Clean up
rm(tot_games, tot_betting, fin_data)
gc()

# ============================================================================
# SECTION 3: TEAM TALENT COMPOSITE
# ============================================================================

cat("\n=== DOWNLOADING TEAM TALENT DATA ===\n")

tot_talent <- data.frame()
for(i in seq_along(years_download)){
  tmp_talent <- tryCatch(
    cfbd_team_talent(year = years_download[i]),
    error = function(e) data.frame()
  )
  if(nrow(tmp_talent) > 0){
    tmp_talent$year <- years_download[i]
    tmp_talent$scaled_talent <- as.vector(scale(tmp_talent$talent))
    tot_talent <- rbind(tot_talent, tmp_talent)
  }
  cat(paste0("Year ", years_download[i], ": ", nrow(tmp_talent), " teams\n"))
}

talent_data <- tot_talent %>%
  select(team = school, year, talent, scaled_talent) %>%
  distinct()

cat(paste0("Total talent records: ", nrow(talent_data), "\n"))

rm(tot_talent)
gc()

# ============================================================================
# SECTION 4: COACHES DATA
# ============================================================================

cat("\n=== DOWNLOADING COACHES DATA ===\n")

coach_years <- seq(2015, 2025)
tot_coaches <- data.frame()

for(i in seq_along(coach_years)){
  tmp_coaches <- tryCatch(
    cfbd_coaches(year = coach_years[i]),
    error = function(e) data.frame()
  )
  
  if(nrow(tmp_coaches) > 0){
    tmp_coaches <- tmp_coaches %>%
      mutate(
        coach_name = paste0(first_name, ' ', last_name)
      ) %>%
      select(coach_name, team = school, year, games, wins, losses)
    tot_coaches <- rbind(tot_coaches, tmp_coaches)
  }
  cat(paste0("Year ", coach_years[i], ": ", nrow(tmp_coaches), " coaches\n"))
}

coach_data <- tot_coaches %>%
  select(coach_name, team, year) %>%
  distinct()

cat(paste0("Total coach-team-year records: ", nrow(coach_data), "\n"))

rm(tot_coaches)
gc()

# ============================================================================
# SECTION 5: PLAY-BY-PLAY DATA - MEMORY EFFICIENT (ONE YEAR AT A TIME)
# ============================================================================

cat("\n=== DOWNLOADING & PROCESSING PLAY-BY-PLAY DATA (YEAR BY YEAR) ===\n")
cat("Processing one year at a time to manage memory...\n\n")

# Initialize empty dataframe for all 4th down plays
all_fourth_down_plays <- data.frame()

for(i in seq_along(years_download)){
  
  current_year <- years_download[i]
  cat(paste0("========== YEAR ", current_year, " ==========\n"))
  
  # ----- 5a: Load PBP for this year only -----
  year_pbp <- data.frame()
  
  for(j in seq_along(weeks)){
    cat(paste0("  Downloading Week ", weeks[j], "...\n"))
    tmp_pbp <- tryCatch(
      cfbd_pbp_data(year = current_year, week = weeks[j], epa_wpa = TRUE),
      error = function(e) data.frame()
    )
    if(nrow(tmp_pbp) > 0){
      tmp_pbp$week <- weeks[j]
      tmp_pbp$year <- current_year
      year_pbp <- rbind(year_pbp, tmp_pbp)
    }
  }
  
  cat(paste0("  Total plays for ", current_year, ": ", nrow(year_pbp), "\n"))
  
  # Skip if no data for this year
  if(nrow(year_pbp) == 0){
    cat(paste0("  No data for ", current_year, ", skipping...\n\n"))
    next
  }
  
  # ----- 5b: Filter to 4th down immediately -----
  year_4th_down <- year_pbp %>%
    filter(down == 4) %>%
    filter(!is.na(play_type))
  
  cat(paste0("  4th down plays: ", nrow(year_4th_down), "\n"))
  
  # Clear full PBP from memory immediately
  rm(year_pbp, tmp_pbp)
  gc()
  
  # Skip if no 4th down plays
  if(nrow(year_4th_down) == 0){
    cat(paste0("  No 4th down plays for ", current_year, ", skipping...\n\n"))
    next
  }
  
  # ----- 5c: Create target variable -----
  year_4th_down <- year_4th_down %>%
    mutate(
      is_punt = play_type %in% punt_plays,
      is_fg = play_type %in% fg_plays | grepl("Field Goal", play_type, ignore.case = TRUE),
      is_penalty = play_type %in% penalty_plays,
      went_for_it = as.integer(!(is_punt | is_fg | is_penalty))
    )
  
  cat(paste0("  Went for it: ", sum(year_4th_down$went_for_it), 
             " (", round(mean(year_4th_down$went_for_it) * 100, 1), "%)\n"))
  
  # ----- 5d: Calculate within-game contextual features -----
  # These features depend on previous plays within the same game/team
  # Safe to calculate per-year since games don't span years
  
  year_4th_down <- year_4th_down %>%
    arrange(game_id, id_play) %>%
    group_by(game_id, pos_team) %>%
    mutate(
      # Previous 4th down play EPA (lagged)
      prev_play_epa = lag(EPA, default = 0),
      prev_play_success = as.integer(lag(EPA, default = 0) > 0),
      
      # Cumulative stats up to this point
      plays_before = row_number() - 1,
      cumulative_epa = cumsum(lag(EPA, default = 0)),
      
      # 4th down success earlier in game (only for go-for-it attempts)
      prev_4th_down_attempts = cumsum(lag(went_for_it, default = 0)),
      prev_4th_down_successes = cumsum(lag(went_for_it * as.integer(EPA > 0), default = 0)),
      prev_4th_down_success_rate = ifelse(
        prev_4th_down_attempts > 0,
        prev_4th_down_successes / prev_4th_down_attempts,
        0.5  # Default to 50% if no prior attempts
      )
    ) %>%
    ungroup()
  
  # ----- 5e: Append to cumulative dataframe -----
  all_fourth_down_plays <- rbind(all_fourth_down_plays, year_4th_down)
  
  cat(paste0("  Cumulative 4th down plays: ", nrow(all_fourth_down_plays), "\n\n"))
  
  # Clear year data from memory
  rm(year_4th_down)
  gc()
}

cat(paste0("=== TOTAL 4TH DOWN PLAYS COLLECTED: ", nrow(all_fourth_down_plays), " ===\n"))

# ============================================================================
# SECTION 6: MERGE ALL DATA SOURCES
# ============================================================================

cat("\n=== MERGING DATA SOURCES ===\n")

# Merge with game data
fourth_down_merged <- all_fourth_down_plays %>%
  left_join(
    game_data,
    by = c("game_id", "year" = "season", "week")
  )

cat(paste0("After game merge: ", nrow(fourth_down_merged), " rows\n"))

# Clear all_fourth_down_plays to save memory
rm(all_fourth_down_plays)
gc()

# Determine if possession team is home or away
fourth_down_merged <- fourth_down_merged %>%
  mutate(
    pos_team_is_home = as.integer(pos_team == home_team),
    pos_team_is_away = as.integer(pos_team == away_team)
  )

# Merge talent for possession team
fourth_down_merged <- fourth_down_merged %>%
  left_join(
    talent_data %>% rename(pos_talent = talent, pos_scaled_talent = scaled_talent),
    by = c("pos_team" = "team", "year")
  )

# Merge talent for defensive team
fourth_down_merged <- fourth_down_merged %>%
  left_join(
    talent_data %>% rename(def_talent = talent, def_scaled_talent = scaled_talent),
    by = c("def_pos_team" = "team", "year")
  )

# Calculate talent gap
fourth_down_merged <- fourth_down_merged %>%
  mutate(
    talent_gap = pos_talent - def_talent,
    scaled_talent_gap = pos_scaled_talent - def_scaled_talent
  )

# Merge coach data for possession team
fourth_down_merged <- fourth_down_merged %>%
  left_join(
    coach_data,
    by = c("pos_team" = "team", "year")
  )

cat(paste0("After all merges: ", nrow(fourth_down_merged), " rows\n"))

# ============================================================================
# SECTION 7: ENGINEER FINAL FEATURES
# ============================================================================

cat("\n=== ENGINEERING FINAL FEATURES ===\n")
fourth_down_merged <- fourth_down_merged %>% 
  rename(
    spread = spread.x
  ) %>% 
  select(-spread.y)

fourth_down_final <- fourth_down_merged %>%
  mutate(
    # Time features
    seconds_remaining_half = (clock_minutes * 60) + clock_seconds,
    seconds_remaining_game = case_when(
      period == 1 ~ (clock_minutes * 60) + clock_seconds + (30 * 60),
      period == 2 ~ (clock_minutes * 60) + clock_seconds,
      period == 3 ~ (clock_minutes * 60) + clock_seconds + (15 * 60),
      period == 4 ~ (clock_minutes * 60) + clock_seconds,
      TRUE ~ (clock_minutes * 60) + clock_seconds
    ),
    minutes_remaining_game = seconds_remaining_game / 60,
    
    # Score features (from possession team perspective)
    point_differential = pos_team_score - def_pos_team_score,
    total_score = pos_team_score + def_pos_team_score,
    is_trailing = as.integer(point_differential < 0),
    is_leading = as.integer(point_differential > 0),
    is_tied = as.integer(point_differential == 0),
    abs_point_diff = abs(point_differential),
    
    # Betting/underdog features
    pos_team_spread = case_when(
      pos_team_is_home == 1 ~ -spread,  # Home team spread (negative = favored)
      pos_team_is_away == 1 ~ spread,
      TRUE ~ NA_real_
    ),
    pos_team_is_underdog = as.integer(pos_team_spread > 0),
    pos_team_is_favorite = as.integer(pos_team_spread < 0),
    
    # Field position features
    in_opponent_territory = as.integer(yards_to_goal <= 50),
    in_own_territory = as.integer(yards_to_goal > 50),
    in_red_zone = as.integer(yards_to_goal <= 20),
    in_scoring_position = as.integer(yards_to_goal <= 35),
    
    # Down and distance features
    short_yardage = as.integer(distance <= 3),
    medium_yardage = as.integer(distance > 3 & distance <= 7),
    long_yardage = as.integer(distance > 7),
    log_distance = log(distance + 1),
    
    # Goal to go indicator
    goal_to_go = as.integer(Goal_To_Go),
    
    # Game situation features
    late_and_close = as.integer(period >= 4 & abs_point_diff <= 8),
    garbage_time = as.integer(
      (period == 4 & abs_point_diff > 24) |
        (period == 3 & abs_point_diff > 35)
    ),
    two_minute_drill = as.integer(
      (period == 2 | period == 4) & seconds_remaining_half <= 120
    ),
    
    # EPA/WPA context
    wp_leverage = abs(wp_before - 0.5),  # How important is this play?
    
    # Convert success for previous plays
    prev_play_successful = as.integer(prev_play_success == 1),
    
    # Conference game indicator
    is_conference_game = as.integer(conference_game == TRUE),
    
    # Neutral site indicator
    is_neutral_site = as.integer(neutral_site == TRUE)
  )

# Clear merged data
rm(fourth_down_merged)
gc()

# ============================================================================
# SECTION 8: SELECT FINAL COLUMNS & CLEAN
# ============================================================================

cat("\n=== SELECTING FINAL COLUMNS ===\n")

model_data <- fourth_down_final %>%
  select(
    # Identifiers
    id_play,
    game_id,
    year,
    week,
    pos_team,
    def_pos_team,
    coach_name,
    
    # Target variable
    went_for_it,
    
    # Play context
    play_type,
    play_text,
    period,
    down,
    distance,
    log_distance,
    yards_to_goal,
    
    # Time features
    clock_minutes,
    clock_seconds,
    seconds_remaining_half,
    seconds_remaining_game,
    minutes_remaining_game,
    
    # Score features
    pos_team_score,
    def_pos_team_score,
    point_differential,
    total_score,
    is_trailing,
    is_leading,
    is_tied,
    abs_point_diff,
    
    # Betting features
    spread,
    pos_team_spread,
    pos_team_is_underdog,
    pos_team_is_favorite,
    
    # Field position
    in_opponent_territory,
    in_own_territory,
    in_red_zone,
    in_scoring_position,
    goal_to_go,
    
    # Down and distance categories
    short_yardage,
    medium_yardage,
    long_yardage,
    
    # Talent features
    pos_talent,
    def_talent,
    talent_gap,
    scaled_talent_gap,
    
    # Game situation
    late_and_close,
    garbage_time,
    two_minute_drill,
    is_conference_game,
    is_neutral_site,
    pos_team_is_home,
    
    # Historical context within game
    prev_play_epa,
    prev_play_successful,
    cumulative_epa,
    prev_4th_down_attempts,
    prev_4th_down_success_rate,
    
    # Advanced metrics
    EPA,
    wp_before,
    wp_leverage
  ) %>%
  # Remove rows with missing critical features
  filter(
    !is.na(distance),
    !is.na(yards_to_goal),
    !is.na(period),
    !is.na(went_for_it)
  )

# Handle remaining NAs in numeric columns with sensible defaults
model_data <- model_data %>%
  mutate(
    across(where(is.numeric), ~replace_na(., 0)),
    coach_name = replace_na(coach_name, "Unknown")
  )

# Clear fourth_down_final
rm(fourth_down_final)
gc()

# ============================================================================
# SECTION 9: SUMMARY STATISTICS
# ============================================================================

cat("\n=== DATA SUMMARY ===\n")
cat(paste0("Total 4th down plays in final dataset: ", nrow(model_data), "\n"))
cat(paste0("Unique games: ", n_distinct(model_data$game_id), "\n"))
cat(paste0("Unique coaches: ", n_distinct(model_data$coach_name), "\n"))
cat(paste0("Years covered: ", min(model_data$year), " - ", max(model_data$year), "\n"))
cat(paste0("\nTarget distribution:\n"))
cat(paste0("  Went for it: ", sum(model_data$went_for_it), 
           " (", round(mean(model_data$went_for_it) * 100, 2), "%)\n"))
cat(paste0("  Punt/FG: ", sum(model_data$went_for_it == 0),
           " (", round(mean(model_data$went_for_it == 0) * 100, 2), "%)\n"))

# Year-by-year breakdown
cat("\n=== YEAR-BY-YEAR BREAKDOWN ===\n")
year_summary <- model_data %>%
  group_by(year) %>%
  summarise(
    total_plays = n(),
    went_for_it = sum(went_for_it),
    go_rate = round(mean(went_for_it) * 100, 1),
    .groups = "drop"
  )
print(year_summary)

# ============================================================================
# SECTION 10: EXPORT DATA
# ============================================================================

cat("\n=== EXPORTING DATA ===\n")
model_data <- model_data %>% 
  filter(
    play_type != "Timeout", 
    play_type != "Uncategorized", 
    play_type != "End Period", 
    play_type != "End of Regulation", 
    play_type != "End of Game", 
    play_type != "placeholder"
  )

# Export full model data
write_csv(model_data, "fourth_down_model_data.csv")
cat("Saved: fourth_down_model_data.csv\n")

# Export coach reference data for later visualization
coach_summary <- model_data %>%
  group_by(coach_name, pos_team, year) %>%
  summarise(
    total_4th_downs = n(),
    went_for_it_count = sum(went_for_it),
    went_for_it_rate = mean(went_for_it),
    .groups = "drop"
  )

write_csv(coach_summary, "coach_summary_data.csv")
cat("Saved: coach_summary_data.csv\n")

# Export talent data for reference
write_csv(talent_data, "talent_data.csv")
cat("Saved: talent_data.csv\n")

# Final memory cleanup
rm(game_data, talent_data, coach_data)
gc()

cat("\n=== PIPELINE COMPLETE ===\n")
cat(paste0("Final memory usage: ", round(sum(gc()[,2]), 1), " MB\n"))
