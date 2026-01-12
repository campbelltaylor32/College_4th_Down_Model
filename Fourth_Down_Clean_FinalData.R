require(tidyverse)
require(data.table)


### Load in 4th Down Data ### 
data <- fread("fourth_down_model_data.csv")

### look at which plays are potentially falsely classified
data %>%
  group_by(play_type) %>%
  summarise(
    Went = max(went_for_it)
  ) %>% 
  arrange(desc(Went)) %>% 
  print(n = 35)


special_teams_play_types <- c(
  "Blocked Punt (Safety)", 
  "Defensive 2pt Conversion", 
  "Punt (Safety)", 
  "Punt Return", 
  "Punt Team Fumble Recovery", 
  "Punt Team Fumble Recovery Touchdown"
)

fumble_play_types <- c(
  "Fumble Return Touchdown", 
  "Fumble Recovery (Own)", 
  "Fumble Recovery (Opponent) Touchdown", 
  "Fumble Recovery (Opponent)", 
  "Fumble"
)

data <- data %>%
  mutate(
    went_for_it = ifelse(
      went_for_it == 1 & (
        ### explicit special teams play types
        play_type %in% special_teams_play_types |
          
          ### fumbles that occurred on punts or kicks
          (
            play_type %in% fumble_play_types &
              grepl("punt|kick", play_text, ignore.case = TRUE)
          ) |
          
          ### safeties caused by punts
          (
            play_type == "Safety" &
              grepl("punt", play_text, ignore.case = TRUE)
          )
      ),
      0,
      went_for_it
    )
  )


### Confirm Changes Worked ###
data %>%
  group_by(play_type) %>%
  summarise(
    Went = max(went_for_it)
  ) %>% 
  arrange(desc(Went)) %>% 
  print(n = 35)

### Write out Changed File ### 
data %>%
  write.csv(
    file = "Fourth_Down_Model_Data_v2.csv", 
    row.names = FALSE
  )


