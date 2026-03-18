# NFL Project #
install.packages("nflfastR")
install.packages("nflreadr")

library(nflfastR)
library(nflreadr)
library(tidyverse)

pbp_data_multi <- load_pbp(c(2022, 2023, 2024, 2025))
view(field_descriptions)
pbp_data_multi

# Creating Kick Dataset

kicks <- pbp_data_multi %>%
  filter(play_type %in% c("field_goal", "extra_point")) %>%
  filter(!is.na(kicker_player_name))

# Kicker Related Columns

names(pbp_data_multi)[str_detect(names(pbp_data_multi), "kick")]

nrow(kicks)
length(unique(kicks$kicker_player_name))
table(kicks$season)

# Success Rate, Binary Variable 

kicks <- kicks %>%
  mutate(
    kick_made = case_when(
      play_type == "field_goal" & field_goal_result == "made" ~ 1,
      play_type == "extra_point" & extra_point_result == "good" ~ 1,
      TRUE ~ 0
    )
  )

# Overall success rate 

mean(kicks$kick_made)

# Separate FG vs XP
kicks %>%
  group_by(play_type) %>%
  summarise(success_rate = mean(kick_made),
            attempts = n())

# Distribution of Kick Distance for Field Goals 
fg_only <- kicks %>%
  filter(play_type == "field_goal")

summary(fg_only$kick_distance)

ggplot(fg_only, aes(x = kick_distance)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Distribution of Field Goal Distances")

# Accuracy by Distance

fg_only <- fg_only %>%
  mutate(
    distance_bucket = case_when(
      kick_distance < 30 ~ "Under 30",
      kick_distance < 40 ~ "30-39",
      kick_distance < 50 ~ "40-49",
      kick_distance < 60 ~ "50-59",
      TRUE ~ "60+"
    )
  )

fg_only %>%
  group_by(distance_bucket) %>%
  summarise(
    attempts = n(),
    success_rate = mean(kick_made)
  )

# Clutch Factor

fg_only <- fg_only %>%
  mutate(
    clutch = ifelse(qtr == 4 & 
                      abs(score_differential) <= 8 & 
                      game_seconds_remaining <= 300, 
                    1, 0)
  )

# Verification: check the counts to make sure it worked
table(fg_only$clutch)

# KPOV: Evaluating Kicker Reliability under Pressure
clutch_rankings <- fg_only %>%
  filter(clutch == 1) %>%
  group_by(kicker_player_name) %>%
  summarise(
    clutch_attempts = n(),
    clutch_accuracy = mean(kick_made, na.rm = TRUE),
    clutch_epa = sum(epa, na.rm = TRUE)
  ) %>%
  arrange(desc(clutch_accuracy), desc(clutch_epa))

clutch_rankings

# KPIV #

kicker_percentages
kick_distance
punt_blocked
game_seconds_remaining
score_differential
roof
posteam_type
wind
temp
surface

# --- 1. Quantitative KPIVs (Omit NAs) ---
quant_kpivs <- kicks %>% 
  select(kick_distance, wind, temp, game_seconds_remaining) %>%
  drop_na() # Removes any row with a missing value in these 4 columns

# Five Number Summary
summary(quant_kpivs)

# Distance Histogram (Using the cleaned kicks data)
ggplot(kicks %>% drop_na(kick_distance), aes(x = kick_distance)) + 
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Kick Distances (2022-2025)", x = "Yards")

# Wind vs Result (Omit NAs in wind and result)
ggplot(kicks %>% drop_na(wind, field_goal_result), aes(x = field_goal_result, y = wind)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Wind Speed vs. Field Goal Outcome")


# --- 2. Categorical KPIVs (Stadium & Score) ---

# Frequency of Stadium Types (Excluding NAs)
table(kicks$roof, useNA = "no")

# Accuracy by Stadium Type (Already has filter(!is.na))
kicks %>%
  filter(!is.na(roof), !is.na(kick_made)) %>%
  group_by(roof) %>%
  summarise(accuracy = mean(kick_made)) %>%
  ggplot(aes(x = roof, y = accuracy, fill = roof)) +
  geom_col() +
  coord_cartesian(ylim = c(0.7, 1.0)) + 
  labs(title = "Kicking Accuracy by Stadium Roof Type")

# 5-Number summary for Score Differential (Omit NAs)
summary(kicks$score_differential[!is.na(kicks$score_differential)])

# Density of kicks by score margin (Omit NAs)
ggplot(kicks %>% drop_na(score_differential), aes(x = score_differential)) +
  geom_density(fill = "orange", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "When do kicks happen?", subtitle = "0 indicates a tied game")

# Calculate Season-Long Kicking Percentages
kicker_percentages <- kicks %>%
  group_by(kicker_player_name) %>%
  summarise(
    # 1. Field Goal Percentage
    fg_attempts = sum(play_type == "field_goal", na.rm = TRUE),
    fg_made = sum(field_goal_result == "made", na.rm = TRUE),
    fg_pct = ifelse(fg_attempts > 0, (fg_made / fg_attempts) * 100, NA),
    
    # 2. Extra Point Percentage
    xp_attempts = sum(play_type == "extra_point", na.rm = TRUE),
    xp_made = sum(extra_point_result == "good", na.rm = TRUE),
    xp_pct = ifelse(xp_attempts > 0, (xp_made / xp_attempts) * 100, NA),
    
    .groups = "drop"
  ) %>%
  # Filter for active kickers (e.g., at least 10 FG attempts)
  filter(fg_attempts >= 10) %>%
  arrange(desc(fg_pct))

# View the results
print(kicker_percentages)

ggplot(kicker_percentages, aes(x = fg_pct, y = xp_pct)) +
  geom_point(aes(size = fg_attempts), color = "#006778", alpha = 0.6) +
  geom_text(aes(label = kicker_player_name), vjust = -1, size = 3) +
  labs(
    title = "Field Goal vs. Extra Point Consistency",
    subtitle = "Size indicates total FG attempts",
    x = "Field Goal Accuracy (%)",
    y = "Extra Point Accuracy (%)"
  ) +
  theme_minimal()

# KPOV #

field_goal_result
extra_point_result
kick_distance (max)
fg_prob
epa

# --- KPOV EDA: EPA and fg_prob (Omitting NAs) ---

# 1. Five Number Summary
# We use drop_na() specifically on these two columns to see the true range
kicks %>% 
  select(epa, fg_prob) %>% 
  drop_na() %>% 
  summary()

# 2. Visual: Distribution of EPA
# High-leverage kicks (game winners) create the "tails" in this distribution
ggplot(kicks %>% drop_na(epa), aes(x = epa)) +
  geom_histogram(binwidth = 0.5, fill = "purple", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Expected Points Added (EPA)", 
    subtitle = "Positive values (Right) = Performance above expected; Negative (Left) = Costly misses",
    x = "EPA Value",
    y = "Frequency"
  )

# 3. Visual: FG Probability vs Distance (The "Logic" Check)
# This helps confirm that your fg_prob (Input) aligns with kick_distance
ggplot(kicks %>% drop_na(fg_prob, kick_distance), aes(x = kick_distance, y = fg_prob)) +
  geom_point(alpha = 0.1, color = "darkgreen") +
  geom_smooth(color = "orange") +
  labs(title = "Expected Make Probability by Distance", x = "Yards", y = "Probability (0 to 1)")

# Create the Ranking Table
kicker_rankings <- kicks %>%
  filter(!is.na(epa), !is.na(fg_prob)) %>%
  group_by(kicker_player_name, posteam) %>%
  summarise(
    attempts = n(),
    avg_dist = mean(kick_distance, na.rm = TRUE),
    total_epa = sum(epa),
    # Calculate Field Goals Over Expected (Cumulative)
    total_fgoe = sum(kick_made - fg_prob), 
    # Logic: How much better than 'average' are they per kick?
    efficiency_index = total_fgoe / attempts,
    .groups = "drop"
  ) %>%
  filter(attempts > 20) %>% # Exclude small sample sizes
  arrange(desc(total_epa))

# Display the Top 10 Potential Targets
head(kicker_rankings, 10)

ggplot(kicker_rankings, aes(x = efficiency_index, y = total_epa)) +
  geom_point(aes(size = attempts), color = "#A71930", alpha = 0.6) +
  geom_text(aes(label = kicker_player_name), vjust = -1, size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "NFL Kicker Performance Tiers (2022-2025)",
    subtitle = "Top Right = Elite Value & High Efficiency (Primary Trade Targets)",
    x = "Field Goals Over Expected (Per Attempt)",
    y = "Total Expected Points Added (EPA)"
  ) +
  theme_minimal()

library(ggrepel) # To prevent text overlapping if you use scatter
install.packages("viridis")
library(viridis)

# 1. Create a "Power Rank" by combining KPOVs
top_kickers <- kicker_rankings %>%
  mutate(power_score = total_epa * (1 + efficiency_index)) %>% # Weights efficiency
  slice_max(power_score, n = 15) # Keep only the elite targets

# 2. Precise Bar Chart
ggplot(top_kickers, aes(x = reorder(kicker_player_name, power_score), y = power_score, fill = power_score)) +
  geom_col(show.legend = FALSE) +
  coord_flip() + # Makes names easier to read
  scale_fill_viridis_c(option = "mako") + 
  theme_minimal() +
  labs(
    title = "Top 15 NFL Kicker Trade Targets (2022-2025)",
    subtitle = "Ranked by Kicker Power Score (EPA adjusted for Efficiency)",
    x = NULL,
    y = "Kicker Power Score"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.y = element_text(size = 11)
  )
