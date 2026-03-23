library(nflreadr)
library(dplyr)
library(ggplot2)

# Model 1 - Logistic Regression 

pbp <- load_pbp(2024)

fg <- pbp |>
  filter(play_type == "field_goal") |>
  mutate(
    made_kick = ifelse(field_goal_result == "made", 1, 0),
    roof = ifelse(is.na(roof), "unknown", roof)
  ) |>
  filter(
    !is.na(kick_distance),
    !is.na(kicker_player_name)
  )

# Logistic regression with multiple KPIVs
logit_model <- glm(
  made_kick ~ kick_distance + score_differential + roof,
  data = fg,
  family = "binomial"
)

summary(logit_model)

# Add predicted probabilities
fg <- fg |>
  mutate(
    expected_prob = predict(logit_model, type = "response")
  )

# Kicker-level results
model1_results <- fg |>
  group_by(kicker_player_name) |>
  summarise(
    attempts = n(),
    actual_pct = mean(made_kick),
    expected_pct = mean(expected_prob),
    performance_vs_expected = actual_pct - expected_pct,
    avg_distance = mean(kick_distance),
    .groups = "drop"
  ) |>
  filter(attempts >= 10) |>
  arrange(desc(performance_vs_expected))

print(model1_results)

# Plot (still shows distance effect clearly)
ggplot(fg, aes(x = kick_distance, y = made_kick)) +
  geom_jitter(height = 0.05, alpha = 0.3) +
  stat_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Field Goal Success Probability by Distance",
    x = "Kick Distance (yards)",
    y = "Made Kick (0 = Miss, 1 = Make)"
  )
 
