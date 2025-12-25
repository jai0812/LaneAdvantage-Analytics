library(tidyverse)
library(stringr)

df <- read_csv("lane_results_advanced.csv")
# data cleaning
# Merging  duplicate columns 
df <- df %>%
  mutate(
    athlete = coalesce(as.character(`athlete...8`), 
                       as.character(`athlete...9`), 
                       as.character(`athlete...10`), 
                       as.character(`athlete...11`)),
    
    country = coalesce(as.character(`country...12`),
                       as.character(`country...13`),
                       as.character(`country...14`)),
    
    reaction_time = coalesce(as.character(`reaction_time...16`),
                             as.character(`reaction_time...17`))
  )
# Keep only the useful columns
df <- df %>%
  select(
    event,
    gender,
    competition,
    year,
    round_raw,
    lane,
    position,
    athlete,
    country,
    time_or_mark,
    reaction_time
  )

# Cleaning time column: remove DQ / DNS / DNF / DSQ / dashes, then make numeric
invalid_codes <- c("DQ", "DNF", "DNS", "DSQ", "—", "-", "")

df <- df %>%
  mutate(
    time_chr = as.character(time_or_mark)
  ) %>%
  filter(!(time_chr %in% invalid_codes)) %>%
  filter(!is.na(time_chr)) %>%
  mutate(
    time = as.numeric(time_chr)
  ) %>%
  filter(!is.na(time))  

# Keep only 100m, 200m, 400m
df <- df %>%
  filter(event %in% c("100m", "200m", "400m"))

df <- df %>%
  mutate(
    round_index = as.integer(str_extract(round_raw, "\\d+"))
  )

df_rounds <- df %>%
  group_by(competition, year, gender, event) %>%
  mutate(
    max_round = max(round_index, na.rm = TRUE),
    second_max_round = ifelse(n_distinct(round_index) >= 2,
                              sort(unique(round_index), decreasing = TRUE)[2],
                              NA_integer_)
  ) %>%
  ungroup()

df_clean <- df_rounds %>%
  filter(
    round_index == max_round | round_index == second_max_round
  )

df_clean <- df_clean %>%
  mutate(
    position = as.numeric(position),
    lane = as.numeric(lane)
  ) %>%
  filter(!is.na(position), !is.na(lane))

df_clean <- df_clean %>%
  transmute(
    event,
    year,
    gender,
    competition,
    round = round_raw,    
    lane,
    position,
    athlete,
    country,
    time,
    reaction_time
  ) %>%
  arrange(event, year, gender, competition, round, lane)

#
# Save cleaned data
write_csv(df_clean, "cleaned_lane_data.csv")

cat("Rows in cleaned dataset:", nrow(df_clean), "\n\n")

df_clean %>% head() %>% print()
df_clean %>% count(event) %>% print()
df_clean %>% count(competition, year) %>% print()

# done with cleaning
##########################
## now derive the variables

df <- df_clean %>%
  mutate(
    lane        = as.integer(lane),
    position    = as.integer(position),
    time        = as.numeric(time),
    reaction_time = as.numeric(reaction_time),
    # 1 = winner, 0 = not winner
    win         = if_else(position == 1, 1L, 0L),
    event_type  = factor(event),
    competition = factor(competition),
    gender      = factor(gender),
    
    # lane groups
    lane_group = case_when(
      lane %in% 1:2 ~ "Inside",
      lane %in% 3:6 ~ "Middle",
      lane %in% 7:9 ~ "Outside",
      TRUE          ~ NA_character_
    ),
    lane_group = factor(lane_group, levels = c("Inside","Middle","Outside"))
  )

glimpse(df)



## Does Lane number change probablity of winning ? 
## i used Chi-Square - lane vs win

tab_lane_win <- table(df$lane, df$win)
tab_lane_win

chisq_h1 <- chisq.test(tab_lane_win)
chisq_h1

## logistic regression

df %>% group_by(lane) %>%
  summarise(wins = sum(win), total = n())


model_h1b <- glm(win ~ lane, data = df, family = binomial)
summary(model_h1b)
exp(coef(model_h1b))

lane_summary

# plot win rate by lane
lane_summary <- df %>%
  group_by(lane) %>%
  summarise(
    n = n(),
    wins = sum(win),
    win_rate = wins / n
  )

ggplot(lane_summary, aes(x = factor(lane), y = win_rate)) +
  geom_col(fill = "#1f77b4") +
  geom_text(aes(label = scales::percent(win_rate, accuracy = 0.1)),
            vjust = -0.5, size = 4) +
  labs(
    x = "Lane Number",
    y = "Win Probability",
    title = "Win Probability by Lane"
  ) +
  theme_minimal(base_size = 13)

# heat map 
tab_heat <- df %>%
  group_by(lane, win) %>%
  summarise(n = n(), .groups = "drop")

ggplot(tab_heat, aes(x = factor(lane), y = factor(win), fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), color = "white", size = 4) +
  scale_fill_gradient(low = "#6baed6", high = "#08519c") +
  labs(
    x = "Lane",
    y = "Win (1) or Not Win (0)",
    title = "Heatmap of Wins and Non-Wins by Lane"
  ) +
  theme_minimal(base_size = 13)

###
###
###
###
# H2 " Do average finishing times differ by lane?
##
##
##


df2 <- df %>%
  filter(!is.na(time), time > 0) %>%
  mutate(
    lane = as.factor(lane)
  )

df2 %>% 
  group_by(lane) %>%
  summarise(
    n = n(),
    mean_time = mean(time),
    sd_time = sd(time)
  )
# one away anova 
# it produces f stat, df, and p value

anova_h2 <- aov(time ~ lane, data = df2)
summary(anova_h2)

TukeyHSD(anova_h2)

lane_means <- df2 %>%
  group_by(lane) %>%
  summarise(mean_time = mean(time))

ggplot(lane_means, aes(lane, mean_time)) +
  geom_point(size = 4, color = "#ff7f0e") +
  geom_line(group = 1, color = "#ff7f0e") +
  labs(
    title = "Mean Finishing Time by Lane",
    x = "Lane Number",
    y = "Average Time"
  ) +
  theme_minimal(base_size = 13)


##
##
##
## Q3. Is there an advantage in inside , middle or outside lanes
##  so what we did lane the groups (standard classification )
#   inside 1-2 : tightest, worst visibility
#   middle 3-6 : preferredd lanes - best curvatures + pacing
#   outside 7-9 : flattest curve but less visiblility of competetion

df3 <- df %>%
  mutate(
    lane_group = case_when(
      lane %in% c(1,2) ~ "Inside",
      lane %in% c(3,4,5,6) ~ "Middle",
      lane %in% c(7,8,9) ~ "Outside"
    )
  ) %>%
  mutate(lane_group = factor(lane_group, levels = c("Inside", "Middle", "Outside")))

df3 %>%
  group_by(lane_group) %>%
  summarise(
    n = n(),
    wins = sum(win),
    win_rate = wins / n,
    mean_time = mean(time, na.rm = TRUE)
  )

tab_group <- table(df3$lane_group, df3$win)
chisq_h3 <- chisq.test(tab_group)
chisq_h3

anova_h3 <- aov(time ~ lane_group, data = df3)
summary(anova_h3)


ggplot(df3 %>% group_by(lane_group) %>% summarise(win_rate = mean(win)),
       aes(x = lane_group, y = win_rate)) +
  geom_col(fill = "#1f77b4") +
  geom_text(aes(label = scales::percent(win_rate, accuracy = 0.1)),
            vjust = -0.5, size = 5) +
  labs(
    title = "Win Probability by Lane Group",
    x = "Lane Group",
    y = "Win Probability"
  ) +
  theme_minimal(base_size = 15)


# Q4,
# DOES THE LANE GIVES ADVANTAGE IN EVERY RACE EVENT? OR DOES LANE 
# ADVANTAGE DIFFER BY RACE EVENT

df4 <- df3 %>%
  filter(event %in% c("100m", "200m", "400m")) %>%
  mutate(event = factor(event, levels = c("100m", "200m", "400m")))

df4 %>%
  group_by(event, lane_group) %>%
  summarise(
    n = n(),
    wins = sum(win),
    win_rate = wins / n
  )
# TWO WAY ANOVA
anova_h4 <- aov(time ~ lane_group * event, data = df4)
summary(anova_h4)

tab_h4 <- table(df4$lane_group, df4$event, df4$win)
chisq_h4 <- chisq.test(table(df4$lane_group, df4$event))
chisq_h4

# 
df4 %>%
  group_by(event, lane_group) %>%
  summarise(win_rate = mean(win)) %>%
  ggplot(aes(x = lane_group, y = win_rate, fill = event)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::percent(win_rate, accuracy = 0.1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  labs(
    title = "Win Probability by Lane Group across Events",
    x = "Lane Group",
    y = "Win Probability"
  ) +
  theme_minimal(base_size = 15)

#-------------------
#stat for gender


# Keep only rows with non-missing reaction time
df_rt <- df %>% filter(!is.na(reaction_time))

# Summary by gender
df_rt %>%
  group_by(gender) %>%
  summarise(
    n = n(),
    mean_rt = mean(reaction_time),
    sd_rt = sd(reaction_time)
  )

# t-test (if you want to formally test difference)
t.test(reaction_time ~ gender, data = df_rt)

# Boxplot for presentation
ggplot(df_rt, aes(x = gender, y = reaction_time)) +
  geom_boxplot(fill = "#1f77b4", alpha = 0.7) +
  labs(
    title = "Reaction Time by Gender",
    x = "Gender",
    y = "Reaction Time (s)"
  ) +
  theme_minimal(base_size = 14)



