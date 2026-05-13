# STATS 220 Peoject 4. 
library(tidyverse)
library(knitr)

df <- read_csv("STATS 220 Project 2 survey (Responses) - Form responses 1 (1).csv")

colnames(df) <- c("timestamp", "played_today", "hours", "game")

# Bar chart
plot1 <- df %>%
  filter(game != "I didn't play today") %>%
  count(game, sort = TRUE) %>%
  ggplot(aes(x = reorder(game, n), y = n, fill = game)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 20, by = 1)) +
  labs(
    title = "Most Popular Games Among Respondents",
    x = "Game",
    y = "Number of Respondents"
  )

print(plot1)

# Histogram
plot2 <- df %>%
  ggplot(aes(x = hours)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Hours Spent Gaming",
    subtitle = "Among respondents who played today",
    x = "Hours Played",
    y = "Count"
  ) +
  theme_minimal(base_size = 13)

summary 

print(plot2)

# Box plot
plot3 <- df %>%
  filter(played_today == "Yes") %>%
  ggplot(aes(x = reorder(game, hours, median), y = hours, fill = game)) +
  geom_boxplot(show.legend = FALSE, outlier.colour = "red", outlier.shape = 16) +
  coord_flip() + scale_y_continuous(breaks = seq(0, 20, by = 1)) +
  labs(
    title = "Hours Played by Game",
    x = "Game",
    y = "Hours Played"
  ) +
  theme_minimal(base_size = 13)

print(plot3)

# Pie Chart

df %>%
  count(played_today) %>%
  rename("Played Today" = played_today, "Count" = n) %>%
  kable(caption = "Summary of Respondents Who Played Today")

plot4 <- df %>%
  count(played_today) %>%
  ggplot(aes(x = "", y = n, fill = played_today)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Did Respondents Play Games Today?",
    fill = "Played Today"
  ) +
  theme_void(base_size = 13)

print(plot4)
summary(plot4)
