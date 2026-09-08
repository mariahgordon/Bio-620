#Mariah
#Data Exploration
library(ggplot2)
library(dplyr)

df<-read.csv("Roxs - Sheet1.csv", header = TRUE)
df$Date <- as.Date(df$Date, format = "%m/%d/%y")

#my error graph , to much data
ggplot(df, aes(x = Date, y = P4..ng.ml.)) +
  geom_point() +
  geom_line() +
  scale_x_date(
    date_breaks = "4 months",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "P4 Concentration Over Time",
    x = "Date",
    y = "P4 (ng/mL)"
  ) +
  theme_minimal()


elephant_data <- elephant_data %>%
  arrange(Date) %>%
  mutate(
    gap = as.numeric(Date - lag(Date)),
    group = cumsum(ifelse(is.na(gap) | gap > 60, 1, 0))
  )
elephant_data <- elephant_data %>%
  arrange(Date) %>%
  mutate(
    gap = as.numeric(Date - lag(Date))
  )

ggplot(df, aes(x = Date, y = P4..ng.ml.)) +
  geom_point() +
  geom_line() +
  scale_x_date(
    breaks = seq(
      from = as.Date("2022-11-21"),
      to   = as.Date("2026-05-21"),
      by   = "3 months"
    ),
    date_labels = "%m/%d/%Y",
    limits = c(
      as.Date("2022-11-21"),
      as.Date("2026-05-21")
    )
  ) +
  labs(
    title = "P4 Concentration Over Time",
    x = "Date",
    y = "P4 (ng/mL)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

#lh and dates not signifcant preds.
lm(P4..ng.ml. ~ Date + LH..pg.well., data = df)


