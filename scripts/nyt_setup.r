# Load in libraries ----
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(MetBrewer)

# Read in data file ----
nyt <-
  read.csv("./data/nyt_data.csv") %>%
  mutate(date = as.Date(date),
         year = as.integer(format(date, "%Y"))) %>%
  group_by(year, day) %>%
  mutate(avg_elapsed_seconds = round(mean(elapsed_seconds), 2)) %>%
  ungroup()

theme_pal <- "Hiroshige"

# Analysis ----
## By the books ----
### Total solved ----
solved_total <-
  nyt %>%
  filter(solved == 1) %>%
  group_by(day, checked) %>%
  count() %>%
  mutate(checked = ifelse(checked == 1, "Checked", "Legit"))

ggplotly({
  ggplot(solved_total,
         aes(
           x = reorder(day, -n),
           y = n,
           fill = checked,
           text = paste0("Day: ", day,
                         "<br>",
                         "Solve status: ", checked,
                         "<br>",
                         "Count: ", n)
         )) +
    geom_bar(stat = "identity",
             position = "stack") +
    theme_minimal() +
    scale_fill_manual(values = c(
      met.brewer(theme_pal)[1],
      met.brewer(theme_pal, direction = -1)[1]
    )) +
    labs(
      title = "Distribution of solved crosswords by solve status",
      x = "Day",
      y = "Solved crosswords",
      fill = "Solve\nstatus"
    )
}, tooltip = "text") %>%
  layout(hovermode = "x unified")

### Breakdown by year ----
solved_year <-
  nyt %>%
  filter(solved == 1) %>%
  group_by(year, day, checked) %>%
  count() %>%
  mutate(checked = ifelse(checked == 1, "Checked", "Legit"))

### Progress by year ----
progress_year <-
  nyt %>%
  filter(solved == 1,
         year != 2023) %>%
  group_by(year, checked) %>%
  count() %>%
  mutate(checked = ifelse(checked == 1, "Checked", "Legit")) %>%
  ungroup() %>%
  group_by(checked) %>%
  mutate(avg = mean(n)) %>%
  ungroup()

## Mondays attempts ----
mondays <-
  nyt %>%
  filter(day == "Mon", solved == 1, checked == 0, revealed == 0)

## Tuesday attempts ----
tuesdays <-
  nyt %>%
  filter(day == "Tue", solved == 1, checked == 0, revealed == 0)

## Backtracking ----
backtrack <-
  nyt %>%
  filter(year != 2023, solved == 1, checked == 0, revealed == 0) %>%
  group_by(year) %>%
  count() %>%
  ungroup() %>%
  mutate(avg = round(mean(n)))

## Streaks ----
streaks <-
  nyt %>%
  select(-c(elapsed_seconds, avg_elapsed_seconds)) %>%
  group_by(grp = with(rle(streak_eligible),
    rep(seq_along(lengths), lengths))) %>%
    mutate(counter = seq_along(grp)) %>%
  ungroup() %>%
  select(-grp) %>%
  filter(solved == 1, streak_eligible == 1) %>%
  mutate(key = as.integer(format(date, "%Y%W")))

max_streaks_temp <-
  streaks %>%
  filter(counter == max(counter))

max_streaks <-
  streaks %>%
  filter(key %in% max_streaks_temp$key)

# Functions ----
plot_line <- function(df, df_lab) {
  ggplotly({
    ggplot(df,
           aes(x = date, y = elapsed_seconds)) +
      geom_line(
        group = 1,
        size = 1,
        colour = met.brewer(name = theme_pal)[10]
      ) +
      geom_line(
        aes(x = date,
        y = avg_elapsed_seconds),
        colour = met.brewer(name = theme_pal)[1],
        size = 1,
        linetype = "dotted"
      ) +
      geom_smooth(
        method = lm,
        se = FALSE,
        fullrange = TRUE,
        aes(color = elapsed_seconds),
        colour = met.brewer(name = theme_pal)[3],
        size = 1,
        linetype = "dashed"
      ) +
      theme_minimal() +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(
        title =
        paste0("Elapsed time (seconds) of solved crosswords on ",
        df_lab),
        x = "Reference period",
        y = "Elapsed time (seconds)"
      )
  },
  tooltip = NULL)
}
