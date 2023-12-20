library(tidyverse)
library(lubridate)
library(glue)
library(ggtext)

# set locale for month abbreviations in plot
Sys.setlocale(category = "LC_ALL", locale = "de_DE.UTF-8")

# Google palette
google_colors <- c("#4285F4", "#DB4437", "#F4B400", "#0F9D58")
google_colors_desaturated <- colorspace::desaturate(google_colors, 0.2)


## DATA PREP ===================================================================

#' Manual task: Download trends data one by one from 
#' https://trends.google.com/trends/yis/2023/DE/ and place them in /data

# Read downloaded files
filenames <- list.files("data", pattern = "multiTimeline.*\\.csv")
dfs <- map(file.path("data", filenames), function(x) read_csv(x, skip = 2))

# Replaces the "<1" value with a numeric value and converts x to a numeric variable
replace_lessthan1 <- function(x, num_value = 0.5) {
  ifelse(x == "<1", num_value, as.numeric(x))
}


# Exclude keywords
excluded_keywords <- c("Wednesday")

trends_combined <- dfs %>% 
  bind_cols() %>% 
  select(-matches("Woche...([2-9]|\\d{2})")) %>% 
  rename(date = 1) %>% 
  mutate(across(-date, replace_lessthan1)) %>% 
  pivot_longer(cols = -date, names_to = "keyword", values_to = "hits_rescaled") %>% 
  mutate(
    date = as_datetime(date),
    keyword = str_to_title(keyword),
    keyword = str_remove(keyword, ": \\(.+\\)"),
    keyword = case_match(
      keyword,
      "Chatgpt" ~ "ChatGPT",
      "Sinead Oconnor" ~ "Sinéad O'Connor",
      .default = keyword
    )
  ) %>% 
  filter(!keyword %in% excluded_keywords)


# identify "local" peak hits date for each keyword - sort search topics in plot
keywords_by_peak_date <- trends_combined %>%
  filter(hits_rescaled >= 90) %>%
  group_by(keyword) %>%
  summarize(peak_date = min(date)) %>%
  arrange(desc(peak_date)) %>%
  mutate(
    row_color = google_colors_desaturated[(row_number() %% length(google_colors) + 1)],
    # add a colored dot as a guidance in the y-axis labels
    label = glue::glue(
      # "{keyword}
      # <span style='color: {row_color};font-family:Arial;'>\u2022</span>"
      "<span style='color: {row_color}'>{keyword}</span>"
      )
  ) %>%
  select(-row_color)

# Define the overlap of ridgelines 
# Choose a smaller value for a bigger overlap (>100 = no overlap)
added_height <- 80

# Prepare data for plot
df_plot <- trends_combined %>% 
  inner_join(keywords_by_peak_date, by = "keyword") %>% 
  arrange(desc(peak_date), keyword) %>% 
  nest(data = -keyword) %>% 
  mutate(row = row_number()) %>% 
  unnest(data) %>% 
  # calculate position on y-axis
  mutate(added = added_height * row,
         y = hits_rescaled + added)


keywords <- keywords_by_peak_date$keyword

## PLOT ========================================================================

# Annotations
plot_titles <- list(
  title = glue("Google Trends <span style='color:{google_colors[1]}'>2023</span>"),
  subtitle = "Wöchentliches relatives Suchaufkommen der Top-Suchbegriffe in verschiedenen
  Kategorien in Deutschland",
  caption = "Wöchentliches Suchaufkommens skaliert auf einen Wertbereich von 
  0 bis 100 (maximales Suchinteresse je Suchbegriff).
  Daten: **Google Trends** (Websuche Deutschland) | Visualisierung: **Ansgar Wolsing**")

g <- df_plot %>% 
  ggplot(aes(date, y, group = keyword)) +
  geom_ribbon(
    aes(ymin = added, ymax = y, fill = factor(row %% length(google_colors))),
    col = "white", linewidth = 0.2, alpha = 0.7
  ) +
  scale_x_datetime(
    expand = c(0.0005, 0), 
    date_breaks = "1 month", date_labels = "%b", position = "top") +
  scale_y_continuous(breaks = seq(added_height, added_height * nrow(keywords_by_peak_date), added_height),
                     labels = keywords_by_peak_date$label, expand = c(0, 0)
  ) +
  scale_fill_manual(values = google_colors) +
  guides(fill = "none") +
  labs(title = plot_titles$title,
       subtitle = plot_titles$subtitle,
       caption = plot_titles$caption,
       x = NULL, y = NULL) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    text = element_text(color = "grey85"),
    plot.title = element_markdown(family = "Roboto Condensed", face = "bold",
                                  margin = margin(t = 6, l = 6, r = 6, b = 8),
                                  size = 22, color = "white"),
    plot.subtitle = element_textbox_simple(margin = margin(l = 6, t = 6, b = 18), 
                                           lineheight = 1.2, size = 10),
    plot.caption = element_textbox_simple(size = 8, hjust = 0,
                                          margin = margin(t = 16, b = 6),
                                          lineheight = 1.25, color = "grey80"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(t = 4, l = 16, r = 16, b = 4),
    axis.text.y = element_markdown(hjust = 1, vjust = 0, color = "grey90",
                                   family = "Roboto Condensed", size = 8,
                                   margin = margin(l = 6, r = 4)),
    axis.text.x = element_text(hjust = 0, color = "grey80"),
    plot.background = element_rect(color = NA, fill = "grey8"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, color = "grey94"),
    panel.grid.minor = element_blank())
g

ggsave(file.path("plots", "gtrends_de_2023_dark.png"), device = ragg::agg_png,
       dpi = 400, width = 6, height = 8)



## Light version ---------------------------------------------

g + theme(
  plot.background = element_rect(color = NA, fill = "white"),
  text = element_text(color = "grey35"),
  axis.text.x = element_markdown(color = "grey35"),
  axis.text.y = element_markdown(color = "grey24", family = "Roboto Condensed", 
                                 size = 9, face = "bold"),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "grey70"),
  panel.ontop = TRUE,
  plot.title = element_markdown(color = "black", family = "Roboto", face = "bold"),
  plot.caption = element_textbox_simple(color = "grey45",
                                        margin = margin(t = 16, b = 6))
)

ggsave(file.path("plots", "gtrends_de_2022_light.png"), device = ragg::agg_png,
       dpi = 400, width = 6, height = 7)
