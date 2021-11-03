# Install and Load Packages -----------------------------------------------

# Load Packages
library(tidyverse)
library(lubridate)

# Load Data ---------------------------------------------------------------

# Load Data
df <- read_rds("data/tweet-riset.rds")

# Plot Trend
trend <- df %>%
  mutate(created_at = created_at + 7 * 60 * 60) %>%
  mutate(tanggal = floor_date(created_at, "12 hours")) %>%
  group_by(tanggal) %>%
  count() %>%
  ggplot(aes(x = tanggal, y = n)) +
  geom_line(colour = "#f8766d", size = 0.7) +
  geom_point(colour = "grey30", size = 1.5, alpha = 0.7) +
  geom_text(aes(label = n),
    vjust = "inward", hjust = "inward", size = 3, colour = "grey30",
    show.legend = FALSE, family = "Titillium Web"
  ) +
  labs(
    x = "", y = "Jumlah Tweet",
    title = "Trend Percakapan Terkait Riset",
    subtitle = "Di Twitter",
    caption = bquote(bold("Dianalisis Oleh") ~ "______")
  )
trend

# Simpan Visualisasi
ggsave(trend,
  filename = "plot/trend.png",
  width = 20,
  height = 20,
  dpi = 300,
  type = "cairo",
  units = "cm",
  limitsize = FALSE
)

# Mencari Akun dengan Jumlah Tweet Terbanyak
top_account <- df %>%
  select(screen_name) %>%
  group_by(screen_name) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = reorder(screen_name, n), y = n, fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    x = "", y = "Jumlah Tweet",
    title = "Akun dengan Jumlah Tweet Terbanyak",
    subtitle = 'Pada Percakapan dengan kata kunci "Riset" di Twitter',
    caption = bquote(bold("Dianalisis Oleh") ~ "______")
  )
top_account

# Simpan Plot
ggsave(top_account,
  filename = "plot/top_account.png",
  width = 20,
  height = 20,
  dpi = 300,
  type = "cairo",
  units = "cm",
  limitsize = FALSE
)
