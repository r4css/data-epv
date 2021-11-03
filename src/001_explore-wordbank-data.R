# Install and Load Packages -----------------------------------------------

# Install Packages
install.packages(c("tidyverse", "summarytools", "skimr", "wbstats"))

library(tidyverse)
library(summarytools)
library(skimr)
library(wbstats)

# Load Data ---------------------------------------------------------------

# Set Indicator
indicators <- c(
  gdp_capita = "NY.GDP.PCAP.CD",
  pop = "SP.POP.TOTL"
)

# Load Data
df <- wb_data(indicators) %>%
  left_join(wb_countries(), "iso3c") 

# Data Exploration: General -----------------------------------------------

# Menampilkan 5 Baris Teratas Data
head(df, 5)

# Menampilkan Daftar Variable, dan Contoh Data
glimpse(df)

# Menampilkan Daftar Variable, Nilai Kosong, Distribusi
skim(df)

# Menampilkan Daftar Variable, Nilai Kosong, Distribusi
dfSummary(df, style = "grid")

# Data Exploration: dplyr -------------------------------------------------

# Melihat GDP Negara-Negara di region East Asia & Pacific pada tahun 2020
df %>%
  filter(region == "East Asia & Pacific" & date == 2020) %>%
  select(country.x, gdp_capita)


# Melihat Negara dengan GDP tertinggi di region East Asia & Pacific pada tahun 2020
df %>%
  filter(region == "East Asia & Pacific" & date == 2020) %>%
  select(country.x, gdp_capita) %>%
  arrange(desc(gdp_capita)) %>%
  slice_head(n = 5)

# Data Visualization ------------------------------------------------------

# Set Parameter
options(scipen = 10000)

# Memvisualisasikan hubungan antara population dan gdp di tahun 2020
plot_hubungan <- df %>%
  filter(date == 2020) %>%
  ggplot(aes(x = pop, y = gdp_capita, color = region)) +
  geom_point() +
  scale_x_log10() +
  labs(
    x = "Population",
    y = "GDP per Capita",
    color = "Region"
  )
plot_hubungan

# Simpan Plot
ggsave(plot_hubungan,
  filename = "plot/hubungan.png",
  width = 20,
  height = 20,
  dpi = 300,
  type = "cairo",
  units = "cm",
  limitsize = FALSE
)

# Memvisualisasikan Trend Perkembangan GDP Percapita di Indonesia
plot_trend <- df %>%
  filter(country.x == "Indonesia" | country.x == "Malaysia" | country.x == "Singapore") %>%
  select(date, gdp_capita, country.x) %>%
  ggplot(aes(x = date, y = gdp_capita, color = country.x)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Date",
    y = "GDP per Capita"
  )
plot_trend

# Simpan Plot
ggsave(plot_hubungan,
  filename = "plot/trend-gdp.png",
  width = 20,
  height = 20,
  dpi = 300,
  type = "cairo",
  units = "cm",
  limitsize = FALSE
)
