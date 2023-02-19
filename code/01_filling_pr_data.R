# FILLING PR DATA WITH HYFO PACKAGE @

library(tidyverse)
library(sf)
library(hyfo)
Sys.setenv(TZ = 'GMT')

#### INPUT ####
# minimum percentage of data of a station to be used as auxiliar station
# to complete
min_percent <- 60  # betwween 0 and  100
#  minimum number of values between 2 stations to calculate cor
min_n_values <- 84  # number of months with data

#  base data
file_pr_m <- "output/00_processed_data/df_pr_m.rds"

#### OUTPUT ####
file_pr_m_filled_csv <- "output/01_filling_pr/df_pr_m__filled.csv"
file_pr_m_filled_rds <- "output/01_filling_pr/df_pr_m__filled.rds"

file_plot_filled_series <- 
  "output/01_filling_pr/plots/filled_stas_pr_series.pdf"


#### reading INPUT ####
df_pr_m <- readRDS(file_pr_m) %>% filter(dates < "2020-01-01")

#### Exploratory plot####
df_pr_m %>% gather(sta, pr,-1) %>%
  ggplot(aes(dates, pr)) + geom_line() +
  facet_wrap( ~ sta) +
  theme_bw() +
  labs(x = NULL, y = "Precipitation (mm/month)")


#### FILLING PROCESSS ####
df_pr_m__filled <- df_pr_m

df_percent_stas <- df_pr_m %>%
  summarise_at(-1, function(x)
    sum(!is.na(x)) / length(x) * 100)
namcod_sel <-
  names(df_percent_stas)[as.numeric(df_percent_stas) > min_percent]
message(
  length(namcod_sel),
  " stations selected as valid auxiliar stations (",
  min_percent,
  "% of data)"
)

for (namcod in names(df_pr_m)[-1]) {
  if (namcod  %in% namcod_sel) {
    df_pr_m__sel <- df_pr_m[, c("dates", namcod_sel)]
  } else {
    df_pr_m__sel <- df_pr_m[, c("dates", namcod, namcod_sel)]
  }
  
  df_n_months <-
    df_pr_m__sel %>% filter(is.finite(!!sym(namcod))) %>%
    summarise_at(-1, function(x)
      sum(!is.na(x)))
  namcod_sel_it <-
    names(df_pr_m__sel[-1])[as.numeric(df_n_months) > min_n_values]
  
  df_pr_m__sel <- df_pr_m__sel[, c("dates", namcod_sel_it)]
  
  df_filled_temp <- df_pr_m__sel %>% rename(Date = dates) %>%
    mutate(Date = as.Date(Date)) %>%
    as.data.frame() %>%
    fillGap(corPeriod = "monthly")
  names(df_filled_temp) <- names(df_pr_m__sel)
  df_pr_m__filled[[namcod]] <- df_filled_temp[[namcod]]
}


#### Exploratory plot####
base_size_figs <- 8 # 7 : recommended for normal text by elsevier

df_pr_m__filled__arr <-
  df_pr_m__filled %>% gather(sta, pr_filled,-1)
df_pr_m__arr <-
  df_pr_m %>% gather(sta, pr,-1)
df_pr_m__filled__arr <-
  df_pr_m__filled__arr %>% full_join(df_pr_m__arr) %>%
  mutate(is_filled = !is.finite(pr)) %>%
  select(-pr)

df_pr_m__filled__arr <- df_pr_m__filled__arr %>% 
  separate(sta,c("name", "code"), sep = "_")

df_pr_m__filled__arr %>%
  ggplot(aes(dates, pr_filled, col = is_filled)) +
  geom_path(aes(group = 1)) +
  facet_wrap( ~ name) +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Observed", "Filled")) +
  ggthemes::theme_few(base_size = base_size_figs) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
    # line = element_line(colour = "black"),
    # panel.border = element_rect(colour = "black"),
    # legend.position = "bottom",
    # axis.text = element_blank(),
    # axis.ticks = element_blank(),
    panel.grid.major = element_line(colour = "grey92"), #"grey80"
    panel.grid.minor.x = element_line(colour = "grey92"),
    # panel.grid.minor.y = element_line(colour = "transparent"),
    strip.text.x = element_text(size = base_size_figs * 1.1),
    strip.text.y = element_text(size = base_size_figs * 1.1),
    ) + 
  theme(legend.position = c(0.75, 0.1)) +
  labs(x = NULL, y = "Precipitation (mm/month)", col = NULL)

ggsave(filename = file_plot_filled_series,
       width = 190,height = 110,units = "mm")

#### writing output ####
write.csv(df_pr_m__filled,
          file_pr_m_filled_csv,
          row.names = F,
          quote = F)
saveRDS(df_pr_m__filled, file_pr_m_filled_rds)
