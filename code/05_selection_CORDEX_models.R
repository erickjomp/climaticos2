library(tidyverse)
Sys.setenv(TZ = 'GMT')

#### INPUT ####
file_series0_pr_CORDEX_monthly__rds <- 
  "output/04_extraction_CORDEX_series/df_pr_CORDEX_m0.rds"
file_series0_PET_CORDEX_monthly__rds <- 
  "output/04_extraction_CORDEX_series/df_PET_CORDEX_m0.rds"

file_models_to_exclude <- 
  "output/05_selection_CORDEX_series/input/models_to_exclude.csv"

#### OUTPUT ####
file_series_pr_CORDEX_monthly__rds <- 
  "output/05_selection_CORDEX_series//df_pr_CORDEX_m.rds"
file_series_pr_CORDEX_monthly__csv <- 
  "output/05_selection_CORDEX_series//df_pr_CORDEX_m.csv"
file_series_PET_CORDEX_monthly__rds <- 
  "output/05_selection_CORDEX_series//df_PET_CORDEX_m.rds"
file_series_PET_CORDEX_monthly__csv <- 
  "output/05_selection_CORDEX_series//df_PET_CORDEX_m.csv"
file_models_selected <- 
  "output/05_selection_CORDEX_series/df_models_selected.csv"

#### reading INPUT ####
df_pr_CORDEX_m0 <- 
  readRDS(file_series0_pr_CORDEX_monthly__rds)
df_PET_CORDEX_m0 <- 
  readRDS(file_series0_PET_CORDEX_monthly__rds)

df_models_to_exclude <- 
  read.csv(file_models_to_exclude)


#### processing ####
names_models_pr <- unique(df_pr_CORDEX_m0$name)
names_models_PET <- unique(df_PET_CORDEX_m0$name)

names_models_sel <- 
  names_models_pr[names_models_pr %in% names_models_PET]
names_models_sel <- 
  names_models_sel[!(names_models_sel %in% df_models_to_exclude$name)]

#### filtering ####
df_pr_CORDEX_m <- 
  df_pr_CORDEX_m0 %>% filter(name %in% names_models_sel)
df_PET_CORDEX_m <- 
  df_PET_CORDEX_m0 %>% filter(name %in% names_models_sel)

# generatimg table model
df_models_selected <- 
  df_pr_CORDEX_m %>% select(RCM, GCM, name) %>% 
  filter(!duplicated(name))


#### WRITING OUTPUT ####
df_pr_CORDEX_m %>% 
  saveRDS(file_series_pr_CORDEX_monthly__rds)
df_pr_CORDEX_m %>% 
  write.csv(file_series_pr_CORDEX_monthly__csv,
            quote = FALSE, row.names = FALSE)
df_PET_CORDEX_m %>% 
  saveRDS(file_series_PET_CORDEX_monthly__rds)
df_PET_CORDEX_m %>% 
  write.csv(file_series_PET_CORDEX_monthly__csv,
            quote = FALSE, row.names = FALSE)
df_models_selected %>% 
  write.csv(file_models_selected,
            quote = FALSE, row.names = FALSE)
