library(readr)
library(dplyr)

url <- "https://raw.githubusercontent.com/manuelbohn/previc/main/data/previc_data.csv"

df <- read_csv(url)

df <- df %>%
  rename(
    id = subjID,  
    item = word, 
    resp = score,  
    itemcov_word_type = word_type,
    itemcov_english_trans = english,
    cov_sex = sex,
    cov_age = age,
    itemcov_aoa = aoa_rating_german
  )

df <- df %>%
  select(id, item, resp, trial, starts_with("cov_"), starts_with("itemcov_"))

write.csv(df, "previc_bohn2023.csv")
