library(tidyverse)
library(haven)
library(labelled)

# load data
df <- read_sav('R305A180293_child-level_CT_v35.sav')

names(df) <- tolower(names(df))

# select relevant columns
df <- df |>
  select(contains('ss1'),
         contains('ss2'),
         contains('ss3'),
         contains('ss4'),
         contains('ss5'),
         contains('ss6'),
         contains('ss7'),
         contains('ss8'),
         contains('ss9'),
         contains('ss10'),
         contains('as1'),
         contains('as2'),
         contains('as3'),
         contains('as4'),
         contains('as5'),
         contains('as6'),
         contains('as7'),
         contains('as8'),
         contains('as9'),
         contains('as10'),
         starts_with('wj_lw'),
         starts_with('wj_ap'),
         starts_with('dn_1'),
         starts_with('dn_2'),
         starts_with('dn_3'),
         starts_with('dn_4'),
         starts_with('dn_5'),
         starts_with('dn_6'),
         starts_with('dn_7'),
         starts_with('dn_8'),
         starts_with('dn_9'),
         contains('_1s_'),
         contains('_2s_'),
         contains('_3s_'),
         contains('_4s_'),
         contains('_5s_'),
         contains('_6s_'),
         starts_with('box1'),
         starts_with('box2'),
         starts_with('htks1_1'),
         starts_with('htks1_2'),
         starts_with('htks1_3'),
         starts_with('htks1_4'),
         starts_with('htks1_5'),
         starts_with('htks1_6'),
         starts_with('htks1_7'),
         starts_with('htks1_8'),
         starts_with('htks1_9')) |> 
  select(-starts_with('emt1'),
         -starts_with('emt2'),
         -starts_with('emt3'),
         -starts_with('emt4'),
         -ends_with('hapb'),
         -ends_with('sadb'),
         -ends_with('angb'),
         -ends_with('afrb'),
         -ends_with('hapa'),
         -ends_with('sada'),
         -ends_with('anga'),
         -ends_with('afra'),
         -wj_lww_t1,
         -wj_lwss_t1,
         -wj_apw_t1,
         -wj_apss_t1,
         -contains('notes')) |>
  mutate(
    across(starts_with('box'), ~if_else(. == 0.5, NA, .)),
    id = row_number()
  )

# drop empty / single-response variables
drop_vars <- c()

for (i in 1:ncol(df)) {
  unique_vals <- unique(df[[i]])
  unique_len <- length(unique_vals)
  
  if (unique_len == 1 & is.na(unique_vals[1])) {
    drop_vars <- append(drop_vars, names(df)[i])
  }
  
  if (unique_len == 2) {
    if (is.na(unique_vals[1]) | is.na(unique_vals[2])) {
      drop_vars <- append(drop_vars, names(df)[i])
    }
  }
}

# reshape to long (item = original name)
df_long <- df |>
  select(-all_of(drop_vars)) |>
  pivot_longer(cols = -id,
               names_to = "item",
               values_to = "resp",
               values_drop_na = TRUE)

# remove labels
df_long$resp <- remove_labels(df_long$resp)

preschool_sel_pl <- df_long |>
  filter(str_detect(item, "ss") | str_detect(item, "as"))

preschool_sel_dn <- df_long |>
  filter(str_detect(item, "dn"))

preschool_sel_wj <- df_long |>
  filter(str_detect(item, "wj"))

preschool_sel_akt <- df_long |>
  filter(str_detect(item, "_[1-6]s_"))

preschool_sel_box <- df_long |>
  filter(str_detect(item, "box"))

preschool_sel_emt <- df_long |>
  filter(str_detect(item, "emt"))

preschool_sel_htks <- df_long |>
  filter(str_detect(item, "htks"))

save(preschool_sel_pl,  file = "preschool_sel_pl.Rdata")
save(preschool_sel_dn,  file = "preschool_sel_dn.Rdata")
save(preschool_sel_wj,  file = "preschool_sel_wj.Rdata")
save(preschool_sel_akt, file = "preschool_sel_akt.Rdata")
save(preschool_sel_box, file = "preschool_sel_box.Rdata")
save(preschool_sel_emt, file = "preschool_sel_emt.Rdata")
save(preschool_sel_htks,file = "preschool_sel_htks.Rdata")

# check
table(df_long$resp)