
# Script 2/3
# Images for individual identification, population abundance, sex ratio and second turnover calculation method

# Load required packages

if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('lubridate')) install.packages('lubridate'); library('lubridate')
if (!require('writexl')) install.packages('writexl'); library('writexl')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')

# Import dataset of images, select desired period and remove juvenile observations

export_lynxDB <- read.csv("data/data_for_scripts_01_02.csv")

b <- export_lynxDB2 <- export_lynxDB %>%
  mutate(datum_vrijeme = ymd_hms(`datum_vrijeme`, tz = Sys.timezone()))

b <- b %>%
  filter(`datum_vrijeme` >= "2010-05-01 00:00:00 UTC")

b <- b %>%
  filter(!grepl("mlado|mladu", oznaka, ignore.case = TRUE))


# *************************  1. Minimum and maximum quantities of identified lynx  *************************

# List of individuals depending on which side they are captured from

lynx_L <- b %>% 
  group_by(oznaka, slikana_strana_zivotinje) %>%
  summarize(slikana_strana_zivotinje = first(slikana_strana_zivotinje)) %>% 
  rename(slikana_strana_zivotinje3=slikana_strana_zivotinje) %>%
  filter(slikana_strana_zivotinje3 == 'lijeva')   # 211 captured from the left side                        

lynx_R <- b %>% 
  group_by(oznaka, slikana_strana_zivotinje) %>% 
  summarize(slikana_strana_zivotinje = first(slikana_strana_zivotinje)) %>% 
  rename(slikana_strana_zivotinje2=slikana_strana_zivotinje) %>% 
  filter(slikana_strana_zivotinje2 == 'desna')   # 225 captured from the right side                       

lynx_LR <- lynx_L %>% inner_join(lynx_R)   # 150 captured from both sides                   

# Minimum quantity: the highest between the number of captures from the right side and those from the left side. Here, the first ones are more

ifelse(nrow(lynx_L) <= nrow(lynx_R), lynx_min <- lynx_R, lynx_min <- lynx_L)   # 225

# Maximum lynx number: case in which all the individuals captured from the left side are different than those captured from the right side

ifelse(nrow(lynx_L) <= nrow(lynx_R), lynx_max <- full_join(lynx_R, anti_join(lynx_L, lynx_LR)), lynx_max <- full_join(lynx_L,anti_join(lynx_R, lynx_LR)))   # 286


# ********************** 2. Temporal intervals between sightings for each individual between 2018 and 2023 ************************

b <- b %>%
  semi_join(lynx_min)

b_18_23 <- b %>%
  filter(`datum_vrijeme` >= "2018-05-01 00:00:00 UTC" & `datum_vrijeme` <= "2023-04-30 23:59:00 UTC")   # now 201 individuals

# Recreate ris_interval and ris_interval_org between 05/2018 and 04/2023

# Table with the interval between first and last sighting for each animal

ris_interval_18_23 <- b_18_23 %>%
  select ('datum_vrijeme', 'oznaka') %>% 
  mutate(datum = as.Date(datum_vrijeme, format = "%d.%m.%Y")) %>% 
  group_by(oznaka) %>%
  summarise(
    end = max(datum, na.rm = TRUE),
    start = min(datum, na.rm = TRUE)
  ) %>%
  arrange(oznaka) %>% 
  mutate(sighting_interval = interval(start, end))

# Add minimum age (days)

ris_interval_org_18_23 <- ris_interval_18_23 %>% 
  select(oznaka, start, end) %>% 
  mutate(min_starost_zivotinje = end-start)         

# Table with temporal intervals between different sightings for each individual + date of each sighting

ris_interval_every_18_23 <- b_18_23 %>%
  select (datum_vrijeme, oznaka) %>%
  arrange(oznaka, datum_vrijeme) %>%
  mutate(datum = as.Date(datum_vrijeme, format = "%d.%m.%Y")) %>% 
  group_by(oznaka) %>%
  mutate(time_interval = datum - lag(datum)) %>%
  arrange(desc(time_interval)) %>%
  select(oznaka, time_interval, datum)

# Join start and end date of the capture interval and minimum age for each individual

ris_interval_every_18_23 <- ris_interval_every_18_23 %>% 
  left_join(ris_interval_org_18_23, by = "oznaka")                 
                                                                   
# Reorganize

ris_interval_every_18_23 <- ris_interval_every_18_23[,c(1,4,3,5,2,6)]
colnames(ris_interval_every_18_23)[3] <- "middle_sightings"
colnames(ris_interval_every_18_23)[5] <- "interval_between_sightings"
colnames(ris_interval_every_18_23)[6] <- "total_capture_interval_(min_age)"

# Table with minimum age of each individual (days and years)

min_lynx_age_new_18_23 <- ris_interval_org_18_23 %>% 
  group_by(oznaka) %>% 
  summarise(min_age = max(min_starost_zivotinje)) %>%
  mutate(min_age_year = min_age/365) %>% 
  arrange(desc(min_age)) %>% 
  write_xlsx(., "min_lynx_age_new_18_23.xlsx")

# Keep only years

min_lynx_age_new_18_23 <- subset(min_lynx_age_new_18_23, , select = c(oznaka, min_age_year))
min_lynx_age_new_18_23[] <- lapply(min_lynx_age_new_18_23, gsub, pattern = ' days', replacement = ' ')   

# Table with date of first and last sighting, total capture interval and minimum age (years)

ris_interval_avg_18_23 <- ris_interval_org_18_23
colnames(ris_interval_avg_18_23)[4] <- 'total_capture_interval_(min_age)'
ris_interval_avg_18_23 <- ris_interval_avg_18_23 %>% 
  left_join(min_lynx_age_new_18_23, by = "oznaka") 
colnames(ris_interval_avg_18_23)[5] <- 'min_age_years'               

# Add the average interval between sightings for each lynx

# Separate table with date of each sighting and interval between sightings

ris_interval_avg2_18_23 <- subset(ris_interval_every_18_23, , select = -c(6))
ris_interval_avg2_18_23 <- na.omit(ris_interval_avg2_18_23)  

# Table with average interval between sightings (days) for each individual
                                                                          
avg_int_18_23 <- aggregate(ris_interval_avg2_18_23[, 5], list(ris_interval_avg2_18_23$oznaka), mean)
colnames(avg_int_18_23)[1] <- "oznaka"
colnames(avg_int_18_23)[2] <- "avg_int_between_sightings" 

# Join the 2 previous tables

ris_interval_avg2_18_23 <- ris_interval_avg2_18_23 %>% 
  left_join(avg_int_18_23, by = "oznaka")                                         

# Table with date of first and last sighting, total capture interval, minimum age (years) and average interval between sightings (days)

ris_interval_avg_18_23 <- ris_interval_avg_18_23 %>% 
  left_join(avg_int_18_23, by = "oznaka")                                  

ris_interval_avg_18_23[, c(5:6)] <- sapply(ris_interval_avg_18_23[, c(5:6)], as.numeric)      

# Add number of sightings for each individual

n_sightings_18_23 <- ris_interval_every_18_23 %>% count(oznaka)
ris_interval_avg_18_23 <- ris_interval_avg_18_23 %>% 
  left_join(n_sightings_18_23, by = "oznaka") 
colnames(ris_interval_avg_18_23)[7] <- "n_sightings"
ris_interval_avg_18_23 <- ris_interval_avg_18_23[,c(1,2,3,4,5,7,6)]

# Add individuals' sex

ris_spol_18_23 <- subset(b_18_23, , select = c(oznaka, spol))
ris_spol_18_23 <- ris_spol_18_23 %>%
  filter(duplicated(oznaka) == FALSE)

ris_interval_avg_18_23 <- ris_interval_avg_18_23 %>% 
  left_join(ris_spol_18_23, by = "oznaka")            

table(ris_interval_avg_18_23$spol)


# ******** 3. Quantity of images of IDed animals between 05/2010 and 04/2024 (this time not with minimum identified applied) ********

# Import all records (also not IDed images)

export_lynxDB_allobs <- read.csv("data/data_for_scripts_02section3_03.csv")

# Separate IDed images, select time period and remove juveniles

export_lynxDB_idobs <- export_lynxDB_allobs[-which(export_lynxDB_allobs$oznaka == ""), ]

b_idph_10_24 <- export_lynxDB_idobs %>%
  filter(ime == 'Image') %>%
  mutate(datum_vrijeme = ymd_hms(`datum_vrijeme`, tz=Sys.timezone())) %>%
  filter(datum_vrijeme >= "2010-05-01 00:00:00 UTC" & datum_vrijeme <= "2024-04-30 23:59:00 UTC") %>% 
  filter(duplicated(datum_vrijeme) == FALSE)  

b_idph_10_24 <- b_idph_10_24 %>%
  filter(!grepl("mlado|mladu", oznaka, ignore.case = TRUE))       

# Assign seasons

b_idph_10_24 <- b_idph_10_24 %>%
  mutate(
    season = case_when(
      datum_vrijeme < "2012-05-01" ~ "11/12",
      datum_vrijeme < "2013-05-01" ~ "12/13",
      datum_vrijeme < "2014-05-01" ~ "13/14",
      datum_vrijeme < "2015-05-01" ~ "14/15",
      datum_vrijeme < "2016-05-01" ~ "15/16",
      datum_vrijeme < "2017-05-01" ~ "16/17",
      datum_vrijeme < "2018-05-01" ~ "17/18",
      datum_vrijeme < "2019-05-01" ~ "18/19",
      datum_vrijeme < "2020-05-01" ~ "19/20",
      datum_vrijeme < "2021-05-01" ~ "20/21",
      datum_vrijeme < "2022-05-01" ~ "21/22",
      datum_vrijeme < "2023-05-01" ~ "22/23",
      datum_vrijeme < "2024-05-01" ~ "23/24"
    )
  )

nrow(b_idph_10_24[b_idph_10_24$season == "11/12",])   # Check quantity for a given season

# Plot

idph_10_24_plot <- ggplot(b_idph_10_24, aes(x = season, group = 1)) +
  geom_line(stat = "count", linewidth = 1.5) +
  ggtitle("Number of photos of IDed individuals by season") +
  xlab("Season") +
  ylab("Number")

# b_idph_10_24 needs to be stored for later use, so it's already in the "data" folder


# ******* 4. Quantity of IDed animals between 05/2010 and 04/2024 (still not with minimum identified applied) *******

b_ind_10_24 <- b

b_ind_10_24 <- b_ind_10_24 %>%
  filter(!grepl("mlado|mladu", oznaka, ignore.case = TRUE))

# Assign seasons

b_ind_10_24 <- b_ind_10_24 %>%
  mutate(
    season = case_when(
      datum_vrijeme < "2012-05-01" ~ "11/12",
      datum_vrijeme < "2013-05-01" ~ "12/13",
      datum_vrijeme < "2014-05-01" ~ "13/14",
      datum_vrijeme < "2015-05-01" ~ "14/15",
      datum_vrijeme < "2016-05-01" ~ "15/16",
      datum_vrijeme < "2017-05-01" ~ "16/17",
      datum_vrijeme < "2018-05-01" ~ "17/18",
      datum_vrijeme < "2019-05-01" ~ "18/19",
      datum_vrijeme < "2020-05-01" ~ "19/20",
      datum_vrijeme < "2021-05-01" ~ "20/21",
      datum_vrijeme < "2022-05-01" ~ "21/22",
      datum_vrijeme < "2023-05-01" ~ "22/23",
      datum_vrijeme < "2024-05-31" ~ "23/24"
    )
  )

nrow(b_ind_10_24[b_ind_10_24$season == "11/12",])   # Check quantity for a given season

# Group

b_ind_10_24_grp <- b_ind_10_24 %>%
  group_by(oznaka, season) %>%
  filter(row_number(oznaka) == 1)  

ind_10_24_grp_plot <- ggplot(b_ind_10_24_grp, aes(x = season, group = 1)) +
  geom_line(stat = "count", linewidth = 1.5) +
  ggtitle("Number of IDed individuals by season") +
  xlab("Season") +
  ylab("Number")


# ! Before running section 5, run all Script 03 !


# ******** 5. Population abundance and sex ratio between 2018 and 2023 ********

# Add sex to the individual sightings table

ris_interval_every2_18_23 <- ris_interval_every_18_23 %>% 
  left_join(ris_spol_18_23, by = "oznaka") 

# Retain individual name, sighting dates and sex

ris_interval_every2_18_23 <- subset(ris_interval_every2_18_23, , select = c(1,3,7))

# Assign seasons

ris_interval_every2_18_23 <- ris_interval_every2_18_23 %>%
  mutate(
    season = case_when(
      middle_sightings < "2019-05-01" ~ "18/19",
      middle_sightings < "2020-05-01" ~ "19/20",
      middle_sightings < "2021-05-01" ~ "20/21",
      middle_sightings < "2022-05-01" ~ "21/22",
      middle_sightings <= "2023-04-30" ~ "22/23"
    )
  )

# Retain one sighting per individual in each season, then sex and season for sex-ratio calculation

ris_interval_every2_each_18_23 <- ris_interval_every2_18_23 %>%
  group_by(oznaka, season) %>%
  select(oznaka, middle_sightings, spol, season) %>%
  filter(row_number(oznaka) == 1)

ris_interval_every3_each_18_23 <- subset(ris_interval_every2_each_18_23, , select = c(3:4))

# Rename sex categories

colnames(ris_interval_every3_each_18_23)[1] <- "Sex"

ris_interval_every3_each_18_23$Sex[ris_interval_every3_each_18_23$Sex == "M"] <- "Males"
ris_interval_every3_each_18_23$Sex[ris_interval_every3_each_18_23$Sex == "Z"] <- "Females"

# Remove one female with uncertain identification

r <- which(
  ris_interval_every3_each_18_23$Sex == "Females" &
    ris_interval_every3_each_18_23$season == "18/19"
)[1]

ris_interval_every3_each_18_23 <- ris_interval_every3_each_18_23[-r, ]

# Plot

ggplot(ris_interval_every3_each_18_23,
  aes(x = season, fill = Sex)) +
  geom_bar(position = "fill") +
  xlab("Season") +
  ylab("Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set1") +
  theme(
    axis.title.x = element_text(size = 27, margin = margin(t = 16)),
    axis.title.y = element_text(size = 27, margin = margin(r = 16)),
    axis.text.x = element_text(size = 25, margin = margin(t = 8)),
    axis.text.y = element_text(size = 25, margin = margin(r = 8)),
    legend.title = element_text(size = 27),
    legend.text = element_text(size = 27),
    text = element_text(family = "Calibri")
  )


# ******** 6. Second turnover calculation method ********

nrow(b_ind_10_24_grp[b_ind_10_24_grp$season == "18/19",])   # Check quantity for a given season

# Create separate tables for each season and check sex composition, then retain individual names for each season

inds_18_19 <- b_ind_10_24_grp %>%
  filter(season == "18/19")
table(inds_18_19$spol)

inds_19_20 <- b_ind_10_24_grp %>%
  filter(season == "19/20")
table(inds_19_20$spol) 

inds_20_21 <- b_ind_10_24_grp %>%
  filter(season == "20/21")
table(inds_20_21$spol) 

inds_21_22 <- b_ind_10_24_grp %>%
  filter(season == "21/22")
table(inds_21_22$spol) 

inds_22_23 <- b_ind_10_24_grp %>%
  filter(season == "22/23")
table(inds_22_23$spol) 

inds_18_19_names <- subset(inds_18_19, , select = c(2))

inds_19_20_names <- subset(inds_19_20, , select = c(2))

inds_20_21_names <- subset(inds_20_21, , select = c(2))

inds_21_22_names <- subset(inds_21_22, , select = c(2))

inds_22_23_names <- subset(inds_22_23, , select = c(2))

# Table of all sightings of all individuals

ris_18_23 <- b_18_23

ris_org_18_23 <- ris_18_23 %>%
  rename(ime = "oznaka") %>%
  mutate(datum = ymd_hms(datum_vrijeme, tz = Sys.timezone())) %>% 
  select(-datum_vrijeme) %>% 
  arrange(ime)

ris_org_18_23 <- subset(ris_org_18_23, , select = -c(1, 3, 5:6))  

# Table with first and last sightings and sex for all individuals

ris_startend_18_23 <- ris_org_18_23 %>%
  group_by(ime) %>%
  summarise(
    start = min(datum, na.rm = TRUE),
    end = max(datum, na.rm = TRUE)
  ) %>%
  arrange(ime)

spol_18_23 <- subset(ris_org_18_23, , select = c(1:2))
spol_18_23 <- spol_18_23 %>%
  filter(duplicated(ime) == FALSE)

ris_startend_18_23 <- ris_startend_18_23 %>% 
  left_join(spol_18_23, by = "ime")

ris_startend_18_23 <- as.data.frame(ris_startend_18_23)

# Define residents and non-residents

# Table with all sightings in date format

ris_rnr_18_23 <- ris_org_18_23 %>% 
  mutate(datum = as_date(datum))

# Table with start and end dates of each capture interval and date of each sighting

ris_rnr2_18_23 <- ris_rnr_18_23 %>% 
  group_by(ime) %>%
  summarise(
    end = max(datum, na.rm = TRUE),
    start = min(datum, na.rm = TRUE)
  ) %>%
  arrange(ime) %>% 
  mutate(sighting_interval = interval(start, end))    

ris_rnr2_18_23 <- ris_rnr2_18_23 %>% 
  left_join(ris_rnr_18_23, by = "ime")

ris_rnr2_18_23 <- ris_rnr2_18_23[,c(1,3,2,4,5,6)]

length(unique(ris_rnr2_18_23$ime))

# Same table as ris_rnr2_18_23 but this time grouped by individual and with temporal intervals (days)

ris_rnr3_18_23 <- subset(ris_rnr2_18_23, , select = -c(6))

ris_rnr4_18_23 <- ris_rnr3_18_23 %>% 
  filter(duplicated(ime) == FALSE)

ris_rnr_intervals_18_23 <- ris_rnr4_18_23 %>% 
  select(ime, start, end) %>% 
  mutate(interval = end-start)

ris_rnr_intervals_18_23 <- subset(ris_rnr_intervals_18_23, , select = -c(2:3))

ris_rnr4_18_23 <- ris_rnr4_18_23 %>% 
  left_join(ris_rnr_intervals_18_23, by = "ime") 

ris_rnr4_18_23 <- ris_rnr4_18_23 %>%
  mutate(interval = as.numeric(interval))

table(ris_rnr4_18_23$spol)   # 55 males, 90 unknown and 56 females should be obtained here

# Define as non-residents the individuals with a total capture interval equal or shorter than
# 30 days, and as residents all others

ris_rnr5_18_23 <- ifelse(ris_rnr4_18_23$interval <= 30, "non res", "res")
ris_rnr5_18_23 <- as.data.frame(ris_rnr5_18_23)

ris_rnr6_18_23 <- cbind(ris_rnr4_18_23, ris_rnr5_18_23)

table(ris_rnr6_18_23$ris_rnr5_18_23)
table(ris_rnr6_18_23$ris_rnr5_18_23, ris_rnr6_18_23$spol)   # 67 non-residents (11 males, 44 unknown, 12 females) and
                                                            # 134 residents (44 males, 46 unknown, 44 females) should be obtained here

# Remove non-residents

category <- subset(ris_rnr6_18_23, , select = -c(2:4,6))

colnames(category)[1] <- "oznaka"

inds_18_19_names_cat <- inds_18_19_names %>%
  left_join(category, by = "oznaka")
inds_18_19_names_cat_res <- inds_18_19_names_cat %>%
  filter(ris_rnr5_18_23 == "res")                       # 47 individuals should be obtained here

inds_19_20_names_cat <- inds_19_20_names %>%
  left_join(category, by = "oznaka")
inds_19_20_names_cat_res <- inds_19_20_names_cat %>%
  filter(ris_rnr5_18_23 == "res")                       # 61 individuals

inds_20_21_names_cat <- inds_20_21_names %>%
  left_join(category, by = "oznaka")
inds_20_21_names_cat_res <- inds_20_21_names_cat %>%
  filter(ris_rnr5_18_23 == "res")                       # 74 individuals

inds_21_22_names_cat <- inds_21_22_names %>%
  left_join(category, by = "oznaka")
inds_21_22_names_cat_res <- inds_21_22_names_cat %>%
  filter(ris_rnr5_18_23 == "res")                       # 64 individuals

inds_22_23_names_cat <- inds_22_23_names %>%
  left_join(category, by = "oznaka")
inds_22_23_names_cat_res <- inds_22_23_names_cat %>%
  filter(ris_rnr5_18_23 == "res")                       # 72 individuals

# Differences between seasons

diff1819_1920_res <- setdiff(inds_18_19_names_cat_res, inds_19_20_names_cat_res)   # 16 individuals
rate1819_1920 <- nrow(diff1819_1920_res) / nrow(inds_18_19_names_cat_res) * 100   # 34.04 %

diff1920_2021_res <- setdiff(inds_19_20_names_cat_res, inds_20_21_names_cat_res)   # 16 individuals
rate1920_2021 <- nrow(diff1920_2021_res) / nrow(inds_19_20_names_cat_res) * 100   # 26.22 %

diff2021_2122_res <- setdiff(inds_20_21_names_cat_res, inds_21_22_names_cat_res)   # 29 individuals
rate2021_2122 <- nrow(diff2021_2122_res) / nrow(inds_20_21_names_cat_res) * 100   # 39.18 %

diff2122_2223_res <- setdiff(inds_21_22_names_cat_res, inds_22_23_names_cat_res)   # 16 individuals
rate2122_2223 <- nrow(diff2122_2223_res) / nrow(inds_21_22_names_cat_res) * 100   # 25 %

turnover_res <- data.frame(
  season_diff_res = c(
    "18/19 - 19/20",
    "19/20 - 20/21",
    "20/21 - 21/22",
    "21/22 - 22/23"
  ),
  rate_res = c(
    rate1819_1920,
    rate1920_2021,
    rate2021_2122,
    rate2122_2223
  )
)

mean(turnover_res$rate_res)

# Plot

turnover_res_plot <- ggplot(turnover_res, aes(x = season_diff_res, y = rate_res)) +
  geom_bar(stat = "identity") +
  xlab("Seasons") +
  ylab("Turnover rate (%)") +
  theme(axis.title.x = element_text(size = 27, margin = margin(t = 16)),
        axis.title.y = element_text(size = 27, margin = margin(r = 16)),
        axis.text.x = element_text(size = 25, margin = margin(t = 8)),
        axis.text.y = element_text(size = 25, margin = margin(r = 8)),
        text = element_text(family = "Calibri"))
