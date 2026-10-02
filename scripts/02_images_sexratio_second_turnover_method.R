
# Script 2/3
# Images for individual identification, population abundance and sex ratio and second turnover calculation method

# Load required packages

if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('lubridate')) install.packages('lubridate'); library('lubridate')
if (!require('writexl')) install.packages('writexl'); library('writexl')

if (!require('vistime')) install.packages('vistime'); library('vistime')
if (!require('zoo')) install.packages('zoo'); library('zoo')
if (!require('stringi')) install.packages('stringi'); library('stringi')
if (!require('fuzzyjoin')) install.packages('fuzzyjoin'); library('fuzzyjoin')
if (!require('sf')) install.packages('sf'); library('sf')
if (!require('plotly')) install.packages('plotly'); library('plotly')
if (!require('cowplot')) install.packages('cowplot'); library('cowplot')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')


# Import all images, select desired period and remove juvenile observations

export_lynxDB <- read.csv("data/data_for_scripts_01_02.csv")

b <- export_lynxDB2 <- export_lynxDB %>%
  mutate(
    datum_vrijeme = ymd_hms(`datum_vrijeme`, tz = Sys.timezone())
  )

b <- b %>%
  filter(`datum_vrijeme` >= "2010-05-01 00:00:00 UTC")

b <- b %>%
  filter(!grepl(('mlado'), oznaka)) %>%
  filter(!grepl(('mladu'), oznaka)) %>%
  filter(!grepl(('Mlado'), oznaka)) %>%
  filter(!grepl(('Mladu'), oznaka))


# *************************  1. minimum and maximum numbers  *************************

lynx_L <- b %>% 
  group_by(oznaka, slikana_strana_zivotinje) %>%
  summarize(slikana_strana_zivotinje = first(slikana_strana_zivotinje)) %>% 
  rename(slikana_strana_zivotinje3=slikana_strana_zivotinje) %>%
  filter(slikana_strana_zivotinje3 == 'lijeva')                           

lynx_R <- b %>% 
  group_by(oznaka, slikana_strana_zivotinje) %>% 
  summarize(slikana_strana_zivotinje = first(slikana_strana_zivotinje)) %>% 
  rename(slikana_strana_zivotinje2=slikana_strana_zivotinje) %>% 
  filter(slikana_strana_zivotinje2 == 'desna')                          

lynx_LR <- lynx_L %>% inner_join(lynx_R)                      

ifelse(nrow(lynx_L) <= nrow(lynx_R), lynx_min <- lynx_R, lynx_min <- lynx_L)  

ifelse(nrow(lynx_L) <= nrow(lynx_R), lynx_max <- full_join(lynx_R, anti_join(lynx_L, lynx_LR)), lynx_max <- full_join(lynx_L,anti_join(lynx_R, lynx_LR)))


# ********************** 2. intervals between sightings for each lynx 2018 - 2023 ************************

b <- b %>%
  semi_join(lynx_min)

b_18_23 <- b %>%
  filter(`datum_vrijeme` >= "2018-05-01 00:00:00 UTC" & `datum_vrijeme` <= "2023-04-30 23:59:00 UTC")

# need to recreate ris_interval e ris_interval_org between 05/2018 and 04/2023

# table with the interval between first and last sighting for each lynx

ris_interval_18_23 <- b_18_23 %>% select ('datum_vrijeme', 'oznaka') %>% 
  mutate(datum = as.Date(datum_vrijeme, format = "%d.%m.%Y")) %>% 
  group_by(oznaka) %>%
  summarise(
    end = max(datum, na.rm = T),
    start = min(datum, na.rm = T)
  ) %>%
  arrange(oznaka) %>% 
  mutate(sighting_interval = interval(start, end))

# add the minimum age (days)

ris_interval_org_18_23 <- ris_interval_18_23 %>% 
  select(oznaka, start, end) %>% 
  mutate(min_starost_zivotinje = end-start)         

# table with intervals (days) between different sightings for each lynx within the total capture interval for any given lynx
# and dates of sightings

ris_interval_every_18_23 <- b_18_23 %>% select (datum_vrijeme, oznaka) %>%
  arrange(oznaka, datum_vrijeme) %>%
  mutate(datum = as.Date(datum_vrijeme, format = "%d.%m.%Y")) %>% 
  group_by(oznaka) %>%
  mutate(time_interval = datum - lag(datum)) %>%
  arrange(desc(time_interval)) %>%
  select(oznaka, time_interval, datum)    # NAs in time_interval column: first sighting of individuals and individuals seen
                                          # once

ris_interval_every_18_23 <- ris_interval_every_18_23 %>% 
  left_join(ris_interval_org_18_23, by = "oznaka")                 # joins start and end date of the capture interval and the
                                                                   # minimum age for each lynx in days

# reorganizing

ris_interval_every_18_23 <- ris_interval_every_18_23[,c(1,4,3,5,2,6)]
colnames(ris_interval_every_18_23)[3] <- "middle_sightings"
colnames(ris_interval_every_18_23)[5] <- "interval_between_sightings"
colnames(ris_interval_every_18_23)[6] <- "total_capture_interval_(min_age)"   # table with date of first, middle and last
                                                                              # sightings, intervals between sightings and
                                                                              # total capture interval (minimum age) in days

ris_interval_org_18_23 %>% 
  group_by(oznaka) %>% 
  summarise(min_age = max(min_starost_zivotinje)) %>%
  mutate(min_age_year = min_age/365) %>% 
  arrange(desc(min_age)) %>% 
  write_xlsx(., "min_lynx_age_new_18_23.xls")   # table with each lynx's minimum age (days and years)


# table only with age in years

min_lynx_age_new_18_23 <- ris_interval_org_18_23 %>% 
  group_by(oznaka) %>% 
  summarise(min_age = max(min_starost_zivotinje)) %>%
  mutate(min_age_year = min_age/365) %>% 
  arrange(desc(min_age))

min_lynx_age_new_18_23 <- subset(min_lynx_age_new_18_23, , select = c(oznaka, min_age_year))
min_lynx_age_new_18_23[] <- lapply(min_lynx_age_new_18_23, gsub, pattern = ' days', replacement = ' ')   

# table with date of first and last sighting, total capture interval, minimum age (years)

ris_interval_avg_18_23 <- ris_interval_org_18_23
colnames(ris_interval_avg_18_23)[4] <- 'total_capture_interval_(min_age)'
ris_interval_avg_18_23 <- ris_interval_avg_18_23 %>% 
  left_join(min_lynx_age_new_18_23, by = "oznaka") 
colnames(ris_interval_avg_18_23)[5] <- 'min_age_years'               

# add the average interval between sightings for each lynx

ris_interval_avg2_18_23 <- subset(ris_interval_every_18_23, , select = -c(6))
ris_interval_avg2_18_23 <- na.omit(ris_interval_avg2_18_23)                       # separate table with date of first, middle and last
                                                                                  # sightings and interval between sightings (no NAs)

avg_int_18_23 <- aggregate(ris_interval_avg2_18_23[, 5], list(ris_interval_avg2_18_23$oznaka), mean)
colnames(avg_int_18_23)[1] <- "oznaka"
colnames(avg_int_18_23)[2] <- "avg_int_between_sightings"                         # table with average interval between sightings (days) for each lynx

ris_interval_avg2_18_23 <- ris_interval_avg2_18_23 %>% 
  left_join(avg_int_18_23, by = "oznaka")                                         # join the 2 previous tables

ris_interval_avg_18_23 <- ris_interval_avg_18_23 %>% 
  left_join(avg_int_18_23, by = "oznaka")                                         # table with date of first and last sightings, total capture
                                                                                  # interval, minimum age (years) and average interval between
                                                                                  # sightings (days)

ris_interval_avg_18_23[, c(5:6)] <- sapply(ris_interval_avg_18_23[, c(5:6)], as.numeric)      

# create and add a column with number of sightings for each lynx

n_sightings_18_23 <- ris_interval_every_18_23 %>% count(oznaka)
ris_interval_avg_18_23 <- ris_interval_avg_18_23 %>% 
  left_join(n_sightings_18_23, by = "oznaka") 
colnames(ris_interval_avg_18_23)[7] <- "n_sightings"
ris_interval_avg_18_23 <- ris_interval_avg_18_23[,c(1,2,3,4,5,7,6)]

# add column with individuals' sex

ris_spol_18_23 <- subset(b_18_23, , select = c(oznaka, spol))
ris_spol_18_23 <- ris_spol_18_23 %>%
  filter(duplicated(oznaka) == FALSE)

ris_interval_avg_18_23 <- ris_interval_avg_18_23 %>% 
  left_join(ris_spol_18_23, by = "oznaka")            

table(ris_interval_avg_18_23$spol)


# ******** 3. number of images of IDed animals between 05/2010 and 04/2024 (not with minimum identified lynx number) ********

# change working directory

setwd(dirname(file.choose()))

# import database of all observations (also not IDed)

export_lynxDB_allobs <- read.csv(file = file.choose())

export_lynxDB_idobs <- export_lynxDB_allobs[-which(export_lynxDB_allobs$oznaka == ""), ]  # only IDed obs

b_idph_10_24 <- export_lynxDB_idobs %>%
  filter(ime == 'Image') %>%
  mutate(datum_vrijeme = ymd_hms(`datum_vrijeme`, tz=Sys.timezone())) %>%
  filter(datum_vrijeme >= "2010-05-01 00:00:00 UTC" & datum_vrijeme <= "2024-04-30 23:59:00 UTC") %>% 
  filter(duplicated(datum_vrijeme) == FALSE)  

min(b_idph_10_24$datum_vrijeme)
max(b_idph_10_24$datum_vrijeme)

b_idph_10_24 <- b_idph_10_24 %>%
  filter(!grepl(('mlado'), oznaka)) %>%
  filter(!grepl(('mladu'), oznaka)) %>%
  filter(!grepl(('Mlado'), oznaka)) %>%
  filter(!grepl(('Mladu'), oznaka))        # 2740 (without kittens)

b_idph_10_24[, 'season'] = NA

b_idph_10_24$season <- ifelse(b_idph_10_24$datum_vrijeme < "2012-05-01", "11/12",
                              ifelse(b_idph_10_24$datum_vrijeme < "2013-05-01", "12/13",
                                     ifelse(b_idph_10_24$datum_vrijeme < "2014-05-01", "13/14",
                                            ifelse(b_idph_10_24$datum_vrijeme < "2015-05-01", "14/15",
                                                   ifelse(b_idph_10_24$datum_vrijeme < "2016-05-01", "15/16",
                                                          ifelse(b_idph_10_24$datum_vrijeme < "2017-05-01", "16/17",
                                                                 ifelse(b_idph_10_24$datum_vrijeme < "2018-05-01", "17/18",
                                                                        ifelse(b_idph_10_24$datum_vrijeme < "2019-05-01", "18/19",
                                                                               ifelse(b_idph_10_24$datum_vrijeme < "2020-05-01", "19/20",
                                                                                      ifelse(b_idph_10_24$datum_vrijeme < "2021-05-01", "20/21",
                                                                                             ifelse(b_idph_10_24$datum_vrijeme < "2022-05-01", "21/22",
                                                                                                    ifelse(b_idph_10_24$datum_vrijeme < "2023-05-01", "22/23",
                                                                                                           ifelse(b_idph_10_24$datum_vrijeme < "2024-05-01", "23/24",
                                                                                                           )
                                                                                                    )
                                                                                             )
                                                                                      )
                                                                               )
                                                                        )
                                                                 )
                                                          )
                                                   )
                                            )
                                     )
                              )
)

nrow(b_idph_10_24[b_idph_10_24$season == "11/12",])  # 53
nrow(b_idph_10_24[b_idph_10_24$season == "12/13",])  # 43
nrow(b_idph_10_24[b_idph_10_24$season == "13/14",])  # 47
nrow(b_idph_10_24[b_idph_10_24$season == "14/15",])  # 51
nrow(b_idph_10_24[b_idph_10_24$season == "15/16",])  # 41
nrow(b_idph_10_24[b_idph_10_24$season == "16/17",])  # 43
nrow(b_idph_10_24[b_idph_10_24$season == "17/18",])  # 38
nrow(b_idph_10_24[b_idph_10_24$season == "18/19",])  # 282
nrow(b_idph_10_24[b_idph_10_24$season == "19/20",])  # 357
nrow(b_idph_10_24[b_idph_10_24$season == "20/21",])  # 472
nrow(b_idph_10_24[b_idph_10_24$season == "21/22",])  # 570
nrow(b_idph_10_24[b_idph_10_24$season == "22/23",])  # 583
nrow(b_idph_10_24[b_idph_10_24$season == "23/24",])  # 160

ggplot(b_idph_10_24, aes(x = season, group = 1)) +
  geom_line(stat = "count", linewidth = 1.5) +
  ggtitle("Number of photos of IDed individuals by season") +
  xlab("Season") +
  ylab("Number")


# ******* 4. number of IDed animals between 05/2010 and 04/2024 (not with minimum identified lynx number) *******

b_ind_10_24 <- b

length(unique(b_ind_10_24$oznaka))  # 225

min(b_ind_10_24$datum_vrijeme)
max(b_ind_10_24$datum_vrijeme)

b_ind_10_24 <- b_ind_10_24 %>%
  filter(!grepl(('mlado'), oznaka)) %>%
  filter(!grepl(('mladu'), oznaka)) %>%
  filter(!grepl(('Mlado'), oznaka)) %>%
  filter(!grepl(('Mladu'), oznaka))        # 2931 (without kittens)

b_ind_10_24[, 'season'] = NA

b_ind_10_24$season <- ifelse(b_ind_10_24$datum_vrijeme < "2012-05-01", "11/12",
                             ifelse(b_ind_10_24$datum_vrijeme < "2013-05-01", "12/13",
                                    ifelse(b_ind_10_24$datum_vrijeme < "2014-05-01", "13/14",
                                           ifelse(b_ind_10_24$datum_vrijeme < "2015-05-01", "14/15",
                                                  ifelse(b_ind_10_24$datum_vrijeme < "2016-05-01", "15/16",
                                                         ifelse(b_ind_10_24$datum_vrijeme < "2017-05-01", "16/17",
                                                                ifelse(b_ind_10_24$datum_vrijeme < "2018-05-01", "17/18",
                                                                       ifelse(b_ind_10_24$datum_vrijeme < "2019-05-01", "18/19",
                                                                              ifelse(b_ind_10_24$datum_vrijeme < "2020-05-01", "19/20",
                                                                                     ifelse(b_ind_10_24$datum_vrijeme < "2021-05-01", "20/21",
                                                                                            ifelse(b_ind_10_24$datum_vrijeme < "2022-05-01", "21/22",
                                                                                                   ifelse(b_ind_10_24$datum_vrijeme < "2023-05-01", "22/23",
                                                                                                          ifelse(b_ind_10_24$datum_vrijeme < "2024-05-31", "23/24",
                                                                                                          )
                                                                                                   )
                                                                                            )
                                                                                     )
                                                                              )
                                                                       )
                                                                )
                                                         )
                                                  )
                                           )
                                    )
                             )
)

nrow(b_ind_10_24[b_ind_10_24$season == "11/12",])  # 19
nrow(b_ind_10_24[b_ind_10_24$season == "12/13",])  # 11
nrow(b_ind_10_24[b_ind_10_24$season == "13/14",])  # 6
nrow(b_ind_10_24[b_ind_10_24$season == "14/15",])  # 14
nrow(b_ind_10_24[b_ind_10_24$season == "15/16",])  # 31
nrow(b_ind_10_24[b_ind_10_24$season == "16/17",])  # 41
nrow(b_ind_10_24[b_ind_10_24$season == "17/18",])  # 38
nrow(b_ind_10_24[b_ind_10_24$season == "18/19",])  # 288
nrow(b_ind_10_24[b_ind_10_24$season == "19/20",])  # 411
nrow(b_ind_10_24[b_ind_10_24$season == "20/21",])  # 607
nrow(b_ind_10_24[b_ind_10_24$season == "21/22",])  # 714
nrow(b_ind_10_24[b_ind_10_24$season == "22/23",])  # 653
nrow(b_ind_10_24[b_ind_10_24$season == "23/24",])  # 98

b_ind_10_24_grp <- b_ind_10_24 %>%
  group_by(oznaka, season) %>%
  filter(row_number(oznaka) == 1)  

ggplot(b_ind_10_24_grp, aes(x = season, group = 1)) +
  geom_line(stat = "count", linewidth = 1.5) +
  ggtitle("Number of IDed individuals by season") +
  xlab("Season") +
  ylab("Number")



# ! Before running section 5, run all script 03 !




# ******** 5. sex ratios ********

ris_interval_every2_18_23 <- ris_interval_every_18_23 %>% 
  left_join(ris_spol_18_23, by = "oznaka")

length(unique(ris_interval_every_18_23$oznaka)) 


ris_interval_every2_18_23 <- subset(ris_interval_every2_18_23, , select = c(1,3,7))


ris_interval_every2_18_23[, 'season'] = NA

ris_interval_every2_18_23$season <- ifelse(ris_interval_every2_18_23$middle_sightings < "2019-05-01", "18/19",
                                           ifelse(ris_interval_every2_18_23$middle_sightings < "2020-05-01", "19/20",
                                                  ifelse(ris_interval_every2_18_23$middle_sightings < "2021-05-01", "20/21",
                                                         ifelse(ris_interval_every2_18_23$middle_sightings < "2022-05-01", "21/22",
                                                                ifelse(ris_interval_every2_18_23$middle_sightings <= "2023-04-30", "22/23",
                                                                )
                                                         )
                                                  )
                                           )
)

ris_interval_every2_each_18_23 <- ris_interval_every2_18_23 %>%
  group_by(oznaka, season) %>%
  select(oznaka, middle_sightings, spol, season) %>%
  filter(row_number(oznaka) == 1)

length(unique(ris_interval_every2_each_18_23$oznaka))   

ris_interval_every3_each_18_23 <- subset(ris_interval_every2_each_18_23, , select = c(3:4))

colnames(ris_interval_every3_each_18_23)[1] <- "Sex"

ris_interval_every3_each_18_23$Sex[ris_interval_every3_each_18_23$Sex == "M"] <- "Males"
ris_interval_every3_each_18_23$Sex[ris_interval_every3_each_18_23$Sex == "Z"] <- "Females"

r <- which(
  ris_interval_every3_each_18_23$Sex == "Females" &
    ris_interval_every3_each_18_23$season == "18/19"
)[1]

ris_interval_every3_each_18_23 <- ris_interval_every3_each_18_23[-r, ]

ggplot(ris_interval_every3_each_18_23, aes(x = season, fill = Sex)) +
  geom_bar(position = "fill") +
  xlab("Season") +
  ylab("Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.title.x = element_text(size = 27, margin = margin(t = 16)),
        axis.title.y = element_text(size = 27, margin = margin(r = 16)),
        axis.text.x = element_text(size = 25, margin = margin(t = 8)),
        axis.text.y = element_text(size = 25, margin = margin(r = 8)),
        legend.title = element_text(size = 27),
        legend.text = element_text(size = 27),
        text = element_text(family = "Calibri")
  )


# ******** 6. second turnover method ********

if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('writexl')) install.packages('writexl'); library('writexl')
if (!require('readxl')) install.packages('readxl'); library('readxl')
if (!require('vistime')) install.packages('vistime'); library('vistime')

nrow(b_ind_10_24_grp[b_ind_10_24_grp$season == "18/19",]) 
nrow(b_ind_10_24_grp[b_ind_10_24_grp$season == "19/20",])  
nrow(b_ind_10_24_grp[b_ind_10_24_grp$season == "20/21",])  
nrow(b_ind_10_24_grp[b_ind_10_24_grp$season == "21/22",])  
nrow(b_ind_10_24_grp[b_ind_10_24_grp$season == "22/23",])  

# grouped by sex

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

ris_18_23 <- b_18_23

ris_org_18_23 <- ris_18_23 %>%
  rename(ime = "oznaka") %>%
  mutate(datum = ymd_hms(datum_vrijeme, tz = Sys.timezone())) %>% 
  select(-datum_vrijeme) %>% 
  arrange(ime)

length(unique(ris_org_18_23$ime))
min(ris_org_18_23$datum)
max(ris_org_18_23$datum)

ris_org_18_23 <- subset(ris_org_18_23, , select = -c(1, 3, 5:6))  

ris_startend_18_23 <- ris_org_18_23 %>%
  group_by(ime) %>%
  summarise(
    start = min(datum, na.rm = T),
    end = max(datum, na.rm = T)
  ) %>%
  arrange(ime)

spol_18_23 <- subset(ris_org_18_23, , select = c(1:2))
spol_18_23 <- spol_18_23 %>%
  filter(duplicated(ime) == FALSE)

ris_startend_18_23 <- ris_startend_18_23 %>% 
  left_join(spol_18_23, by = "ime")

ris_startend_18_23 <- as.data.frame(ris_startend_18_23)      # table with first and last sighting and sex for the 201 individuals

# defining residents and non-residents

ris_rnr_18_23 <- ris_org_18_23 %>% 
  mutate(datum = as_date(datum))

class(ris_rnr_18_23$datum)           # table with all sightings in date format

ris_rnr2_18_23 <- ris_rnr_18_23 %>% 
  group_by(ime) %>%
  summarise(
    end = max(datum, na.rm = T),
    start = min(datum, na.rm = T)
  ) %>%
  arrange(ime) %>% 
  mutate(sighting_interval = interval(start, end))    

ris_rnr2_18_23 <- ris_rnr2_18_23 %>% 
  left_join(ris_rnr_18_23, by = "ime")

ris_rnr2_18_23 <- ris_rnr2_18_23[,c(1,3,2,4,5,6)]

length(unique(ris_rnr2_18_23$ime))        # table with start and end of each capture interval and date of each sighting

ris_rnr3_18_23 <- subset(ris_rnr2_18_23, , select = -c(6))

ris_rnr4_18_23 <- ris_rnr3_18_23 %>% 
  filter(duplicated(ime) == FALSE)

ris_rnr_intervals_18_23 <- ris_rnr4_18_23 %>% 
  select(ime, start, end) %>% 
  mutate(interval = end-start)

ris_rnr_intervals_18_23 <- subset(ris_rnr_intervals_18_23, , select = -c(2:3))

ris_rnr4_18_23 <- ris_rnr4_18_23 %>% 
  left_join(ris_rnr_intervals_18_23, by = "ime")    # same table as rnr2 but grouped by individual and with intervals in days

ris_rnr4_18_23 <- ris_rnr4_18_23 %>%
  mutate(interval = as.numeric(interval))

table(ris_rnr4_18_23$spol)


ris_rnr5_18_23 <- ifelse(ris_rnr4_18_23$interval <= 30, "non res", "res")
ris_rnr5_18_23 <- as.data.frame(ris_rnr5_18_23)

ris_rnr6_18_23 <- cbind(ris_rnr4_18_23, ris_rnr5_18_23)

table(ris_rnr6_18_23$ris_rnr5_18_23)
table(ris_rnr6_18_23$ris_rnr5_18_23, ris_rnr6_18_23$spol)    # 134 res: 44 M, 46 unknown, 44 Z
                                                             # 67 non res: 11 M, 44 unknown, 12 Z

# removing non residents

cat <- subset(ris_rnr6_18_23, , select = -c(2:4,6))

colnames(cat)[1] <- "oznaka"

inds_18_19_names_cat <- inds_18_19_names %>%
  left_join(cat, by = "oznaka")
inds_18_19_names_cat_res <- inds_18_19_names_cat %>%
  filter(ris_rnr5_18_23 == "res")                       # 47

inds_19_20_names_cat <- inds_19_20_names %>%
  left_join(cat, by = "oznaka")
inds_19_20_names_cat_res <- inds_19_20_names_cat %>%
  filter(ris_rnr5_18_23 == "res")                       # 61

inds_20_21_names_cat <- inds_20_21_names %>%
  left_join(cat, by = "oznaka")
inds_20_21_names_cat_res <- inds_20_21_names_cat %>%
  filter(ris_rnr5_18_23 == "res")                       # 74

inds_21_22_names_cat <- inds_21_22_names %>%
  left_join(cat, by = "oznaka")
inds_21_22_names_cat_res <- inds_21_22_names_cat %>%
  filter(ris_rnr5_18_23 == "res")                       # 64

inds_22_23_names_cat <- inds_22_23_names %>%
  left_join(cat, by = "oznaka")
inds_22_23_names_cat_res <- inds_22_23_names_cat %>%
  filter(ris_rnr5_18_23 == "res")                       # 72



diff1819_1920_res <- setdiff(inds_18_19_names_cat_res, inds_19_20_names_cat_res)  # 16
1600/47  # 34.04 %

diff1920_2021_res <- setdiff(inds_19_20_names_cat_res, inds_20_21_names_cat_res)  # 16
1600/61  # 26.22 %

diff2021_2122_res <- setdiff(inds_20_21_names_cat_res, inds_21_22_names_cat_res)  # 29
2900/74  # 39.18 %

diff2122_2223_res <- setdiff(inds_21_22_names_cat_res, inds_22_23_names_cat_res)  # 16
1600/64  # 25 %

season_diff_res <- c("18/19 - 19/20", "19/20 - 20/21", "20/21 - 21/22", "21/22 - 22/23")
rate_res <- c("34.04", "26.22", "39.18", "25")

turnover_res <- data.frame(season_diff_res, rate_res)
turnover_res[, c(2)] <- sapply(turnover_res[, c(2)], as.numeric)

(34.04 + 26.22 + 39.18 + 25) / 4   # 31.11 %

ggplot(turnover_res, aes(x = season_diff_res, y = rate_res)) +
  geom_bar(stat = "identity") +
  xlab("Seasons") +
  ylab("Turnover rate (%)") +
  theme(axis.title.x = element_text(size = 27, margin = margin(t = 16)),
        axis.title.y = element_text(size = 27, margin = margin(r = 16)),
        axis.text.x = element_text(size = 25, margin = margin(t = 8)),
        axis.text.y = element_text(size = 25, margin = margin(r = 8)),
        text = element_text(family = "Calibri"))

