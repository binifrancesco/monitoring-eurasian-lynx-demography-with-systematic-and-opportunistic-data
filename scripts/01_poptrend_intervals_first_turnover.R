
# load required packages

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

# set working directory

setwd(dirname(file.choose()))

# import all images, select desired period and remove kitten observations

export_lynxDB <- read.csv(file = file.choose())

export_lynxDB2 <- export_lynxDB %>%
  mutate(
    datum_vrijeme = ymd_hms(`datum_vrijeme`, tz = Sys.timezone())
  )

min(export_lynxDB2$datum_vrijeme)
max(export_lynxDB2$datum_vrijeme)

b_10_24 <- export_lynxDB2 %>%
  filter(`datum_vrijeme` >= "2010-05-01 00:00:00 UTC")

b_10_24 <- b_10_24 %>%
  filter(!grepl(('mlado'), oznaka)) %>%
  filter(!grepl(('mladu'), oznaka)) %>%
  filter(!grepl(('Mlado'), oznaka)) %>%
  filter(!grepl(('Mladu'), oznaka))


# *************************  1. minimum and maximum numbers of identified lynx  *************************

# list of lynx depending on which side is captured

lynx_L <- b_10_24 %>% 
  group_by(oznaka, slikana_strana_zivotinje) %>%
  summarize(slikana_strana_zivotinje = first(slikana_strana_zivotinje)) %>% 
  rename(slikana_strana_zivotinje3=slikana_strana_zivotinje) %>%
  filter(slikana_strana_zivotinje3 == 'lijeva')                           # 211 captured from the left side

lynx_R <- b_10_24 %>% 
  group_by(oznaka, slikana_strana_zivotinje) %>% 
  summarize(slikana_strana_zivotinje = first(slikana_strana_zivotinje)) %>% 
  rename(slikana_strana_zivotinje2=slikana_strana_zivotinje) %>% 
  filter(slikana_strana_zivotinje2 == 'desna')                            # 225 captured from the right side

lynx_LR <- lynx_L %>% inner_join(lynx_R)                      # 150 captured from both sides

# minimum lynx number, it takes the highest between the number of right or left captures. Here the right captures are more

ifelse(nrow(lynx_L) <= nrow(lynx_R), lynx_min <- lynx_R, lynx_min <- lynx_L)     # 225

# maximum lynx number, if all those captured from the left side are different than those captured from the right side

ifelse(nrow(lynx_L) <= nrow(lynx_R), lynx_max <- full_join(lynx_R, anti_join(lynx_L, lynx_LR)), lynx_max <- full_join(lynx_L,anti_join(lynx_R, lynx_LR)))

# 286


# ***********************  2. capture intervals and population trend plot  ***************************

b_10_24 <- b_10_24 %>%
  semi_join(lynx_min)

# table with the interval between first and last sighting for each lynx between 2008 and 2024

ris_interval <- b_10_24 %>% select ('datum_vrijeme', 'oznaka') %>% 
  mutate(datum = as.Date(datum_vrijeme, format = "%d.%m.%Y")) %>% 
  group_by(oznaka) %>%
  summarise(
    end = max(datum, na.rm = T),
    start = min(datum, na.rm = T)
  ) %>%
  arrange(oznaka) %>% 
  mutate(sighting_interval = interval(start, end))

# add the minimum age (days)

ris_interval_org <- ris_interval %>% 
  select(oznaka, start, end) %>% 
  mutate(min_starost_zivotinje = end-start)

# make a time window of 1 year to sample lynx abundance

time_window <- 12

start_project <-  min(ris_interval$start) 
end_project <- max(ris_interval$end)
month_seq <-  seq(start_project, end_project-months(time_window), by = "month")
month_int <-  interval(floor_date(month_seq, "month"), (floor_date(month_seq+months(time_window), "month")))

for (i in 1:length(month_int)) {
  column_name <- as.character(floor_date(month_seq[i], "month"))
  ris_interval$temp <- intersect(ris_interval$sighting_interval, month_int[i]) %>% 
    as.duration() %>% 
    as.numeric("days")
  names(ris_interval)[names(ris_interval) == "temp"] <- column_name
}

ris_interval2 <- ris_interval %>% 
  mutate(across(.cols = 5: ncol(ris_interval), ~replace(., !is.na(.), 1))) %>% 
  mutate(across(.cols = 5: ncol(ris_interval), ~replace(., is.na(.), 0)))       

ris_interval3 <- ris_interval2 %>% summarise(across(where(is.numeric), sum)) %>% 
  select(-sighting_interval)                                                    

ris_interval4 <-  head(ris_interval3) %>%
  rownames_to_column() %>%
  pivot_longer(, cols = -rowname) %>%
  pivot_wider(, names_from = rowname) %>%
  rename("category" = 1, num = 2) %>%
  as.data.frame()

# plot

minpop_plot <- ggplot(ris_interval4) +
  aes(x = category, y = num) +
  geom_point(shape = "circle", size = 3, colour = "#112446") +
  ylab("Minimum population size") +
  scale_x_discrete(name = "Date", , , ) +
  theme(
    axis.text.x = element_text(size = 16, margin = margin(t = 8), angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 25, margin = margin(r = 8)),
    axis.title.y = element_text(size = 27, margin = margin(r = 16)),
    axis.title  = element_text(size = 12),
    axis.title.x = element_text(size = 27, margin = margin(t = 16)),
    text = element_text(family = "Calibri"),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

minpop_plot


# ****************  3. first turnover calculation and minimum age  *******************

b_10_24_2 <- b_10_24

# recreate between 2011 and 2024

# table with the interval between first and last sighting for each lynx

ris_interval_11_24 <- b_10_24_2 %>% select ('datum_vrijeme', 'oznaka') %>% 
  mutate(datum = as.Date(datum_vrijeme, format = "%d.%m.%Y")) %>% 
  group_by(oznaka) %>%
  summarise(
    end = max(datum, na.rm = T),
    start = min(datum, na.rm = T)
  ) %>%
  arrange(oznaka) %>% 
  mutate(sighting_interval = interval(start, end))

# add the minimum age (days)

ris_interval_org_11_24 <- ris_interval_11_24 %>% 
  select(oznaka, start, end) %>% 
  mutate(min_starost_zivotinje = end-start)

# table with intervals (days) between different sightings for each lynx within the total capture interval for any given lynx
# and dates of sightings

ris_interval_every_11_24 <- b_10_24_2 %>% select (datum_vrijeme, oznaka) %>%
  arrange(oznaka, datum_vrijeme) %>%
  mutate(datum = as.Date(datum_vrijeme, format = "%d.%m.%Y")) %>% 
  group_by(oznaka) %>%
  mutate(time_interval = datum - lag(datum)) %>%
  arrange(desc(time_interval)) %>%
  select(oznaka, time_interval, datum)

ris_interval_every_11_24 <- ris_interval_every_11_24 %>% 
  left_join(ris_interval_org, by = "oznaka")

# reorganizing

ris_interval_every_11_24 <- ris_interval_every_11_24[,c(1,4,3,5,2,6)]
colnames(ris_interval_every_11_24)[3] <- "middle_sightings"
colnames(ris_interval_every_11_24)[5] <- "interval_between_sightings"
colnames(ris_interval_every_11_24)[6] <- "total_capture_interval_(min_age)"      # table with date of first, middle and last sightings,
                                                                                 # interval between sightings and total capture interval
                                                                                 # (minimum age) in days

# table only with age in years

min_lynx_age_new_11_24 <- ris_interval_org_11_24 %>% 
  group_by(oznaka) %>% 
  summarise(min_age = max(min_starost_zivotinje)) %>%
  mutate(min_age_year = min_age/365) %>% 
  arrange(desc(min_age))

min_lynx_age_new_11_24 <- subset(min_lynx_age_new_11_24, , select = c(oznaka, min_age_year))
min_lynx_age_new_11_24[] <- lapply(min_lynx_age_new_11_24, gsub, pattern = ' days', replacement = ' ')

# table with date of first and last sighting, total capture interval, minimum age (years)

ris_interval_avg_11_24 <- ris_interval_org_11_24
colnames(ris_interval_avg_11_24)[4] <- 'total_capture_interval_(min_age)'
ris_interval_avg_11_24 <- ris_interval_avg_11_24 %>% 
  left_join(min_lynx_age_new_11_24, by = "oznaka") 
colnames(ris_interval_avg_11_24)[5] <- 'min_age_years'           

# adding 1 year to each

ris_interval_avg_11_24[, c(4:5)] <- sapply(ris_interval_avg_11_24[, c(4:5)], as.numeric)

ris_interval_avg_11_24 <- ris_interval_avg_11_24 %>% 
  mutate(real_min_age_years = (min_age_years) + 1)

# exclude those seen for the first time after 05/2018

ris_interval_avg2_11_24 <- ris_interval_avg_11_24 %>%
  filter(start < "2018-05-01")

min(ris_interval_avg2_11_24$start)

summary(ris_interval_avg2_11_24$real_min_age_years)
sd(ris_interval_avg2_11_24$real_min_age_years)

# calculate turnover

77.2 / 2.5  # 30.88

# plot of minimum age

ris_spol_11_24 <- subset(b_10_24_2, , select = c(2,4))
ris_spol_11_24 <- ris_spol_11_24 %>%
  filter(duplicated(oznaka) == FALSE)

ris_interval_avg2_11_24 <- ris_interval_avg2_11_24 %>%
  left_join(ris_spol_11_24, by = "oznaka")

colnames(ris_interval_avg2_11_24)[7] <- "Sex"

ris_interval_avg2_11_24$Sex[ris_interval_avg2_11_24$Sex == "M"] <- "Males"
ris_interval_avg2_11_24$Sex[ris_interval_avg2_11_24$Sex == "Z"] <- "Females"

ggplot(ris_interval_avg2_11_24, aes(x = real_min_age_years, fill = Sex)) +
  geom_histogram(bins = 10) +
  xlab("Age") +
  ylab("Frequency") +
  scale_x_continuous(breaks = seq(0, 10, 0.5)) +
  scale_fill_brewer(palette = "Set1") +
  geom_vline(aes(xintercept = mean(real_min_age_years), linetype = "Mean"), colour = "black", linewidth = 1) +
  geom_vline(aes(xintercept = median(real_min_age_years), linetype = "Median"), colour = "black", linewidth = 1) +
  scale_linetype_manual(name = "Statistics", values = c("Mean" = "dashed", "Median" = "solid")) +
  theme(axis.title.x = element_text(size = 27, margin = margin(t = 16)),
        axis.title.y = element_text(size = 27, margin = margin(r = 16)),
        axis.text.x = element_text(size = 25, margin = margin(t = 8)),
        axis.text.y = element_text(size = 25, margin = margin(r = 8)),
        legend.title = element_text(size = 27),
        legend.text = element_text(size = 27),
        text = element_text(family = "Calibri")
  )


# ********************** 3. intervals between sightings for each lynx ************************

ris_interval_avg_11_24 <- subset(ris_interval_avg_11_24, , select = -c(6))


ris_interval_avg3_11_24 <- subset(ris_interval_every_11_24, , select = -c(6))
ris_interval_avg3_11_24 <- na.omit(ris_interval_avg3_11_24)                       # separate table with date of first, middle and last
                                                                                  # sightings and interval between sightings (no NAs)

avg_int_11_24 <- aggregate(ris_interval_avg3_11_24[, 5], list(ris_interval_avg3_11_24$oznaka), mean)
colnames(avg_int_11_24)[1] <- "oznaka"
colnames(avg_int_11_24)[2] <- "avg_int_between_sightings"                         # table with average interval between sightings (days) for each lynx

ris_interval_avg3_11_24 <- ris_interval_avg3_11_24 %>% 
  left_join(avg_int_11_24, by = "oznaka")                                         # join the 2 previous tables

ris_interval_avg_11_24 <- ris_interval_avg_11_24 %>% 
  left_join(avg_int_11_24, by = "oznaka")                                         # table with date of first and last sightings, total capture
                                                                                  # interval, minimum age (years) and average interval between
                                                                                  # sightings (days)

ris_interval_avg_11_24[, c(5:6)] <- sapply(ris_interval_avg_11_24[, c(5:6)], as.numeric)

# create and add a column with number of sightings for each lynx

n_sightings_11_24 <- ris_interval_every_11_24 %>% count(oznaka)
ris_interval_avg_11_24 <- ris_interval_avg_11_24 %>% 
  left_join(n_sightings_11_24, by = "oznaka") 
colnames(ris_interval_avg_11_24)[7] <- "n_sightings"
ris_interval_avg_11_24 <- ris_interval_avg_11_24[,c(1,2,3,4,5,7,6)]

# add column with individuals' sex

ris_spol_11_24 <- subset(b_10_24_2, , select = c(oznaka, spol))
ris_spol_11_24 <- ris_spol_11_24 %>%
  filter(duplicated(oznaka) == FALSE)

ris_interval_avg_11_24 <- ris_interval_avg_11_24 %>% 
  left_join(ris_spol_11_24, by = "oznaka")             

table(ris_interval_avg_11_24$spol)

# calculating parameters

ris_interval_avg_11_24[, c(4:7)] <- sapply(ris_interval_avg_11_24[, c(4:7)], as.numeric)

ris_interval_avg_once_11_24 <- ris_interval_avg_11_24[ris_interval_avg_11_24$n_sightings == "1", ] # separate table only with
                                                                                                   # individuals seen just once

ris_interval_avg_11_24 <- ris_interval_avg_11_24 %>%    # delete NAs (individuals seen just once)
  na.omit(ris_interval_avg_11_24)    # 185 individuals seen more than once

ris_interval_every2_11_24 <- subset(ris_interval_every_11_24, , select = -c(2:4,6))
ris_interval_every3_11_24 <- ris_interval_every2_11_24 %>%
  na.omit(ris_interval_every2_11_24)

max_int_11_24 <- ris_interval_every3_11_24 %>%
  filter(interval_between_sightings == max(interval_between_sightings))

max_int2_11_24 <- max_int_11_24 %>%
  filter(duplicated(oznaka) == FALSE)

ris_interval_avg_11_24 <- ris_interval_avg_11_24 %>% 
  left_join(max_int2_11_24, by = "oznaka")            

ris_interval_avg_11_24[, c(9)] <- sapply(ris_interval_avg_11_24[, c(9)], as.numeric)
colnames(ris_interval_avg_11_24)[9] <- "max_int_between_sightings_days"

# join the two data frames

ris_interval_avg_once_11_24[, 'max_int_between_sightings_days'] = NA
ris_interval_avg_all_11_24 <- rbind(ris_interval_avg_11_24, ris_interval_avg_once_11_24)
ris_interval_avg_all_11_24 <- ris_interval_avg_all_11_24[order(ris_interval_avg_all_11_24$oznaka),]

# removing 21 individuals seen twice in the same day

ris_interval_avg5_11_24 <- ris_interval_avg_11_24[!(ris_interval_avg_11_24$`total_capture_interval_(min_age)` %in% "0"),]   # 163

colnames(ris_interval_avg5_11_24)[8] <- "Sex"

ris_interval_avg5_11_24$Sex[ris_interval_avg5_11_24$Sex == "M"] <- "Males"
ris_interval_avg5_11_24$Sex[ris_interval_avg5_11_24$Sex == "Z"] <- "Females"

# parameters

ris_interval_avg5_11_24[, c(4:7, 9)] <- sapply(ris_interval_avg5_11_24[, c(4:7, 9)], as.numeric)

summary(ris_interval_avg5_11_24$n_sightings)
sd(ris_interval_avg5_11_24$n_sightings)

summary(ris_interval_avg5_11_24$avg_int_between_sightings)
sd(ris_interval_avg5_11_24$avg_int_between_sightings)

summary(ris_interval_avg5_11_24$max_int_between_sightings_days)
sd(ris_interval_avg5_11_24$max_int_between_sightings_days)

# plot

g10 <- ggplot(ris_interval_avg5_11_24, aes(x = n_sightings, fill = Sex)) +
  geom_histogram(bins = 10) +
  xlab("Sightings") +
  ylab("Frequency") +
  ggtitle("Number of sightings") +
  scale_x_continuous(breaks = seq(0, 250, 15)) +
  scale_fill_brewer(palette = "Set1") +
  geom_vline(aes(xintercept = mean(n_sightings)), colour = "black", lty = 'dashed', linewidth = 1) +
  geom_vline(aes(xintercept = median(n_sightings)), colour = "black", linewidth = 1) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 19, margin = margin(t = 16)),
        axis.title.y = element_text(size = 19, margin = margin(r = 16)),
        plot.title = element_text(size = 19),
        axis.text.x = element_text(size = 13, margin = margin(t = 8)),
        axis.text.y = element_text(size = 13, margin = margin(r = 8)),
        text = element_text(family = "Calibri")
  )

g11 <- ggplot(ris_interval_avg5_11_24, aes(x = max_int_between_sightings_days, fill = Sex)) +
  geom_histogram(bins = 10) +
  xlab("Days") +
  ylab("Frequency") +
  ggtitle("Maximum interval between sightings (days)") +
  scale_x_continuous(breaks = seq(0, 1700, 150)) +
  scale_fill_brewer(palette = "Set1") +
  geom_vline(aes(xintercept = mean(max_int_between_sightings_days)), colour = "black", lty = 'dashed', linewidth = 1) +
  geom_vline(aes(xintercept = median(max_int_between_sightings_days)), colour = "black", linewidth = 1) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 19, margin = margin(t = 16)),
        axis.title.y = element_text(size = 19, margin = margin(r = 16)),
        plot.title = element_text(size = 19),
        axis.text.x = element_text(size = 13, margin = margin(t = 8)),
        axis.text.y = element_text(size = 13, margin = margin(r = 8)),
        text = element_text(family = "Calibri")
  )

g12 <- ggplot(ris_interval_avg5_11_24,
              aes(x = avg_int_between_sightings, fill = Sex)) +
  geom_histogram(bins = 10) +
  xlab("Days") +
  ylab("Frequency") +
  ggtitle("Average interval between sightings (days)") +
  scale_x_continuous(breaks = seq(0, 850, 50)) +
  scale_fill_brewer(palette = "Set1") +
  geom_vline(aes(xintercept = mean(avg_int_between_sightings),
                 linetype = "Mean"),
             color = "black", linewidth = 1) +
  geom_vline(aes(xintercept = median(avg_int_between_sightings),
                 linetype = "Median"),
             color = "black", linewidth = 1) +
  scale_linetype_manual(
    name = "Statistics",
    values = c("Mean" = "dashed", "Median" = "solid")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(size = 5)
    )
  ) +
  theme(
    axis.title = element_text(size = 12),
    axis.title.y = element_text(margin = margin(t = 0, r = 22, b = 0, l = 0)),
    plot.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    text = element_text(family = "Calibri"),
    legend.key.size = unit(1, "cm"),
    legend.box = "vertical"
  )

legend <- get_legend(g12)

g12_noleg <- g12 + theme(legend.position = "none")

g10_l <- ggdraw(g10) +
  draw_label("a", x = 0.98, y = 0.98,
             hjust = 1, vjust = 1,
             size = 13)

g11_l <- ggdraw(g11) +
  draw_label("b", x = 0.98, y = 0.98,
             hjust = 1, vjust = 1,
             size = 13)

g12_l <- ggdraw(g12_noleg) +
  draw_label("c", x = 0.98, y = 0.98,
             hjust = 1, vjust = 1,
             size = 13)

plots_with_space <- plot_grid(
  g10_l,
  NULL,
  g11_l,
  NULL,
  g12_l,
  ncol = 1,
  rel_heights = c(1, 0.08, 1, 0.08, 1)
)

parameters_plot <- plot_grid(
  plots_with_space,
  legend,
  rel_widths = c(1, 0.20)
)

parameters_plot

