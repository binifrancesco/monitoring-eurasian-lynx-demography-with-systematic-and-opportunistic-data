
# Script 3/3
# All images and only IDed images compared

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
if (!require('ggthemes')) install.packages('ggthemes'); library('ggthemes')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')


# Import all records (also not IDed images), select desired period and remove juveniles

export_lynxDB_allobs2 <- read.csv("data/data_for_scripts_02part3_03.csv")

b_allph_10_24 <- export_lynxDB_allobs2 %>%
  filter(ime == 'Image') %>% 
  mutate(datum_vrijeme = ymd_hms(`datum_vrijeme`, tz=Sys.timezone())) %>%
  filter(datum_vrijeme >= "2010-05-01 00:00:00 UTC" & datum_vrijeme <= "2024-04-30 23:59:00 UTC") %>% 
  filter(duplicated(datum_vrijeme) == FALSE)

b_allph_10_24 <- b_allph_10_24 %>%
 filter(!grepl("mlado|mladu", oznaka, ignore.case = TRUE))   

b2_allph_10_24 <- b_allph_10_24

b2_allph_10_24[, 'season'] = NA

nrow(b2_allph_10_24[b2_allph_10_24$datum_vrijeme < "2012-05-01", ])
nrow(b2_allph_10_24[b2_allph_10_24$datum_vrijeme < "2013-05-01" & b2_allph_10_24$datum_vrijeme >= "2012-05-01", ]) # Change to check

b2_allph_10_24$season <- ifelse(b2_allph_10_24$datum_vrijeme < "2012-05-01", "11/12",
                                ifelse(b2_allph_10_24$datum_vrijeme < "2013-05-01", "12/13",
                                       ifelse(b2_allph_10_24$datum_vrijeme < "2014-05-01", "13/14",
                                              ifelse(b2_allph_10_24$datum_vrijeme < "2015-05-01", "14/15",
                                                     ifelse(b2_allph_10_24$datum_vrijeme < "2016-05-01", "15/16",
                                                            ifelse(b2_allph_10_24$datum_vrijeme < "2017-05-01", "16/17",
                                                                   ifelse(b2_allph_10_24$datum_vrijeme < "2018-05-01", "17/18",
                                                                          ifelse(b2_allph_10_24$datum_vrijeme < "2019-05-01", "18/19",
                                                                                 ifelse(b2_allph_10_24$datum_vrijeme < "2020-05-01", "19/20",
                                                                                        ifelse(b2_allph_10_24$datum_vrijeme < "2021-05-01", "20/21",
                                                                                               ifelse(b2_allph_10_24$datum_vrijeme < "2022-05-01", "21/22",
                                                                                                      ifelse(b2_allph_10_24$datum_vrijeme < "2023-05-01", "22/23",
                                                                                                             ifelse(b2_allph_10_24$datum_vrijeme < "2024-05-01", "23/24",
                                                                                                             )
                                                                                                      )
                                                                                               )
                                                                                        )
                                                                                 )))))))))

nrow(b2_allph_10_24[b2_allph_10_24$season == "11/12",]) # 56
nrow(b2_allph_10_24[b2_allph_10_24$season == "12/13",]) # 59
nrow(b2_allph_10_24[b2_allph_10_24$season == "13/14",]) # 73
nrow(b2_allph_10_24[b2_allph_10_24$season == "14/15",]) # 67
nrow(b2_allph_10_24[b2_allph_10_24$season == "15/16",]) # 151
nrow(b2_allph_10_24[b2_allph_10_24$season == "16/17",]) # 168
nrow(b2_allph_10_24[b2_allph_10_24$season == "17/18",]) # 143
nrow(b2_allph_10_24[b2_allph_10_24$season == "18/19",]) # 418
nrow(b2_allph_10_24[b2_allph_10_24$season == "19/20",]) # 455
nrow(b2_allph_10_24[b2_allph_10_24$season == "20/21",]) # 620
nrow(b2_allph_10_24[b2_allph_10_24$season == "21/22",]) # 715
nrow(b2_allph_10_24[b2_allph_10_24$season == "22/23",]) # 883

b3_allph_10_24 <- subset(b2_allph_10_24, , select = c(1,10))

ggplot(b3_allph_10_24, aes(x = season, group = 1)) +
  geom_line(stat = "count", linewidth = 1.5) +
  xlab("Season") +
  ylab("Number") +
  ggtitle("Number of photos of IDed and not IDed individuals by season")

tmp <- load(file = file.choose())

ggplot(b_idph_10_24, aes(x = season, group = 1)) +
  geom_line(stat = "count", linewidth = 1.5) +
  geom_point(b_idph_10_24, mapping = aes(x = season), size = 4, stat = "count") +
  guides(size = "none") +
  ggtitle("Number of photos of IDed individuals by season") +
  xlab("Season") +
  ylab("Number")              # plot of images IDed individuals from other script

# lines of IDed and IDed-not IDed together

ggplot() +
  geom_line(b_idph_10_24, mapping = aes(x = season, colour = "only IDed individuals"), stat = "count", group = 1, linewidth = 1.5) +
  geom_point(b_idph_10_24, mapping = aes(x = season), size = 4, colour = "orange", stat = "count") +
  geom_line(b3_allph_10_24, mapping = aes(x = season, colour = "all individuals"), stat = "count", group = 1, linewidth = 1.5) +
  geom_point(b3_allph_10_24, mapping = aes(x = season), size = 4, stat = "count") +
  scale_color_manual(values = c('black', 'orange')) +
  labs(colour = NULL) +
  ggtitle("Number of photos by season") +
  xlab("Season") +
  ylab("Number")
