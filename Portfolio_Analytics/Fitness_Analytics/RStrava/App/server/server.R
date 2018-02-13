library(rStrava)
library(magrittr)
library(purrr)
library(tidyverse)
library(trackeR)

path = "R:/5_IT/5_Secrets/"
load(paste0(path, "Strava_stoken.Rdata"))

# get activities, get activities by location, plot
my_acts <- get_activity_list(stoken)

#### get ids of runs

Id = my_acts %>% map_int("id")



library(lubridate)
StartDate = my_acts %>%
        map_chr("start_date_local") %>%
        ymd_hms()

#library(lubridate)
#dt1 <- '2014-08-23 17:23:02'
#dt1=Start[1]
#t=ymd_hms(dt1)
#wday(t, label = TRUE)




############
source("server/trackeR.R", local = T)