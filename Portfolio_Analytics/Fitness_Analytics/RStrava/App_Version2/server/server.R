library(rStrava)
library(magrittr)
library(purrr)
library(tidyverse)
library(trackeR)
library(DT)

path = "R:/5_IT/5_Secrets/"
load(paste0(path, "Strava_stoken.Rdata"))

# get activities, get activities by location, plot
my_acts <- get_activity_list(stoken)

#### get ids of runs

Id = my_acts %>% map_int("id")
Name = my_acts %>% map_chr("name")
Moving = my_acts %>% map_int("moving_time")/60
Elapsed = my_acts %>% map_int("elapsed_time")/60
Distance = my_acts %>% map_dbl("distance") 

library(lubridate)
StartDate = my_acts %>%
        map_chr("start_date_local") %>%
        ymd_hms()


DATA=tibble(Id,StartDate,Name,Distance,Moving,Elapsed)



#library(lubridate)
#dt1 <- '2014-08-23 17:23:02'
#dt1=Start[1]
#t=ymd_hms(dt1)
#wday(t, label = TRUE)




############
source("server/trackeR.R", local = T)