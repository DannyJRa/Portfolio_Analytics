myinfo <- get_athlete(stoken, id = '18707693')
myinfo$firstname
myinfo$id





####### Streams


id = "1291626974"
types = c("time", "latlng", "distance", "altitude", "velocity_smooth", "heartrate", "cadence", "watts", "temp", "moving", "grade_smooth")
stream = get_streams(stoken, id, request = "activities",
                                                 types, resolution = NULL, series_type = NULL)


##### Extract info
library(magrittr)
library(purrr)
library(tidyverse)


df <- stream %>%
    {
        tibble(
       lat = map_dbl(stream[[1]]$data, 1),
       long = map_dbl(stream[[1]]$data, 2),
       time = map_dbl(stream[[2]]$data, 1) / 60,
        distance = map_dbl(stream[[3]]$data, 1),
      altitude = map_dbl(stream[[4]]$data, 1),
      moving = map_dbl(stream[[6]]$data, 1)
  )
    }
glimpse(df)

