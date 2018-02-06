library(rStrava)
path = "R:/5_IT/5_Secrets/"
load(paste0(path, "Strava_stoken.Rdata"))

# get activities, get activities by location, plot
my_acts <- get_activity_list(stoken)