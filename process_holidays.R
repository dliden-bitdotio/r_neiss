library(RPostgres)
library(DBI)
library(lubridate)
library(tidyverse)
library(dbplyr)
library(timeDate)
#library(tis)

# Holiday Setup
hd = c("Christmas", "New Years Day", "Independence Day",
       "Thanksgiving", "Halloween", "Easter")
christmas_dates = seq(ymd("2016-12-25"), ymd("2020-12-25"), "years")
NY_dates = seq(ymd("2016-01-01"), ymd("2021-01-01"), "years")
Independence_dates = seq(ymd("2016-07-04"), ymd("2020-07-04"), "years")
Thanksgiving_dates = c(ymd("2016-11-24"),
                       ymd("2017-11-23"),
                       ymd("2018-11-22"),
                       ymd("2019-11-28"),
                       ymd("2020-11-26"))
Halloween_dates = seq(ymd("2016-10-31"), ymd("2020-10-31"), "years")
Easter_dates = c(ymd("2016-03-27"),
                 ymd("2017-04-16"),
                 ymd("2018-04-01"),
                 ymd("2019-04-21"),
                 ymd("2020-04-12"))

hd_df <- bind_rows(
  tibble(holiday = hd[1], date = christmas_dates),
  tibble(holiday = hd[2], date = NY_dates),
  tibble(holiday = hd[3], date = Independence_dates),
  tibble(holiday = hd[4], date = Thanksgiving_dates),
  tibble(holiday = hd[5], date = Halloween_dates),
  tibble(holiday = hd[6], date = Easter_dates),
  ) %>% mutate(daterange = case_when(
    holiday != "NewYears" ~ interval(date - days(3), date + days(3)),
    TRUE ~ interval(date - days(2), date+days(3)))) # NY/Christmas overlap

con <- dbConnect(RPostgres::Postgres(), dbname = 'bitdotio', 
                 host = 'db.bit.io',
                 port = 5432,
                 user = 'name',
                 password = Sys.getenv("BITIO_PASS2"))


# Connect
neiss <- con %>%
  tbl(in_schema("bitdotio/neiss", "neiss_2016_2020")) %>%
  collect()

neiss_hd <- neiss
neiss_hd$holiday <- map_lgl(neiss$Treatment_Date,
                            function(x) any(x %within% hd_df$daterange))
neiss_hd$holiday_name <- "Not a Holiday"
neiss_hd$holiday_name[neiss_hd$holiday] <- map_chr(neiss$Treatment_Date[neiss_hd$holiday],
                                                    function(x) hd_df$holiday[which(x %within% hd_df$daterange)])

holidays <- neiss_hd[,c("CPSC_Case_Number", "holiday", "holiday_name")]
holidays$holiday_name[holidays$holiday_name == "Not a Holiday"] = "Not near a holiday"
names(holidays)[2] = "near_holiday"
