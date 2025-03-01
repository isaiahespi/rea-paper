# logging hours worked

# create data.frame (or `tbl_df`)
hours <- tibble::tribble(
  ~ date,
  ~ proj,
  ~ weekday,
  ~ start,
  ~ end,
  ~ duration.hours,
  ~ duration.period,
  ~ cumhours_per_proj
)


# d = Date
d <- timestamp(
  stamp = format(Sys.time(), "%Y-%m-%d"),
  prefix = NULL,
  suffix = NULL
) |>
  as.Date()

# t1 = START TIME
# NOTE: Run this once you START doing stuff to stamp "clock in" time
t1 <- timestamp(
  stamp = format(Sys.time(), "%H:%M:%S"),
  prefix = NULL,
  suffix = NULL
) |> hms::as_hms()

# alternative: input time manually  
t1 <- hms::as_hms("09:20:55")

# t2 = END TIME
# NOTE: Run this once you STOP doing stuff to stamp "clock out" time
t2 <- timestamp(
  stamp = format(Sys.time(), "%H:%M:%S"),
  prefix = NULL,
  suffix = NULL
) |> hms::as_hms()


# tdiff = duration.hours
tdiff <- round(difftime(t2, t1, units = "hours"), 2)

# project/task worked on

proj <- "vets survey"
proj <- "text analysis"
proj <- "prospectus"

# add today's time stamps to data frame and add in additional variables
hours <- hours |>
  # add row to time sheet
  dplyr::add_row(
    date = d,
    proj = proj,
    start = t1,
    end = t2,
    duration.hours = tdiff
  ) |>
  
  # add in column denoting day of the week
  dplyr::mutate(weekday = lubridate::wday(date, label = T, abbr = F),
                .before = start) |>
  
  # convert `duration.hours` to class `dbl` instead of class `difftime`
  # dplyr::mutate(duration.hours = as.numeric(duration.hours), .keep = "unused") |>
  
  # see `duration.hours` as period with hours, minutes, and seconds
  dplyr::mutate(duration.period = lubridate::as.period(duration.hours),
                .after = duration.hours) |>
  
  # group by project/task in order to calculate cumulative hours worked on task
  dplyr::group_by(proj) |>
  
  # calculate cumulative hours worked on particular task
  dplyr::mutate(cumhours_on_proj = cumsum(as.numeric(duration.hours))) |>
  
  # this converts cumulative hours into period format (e.g., xxhours, xxmin)
  dplyr::mutate(
    cumhours_per_proj = lubridate::dhours(cumhours_on_proj) |>
      lubridate::as.period() |>
      round(digits = 2),
    .keep = "unused"
  ) |>
  
  # ungroup data frame back into a 'tbl_df', "tbl", "data.frame" class
  dplyr::ungroup()

hours

# Look at the hours log in a nice simple table just for you (and maybe lawyers)

# install.packages("kableExtra")
## print tibble (table) in a kable table
kableExtra::kable(hours,
                  format = "simple",
                  digits = 2, row.names = FALSE, align = "c",
                  caption = NULL)



# write to .csv timesheet
write.csv(hours, file = "~/path/to/directory/where/you/keep/hours_log.csv", 
          quote = F,
          row.names = F)


# keep a plain text (.txt) file of your hours logged
# easier to upload .txt and keep track of versions in github

# `sink()` Redirects all console output to file "hours.txt"
sink(file = "~/path/to/directory/where/you/keep/hours.txt") 

## print tibble (table) in a kable table
kableExtra::kable(hours,
                  format = "simple",
                  digits = 2, row.names = FALSE, align = "c",
                  caption = NULL)

sink() # Resume writing output to console

# get total hours worked on a particular project or task :::::::::::::::::::####
hours |> 
  dplyr::mutate(new = lubridate::as.duration(duration.hours)) |> 
  dplyr::group_by(proj) |> 
  dplyr::summarise(sum_times = sum(new)) |> 
  dplyr::mutate(sum_times = lubridate::seconds_to_period(sum_times))


# Want to see total hours worked between specific periods of time? :::::::::####
# e.g., "Current pay period" or "Last Semester"?
# Note: this is kind of a pain since you have to input the dates manually
# I'm sure there's a better way but meh.

# table of hours worked in current pay period
hours |>
  dplyr::summarise(
    "Last Pay period" = lubridate::interval(
      start = date[5],
      end = date[9]),
    "Last Pay Period Total Hours" = sum(duration.hours[5:9]),
    "Current Pay Period" = lubridate::interval(
      start = lubridate::ymd("2024-08-11"),
      end = lubridate::ymd("2024-08-24")
    ),
    "Current Pay Period Total Hours" = sum(duration.hours[10:15]) |>
      lubridate::as.period()
  ) |>
  kableExtra::kable(format = "simple")

