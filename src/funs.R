# functions
# https://adv-r.hadley.nz/functions.html
# https://dereksonderegger.github.io/570L/12-user-defined-functions.html
# 

# fun: tabyl crosstabs :::::::::::::::::::::::::::::::::::::::::::::::::::####

# function to create crosstabs using janitor::tabyl() with adorn functions as
# arguments
tabyl.crabs <- function(dat, var1, var2, var3, show_na=FALSE, totals = TRUE, margin_pcts = "row", digits = 2, affix_sign = F, ...){
  
  if (!missing(var1) && !missing(var2) && !missing(var3)){
    crab_df <- dat |> janitor::tabyl({{ var1 }}, {{ var2 }}, {{ var3 }})
  } else if (missing(var3) && !missing(var1) && !missing(var2)){
    crab_df <- dat |> janitor::tabyl({{ var1 }}, {{ var2 }})
  } else {
    stop("please specify var1 & var2 or var1 & var2 & var3. Use janitor::tabyl for tabyl call on single vector")
  }
  
  if (totals == TRUE) {
    crab_df |> 
      janitor::adorn_totals("both") |>
      janitor::adorn_percentages(margin_pcts) |>
      janitor::adorn_pct_formatting(digits = digits, affix_sign = affix_sign) |>
      janitor::adorn_ns("rear") |>
      janitor::adorn_title("combined")
  } else {
    crab_df |> 
      janitor::adorn_percentages(margin_pcts) |>
      janitor::adorn_pct_formatting(digits = digits, affix_sign = affix_sign) |>
      janitor::adorn_ns("rear") |>
      janitor::adorn_title("combined")
  }
  
}

# test it out. It works
# tabyl.crabs(
#   dat = df,
#   var1 = group,
#   var2 = q19,
#   show_na = F,
#   totals = T,
#   margin_pcts = "row",
#   digits = 2,
#   affix_sign = F
# )

# works with 3 variables
# tabyl.crabs(
#   dat = df,
#   var1 = group,
#   var2 = q19,
#   var3 = q7,
#   show_na = F,
#   totals = T,
#   margin_pcts = "row",
#   digits = 2,
#   affix_sign = F
# )

# works with minimal input
# tabyl.crabs(df, group, q19)

# another tabyl fun ::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

item_crosstab <- function(dat, row, col, pcts = TRUE, ...){
  
  if (!missing(row) && !missing(col)) {
    crab_df <- dat |> janitor::tabyl({{ row }}, {{ col }})
  } else if (missing(row) && !missing(col)){
    stop("Row variable not specified. Please specify")
  } else if (!missing(row) && missing(col)){
    stop("Please specify column variable")
  } else {
    stop("Neither row nor column variable specified, but both are required.")
  }
  
  if (pcts == TRUE) {
    crab_df |> 
      janitor::adorn_totals("both") |>
      janitor::adorn_percentages("row") |>
      janitor::adorn_pct_formatting(digits = 2, affix_sign = T) |>
      janitor::adorn_ns("rear") |>
      janitor::adorn_title("combined")
  } else {
    crab_df |> 
      janitor::adorn_totals(c("row", "col")) |> 
      janitor::adorn_title("combined")
  }
  
}

# yet another ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

run_crab <- function(data, row, col){
  out <- data |> 
    janitor::tabyl({{ row }}, {{ col }}) |> 
    janitor::adorn_totals(c("row", "col")) |> 
    janitor::adorn_percentages('row') |> 
    janitor::adorn_pct_formatting(digits = 2) |> 
    janitor::adorn_ns()
  return(out)
}

# fun: Likert Plots:::::::::::::::::::::::::::::::::::::::::::::::::::::::####
# To avoid excessive copy/paste, I made a function to create Likert plots
# it is mostly a wrapper around `ggstats::gglikert`, but with a specific set up
# particular colors, theme, etc. 
likert_plot <- function(data, x, ...,  
                        symmetric = FALSE,
                        variable_labels = NULL,
                        label_size = 2.7,
                        vline = FALSE,
                        title = waiver(), 
                        subtitle = waiver(),
                        caption = waiver(),
                        xlab = waiver()){
  
  p <- ggstats::gglikert(
    data = data, include = {{ x }}, ...,
    variable_labels = variable_labels,
    symmetric = symmetric)+
    
    # customize color
    ggplot2::scale_fill_grey(start = 0.5, end = 0.1)+
    # custom theme
    theme_bw(base_family = "TeX Gyre Pagella")+
    theme(
      legend.position = 'bottom',    # place legend on bottom
      axis.text.x = element_blank(), # remove percentage text along x-axis
      strip.text.y.right = element_text(angle = 0) # make facet label horizontal
        )
    
    if (vline==TRUE) {
    p <- p + geom_vline(xintercept = 0, color = 'black', linewidth = 1.2)
  } else {
      p
  }
  
 
  p <- p + ggplot2::labs(..., title = title, subtitle=subtitle, caption=caption, x = xlab)
  p

}

# working example:
# df |> likert_plot(
#   x = trust.az.items,
#   variable_labels = trust.az.varlabels,
#   title = "Trust in Maricopa County, AZ, Elections"
# )

# working example: x and y var
# df |> likert_plot(
#   x = trust.az.items,
#   y = 'group',
#   facet_rows = vars(.question),
#   facet_label_wrap = 15,
#   variable_labels = trust.az.varlabels,
#   title = "Trust in Maricopa County, AZ, Elections"
# )

# fun: custom barplot :::::::::::::::::::::::::::::::::::::::::::::::::::####

custom_barplot <- function(data, x, group, title = NULL, subtitle = NULL,
                           caption = NULL, legend_fill = NULL,
                           xlab = NULL){
  b <- data |> 
    group_by({{ group }}, {{ x }}) |> 
    count() |> 
    drop_na() |> 
    group_by({{ group }}) |> 
  mutate(prop = round(n/sum(n), digits = 3),
         pct = prop*100,
         res = str_c(pct,'% (', n, ')', sep = "")) |> 
  ggplot(aes(x = {{ x }}, y = pct, fill = {{ group }}))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_text(aes(label = res), position = position_dodge(1.0), size = 2.5, vjust = -0.5)+
  scale_fill_grey(start = 0.5, end = 0.1)
  
  b <- b + ggplot2::labs(
    title = stringr::str_wrap(title, width = 85),
    subtitle = stringr::str_wrap(subtitle, width = 85),
    caption = stringr::str_wrap(caption, width = 85),
    fill = legend_fill,
    y = "Percentage",
    x = stringr::str_wrap(xlab, width = 85))
  b
}

# fun: compute counts and proportions ::::::::::::::::::::::::::::::::::::####

# compute counts and proportions
count_prop <- function(dat, x, drop_na = FALSE, sort = FALSE, pcts = FALSE){
  p <- dat |>  
    count({{ x }}, sort = sort)
  if (drop_na == TRUE){
    p <- p |> 
      drop_na() |> 
      mutate(prop = n/sum(n))
  } else {
    p <- p |>  
      mutate(prop = n/sum(n),
             n_miss = sum(is.na({{ x }})))
  }
  if (pcts==TRUE){
    p <- p |>   
      mutate(pct = prop*100) |> 
      select(-prop)
  } else {
    return(p)
  }
  return(p)
}

# this version might be good too. From "R For Data Science, 2nd edition"
# count_prop <- function(df, var, sort = FALSE) {
#   df |>
#     count({{ var }}, sort = sort) |>
#     mutate(prop = n / sum(n),
#            n_miss = sum(is.na({{ var }})))
# }


# fun: counts the number of missing observations in rows :::::::::::::::::####

count_missing <- function(df, group_vars, x_var) {
  df |> 
    group_by(pick({{ group_vars }})) |> 
    summarize(
      n_miss = sum(is.na({{ x_var }})),
      .groups = "drop"
  )
}

# fun: count_wide :::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# count using all the variables in the rows and columns, then use pivot_wider()
# to rearrange the counts into a grid

count_wide <- function(data, rows, cols) {
  data |> 
    count(pick(c({{ rows }}, {{ cols }}))) |> 
    pivot_wider(
      names_from = {{ cols }}, 
      values_from = n,
      names_sort = TRUE,
      values_fill = 0
    )
}











# fun: time stamps ::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# what a pain in the ass it is to just get a simple time stamp without the date
# the base R way does produce weekday, month, day, hours:min:second, year
# But I often just want the date or the time in hours:minutes
# timestamp()

# base r date() is basically same thing as timestamp. I don't want.
# date()

# Sys.time() gives me the date and time but adds in the time zone. Omitting timezone (tz) is somehow too difficult for me.
# Sys.time()

# there are a bunch of way to format the same Sys.time output, but I have to
# re-learn them all every time or find a reference guide...ugh.
# format(Sys.time(), "%b %e %Y") 
# format(Sys.time(), "%Y-%m-%d") # year-month-day
# format(Sys.time(), "%Y-%m-%d %H:%M") # year-month-day hour:min 24 hour

# year-month-day hour:min:sec 24 hour %T is equivalent to %H:%M:%S
# format(Sys.time(), "%Y-%m-%d %T") 
# format(Sys.time(), "%Y-%m-%d %I:%M") # year-month-day hour:min 12 hour
# format(Sys.time(), "%Y-%m-%d %I:%M%p") # year-month-day hour:min 12 hour AM/PM

# format(Sys.time(), "%H:%M")# just hours:min 24hr, character class
# format(Sys.time(), "%H:%M:%S") # hours:min:sec 24hr
# format(Sys.time(), "%T") # equivalent
# format(Sys.time(), "%I:%M %p") # hours:min AM/PM 12hr
# format(Sys.time(), "%I:%M:%S %p") # hours:min:sec AM/PM 12hr

# this is the other way that formats it as "hms" "difftime"
# but its from tidyverse
# hms::as_hms(format(Sys.time(), "%T")) # just hours:min 24hr
# hms::as_hms(format(Sys.time(), "%H:%M")) # but hms won't work without seconds

# `clockin()`
clockin <- function(date = TRUE, time = FALSE, seconds = FALSE){
  if (date==TRUE && time==TRUE) {
    d <- format(Sys.time(), "%Y-%m-%d %H:%M")
  } else if (date==TRUE && time==TRUE && seconds==TRUE){
    d <- format(Sys.time(), "%Y-%m-%d %T")
  } else if (date==FALSE && time==TRUE && seconds == FALSE) {
    d <- format(Sys.time(), "%H:%M")
  } else if (date == FALSE && time==TRUE && seconds == TRUE) {
    d <- format(Sys.time(), "%T")
  } else if (date == FALSE && time==FALSE && seconds == TRUE) {
    stop("You want me to print only the current seconds? No. Try again.", call.=FALSE)
  } else if (date == FALSE && time==FALSE && seconds == FALSE) {
    rlang::abort(
      "Error in clockin()",
      message = "Dude, what do you want? One of the function args must be true")
  } else {
    d <- format(Sys.time(), "%Y-%m-%d") |>  as.Date()
  }
  return(d)
}













