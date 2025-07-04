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


# fun: frequency tibble ::::::::::::::::::::::::::::::::::::::::::::::::::::####

freq_tibble <- function(data, var1, var2) {
  var1 <- rlang::enquo(var1)
  var2 <- rlang::enquo(var2)
  
  data %>%
    dplyr::count(!!var1, !!var2) %>%
    tidyr::spread(!!var2, n, fill = 0) %>%
    dplyr::mutate(Total := rowSums(dplyr::select(., -!!var1))) %>%
    dplyr::bind_rows(dplyr::bind_cols(
      !!rlang::quo_name(var1) := "Total",
      dplyr::summarize_if(., is.numeric, sum)
    ))
}

# example
# freq_tibble(df, var1 = Gender, var2 = Condition)

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
    title = title,
    subtitle = subtitle,
    caption = stringr::str_wrap(caption, width = 99),
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
#     mutate(prop = n / sum(n))
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













# fun: var_label_tab :::::::::::::::::::::::::::::::::::::::::::::::::::::::####

var_label_tab <- function(x){
    purrr::map(x, ~attr(., "label")) |>
    purrr::map(~ifelse(purrr::is_null(.), "No label", .)) |> 
    tibble::enframe(name = "var", value = "var_label") |>
    tidyr::unnest(cols = c(var_label))
}

# fun: logistic regression model goodness-of-fit statistics ::::::::::::::::####

# creating my own for now
# logistic regression model goodness-of-fit statistics
lrm.gof.stats <- function(x, ...){
  
  out <- data.frame(
    null.deviance = x$null.deviance,
    df.null = x$df.null,
    logLik = as.numeric(stats::logLik(x)),
    AIC = stats::AIC(x),
    BIC = stats::BIC(x),
    deviance = stats::deviance(x),
    df.residual = stats::df.residual(x),
    nobs = stats::nobs(x),
    chisq = as.numeric(x$null.deviance - stats::deviance(x)),
    df = as.numeric(x$df.null - stats::df.residual(x)),
    'P(>chi)' = pchisq(q=x$null.deviance - stats::deviance(x),
                       df = x$df.null - stats::df.residual(x), 
                       lower.tail = F)
  )
  out <- dplyr::as_tibble(out)
  return(out)
}

# lrm.gof.stats(m1)

# logistic regression model goodness-of-fit statistics
lrm.gof.stats <- function(model, ...){
  
  out <- data.frame(
    nobs = stats::nobs(model),
    null.deviance = model$null.deviance,
    df.null = model$df.null,
    logLik = as.numeric(stats::logLik(model)),
    deviance = stats::deviance(model),
    df.residual = stats::df.residual(model),
    chisq = as.numeric(model$null.deviance - stats::deviance(model)),
    df = as.numeric(model$df.null - stats::df.residual(model)),
    'P(>chi)' = pchisq(q=model$null.deviance - stats::deviance(model),
                       df = model$df.null - stats::df.residual(model), 
                       lower.tail = F)
  )
  out <- dplyr::as_tibble(out) |>  
  # rename column variables
  dplyr::rename(
    "Num.Obs."      = "nobs",
    "Log.Lik"       = "logLik",
    "Deviance"      = "deviance",
    "Deviance Null" = "null.deviance",
    "DF"            = "df",
    "chisq"          = "chisq",
    "P(>chisq)"     = "P..chi."
  )
  return(out)
}
# tinytable theme: theme_mitex() :::::::::::::::::::::::::::::::::::::::::::####

# see here: https://vincentarelbundock.github.io/tinytable/vignettes/theme.html#user-written-themes

theme_mitex <- function(x, ...) {
    fn <- function(table) {
        if (isTRUE(table@output == "typst")) {
          table@table_string <- gsub(
            "\\$(.*?)\\$",
            "#mitex(`\\1`)",
            table@table_string)
        }
        return(table)
    }
    x <- style_tt(x, finalize = fn)
    return(x)
}
