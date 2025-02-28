# there's a lot of code here. It deserves its own script.

set.seed(1234)
library(tidyverse)
library(patchwork)
# library(pewmethods)


# prep :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####


# shorten and 'unify' factor levels
# Note: this removes variable labels
df <- df |>
  # use `pewmethods` to preserve order of factor levels
  mutate(across(c(q19, q20, q21, q22, q23, q24,
                  q30, q31, q32, q33, q34, q35),
                ~pewmethods::fct_case_when(
    .x %in% c("Not at all confident",
              "Not at all committed",
              "Not at all concerned") ~ "Not at all confident",
    .x %in% c("Not too confident",
              "Not too committed",
              "Not too concerned",
              "Somewhat unconcerned") ~ "Not too confident",
    .x %in% c("Somewhat confident",
              "Somewhat committed",
              "Somewhat concerned") ~ "Somewhat confident",
    .x %in% c("Very confident",
              "Very committed",
              "Very concerned") ~ "Very confident",
    TRUE ~ NA
  ))) |> 
  # unify factor levels
  mutate(across(c(q25, q36), ~ pewmethods::fct_case_when(
    .x == "Very concerned" ~ "Very concerned",
    .x == "Somewhat concerned" ~ "Somewhat concerned",
    .x %in% c("Not too concerned", "Somewhat unconcerned") ~ "Not too concerned",
    .x == "Not at all concerned" ~ "Not at all concerned",
    TRUE ~ NA))) |>
  # reverse factor levels
  mutate(across(c(q25, q36), ~forcats::fct_rev(.)))

# Return labels to df
df <- df |> 
  labelled::set_variable_labels(!!!df_labels)


# set ggplot theme that will be used for every plot
# theme_get()
old <- theme_set(theme_bw()) # capture current theme

# set/update new theme
theme_set(
  new = ggplot2::theme_bw(base_family = 'Palatino Linotype')+
    theme(legend.position = 'bottom', axis.text.y.left = element_blank()))

# assign items to particular vectors to avoid copy/pasting
trust.az.items <- df |> select(q19, q20, q21, q22, q24) |> colnames() |> dput()
trust.lcl.items <- df |> select(q30, q31, q32, q33, q35)|> colnames() |> dput()
distrust.az.items <- df |> select(q28_1:q28_5)|> colnames() |> dput()
distrust.lcl.items <- df |> select(q40_1:q40_5)|> colnames() |> dput()

# variable labels for az and local area items, respectively
trust.az.varlabels <- c(
  q19 = "Votes counted as intended",
  q20 = "Election officials will do good job",
  q21 = "Election workers will be committed",
  q22 = "Voting process will be fair",
  q24 = "Election technology will be secure"
)

distrust.az.varlabels <- c(
  q28_1 = "Voter fraud will occur",
  q28_2 = "Many votes won't be counted",
  q28_3 = "Many voters will be turned away",
  q28_4 = "Foreign Interference",
  q28_5 = "Election officials will discourage voting"
)

trust.lcl.varlabels <- c(
  q30 = "Votes counted as intended",
  q31 = "Election officials will do good job",
  q32 = "Election workers will be committed",
  q33 = "Voting process will be fair",
  q35 = "Election technology will be secure"
)

distrust.lcl.varlabels <- c(
  q40_1 = "Voter fraud will occur",
  q40_2 = "Many Votes won't be counted",
  q40_3 = "Many voters will be turned away",
  q40_4 = "Foreign Interference",
  q40_5 = "Election officials will discourage voting"
)

# fun: Likert Plots:::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# To avoid excessive copy/paste, I made a function to create Likert plots
# it is mostly a wrapper around `ggstats::gglikert`, but with a specific set up
# particular colors, theme, etc. 
likert_plot <- function(data, x, ...,  
                        symmetric = FALSE,
                        variable_labels = NULL,
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
    theme_bw(base_family = "Palatino Linotype")+
    theme(
      legend.position = 'bottom',  # place legend on bottom
      axis.title.y.left = element_blank(), # blank out y-axis label
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


# Likert plot of trust in elections :::::::::::::::::::::::::::::::::::::::::####

# Likert plot of trust in AZ elections 
trust.likertplot.az <- df |> likert_plot(
  x = all_of(trust.az.items),
  variable_labels = trust.az.varlabels,
  title = "Trust in Maricopa County, AZ, Elections"
)


# Likert plot of trust in Local Area elections 
trust.likertplot.lcl <- likert_plot(
  data = df, 
  x = all_of(trust.lcl.items),
  variable_labels = trust.lcl.varlabels,
  title = "Trust in Local Area Elections")






# Likert plot distrust in elections ::::::::::::::::::::::::::::::::::::::::####

# responses to distrust items, overall sample
distrust.likert.az <- likert_plot(
  df, 
  x = all_of(distrust.az.items), 
  variable_labels = distrust.az.varlabels,
  title = "Distrust in Maricopa County, AZ Elections", 
  subtitle = str_wrap(
    "How likely do you think any or all of the following will happen during this year´s elections in Maricopa County, AZ?",width = 95))

distrust.likert.lcl <- likert_plot(
  df,
  x = all_of(distrust.lcl.items),
  variable_labels = distrust.lcl.varlabels,
  title = "Distrust in Local Area Elections",
  subtitle = str_wrap(
    "How likely do you think any or all of the following will happen during this year´s elections in your local area?",
    width = 95
  )
)




# Likert plot of safety concerns and confidence ::::::::::::::::::::::::::::####

safety.concerns.likert <- likert_plot(
  df, x = c(q25, q36), 
  y_label_wrap = 15, 
  variable_labels = c(
    q25 = 'Thinking about Maricopa County, AZ',
    q36 = "Thinking about Local Area"), 
  title = "Concern for Voter Intimidation, Threats, or Violence",
  subtitle = str_wrap(
    "How concerned should voters feel about potential violence, threats of violence, or intimidation while voting in person at their local polling place?", width = 99))

# q26 In-person voter safety at polling sites

safety.conf.likert <- likert_plot(
  df, x = c(q26, q37), y_label_wrap = 15,
    variable_labels = c(
      q26 = "In Maricopa County, AZ",
      q37 = "In your local area"
    ), 
  title = str_wrap("Confidence for In-person voter safety at Polling Sites", width = 95),
  subtitle = str_wrap("How confident, if at all, are you that in person polling places will be safe places for voters to cast their ballots during the upcoming elections in November?", width = 95)
)




# Likert plots of trust in AZ elections by educational attainment ::::::::::####

# likert plot of responses to trust.az items by education
trust.az.educ.likert <- df |>   
  filter(!is.na(educ_4cat)) |> 
  mutate(
    educ_3cat = pewmethods::fct_case_when(
      educ_4cat %in% c("H.S. or less") ~ "H.S. or less",
      educ_4cat %in% c("Some college no degree") ~ "Some college no Degree",
      educ_4cat %in% c("Postgraduate degree", "College degree") ~ "College Graduate")
    ) |>  
  likert_plot(x = trust.az.items, y = 'educ_3cat', facet_rows = vars(.question),
              facet_label_wrap = 15,
              variable_labels = trust.az.varlabels,
              subtitle = "Maricopa County, AZ")
  




# Likert plot of distrust in AZ elections by educational attainment ::::::::####

# Likert plot of distrust AZ items by educational attainment
distrust.az.educ.likert <- df |> 
  filter(!is.na(educ_4cat)) |> 
  # reverse levels of factor vars to show positive difference in "not likely"
  # mutate(across(c(q28_1:q28_5), ~forcats::fct_rev(.))) |>
  mutate(
    educ_3cat = pewmethods::fct_case_when(
      educ_4cat %in% c("H.S. or less") ~ "H.S. or less",
      educ_4cat %in% c("Some college no degree") ~ "Some college no Degree",
      educ_4cat %in% c("Postgraduate degree", "College degree") ~ "College Graduate")
    ) |>  
  likert_plot(x = distrust.az.items, y = 'educ_3cat', facet_rows=vars(.question),
              facet_label_wrap = 15, variable_labels = distrust.az.varlabels,
              subtitle = "Maricopa County, AZ")

# Likert plots of trust in Local area elections by educational attainment ::####

# likert plot of responses to trust.lcl items by education
trust.lcl.educ.likert <- df |>  
  filter(!is.na(educ_4cat)) |>  
  mutate(
    educ_3cat = pewmethods::fct_case_when(
      educ_4cat %in% c("H.S. or less") ~ "H.S. or less",
      educ_4cat %in% c("Some college no degree") ~ "Some college no Degree",
      educ_4cat %in% c("Postgraduate degree", "College degree") ~ "College Graduate")
    ) |>  
  likert_plot(
    x = all_of(trust.lcl.items), y = 'educ_3cat', facet_rows = vars(.question),
    facet_label_wrap = 15, variable_labels = trust.lcl.varlabels,
    subtitle = "Local Area"
  )



# Likert plot of distrust local area elections by educational attainment :::####

# Likert plot of distrust items by educational attainment
distrust.lcl.educ.likert <- df |> 
  filter(!is.na(educ_4cat)) |> 
  # reverse levels of factor vars to show positive difference in "not likely"
  # mutate(across(c(q40_1:q40_5), ~forcats::fct_rev(.))) |>
  mutate(
    educ_3cat = pewmethods::fct_case_when(
      educ_4cat %in% c("H.S. or less") ~ "H.S. or less",
      educ_4cat %in% c("Some college no degree") ~ "Some college no Degree",
      educ_4cat %in% c("Postgraduate degree", "College degree") ~ "College Graduate")
    ) |>   
  likert_plot(
    x = all_of(distrust.lcl.items), y = 'educ_3cat', facet_rows = vars(.question),
    facet_label_wrap = 15, variable_labels = distrust.lcl.varlabels,
    subtitle = "Local Area"
  )




# Likert plots of trust and distrust in AZ elections by gender :::::::::::::####

# likert plot of responses to trust.az items by education
trust.az.gender.likert <- df |>  
  filter(!is.na(gender_3cat), gender_3cat != 'Other/Refused') |> 
  likert_plot(
    x = all_of(trust.az.items), y = 'gender_3cat', facet_rows = vars(.question),
    facet_label_wrap = 15, variable_labels = trust.az.varlabels,
    subtitle = "Maricopa County, AZ Elections"
  )

# Likert plot of distrust AZ items by educational attainment
distrust.az.gender.likert <- df |> 
  filter(!is.na(gender_3cat), gender_3cat != 'Other/Refused') |> 
  # reverse levels of factor vars to show positive difference in "not likely"
  # mutate(across(c(q28_1:q28_5), ~forcats::fct_rev(.))) |>
  likert_plot(
    x = all_of(distrust.az.items), y = 'gender_3cat', facet_rows = vars(.question),
    facet_label_wrap = 15, variable_labels = distrust.az.varlabels,
    subtitle = "Maricopa County, AZ Elections"
  )

# Likert plots of trust and distrust in Local area elections by gender :::::####

# Likert plot of trust in local area elections by gender
trust.lcl.gender.likert <- df |>  
  filter(!is.na(gender_3cat), gender_3cat != 'Other/Refused') |> 
  likert_plot(
    x = all_of(trust.lcl.items), y = 'gender_3cat',
    facet_rows = vars(.question),
    facet_label_wrap = 15, variable_labels = trust.lcl.varlabels,
    subtitle = "Local Area Elections"
  )


# Likert plot of distrust in local area elections by gender
distrust.lcl.gender.likert <- df |> 
  filter(!is.na(gender_3cat), gender_3cat != 'Other/Refused') |> 
  # reverse levels of factor vars to show positive difference in "not likely"
  # mutate(across(c(q28_1:q28_5), ~forcats::fct_rev(.))) |> 
  likert_plot(
    x = all_of(distrust.lcl.items), y = 'gender_3cat',
    facet_rows = vars(.question),
    facet_label_wrap = 15, variable_labels = distrust.lcl.varlabels,
    subtitle = "Local Area Elections"
  )





# Likert plots of trust and distrust in AZ elections by partisanship :::::::####

# Likert plot of responses to trust.az items by party ID
trust.az.party.likert <- df |>  
  filter(!is.na(partyid_3cat)) |> 
  likert_plot(
    x = all_of(trust.az.items), y = 'partyid_3cat',
    facet_rows = vars(.question), 
    facet_label_wrap = 15,
    variable_labels = trust.az.varlabels,
    subtitle = "Maricopa County, AZ Elections"
  )

# Likert plot of distrust AZ items by party ID
distrust.az.party.likert <- df |> 
  filter(!is.na(partyid_3cat)) |> 
  # reverse levels of factor vars to show positive difference in "not likely"
  # mutate(across(c(q28_1:q28_5), ~forcats::fct_rev(.))) |> 
  likert_plot(
    x = all_of(distrust.az.items), y = 'partyid_3cat',
    facet_rows = vars(.question), 
    facet_label_wrap = 15,
    variable_labels = distrust.az.varlabels,
    subtitle = "Maricopa County, AZ Elections"
  )


# Likert plot of trust and distrust in local area elections by partisanship ####


# Likert plot of responses to trust.lcl items by partisanship
trust.lcl.party.likert <- df |>  
  filter(!is.na(partyid_3cat)) |> 
  likert_plot(
    x = all_of(trust.lcl.items), y = 'partyid_3cat',
    facet_rows = vars(.question), 
    facet_label_wrap = 15,
    variable_labels = trust.lcl.varlabels,
    subtitle = "Local Area Elections"
  )

# Likert plot of distrust AZ items by party ID
distrust.lcl.party.likert <- df |> 
  filter(!is.na(partyid_3cat)) |> 
  # reverse levels of factor vars to show positive difference in "not likely"
  # mutate(across(c(q28_1:q28_5), ~forcats::fct_rev(.))) |> 
  likert_plot(
    x = all_of(distrust.lcl.items), y = 'partyid_3cat',
    facet_rows = vars(.question), 
    facet_label_wrap = 15,
    variable_labels = distrust.lcl.varlabels,
    subtitle = "Local Area Elections"
  )




# Likert plot of trust, AZ and local, by treatment :::::::::::::::::::::::::####

# Likert plot of trust and confidence in election admin comparing treatment and
# control groups
trust.likert.treat.az <- df |>
  likert_plot(x = all_of(trust.az.items), y = 'group', 
    facet_rows = vars(.question),
    labels_size = 2.5, 
    subtitle = "Maricopa County, Arizona")+
  theme(strip.text.y.right = element_blank())

# confidence in elections between experiment conditions for items concerning local area
trust.likert.treat.lcl <- df |>
  likert_plot(x = all_of(trust.lcl.items), y = 'group', 
    facet_rows = vars(.question),
    facet_label_wrap = 10,
    variable_labels = trust.lcl.varlabels,
    labels_size = 2.5, 
    subtitle = "Local Area")+
  theme(strip.text.y.right = element_blank()) +
  theme(
    axis.text.y.left = element_blank(), # remove y-axis text on 2nd plot
    strip.text.y.right = element_text(angle = 0, size = 8)
  )

# Likert distrust, AZ and local, by treatment condition ::::::::::::::::::::####

# distrust in AZ elections by experimental condition
distrust.az.treat.likert <- df |>
  likert_plot(x = all_of(distrust.az.items), y = 'group', 
    facet_rows = vars(.question),
    labels_size = 2.5, 
    subtitle = "Distrust in Maricopa County, AZ Elections  by Treatment")+
  theme(strip.text.y.right = element_blank())

# distrust in local area elections
distrust.lcl.treat.likert <- df |>
  likert_plot(x = all_of(distrust.lcl.items), y = 'group', 
    facet_rows = vars(.question),
    facet_label_wrap = 10,
    variable_labels = distrust.lcl.varlabels,
    labels_size = 2.5, 
    subtitle = "Distrust in Local Elections by Treatment")+
  theme(strip.text.y.right = element_blank()) +
  theme(
    axis.text.y.left = element_blank(), # remove y-axis text on 2nd plot
    strip.text.y.right = element_text(angle = 0, size = 8)
  )




# Likert plots of trust and distrust by partisanship and treatment :::::::::####

# Likert plot of trust in elections scale by experiment condition and partyid
trust.az.treat.partyid <- df |> 
  filter(!is.na(partyid_3cat)) |>
  likert_plot(
    x = all_of(trust.az.items),
    y = 'partyid_3cat',
    facet_rows = vars(.question),
    facet_cols = vars(group),
    facet_label_wrap = 15,
    variable_labels = trust.az.varlabels,
    title = "Trust in Maricopa County, AZ Elections by Treatment and Partisanship")

trust.lcl.treat.partyid <- df |> 
  filter(!is.na(partyid_3cat)) |>
  likert_plot(
    x = all_of(trust.lcl.items),
    y = 'partyid_3cat',
    facet_rows = vars(.question),
    facet_cols = vars(group),
    facet_label_wrap = 15,
    variable_labels = trust.lcl.varlabels,
    title = "Trust in Local Area Elections by Treatment and Partisanship")



# Likert plots of trust and distrust by legitimacy and treatment :::::::::::####

# trust in elections by experiment conditions among those who believe the 2020
# election results were illegitimate, AZ items
trust.az.treat.legit <- df |> 
  filter(!is.na(partyid_3cat), !is.na(q7)) |>
  filter(q7 == "Not legitimate") |> 
  likert_plot(
    x = all_of(trust.az.items), 
    y = 'group',
    facet_rows = vars(.question),
    facet_cols = vars(q7),
    facet_label_wrap = 15,
    variable_labels = trust.az.varlabels,
    subtitle = str_wrap("Maricopa County, AZ",
      width = 95) 
  )+
  theme(strip.text.y.right = element_blank())

# trust in elections by experiment conditions among those who believe the 2020
# election results were illegitimate, Local area items
trust.lcl.treat.legit <- df |> 
  filter(!is.na(partyid_3cat), !is.na(q7)) |>
  filter(q7 == "Not legitimate") |> 
  likert_plot(
    x = all_of(trust.lcl.items), 
    y = 'group',
    facet_rows = vars(.question),
    facet_cols = vars(q7),
    facet_label_wrap = 15,
    variable_labels = trust.lcl.varlabels,
    subtitle = str_wrap("Local Area",
      width = 95) 
  )+
  theme(axis.text.y.left = element_blank())


# Likert plot for safety concerns by treatment :::::::::::::::::::::::::::::####

# Because q25 measures levels of concern/worry, it cannot be included in the
# same Likert plot as q26 as that measures reported confidence that voters will
# be safe to vote in-person

safety.concern.treat.likert <- df |>
  mutate(across(
    c(q25, q36),
    ~ pewmethods::fct_case_when(
      .x == "Very concerned" ~ "Very concerned",
      .x == "Somewhat concerned" ~ "Somewhat concerned",
      .x %in% c("Not too concerned", "Somewhat unconcerned") ~ "Not too concerned",
      .x == "Not at all concerned" ~ "Not at all concerned",
      TRUE ~ NA
    )
  )) |>
  likert_plot(
    x = c(q25, q36),
    y = 'group',
    facet_rows = vars(.question),
    facet_label_wrap = 15,
    variable_labels = c(q25 = 'Thinking about Maricopa County, AZ', q36 = "Thinking about Local Area"),
    title = "Concern for Voter Intimidation, Threats, or Violence by Treatment Condition",
    subtitle = str_wrap(
      "How concerned should voters feel about potential violence, threats of violence, or intimidation while voting in person at their local polling place?",
      width = 99
    )
  )
  


# q26 In-person voter safety at polling sites

safety.conf.treat.likert <- df |>
  likert_plot(
    x = c(q26, q37), 
    y = 'group',
    facet_label_wrap = 15,
    facet_rows = vars(.question),
    variable_labels = c(
      q26 = "In Maricopa County, AZ",
      q37 = "In your local area"
    ),
    title = str_wrap("Confidence for In-person voter safety at Polling Sites by Treatment", width = 95),
    subtitle = str_wrap("How confident, if at all, are you that in person polling places will be safe places for voters to cast their ballots during the upcoming elections in November?", width = 95)
  )


# Patchwork plots all together :::::::::::::::::::::::::::::::::::::::::::::####

# Instead of two separate plots, arrange into one plot while remaining separated
p1.trust <- trust.likertplot.az / trust.likertplot.lcl+
  patchwork::plot_annotation(caption = "Missing values omitted")+
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p2.distrust <- distrust.likert.az/distrust.likert.lcl+
  patchwork::plot_annotation(caption = "Missing values omitted")+
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p3.safety <- safety.concerns.likert/safety.conf.likert+
  patchwork::plot_annotation(caption = "Missing values omitted")+
  patchwork::plot_layout(guides = "keep") &
  theme(legend.position = "bottom")

p4.trust.az.educ <- trust.az.educ.likert + trust.lcl.educ.likert + 
    patchwork::plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

p5.distrust.az.educ <- distrust.az.educ.likert + distrust.lcl.educ.likert +
    patchwork::plot_layout(guides = "collect") & 
    theme(legend.position = "bottom")

p6.trust.az.gender <- trust.az.gender.likert/distrust.az.gender.likert+
  patchwork::plot_annotation(
    caption = str_wrap("Gender Non-binary (n=7) and those who preferred not to say (n=9) response categories omitted for lack of observations. Missing values omitted (n=15)",
                       width = 150), 
    theme = theme(text = element_text(family = "Palatino Linotype")))+
    patchwork::plot_layout(guides = 'keep') & 
    theme(legend.position = "bottom")

p7.trust.lcl.gender <- trust.lcl.gender.likert/distrust.lcl.gender.likert+
  patchwork::plot_annotation(
    caption = str_wrap("Gender Non-binary (n=7) and those who preferred not to say (n=9) response categories omitted for lack of observations. Missing values omitted (n=15)",
                       width = 150), 
    theme = theme(text = element_text(family = "Palatino Linotype")))+
    patchwork::plot_layout(guides = 'keep') & 
    theme(legend.position = "bottom")

p8.trust.az.party <- trust.az.party.likert/distrust.az.party.likert+
    patchwork::plot_layout(guides = 'keep') & 
    theme(legend.position = "bottom")

p9.trust.lcl.party <- trust.lcl.party.likert/distrust.lcl.party.likert+
    patchwork::plot_layout(guides = 'keep') & 
    theme(legend.position = "bottom")

p10.trust.treat <- trust.likert.treat.az + trust.likert.treat.lcl+
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p11.distrust.treat <- distrust.az.treat.likert + distrust.lcl.treat.likert+
  patchwork::plot_layout(guides = 'collect')

p12.trust.treat.partyid <- trust.az.treat.partyid / trust.lcl.treat.partyid+
  patchwork::plot_layout(guides = 'collect')

p13.trust.treat.legit <- trust.az.treat.legit + trust.lcl.treat.legit+
  patchwork::plot_layout(guides = 'collect')+
  patchwork::plot_annotation(
    title = str_wrap("Trust in elections by treatment among those who believe the 2020 election results were not legitimate", width = 95))

p14.safety.treat <- safety.concern.treat.likert/safety.conf.treat.likert+
  patchwork::plot_layout(guides = "keep")

# Save :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# save likert plot objects as a list (a list of lists, really)
likert_plots_list <- list(
  p1.trust                = p1.trust, 
  p2.distrust             = p2.distrust, 
  p3.safety               = p3.safety, 
  p4.trust.az.educ        = p4.trust.az.educ,
  p5.distrust.az.educ     = p5.distrust.az.educ,
  p6.trust.az.gender      = p6.trust.az.gender,
  p7.trust.lcl.gender     = p7.trust.lcl.gender,
  p8.trust.az.party       = p8.trust.az.party,
  p9.trust.lcl.party      = p9.trust.lcl.party, 
  p10.trust.treat         = p10.trust.treat,
  p11.distrust.treat      = p11.distrust.treat,
  p12.trust.treat.partyid = p12.trust.treat.partyid,
  p13.trust.treat.legit   = p13.trust.treat.legit,
  p14.safety.treat        = p14.safety.treat
)

# save likert plot objects as .rds file.
saveRDS(likert_plots_list, file = "data/likert_plots.rds")


# clean up :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

rm(
  trust.likertplot.az,
  trust.likertplot.lcl,
  distrust.likert.az,
  distrust.likert.lcl,
  safety.concerns.likert,
  safety.conf.likert,
  trust.az.educ.likert,
  trust.lcl.educ.likert,
  distrust.az.educ.likert,
  distrust.lcl.educ.likert,
  trust.az.gender.likert,
  distrust.az.gender.likert,
  trust.lcl.gender.likert,
  distrust.lcl.gender.likert,
  trust.az.party.likert,
  distrust.az.party.likert,
  trust.lcl.party.likert,
  distrust.lcl.party.likert,
  trust.likert.treat.az,
  trust.likert.treat.lcl,
  distrust.az.treat.likert,
  distrust.lcl.treat.likert,
  trust.az.treat.partyid,
  trust.lcl.treat.partyid,
  trust.az.treat.legit,
  trust.lcl.treat.legit,
  safety.concern.treat.likert,
  safety.conf.treat.likert,
  distrust.az.items,
  distrust.az.varlabels,
  distrust.lcl.items,
  distrust.lcl.varlabels,
  trust.az.items,
  trust.az.varlabels,
  trust.lcl.items,
  trust.lcl.varlabels,
  p1.trust,
  p2.distrust,
  p3.safety,
  p4.trust.az.educ,
  p5.distrust.az.educ,
  p6.trust.az.gender,
  p7.trust.lcl.gender,
  p8.trust.az.party,
  p9.trust.lcl.party,
  p10.trust.treat,
  p11.distrust.treat,
  p12.trust.treat.partyid,
  p13.trust.treat.legit,
  p14.safety.treat
)
