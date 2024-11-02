# using data exported from Qualtrics on 2024-10-23
# The most recent (i.e., last) response was recorded 2024-09-18 at 6:40pm (1840)
# When exporting the data, Qualtrics allows me to recode 'seen but unanswered'
# questions with code 99. I downloaded the data again just for that because I
# want to see how many of the missing values in my 2024-09-11 dataset are
# attributed to people who skipped survey questions.

# load packages ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
library(tidyverse)
library(haven)
library(surveytoolbox)
library(labelled)
library(sjlabelled)

# load data ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# load raw SPSS export (includes display order vars)
# downloaded on 2024-10-29 at 12:02 PM
raw_spss <- haven::read_sav("data-raw/VMF_Election_Worker_Recruitment_Survey_with_display_order_20241029_T12.02.sav")

# load raw SPSS export (doesn't include display order vars)
# downloaded 2024-10-23 at 1:54 PM
# raw_spss3 <- haven::read_sav(file = "data-raw/VMF_Election_Worker_Recruitment_Survey_2024-10-23_13.54.sav")

# despite being a .sav file imported using `haven::read_sav()`, 
# the data set is not "haven_labelled"
# haven::is.labelled(raw_spss)

# create data dictionary of raw SPSS data set ::::::::::::::::::::::::::::::####

# A data dictionary contains metadata about the data. The
# `labelled::generate_dictionary` function is used to create a data
# dictionary. (alt. `surveytoolbox::data_dict`)
# a data frame is loaded into the R environment with the number of
# observations equal to number of variables in the original data set.
# NOTE: surveytoolbox::data_dict function is used here because the resulting
# dataframe can be saved as a .csv
raw_spss_dict <- raw_spss |> surveytoolbox::data_dict()

# save data dictionary of raw_spss dataset download
write.csv(raw_spss_dict, file = "data/raw_spss_dict.csv")

# create and save survey codebook ::::::::::::::::::::::::::::::::::::::::::####

# this includes only the survey questions: question number (var), the question
# text (label_var), the value labels (label_val), and the numeric codes (value)
codebook <- raw_spss |> 
  janitor::clean_names() |> 
  # get rid of superfluous Qualtrics columns
  # Keep Qualtrics `response_id`
  select(!1:8 & !10:17 & !265) |>
  # subset spss data set to omit the display order variables
  select(!contains("_do_")
         & !contains("_ado_")
         & !contains("_click")
         & !contains("_count")) |>
  select(contains("q"), group) |>
  surveytoolbox::data_dict()

# save codebook as .csv file in data/ directory
write.csv(codebook, file = "data/survey_codebook.csv")
# clean up data set ::::::::::::::::::::::::::::::::::::::::::::::::::::::::####
data <- raw_spss |> 
  janitor::clean_names() |> 
  # rename demographic vars and timing vars and identify lucid vars
  dplyr::rename(
    q13_treat_time = q13_page_submit,
    q16_control_time = q16_page_submit,
    lucid_gender = gender,
    lucid_age = age,
    lucid_hhi = hhi,
    lucid_ethnicity = ethnicity,
    lucid_hispanic = hispanic,
    lucid_education = education,
    lucid_political_party = political_party,
    lucid_region = region,
    lucid_zip = zip,
    lucid_rid = rid,
    gender = q59,
    hisp = q60,
    race = q61,
    educ = q62,
    milserv1 = q63,
    milserv2 = q64,
    milservfam = q65,
    voted2020 = q66,
    choice2020 = q67,
    voteintent = q68,
    partyid = q69,
    partystr_rep = q70,
    partystr_dem = q71,
    partylean = q72,
    ideo = q73,
    ideolean = q74
  ) |>
  # exclude all non-consent 
  # exclude non-citizens
  # exclude those who failed 1st and 2nd attn check
  filter(q1 == 1,
         q3 != 3,
         q9 == 1 | q9 == 0 & q10 == 1) |>
  # add a column of ascending sequential row ids starting at 1 at start of df
  tibble::rowid_to_column("rowID")
  
# fix the age `q2` variable so that it reflects actual age in years ::::::::####
data <- data |> 
  mutate(
    q2 = case_match(
      q2,
      1 ~ 18,
      2 ~ 19,
      3 ~ 20,
      4 ~ 21,
      5 ~ 22,
      6 ~ 23,
      7 ~ 24,
      8 ~ 25,
      9 ~ 26,
      10 ~ 27,
      11 ~ 28,
      12 ~ 29,
      13 ~ 30,
      14 ~ 31,
      15 ~ 32,
      16 ~ 33,
      17 ~ 34,
      18 ~ 35,
      19 ~ 36,
      20 ~ 37,
      21 ~ 38,
      22 ~ 39,
      23 ~ 40,
      24 ~ 41,
      25 ~ 42,
      26 ~ 43,
      27 ~ 44,
      28 ~ 45,
      29 ~ 46,
      30 ~ 47,
      31 ~ 48,
      32 ~ 49,
      33 ~ 50,
      34 ~ 51,
      35 ~ 52,
      36 ~ 53,
      37 ~ 54,
      38 ~ 55,
      39 ~ 56,
      40 ~ 57,
      41 ~ 58,
      42 ~ 59,
      43 ~ 60,
      44 ~ 61,
      45 ~ 62,
      46 ~ 63,
      47 ~ 64,
      48 ~ 65,
      49 ~ 66,
      50 ~ 67,
      51 ~ 68,
      52 ~ 69,
      53 ~ 70,
      54 ~ 71,
      55 ~ 72,
      56 ~ 73,
      57 ~ 74,
      58 ~ 75,
      59 ~ 76,
      60 ~ 77,
      61 ~ 78,
      62 ~ 79,
      63 ~ 80,
      64 ~ 81,
      65 ~ 82,
      66 ~ 83,
      67 ~ 84,
      68 ~ 85,
      69 ~ 86,
      70 ~ 87,
      71 ~ 88,
      72 ~ 89,
      73 ~ 92 # one person indicated 90 or older. R's age in Lucid data is 92
    ),
    .after = q1,
    .keep = "unused"
  ) |> 
  labelled::set_variable_labels(
    q2 = "What is your age?"
  )

# create var to identify display order of popular efficacy questions :::::::####

# Question items Q51 to Q58 were split up into two question sets, Q51:Q54 and
# Q55:Q58. The order of these question sets were randomly reversed. For order 1,
# questions Q51:Q54 ('agent-aim') were presented first, followed by Q55:Q58
# ('agent-action-aim'). For order 2, the presentation was reversed. Two
# variables (columns) in the data set identify the question display order. Here
# I use those two variables to construct a single dummy variable where order 1 =
# 1, and order 2 = 0

# create var to identify display order of popular efficacy questions
data <- data |>  
  mutate(popeff_qdo = case_when(
    fl_22_do_popular_efficacy_agent_aim == 1 
    & fl_22_do_popularefficacy_agent_action_aim == 2 ~ 1,
    
    fl_22_do_popular_efficacy_agent_aim == 2 
    & fl_22_do_popularefficacy_agent_action_aim == 1 ~ 0),
    .after = qset,
    .keep = "unused")

# remove unnecessary columns (variables) from data :::::::::::::::::::::::::####

# these are a few of the variable columns are included automatically when
# exporting from Qualtrics. I don't want to get rid of all of them, so the
# following excludes a select few. In addition, the display order ("do") for
# many of the survey questions were randomly reversed per respondent, as were
# the display order of response options ('ado') for others. These variable
# columns are also excluded
data <- data |> 
  select(
    !contains("recipient_") 
    & !contains("_do_")
    & !contains("_ado_")
    & !contains("_click")
    & !contains("_count"),
    -status,
    -ip_address,
    -user_language, 
    -distribution_channel,
    -external_reference, 
    -q_data_policy_violations)

# fix qset variable column :::::::::::::::::::::::::::::::::::::::::::::::::####

# the `qset` variable contains a blank value "" that is being counted. 
# I use the following code to render that blank "" into an explicit missing
# value NA
data <- data |>
  sjlabelled::set_labels(qset, labels=c("A" = "A", "B" = "B")) |> 
  sjlabelled::set_na(qset, na = "", as.tag = F)

# check 
str(data$qset)
table(data$qset, useNA = "always")

# the only value labels are "A" and "B"
sjlabelled::get_labels(data$qset, values = "n", drop.na = F, drop.unused = F)

# object retains character class
class(data$qset)

# get description of sample by qset and examine partial responses ::::::::::####

# table of sample by `qset`, another by number of 'finished', and another by
# treatment 'group'
data |> 
  datawizard::data_tabulate(c(qset, finished, group))

# five number summary of `progress` by those who didn't complete survey 
summary(data$progress[data$finished == 0])

# this is the 5-number summary of progress among those who didn't complete the
# survey
# Among the people who failed to complete the survey quit after responding to
# approximately 31% of the survey on average, which corresponds to question 23.

# number of missing values (NA) in the qset variable
sum(is.na(data$qset))

# get counts of finished by qset
data |> 
  select(qset, finished) |> 
  group_by(finished, qset) |> 
  count()
# this shows that 129 didn't finish, and 108 of those quit the survey before
# being assigned a `qset` identifier code (e.g., A or B).

# this shows that of the 108 who quit before being assigned a `qset` identifier,
# the furthest progress made through the survey was 52% (according to
# Qualtrics). However, I don't know the question 52% progress corresponds in the
# survey
data |> 
  select(qset, finished, progress) |> 
  filter(finished==0 & is.na(qset)) |>
  group_by(progress, qset) |> 
  count() |> 
  arrange(desc(progress)) |> head()

# From this, I can see that 52% progress through the survey corresponds to
# reaching Q39. A survey participant was not assigned a `qset` identifier code
# if they quit prior to answering q40_1
data |>  
  filter(finished==0 & progress > 50 & progress < 60) |>
  group_by(progress, qset) |> 
  select(q39:ideolean) |> 
  arrange(progress)

# Since responses to questions sets A and B are important to the study,
# any respondent who quit the survey prior to answering any of the questions of
# interest is of no value and can safely be dropped from the survey
# This reduces the sample size from n = 1,395 to n = 1,287
data <- data |> 
  filter(!is.na(qset))

# create data dictionary/codebook ::::::::::::::::::::::::::::::::::::::::::####

# create data dictionary using `labelled::generate_dictionary()`
data_dict <- data |> 
  labelled::set_variable_labels(
    popeff_qdo = "Popular efficacy question display order"
  ) |> 
  labelled::generate_dictionary()

#####
# convert all labelled variables to factor

data <- data |> 
  # make `group` and `qset` factors (because they are character class)
  mutate(group = forcats::fct(group, levels = c("Treatment", "Control")),
         qset = forcats::fct(qset, levels = c("A", "B"))) |> 
  sjlabelled::set_labels(group, labels = c("Control" = 0, "Treatment" = 1)) |>
  sjlabelled::set_labels(qset, labels = c("A" = 0, "B" = 1)) |> 
  # convert all labelled variables to factors
  haven::as_factor(only_labelled = T) |>
  # re-code any empty/blank levels to NA
  mutate(across(where(is.factor), ~ fct_recode(., NULL = ""))) |> 
  # reverse order of identified factor levels for consist direction
  mutate(across(c(q8, q20, q23, q24, q31, q34, q35, q49, q50), .fns = ~fct_rev(.))) |> 
  # reverse order of group levels so control comes first. Better table display
  mutate(group = forcats::fct_rev(group))


# add variable labels back into factorized data set ::::::::::::::::::::::::####

# variable labels are nice to have for plenty of reasons
# since SPSS data is labelled, usually, the variables are <dbl+lbl> instead of
# factors <fct>. It's easier to analyze vars in data as factors, but losing the
# variable labels (i.e., question text) is kind of a bummer. The following puts
# the variable labels form the SPSS data set back into the 'factor-ized' dataset

# To quickly assign the variable labels, first create a named vector via
# deframe() with values as the variable labels and names as the variable names.
data_labels <- data_dict |>
  select(variable, label) |> 
  deframe()

# Now assign the labels using the splice operator. Using the splice operator,
# labels are assigned via matching against the variable name, which means that
# variable order does not matter.

data <- data |> 
  labelled::set_variable_labels(!!!data_labels)

# construct variables ::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# construct the partyid var with 3 categories, true independents
# Note: I use pewmethods::fct_case_when() instead of dplyr::case_when() in order
# to preserve the order in which value labels are passed
data <- data |>
  mutate(
    partyid_3cat = pewmethods::fct_case_when(
      partyid == "Republican" ~ "Republican",
      partystr_rep == "Strong" ~ "Republican",
      partystr_rep == "Not very strong" ~ "Republican",
      partylean == "Republican" ~ "Republican",
      partyid == "Democrat" ~ "Democrat",
      partystr_dem == "Strong" ~ "Democrat",
      partystr_dem == "Not very strong" ~ "Democrat",
      partylean == "Democratic" ~ "Democrat",
      partyid == "Independent" ~ "Independent",
      partyid == "Other" ~ "Independent",
      partylean == "Neither" ~ "Independent"
    ),
    .before = partyid) |> 
  labelled::set_variable_labels(
    partyid_3cat = "Party ID 3 categories, with true Independents")


# create an age group variable
data <- data |>    
  mutate(age_cat = 
           forcats::fct_collapse(
             as_factor(q2),
             "18-24" = c(18:24),
             "25-34" = c(25:34),
             "35-44" = c(35:44),
             "45-54" = c(45:54),
             "55-64" = c(55:64),
             "65-74" = c(65:74),
             "75-84" = c(75:82, 84),
             "85-92" = c(85:86,88, 92)
             ),
         .after = gender) |> 
  labelled::set_variable_labels(
    age_cat = "Age categorized into eight groups"
  )

# create gender_3cat, collapsed version of voted2020, and set var label
data <- data |> 
  dplyr::mutate(gender_3cat = forcats::fct_collapse(
    gender,
    "Male" = "Male",
    "Female" = "Female",
    "Other/Refused" = c("Non-binary / third gender", "Prefer not to say")),
    voted2020.clps = forcats::fct_collapse(
      voted2020,
      "Voted" = "Yes, I'm sure I voted",
      "Didn't vote" = c("I'm sure I didn't vote", "I don't think I voted"),
      "Unsure/Ineligible" = c("I think I voted","I was not eligible to vote")
      )) |> 
  mutate(ideo_3cat = pewmethods::fct_case_when(
    ideo == "Extremely Liberal" | ideo == "Liberal" | ideo == "Slightly liberal" ~ "Liberal",
    ideo == "Extremely conservative" | ideo == "Conservative" | ideo == "Slightly conservative" ~ "Conservative",
    TRUE ~ ideo
  )) |> 
  labelled::set_variable_labels(
    voted2020.clps = "Turnout 2020",
    ideo_3cat = "Ideology, 4 categories"
  )


# create education var with fewer categories
data <- data |> 
  mutate(educ_4cat = forcats::fct_collapse(
    educ,
    "H.S. or less" = c("Less than high school degree",
                       "High school graduate (high school diploma or equivalent including GED)"),
    "Some college no degree" = "Some college but no degree",
    "Undergraduate level degree" = c("Associate degree in college (2-year)",
                                     "Bachelor's degree in college (4-year)"),
    "Graduate level degree" = c("Master's degree", "Doctoral degree", "Professional degree (JD, MD)")),
    .after = educ)

# Now do some heavy duty relocating of columns in the dataframe
data <- data |>  
  dplyr::relocate(
    gender_3cat, .after = gender
  ) |> 
  dplyr::relocate(
    voted2020.clps, .after = voted2020
  ) |> 
  dplyr::relocate(
    ideo_3cat, .before = ideo
  )

# create dummy variables where 1 = 'Increase in confidence' and 0 = "No increase
# in confidence".
# add dummy variables to dataframe
data <- data |>
  mutate(across(
    c(q41.1:q43.6),
    ~ dplyr::case_when(
      . == "Decrease confidence a lot" ~ "Decrease",
      . == "Decrease confidence somewhat" ~ "Decrease",
      . == "No impact on confidence" ~ "No impact",
      . == "Increase confidence somewhat" ~ "Increase",
      . == "Increase confidence a lot" ~ "Increase",
      .default = as.character(.)), .names = "{.col}.clps")) |> 
  mutate(across(c(q41.1.clps:q43.6.clps), 
                ~ dplyr::case_when(
                . == "Increase" ~ 1, 
                . == "No impact" ~ 0,
                . == 'Decrease' ~ 0),
                .names = "{.col}.dum")) |> 
  mutate(across(c(q41.1.clps, q41.2.clps, q41.3.clps, q41.4.clps,
                  q41.5.clps, q41.6.clps, q43.1.clps, q43.2.clps,
                  q43.3.clps, q43.4.clps, q43.5.clps, q43.6.clps),
                ~ haven::as_factor(.)))


# merge question sets ::::::::::::::::::::::::::::::::::::::::::::::::::::::####

# merge (or coalesce) the qset vars that can be merged. 
# The following merges vars from the q44 and q46 set into q41 and q43. `qset` A
# was presented to one half of the respondents, whereas `qset` B was presented
# to the other half. Because of this, the variable columns in the dataframe for
# each question in the series contains missing values representing this fact.
# Below I coalesce the `qset` B question items with `qset` A. The function finds
# the first non-missing value at each position, allowing me to generate a
# complete vector that contains the responses to the series of questions from
# either set. I am able to distinguish between the different question sets since
# they are still grouped by the `qset` variable.

data <- data |> 
  group_by(rowID) |> 
  mutate(q41.1 = dplyr::coalesce(q41_1, q44_1),
         q41.2 = dplyr::coalesce(q41_2, q44_4),
         q41.3 = dplyr::coalesce(q41_3, q44_5),
         q41.4 = dplyr::coalesce(q41_4, q44_2),
         q41.5 = dplyr::coalesce(q41_5, q44_3),
         q41.6 = dplyr::coalesce(q41_6, q44_6),
         
         q43.1 = dplyr::coalesce(q43_1, q46_1),
         q43.2 = dplyr::coalesce(q43_2, q46_4),
         q43.3 = dplyr::coalesce(q43_3, q46_6),
         q43.4 = dplyr::coalesce(q43_4, q46_2),
         q43.5 = dplyr::coalesce(q43_5, q46_3),
         q43.6 = dplyr::coalesce(q43_6, q46_5)) |>
  ungroup() |> 
  
  # add variable labels to coalesced variables
  sjlabelled::var_labels(
    q41.1 = "Election officials test machines",
    q41.2 = "Election officials conduct audits",
    q41.3 = "Partisan Poll watchers observe the election.",
    q41.4 = "Election staff includes/is majority of veterans and family",
    q41.5 = "Election staff includes/is majority of lawyers",
    q41.6 = "Election staff includes/is majority of college students",
    
    q43.1 = "Law enforcement presence.",
    q43.2 = "Partisan Poll watchers observe the election",
    q43.3 = "People holding signs or giving out literature",
    q43.4 = "Election staff includes/is majority of veterans and family",
    q43.5 = "Election staff includes/is majority of lawyers",
    q43.6 = "Election staff includes/is majority of college students"
  ) |> 
  relocate(q41.1:q43.6, .after = q46_6)


# Set variable labels for select variable columns
data <- data |> 
  labelled::set_variable_labels(
    q1    = "Consent to Participate...",
    group = "Experiment Condition",
    qset = "Question Set",
    lucid_age = "Lucid: age",
    lucid_gender = "Lucid: What is your gender?",
    lucid_hhi = "Lucid: What is your current Annual household income before taxes?",
    lucid_ethnicity = "Lucid: What is your race?",
    lucid_hispanic = "Lucid: Are you Hispanic, Latino, or Spanish Origin?",
    lucid_education = "Lucid: What is the highest level of education you have completed?",
    lucid_political_party = "Lucid: Generally Speaking, do you usually think yourself as a Republican, a Democrat, an Independent, or what?",
    lucid_region = "Lucid: For Region, ZIP is asked which is automatically mapped to the Region",
    lucid_zip = "Lucid: What is your ZIP code?",
    lucid_rid = "Lucid respondent ID"
  )

# create data dictionary using `labelled::generate_dictionary()`
data_dict <- data |> 
  labelled::generate_dictionary()


# arrange the dataframe in descending order according to 'end_date', survey
# submission date
# Notably, the start and end date variables are class datetime <dttm>, which
# also includes the time of day the survey was taken. Later I can separate the
# date and time into two columns for both start and end date.
data |> arrange(desc(end_date))

# get a glimpse of the data
# glimpse(data)



# save dataframe and data dictionary :::::::::::::::::::::::::::::::::::::::####

# save df processed data set.
save(data, file = "data/research-paper-data-20241029.Rdata")
write.csv(data, file = "data/research-paper-data-20241029.csv")


# save a data dict that can be saved as .csv
# generate data dictionary
# data dictionary from labelled package. Will save as .csv file
data_dict <- surveytoolbox::data_dict(data)
write.csv(data_dict, file = "data/data_dictionary.csv")




# remove uneeded dataframes from global environment ::::::::::::::::::::::::####
rm(raw_spss, raw_spss_dict, data_labels)

