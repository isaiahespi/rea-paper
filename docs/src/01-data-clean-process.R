# load, clean, and process data imported from Qualtrics
# using data exported from Qualtrics on 2024-10-29 at 12:02 PM


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

# sanitize data downloaded from Qualtrics (i.e., remove PII) :::::::::::::::####

# rename select variables. Identify Lucid embedded variable columns
raw_spss_sanitized <- raw_spss |> 
  # select and remove sensitive PII variables (e.g., IPAddress, Location)
  dplyr::select(
    -IPAddress,
    -LocationLatitude,
    -LocationLongitude,
    -RecipientFirstName,
    -RecipientLastName,
    -RecipientEmail
    )

# create and save survey codebook ::::::::::::::::::::::::::::::::::::::::::####

# this includes only the survey questions: question number (var), the question
# text (label_var), the value labels (label_val), and the numeric codes (value)
codebook <- raw_spss_sanitized |> 
  janitor::clean_names() |> 
  # get rid of superfluous Qualtrics columns
  # Keep Qualtrics `response_id`
  select(!1:7, !9:11, -q_data_policy_violations) |>
  # subset spss data set to omit the display order variables
  select(!contains("_do_")
         & !contains("_ado_")
         & !contains("_click")
         & !contains("_count")) |>
  select(contains("q"), group) |>
  surveytoolbox::data_dict()

# create data dictionary of sanitized raw SPSS data set ::::::::::::::::::::####

# create data dictionary (I prefer output from surveytoolbox)
raw_spss_sanitized_data_dict <- raw_spss_sanitized |> 
  surveytoolbox::data_dict()



# clean up data set ::::::::::::::::::::::::::::::::::::::::::::::::::::::::####

data <-  raw_spss_sanitized |>
  # clean column names
  janitor::clean_names() |>
  # rename column variables. identify lucid variables
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
  # create var to identify display order of popular efficacy items*(see note)
  mutate(popeff_qdo = case_when(
    fl_22_do_popular_efficacy_agent_aim == 1 
    & fl_22_do_popularefficacy_agent_action_aim == 2 ~ 1,
    
    fl_22_do_popular_efficacy_agent_aim == 2 
    & fl_22_do_popularefficacy_agent_action_aim == 1 ~ 0),
    .after = qset,
    .keep = "unused") |> 
  # omit the display order variables and the superfluous timing variables
  select(!contains("_do_")
         & !contains("_ado_")
         & !contains("_click")
         & !contains("_count"),
         -external_reference,
         -distribution_channel,
         -user_language,
         -q_data_policy_violations) |> 
  
  # filter only consent, citizens, and those who passed attention checks
  filter(
    q1 == 1, # include only those who consent (exclude all non-consent) 
    q3 != 3, # exclude non-citizens
    q9 == 1 | q9 == 0 & q10 == 1 # exclude those who failed both attn checks
    ) |>
  
  # add a column of ascending sequential row ids starting at 1 at start of df
  tibble::rowid_to_column("rowID") |> 
  labelled::set_variable_labels(rowID = "Row ID")

# NOTE: Question items Q51 to Q58 were split up into two question sets, Q51:Q54
# and Q55:Q58. The order of these question sets were randomly reversed. For
# order 1, questions Q51:Q54 ('agent-aim') were presented first, followed by
# Q55:Q58 ('agent-action-aim'). For order 2, the presentation was reversed. Two
# variables (columns) in the data set identify the question display order. Here
# I use those two variables to construct a single dummy variable where order 1 =
# 1, and order 2 = 0

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

# fix qset variable column :::::::::::::::::::::::::::::::::::::::::::::::::####

# the `qset` variable contains a blank value "" that is being counted. 
# I use the following code to render that blank "" into an explicit missing
# value NA
data <- data |>
  sjlabelled::set_labels(qset, labels=c("A" = "A", "B" = "B")) |> 
  sjlabelled::set_na(qset, na = "", as.tag = F)

# people who failed to complete the survey quit after responding to
# approximately 31% of the survey on average, which corresponds to question 23.

# 129 people didn't finish the survey. 108 of those who didn't finish, quit the
# survey just before being assigned a `qset` identifier code (e.g., A or B),
# which means 108 respondents didn't answer any survey questions beyond Q39.

# Since responses to questions sets A and B are important to the study,
# any respondent who quit the survey prior to answering any of the questions of
# interest is of no value and can safely be dropped from the survey
# This reduces the sample size from n = 1,395 to n = 1,287
data <- data |> 
  filter(!is.na(qset))

# Set variable labels for select variable columns ::::::::::::::::::::::::::####

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



# create data dictionary/codebook ::::::::::::::::::::::::::::::::::::::::::####
# NOTE: This step must be done before the next step if you want to keep variable
# labels!

# create data dictionary using `surveytoolbox::data_dict()`
data_dict <- data |> 
  labelled::set_variable_labels(
    popeff_qdo = "Popular efficacy question display order"
  ) |> 
  surveytoolbox::data_dict()

# To quickly assign the variable labels, first create a named vector via
# deframe() with values as the variable labels and names as the variable names.
data_labels <- data_dict |>
  select(var, label_var) |> 
  deframe()


# convert all labelled variables to factor :::::::::::::::::::::::::::::::::####
# NOTE: this will remove all variable labels

data <- data |> 
  # make `group` and `qset` factors (because they are character class)
  mutate(group = forcats::fct(group, levels = c("Treatment", "Control")),
         qset = forcats::fct(qset, levels = c("A", "B"))) |> 
  # convert all labelled variables to factors
  haven::as_factor(only_labelled = T, ordered = F) |>
  # re-code any empty/blank levels to NA
  mutate(across(where(is.factor), ~ fct_recode(., NULL = ""))) |> 
  
  # code to recode levels "-99" in factor vars to NA
  mutate(across(where(is.factor), ~forcats::fct_recode(., NULL = "-99"))) |>
  
  # reverse order of identified factor levels for consist direction
  mutate(across(c(q8, q20, q23, q24, q25, q31, q34, q35, q36, q49, q50), .fns = ~fct_rev(.))) |> 
  # reverse order of group levels so control comes first. Better table display
  mutate(group = forcats::fct_rev(group)) |> 
  sjlabelled::set_labels(group, labels = c("Control" = 0, "Treatment" = 1)) |>
  sjlabelled::set_labels(qset, labels = c("A" = 0, "B" = 1))


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


# create an age group variable (groups align with CPS top coding)
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
             "85+" = c(85:86,88, 92)
             ),
         .after = gender) |> 
  labelled::set_variable_labels(
    age_cat = "Age categorized into eight groups"
  )

# create another age group variable with fewer age categories
data <- data |>    
  mutate(age_4cat = 
           forcats::fct_collapse(
             as_factor(q2),
             "18-34" = c(18:34),
             "35-54" = c(35:54),
             "55-74" = c(55:74),
             "75-85+" = c(75:82, 84, 85:86, 88, 92)
             ),
         .after = age_cat) |> 
  labelled::set_variable_labels(
    age_cat = "Age categorized into three groups"
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
  mutate(
    educ_4cat = forcats::fct_collapse(
      educ,
      "H.S. or less" = c(
        "Less than high school degree",
        "High school graduate (high school diploma or equivalent including GED)"
      ),
      "Some college no degree" = "Some college but no degree",
      "College degree" = c(
        "Associate degree in college (2-year)",
        "Bachelor's degree in college (4-year)"
      ),
      "Postgraduate degree" = c(
        "Master's degree",
        "Doctoral degree",
        "Professional degree (JD, MD)"
      )
    ),
    .after = educ
  )

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

# modifying some of the variable categories for ease of interpretation
data <- data |>
  mutate(
    # adding Hispanic or Latino or Spanish origin to race variable
    race2 = dplyr::case_when(
      race == "White or Caucasian" ~ "White",
      race == "Black or African American" ~ "Black",
      hisp == "Yes" ~ "Hispanic",
      .default = race
    ),
    
    # combining race and hispanic variable
    race_hisp = dplyr::case_when(
      hisp == "Yes" & race == "White or Caucasian" ~ "White and Hispanic",
      hisp == "Yes" &
        race == "Black or African American" ~ "Black and Hispanic",
      hisp == "Yes" &
        race == "American Indian" ~ "American Indian and Hispanic",
      hisp == "Yes" & race == "Asian" ~ "Asian and Hispanic",
      hisp == "Yes" & race == "Other" ~ "Hispanic",
      hisp == "No" &
        race == "White or Caucasian" ~ "White or Caucasian",
      hisp == "No" &
        race == "Black or African American" ~ "Black or African American",
      hisp == "No" & race == "American Indian" ~ "American Indian",
      hisp == "No" & race == "Asian" ~ "Asian",
      hisp == "No" & race == "Other" ~ "Other Non-hispanic",
      hisp == "Prefer not to say" &
        race == "White or Caucasian" ~ "White or Caucasian",
      hisp == "Prefer not to say" &
        race == "Black or African American" ~ "Black or African American",
      hisp == "Prefer not to say" &
        race == "American Indian" ~ "American Indian",
      hisp == "Prefer not to say" & race == "Asian" ~ "Asian",
      hisp == "Prefer not to say" & race == "Other" ~ "Other",
      TRUE ~ NA
    ),
    
    # relationship with military service members
    milrelation = dplyr::case_when(
      milserv1 == "Yes" & milserv2 == "Yes" | milserv2 == "No"
      & milservfam == "No" ~ "Served",
      milserv1 == "Yes" & milserv2 == "Yes" | milserv2 == "No"
      & milservfam == "Yes" ~ "Served w/fam",
      milserv1 == "Yes" &
        milserv2 == "No" & milservfam == "No" ~ "Served",
      is.na(milserv2) & milservfam == "Yes" ~ "Only family",
      is.na(milserv2) & milservfam == "No" ~ "No relation",
      TRUE ~ NA
    ),
    
    # variable to show any relationship with military service members
    mil_anyrelation = dplyr::case_when(
      milrelation == "No relation" ~ "No relation",
      milrelation == "Served"
      | milrelation == "Served w/fam"
      | milrelation == "Only family" ~ "Some relation",
    )
  ) |>
  mutate(across(
    c(race2, race_hisp, milrelation, mil_anyrelation),
    ~ forcats::as_factor(.)
  )) |>
  
  # adding a simple "White" or "Non-White" racial category
  mutate(
    race_wnw = dplyr::case_when(
      race2 == "White" ~ "White",
      race2 != "White" ~ "Non-White",
      TRUE ~ NA
    )
  ) |>
  
  # relocate variables in data frame
  dplyr::relocate(race2, .after = race) |>
  dplyr::relocate(race_hisp, .after = race2) |>
  dplyr::relocate(race_wnw, .after = race2) |> 
  dplyr::relocate(milrelation, .after = milservfam) |>
  dplyr::relocate(mil_anyrelation, .after = milrelation)

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
                ~ haven::as_factor(.))) |> 
  
  # add variable labels to coalesced variables
  sjlabelled::var_labels(
    q41.1.clps = "Election officials test machines",
    q41.2.clps = "Election officials conduct audits",
    q41.3.clps = "Partisan Poll watchers observe the election.",
    q41.4.clps = "Election staff includes/is majority of veterans and family",
    q41.5.clps = "Election staff includes/is majority of lawyers",
    q41.6.clps = "Election staff includes/is majority of college students",
    
    q43.1.clps = "Law enforcement presence.",
    q43.2.clps = "Partisan Poll watchers observe the election",
    q43.3.clps = "People holding signs or giving out literature",
    q43.4.clps = "Election staff includes/is majority of veterans and family",
    q43.5.clps = "Election staff includes/is majority of lawyers",
    q43.6.clps = "Election staff includes/is majority of college students"
  ) |> 
  
  # add variable labels to coalesced variables
  sjlabelled::var_labels(
    q41.1.clps.dum = "Election officials test machines",
    q41.2.clps.dum = "Election officials conduct audits",
    q41.3.clps.dum = "Partisan Poll watchers observe the election.",
    q41.4.clps.dum = "Election staff includes/is majority of veterans and family",
    q41.5.clps.dum = "Election staff includes/is majority of lawyers",
    q41.6.clps.dum = "Election staff includes/is majority of college students",
    
    q43.1.clps.dum = "Law enforcement presence.",
    q43.2.clps.dum = "Partisan Poll watchers observe the election",
    q43.3.clps.dum = "People holding signs or giving out literature",
    q43.4.clps.dum = "Election staff includes/is majority of veterans and family",
    q43.5.clps.dum = "Election staff includes/is majority of lawyers",
    q43.6.clps.dum = "Election staff includes/is majority of college students"
  )





# add state FIPS codes to dataframe ::::::::::::::::::::::::::::::::::::::::####

# state and county fips codes are kept in tidycensus data
# tidycensus::fips_codes$state_code |> unique() |> dput()
# tidycensus::fips_codes$state_name |> unique() |> dput()

# create temporary dataframe
st <- data.frame(
  state_fips = tidycensus::fips_codes$state_code |> unique() |> dput(),
  state_name = tidycensus::fips_codes$state_name |> unique() |> dput()
)

# match that dataframe to states (q4) in primary dataframe
st <- st |>
  mutate(states2 = dplyr::case_when(
    # if state_name is in any levels of q4 var, then state_fips codes
    state_name %in% levels(data$q4) ~ state_fips,
    TRUE ~ NA # else NA
  )) |> 
  # filter to exclude missing values
  filter(!is.na(states2)) |> 
  # arrange by alphabetical order of state name
  arrange(state_name) |>  
  # select only state_name and state_fips, renaming state_name to q4
  select(q4=state_name, state_fips)

# st

# check to see what won't join. Should be 0
# data |> anti_join(st, by = join_by(q4))

# left_join keeps all obs in x, in this case, data. Whatever isn't in data will
# be dropped from st (the temp dataframe)
data <- data |> dplyr::left_join(st, by = dplyr::join_by(q4)) |> 
  dplyr::relocate(state_fips, .after = q4) |> 
  dplyr::mutate(state_fips = as.numeric(state_fips)) |> 
  labelled::set_variable_labels(
    state_fips = "State FIPS code"
  )

# remove temporary dataframe
rm(st)



# add variable labels back into factorized data set ::::::::::::::::::::::::####

# variable labels are nice to have for plenty of reasons
# since SPSS data is labelled, usually, the variables are <dbl+lbl> instead of
# factors <fct>. It's easier to analyze vars in data as factors, but losing the
# variable labels (i.e., question text) is kind of a bummer. The following puts
# the variable labels form the SPSS data set back into the 'factor-ized' dataset

# To quickly assign the variable labels, first create a named vector via
# deframe() with values as the variable labels and names as the variable names.
data_labels <- data_dict |>
  select(var, label_var) |> 
  deframe()

# Now assign the labels using the splice operator. Using the splice operator,
# labels are assigned via matching against the variable name, which means that
# variable order does not matter.

data <- data |> 
  labelled::set_variable_labels(!!!data_labels)

# Re-code select variables :::::::::::::::::::::::::::::::::::::::::::::::::####

# create numeric versions of select factor variables
data <- data |> 
  mutate(across(c(q19, q20, q22, q23, q24, q30, q31, q33, q34, q35),
                ~ dplyr::case_when(
  .x == "Not at all confident" ~ 0,
  .x == "Not too confident" ~ 1,
  .x == "Somewhat confident" ~ 2,
  .x == "Very confident" ~ 3,
  TRUE ~ NA), 
  .names = "{col}.r")) |>
  mutate(across(c(q21, q32), ~ dplyr::case_when(
    .x == "Very committed" ~ 3,
    .x == "Somewhat committed" ~ 2,
    .x == "Not too committed"  ~ 1,
    .x == "Not at all committed" ~ 0,
    TRUE ~ NA),
    .names = "{col}.r")) |> 
  mutate(across(c(q28_1:q28_5, q40_1:q40_5), ~ dplyr::case_when(
    .x == "Not likely at all" ~ 0,
    .x == "Not too likely" ~ -1,
    .x == "Somewhat likely" ~ -2,
    .x == "Very likely" ~ -3,
    TRUE ~ NA), 
    .names = "{col}.r")) |> 
  # these are reverse scored, so that higher concern reflects negative safety
  mutate(across(c(q25, q36), ~ dplyr::case_when(
    .x == "Very concerned" ~ -3,
    .x == "Somewhat concerned" ~ -2,
    .x == "Not too concerned" | .x == "Somewhat unconcerned" ~ -1,
    .x == "Not at all concerned" ~ 0,
    TRUE ~ NA),
    .names = "{col}.r")) |> 
  mutate(across(c(q26, q37, q38), ~ dplyr::case_when(
    .x == "Not at all confident" | .x == "Not safe at all" ~ 0,
    .x == "Not too confident" | .x == "Not too safe" ~ 1,
    .x == "Somewhat confident" | .x == "Somewhat safe" ~ 2,
    .x == "Very confident" | .x == "Very safe" ~ 3,
    TRUE ~ NA), 
    .names = "{col}.r")) |> 
  # add in positively coded numeric variables for q28 and q40 series
  mutate(across(c(q28_1:q28_5, q40_1:q40_5), ~ dplyr::case_when(
    .x == "Not likely at all" ~ 0,
    .x == "Not too likely" ~ 1,
    .x == "Somewhat likely" ~ 2,
    .x == "Very likely" ~ 3,
    TRUE ~ NA),
    .names = "{col}.n")) |> 
  
  # dummify partyid_3cat variable
  mutate(
    dem.dum = dplyr::case_when(
      partyid_3cat == "Democrat" ~ 1,
      partyid_3cat != "Democrat" ~ 0,
      TRUE ~ NA),
    rep.dum = dplyr::case_when(
      partyid_3cat == "Republican" ~ 1,
      partyid_3cat != "Republican" ~ 0,
      TRUE ~ NA),
    ind.dum = dplyr::case_when(
      partyid_3cat == "Independent" ~ 1,
      partyid_3cat != "Independent" ~ 0,
      TRUE ~ NA), .after = partyid_3cat
    ) |> 
  # created ordered numeric education variable
  mutate(educ_4cat.num = dplyr::case_when(
    educ_4cat == "H.S. or less" ~ 1,
    educ_4cat == "Some college no degree" ~ 2,
    educ_4cat == "College degree" ~ 3,
    educ_4cat == "Postgraduate degree" ~ 4,
    TRUE ~ NA
  ), .after = educ_4cat) |> 
  
  # add and short variable labels
  sjlabelled::var_labels(
    rowID           = "Sequential Row ID",
    dem.dum         = "PartyID dummy variable Democrat",
    rep.dum         = "PartyID dummy variable Republican",
    ind.dum         = "PartyID dummy variable Independent",
    race2           = "Modified Race variable; indcludes Hispanic as race",
    race_wnw        = "Race: White or Non-White",
    race_hisp       = "Race and Hispanic",
    educ_4cat       = "Educational Attainment in four categories",
    educ_4cat.num   = "Educational Attainment in four categories, numeric",
    mil_anyrelation = "relation or no relationship with military service",
    milrelation     = "particular relationship with military service",
    gender_3cat     = "gender: male, female, other or preferred not to say",
    q19.r   = "[AZ] votes counted as intended",
    q20.r   = "[AZ] Staff will do good job",
    q21.r   = "[AZ] Staff will be committed",
    q22.r   = "[AZ] fair voting process",
    q23.r   = "[AZ] fair voting outcomes",
    q24.r   = "[AZ] securetech",
    q25.r   = "[AZ] concern about potential violence",
    q26.r   = "[AZ] polling sites will be safe places to vote",
    q28_1.r = "[AZ] double voting",
    q28_2.r = "[AZ] votes not counted",
    q28_3.r = "[AZ] voters turned away",
    q28_4.r = "[AZ] foreign interference",
    q28_5.r = "[AZ] EO discourage voters",
    q30.r   = "[Local] votes counted as intended",
    q31.r   = "[Local] Staff will do good job",
    q32.r   = "[Local] Staff will be committed",
    q33.r   = "[Local] fair voting process",
    q34.r   = "[Local] fair voting outcomes",
    q35.r   = "[Local] securetech",
    q36.r   = "[Local] concern about potential violence, Local area",
    q37.r   = "[Local] polling places in local area, AZ will be safe places to vote",
    q38.r   = "[Local] personal safety voting in person in local area",
    q40_1.r = "[Local] double voting",
    q40_2.r = "[Local] votes not counted",
    q40_3.r = "[Local] voters turned away",
    q40_4.r = "[Local] foreign interference",
    q40_5.r = "[Local] EO discourage voters",
    q28_1.n = "[AZ] double voting",
    q28_2.n = "[AZ] votes not counted",
    q28_3.n = "[AZ] voters turned away",
    q28_4.n = "[AZ] foreign interference",
    q28_5.n = "[AZ] EO discourage voters",
    q40_1.n = "[Local] double voting",
    q40_2.n = "[Local] votes not counted",
    q40_3.n = "[Local] voters turned away",
    q40_4.n = "[Local] foreign interference",
    q40_5.n = "[Local] EO discourage voters")


# adjust some select variable labels
data <- data |> labelled::set_variable_labels(
  q28_1 ="There will be voter fraud, that is, people who are not eligible to vote will vote, or vote more than once",
  q28_2 ="Many votes will not actually be counted",
  q28_3 ="Many people will show up to vote and be told they are not eligible",
  q28_4 ="A foreign country will tamper with the votes cast in this area to change the results",
  q28_5 ="Election officials in Maricopa County, Arizona will try to discourage some people from voting",
  q40_1 ="There will be voter fraud, that is, people who are not eligible to vote will vote, or vote more than once",
  q40_2 ="Many votes will not actually be counted",
  q40_3 ="Many people will show up to vote and be told they are not eligible",
  q40_4 ="A foreign country will tamper with the votes cast to change the results",
  q40_5 ="Election officials in your community will try to discourage some people from voting")


# make dummy variable of q7 responses: legitimate and not_legitimate
data <- data |>
  fastDummies::dummy_cols(
    select_columns = "q7",
    ignore_na = T,
    omit_colname_prefix = T
  ) |>
  dplyr::rename_with(.cols = contains("legitimate"),
                     .fn = janitor::make_clean_names) |> 
  labelled::set_variable_labels(
    legitimate = "Belief that 2020 election results were legitimate (Dummy variable)",
    not_legitimate = "Belief that 2020 election results were illegitmate (Dummy Variable)"
  ) |> 
  dplyr::relocate(legitimate, not_legitimate, .after = q7)



# forgot to include partisan identification strength
# create as factor
data$party_strength <- data |>
  mutate(
    partyid_strength = pewmethods::fct_case_when(
      partystr_rep == "Strong" ~ "Strong Republican",
      partystr_rep == "Not very strong" ~ "Weak Republican",
      partylean == "Republican" ~ "Lean Republican",
      partylean == "Neither" ~ "Independent",
      partylean == "Democratic" ~ "Lean Democrat",
      partystr_dem == "Not very strong" ~ "Weak Democrat",
      partystr_dem == "Strong" ~ "Strong Democrat",
      TRUE ~ NA
    ),
    .after = partyid_3cat, .keep = "none"
  ) |>
  labelled::set_variable_labels(
    partyid_strength = "Partisan Identification Strength"
    ) |> 
  deframe()

data <- data |> 
  dplyr::relocate(party_strength, .after = partyid_3cat)

data <- data |> 
  mutate(partyid_5cat = forcats::fct_collapse(
    party_strength,
    Independent = "Independent",
    Republican = c("Strong Republican", "Weak Republican"),
    Democrat = c("Strong Democrat", "Weak Democrat")
    ), .after = partyid_3cat
  ) 


# make dummy variables for each category of partyID strength
data <- data |>
  fastDummies::dummy_cols(select_columns = "party_strength", ignore_na = T) |>
  dplyr::rename_with(.cols = contains("party_strength"),
                     .fn = janitor::make_clean_names) |>
  dplyr::rename_with(.fn = \(x)sub("party_strength_", "", x)) |>
  labelled::set_variable_labels(
    strong_republican = "Strong Republican",
    weak_republican = "Weak Republican",
    lean_republican = "Independent, leans Republican",
    independent = "True independent",
    lean_democrat = "Independent, leans Democrat",
    weak_democrat = "Weak Democrat",
    strong_democrat = "Strong Democrat"
  ) |>
  dplyr::relocate(
    starts_with("strong"),
    starts_with("weak"),
    starts_with("lean"),
    contains("independent"),
    .after = party_strength
  )



# reorder `party_strength`, `partyid_3cat`, and `partyid_5cat` factor levels
# order so that "Independent" # becomes the reference level for each factor
# variable. This means that when the categorical variable (i.e., factor) is used
# in a linear model `lm()`, the 'ind' level will be used as the omitted
# reference category
data <- data |>
  mutate(party_strength = forcats::fct_relevel(party_strength, "Independent", after = 0L),
         partyid_3cat   = forcats::fct_relevel(partyid_3cat, "Independent", after = 0L),
         partyid_5cat   = forcats::fct_relevel(partyid_5cat, "Independent", after = 0L))

# collapse factor levels of safety items (except q38)
# order of factor levels is important.
data <- data |>
  mutate(across(c(q25,q36), ~pewmethods::fct_case_when(
    .x %in% c("Very concerned", "Somewhat concerned") ~ "concerned",
    .x %in% c("Not too concerned", "Somewhat unconcerned", "Not at all concerned") ~ "unconcerned",
    TRUE ~ NA), 
    .names = "{col}_")) |> 
  mutate(across(c(q26, q37), ~pewmethods::fct_case_when(
    .x %in% c("Not too confident", "Not at all confident") ~ "not_confident",
    .x %in% c("Very confident", "Somewhat confident") ~ "confident",
    TRUE ~ NA), 
    .names = "{col}_")) |> 
  
  # create dummy variables of collapsed factor variables
  # 1 = unconcerned (about violence) or confident (in voter safety) 
  # 0 = concerned (about violence) or not confident (in voter safety)
  mutate(across(c(q25_, q36_), ~dplyr::case_when(
    .x %in% c("concerned") ~ 0,
    .x %in% c("unconcerned") ~ 1,
    TRUE ~ NA
  ), .names = "{.col}dum")) |> 
  mutate(across(c(q26_, q37_), ~dplyr::case_when(
    .x %in% c("confident") ~ 1,
    .x %in% c("not_confident") ~ 0,
    TRUE ~ NA
  ), .names = "{.col}dum")) |>  
  dplyr::rename_with(
    .cols = c(q25_, q36_, q26_, q37_),
    .fn = \(x)sub("_", ".clps", x)) |> 
  dplyr::rename_with(
    .cols = c(q25_dum, q26_dum, q36_dum, q37_dum),
    .fn = \(x)sub("_dum", ".dum", x)) |> 
  
  # add variable labels
  labelled::set_variable_labels(
    q26.clps = "Confidence in voter saftey at polling sites in Maricopa County, AZ",
    q25.clps = "Concern for violence in Maricopa County, AZ",
    q36.clps = "Concern for violence in Local Area",
    q37.clps = "Confidence in voter saftey at polling sites in Local Area",
    q25.dum = "Concern for violence in AZ, dummy variable",
    q36.dum = "Concern for violence in local area, dummy variable",
    q26.dum = "Voter safety confidence, AZ, dummy variable",
    q37.dum = "Voter safety confidence, local area, dummy variable"
  )


# Construct composite score variables using `psych::scoreItems` ::::::::::::####

# create a list of scoring keys for scales for AZ and Local area
conf.keys <- list(
  conf.az = c("q19.r","q20.r","q21.r","q22.r","q24.r",
              "q28_1.r","q28_2.r","q28_3.r","q28_4.r","q28_5.r"),
  conf.local = c("q30.r","q31.r","q32.r","q33.r","q35.r",
                 "q40_1.r","q40_2.r","q40_3.r","q40_4.r","q40_5.r"),
  trust.az = c("q19.r","q20.r","q21.r","q22.r","q24.r"),
  trust.local = c("q30.r","q31.r","q32.r","q33.r","q35.r"),
  distrust.az = c("q28_1.n","q28_2.n","q28_3.n","q28_4.n","q28_5.n"),
  distrust.local = c("q40_1.n","q40_2.n","q40_3.n","q40_4.n","q40_5.n")
)


conf.sum.scores <- psych::scoreItems(
  keys = conf.keys,
  items = data,
  totals = T,
  missing = T,
  impute = "median"
)

# The item by scale correlations for each item, corrected for item overlap by
# replacing the item variance with the smc for that item.
# Note that, when SMC < 0, SMC were set to .0
# item.cor.corrected <- conf.sum.scores$item.corrected

conf.mean.scores <- psych::scoreItems(
  keys = conf.keys,
  items = data,
  totals = F,
  missing = T,
  impute = "mean"
)


# add confidence scale scores to data frame
data <- data |> 
  mutate(
    conf.scores.az        = conf.sum.scores$scores[,1],
    conf.scores.lcl       = conf.sum.scores$scores[,2],
    conf.mean.scores.az   = scales::rescale(conf.mean.scores$scores[,1], to = c(0,3)),
    conf.mean.scores.lcl  = scales::rescale(conf.mean.scores$scores[,2], to = c(0,3)),
    conf.zscores.az       = (conf.scores.az - mean(conf.scores.az))/sd(conf.scores.az),
    conf.zscores.lcl      = (conf.scores.lcl - mean(conf.scores.lcl))/sd(conf.scores.lcl),
    conf.az.rescaled      = scales::rescale(conf.scores.az, to = c(-3, 3)),
    conf.lcl.rescaled     = scales::rescale(conf.scores.lcl, to = c(-3, 3))) |> 
  
  # add trust composite scores to data frame
  mutate(
    trust.scores.az = conf.sum.scores$scores[,3], # trust.az
    trust.scores.lcl = conf.sum.scores$scores[,4],
    trust.mean.scores.az = scales::rescale(conf.mean.scores$scores[,3], to = c(0, 3)),
    trust.mean.scores.lcl = scales::rescale(conf.mean.scores$scores[,4], to = c(0, 3)),
    trust.zscores.az       = (trust.scores.az - mean(trust.scores.az))/sd(trust.scores.az),
    trust.zscores.lcl      = (trust.scores.lcl - mean(trust.scores.lcl))/sd(trust.scores.lcl),
    trust.az.rescaled = scales::rescale(trust.scores.az, to = c(0, 3)),
    trust.lcl.rescaled = scales::rescale(trust.scores.lcl, to = c(0, 3))) |> 
  
  # add distrust scale scores to data frame
  mutate(
    distrust.scores.az = conf.sum.scores$scores[,5],
    distrust.scores.lcl = conf.sum.scores$scores[,6],
    distrust.mean.scores.az = scales::rescale(conf.mean.scores$scores[,5], to = c(0, 3)),
    distrust.mean.scores.lcl = scales::rescale(conf.mean.scores$scores[,6], to = c(0, 3)),
    distrust.zscores.az  = (distrust.scores.az - mean(distrust.scores.az))/sd(distrust.scores.az),
    distrust.zscores.lcl = (distrust.scores.lcl - mean(distrust.scores.lcl))/sd(distrust.scores.lcl),
    distrust.az.rescaled = scales::rescale(distrust.scores.az, to = c(0, 3)),
    distrust.lcl.rescaled = scales::rescale(distrust.scores.lcl, to = c(0, 3))) |> 
  
  # add variable labels to sum/mean scores and composite variables
  labelled::set_variable_labels(
    conf.scores.az           = "Sum scores for Confidence in Elections, AZ items", 
    conf.scores.lcl          = "Sum scores for Confidence in Elections, Local area items",
    conf.mean.scores.az      = "Mean scores for confidence in elections, AZ",
    conf.mean.scores.lcl     = "Mean scores for confidence in elections, local",
    conf.zscores.az          = "Standardized AZ confidence scores, mean = 0, sd = 1",
    conf.zscores.lcl         = "Standardized Local area confidence scores, mean = 0, sd = 1",
    conf.az.rescaled         = "Confidence in elections, AZ items, rescaled",
    conf.lcl.rescaled        = "Confidence in elections, local area items, rescaled (-3, 3)",
    trust.scores.az          = "Sum scores for trust in elections, AZ items",
    trust.scores.lcl         = "Sum scores for trust in elections, local area items",
    trust.mean.scores.az     = "Mean scores for trust in elections, AZ items",
    trust.mean.scores.lcl    = "Mean scores for trust in elections, local area items",
    trust.zscores.az         = "Standardized AZ trust scores, mean = 0, sd = 1",
    trust.zscores.lcl        = "Standardized Local area trust scores, mean = 0, sd = 1",
    trust.az.rescaled        = "Trust in elections, AZ items, rescaled (0, 3)",
    trust.lcl.rescaled       = "Trust in elections, local items, rescaled (0, 3)",
    distrust.scores.az       = "Sum scores for distrust in elections, AZ items",
    distrust.scores.lcl      = "Sum scores for distrust in elections, local area items",
    distrust.mean.scores.az  = "Mean scores for distrust in elections, AZ items",
    distrust.mean.scores.lcl = "Mean scores for distrust in elections, local area items",
    distrust.zscores.az      = "Standardized AZ distrust scores, mean = 0, sd = 1",
    distrust.zscores.lcl     = "Standardized Local area distrust scores, mean = 0, sd = 1",
    distrust.az.rescaled     = "distrust in elections, AZ items, rescaled (0, 3)",
    distrust.lcl.rescaled    = "distrust in elections, local items, rescaled (0, 3)"
  )



# create data dictionary using `surveytoolbox::data_dict()` ::::::::::::::::####

# create data dictionary using `surveytoolbox::data_dict()`
data_dict <- data |> 
  surveytoolbox::data_dict()

# To quickly assign the variable labels, first create a named vector via
# deframe() with values as the variable labels and names as the variable names.
data_labels <- data_dict |>
  select(var, label_var) |> 
  deframe()

# Now assign the labels using the splice operator. Using the splice operator,
# labels are assigned via matching against the variable name, which means that
# variable order does not matter.
data <- data |> 
  labelled::set_variable_labels(!!!data_labels)



# save dataframe and data dictionary :::::::::::::::::::::::::::::::::::::::####

# save df processed data set.
save(data, file = "data/research-paper-data-20241029.Rdata")
write.csv(data, file = "data/research-paper-data-20241029.csv")


# save a data dict that can be saved as .csv
# generate data dictionary
# data dictionary from labelled package. Will save as .csv file
# NOTE: I use `surveytoolbox::data_dict()` here instead of
# labelled::generate_dictionary() because the codebook produced by the former is
# preferred.
data_dict <- surveytoolbox::data_dict(data)
write.csv(data_dict, file = "data/data_dictionary.csv")

# save data dictionary of raw_spss_sanitized dataset download
write.csv(raw_spss_sanitized_data_dict, file = "data/raw_spss_sanitized_data_dict.csv")

# save codebook as .csv file in data/ directory
write.csv(codebook, file = "data/survey_codebook.csv")


# Save sanitized raw data set for use as resource in manuscript
haven::write_sav(
  data = raw_spss_sanitized, 
  path = "data/vmf_election_worker_recruitment_survey_raw_spss_sanitized_20241029_T12.02.sav")


# Delete unnecessary objects from global environment :::::::::::::::::::::::####

rm(raw_spss, raw_spss_sanitized, raw_spss_sanitized_data_dict, 
   conf.sum.scores, conf.mean.scores, conf.keys)


