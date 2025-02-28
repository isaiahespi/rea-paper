set.seed(1234)

# load packages
library(tidyverse)

# load data
load("data/research-paper-data-20241029.Rdata")

# subset data and do some other stuff ::::::::::::::::::::::::::::::::::::::####

df <- data |> 
  select(
    rowID,
    response_id,
    group,          # Experimental condition: treatment or control
    partyid_3cat,   # partyID in 3 categories, true independents (not leaners)
    dem.dum,        # dummy variable of democrats
    rep.dum,        # dummy variable of republican
    ind.dum,        # dummy variable of independents
    race2,          # race including Hispanic/Latino/Spanish origin
    race_wnw,       # race dichotomous between White and Non-White
    educ_4cat,      # educational attainment in four categories
    educ_4cat.num,  # educational attainment in four categories, numeric
    gender,         # male, female, Non-binary/Third gender, Pref. not to say
    gender_3cat,    # male, female, other or preferred not to say
    age_4cat,       # age in years in 4 groups
    mil_anyrelation,# relation or no relationship with military service
    milrelation,    # particular relationship with military service
    q5:q8,          # pre-treatment survey items
    q19:q24,        # trust in elections in Maricopa County, AZ
    q25:q26,        # concern for voter safety in Maricopa County, AZ
    q28_1:q28_5,    # eef in Maricopa County, AZ
    q30:q35,        # trust in elections in local area
    q36:q38,        # concern for voter safety in local area
    q40_1:q40_5,    # eef in Maricopa County, AZ
    contains(".r"), # recoded numeric versions of the variables above
    contains(".n")  # recoded numeric versions of the variables above
)

df <- df |> labelled::set_variable_labels(
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
df <- df |>
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
df$party_strength <- data |>
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

df <- df |> 
  dplyr::relocate(party_strength, .after = partyid_3cat)

# make dummy variables for each category of partyID strength
df <- df |>
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

df <- df |> 
  mutate(partyid_5cat = forcats::fct_collapse(
    party_strength,
    Independent = "Independent",
    Republican = c("Strong Republican", "Weak Republican"),
    Democrat = c("Strong Democrat", "Weak Democrat")
    ), .after = partyid_3cat
  ) 


# check to confirm that all party ID modified variables match
df |> 
  select(partyid_3cat, partyid_5cat, party_strength, 6:12) |> 
  group_by(party_strength, partyid_5cat) |> 
  count(partyid_3cat)

# reorder `party_strength`, `partyid_3cat`, and `partyid_5cat` factor levels
# order so that "Independent" # becomes the reference level for each factor
# variable. This means that when the categorical variable (i.e., factor) is used
# in a linear model `lm()`, the 'ind' level will be used as the omitted
# reference category
df <- df |>
  mutate(party_strength = forcats::fct_relevel(party_strength, "Independent", after = 0L),
         partyid_3cat   = forcats::fct_relevel(partyid_3cat, "Independent", after = 0L),
         partyid_5cat   = forcats::fct_relevel(partyid_5cat, "Independent", after = 0L))

# note how it is now placed as the first level that appears
df |> 
  select(party_strength) |>  
  count(party_strength)


# collapse factor levels of safety items (except q38)
# order of factor levels is important.
df <- df |>
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


# create df dictionary
df_dict <- labelled::generate_dictionary(df) |>
  # this allows me to save dictionary as .csv file
  labelled::convert_list_columns_to_character()


# run these in case variable labels are removed
df_labels <- df_dict |>
  select(variable, label) |> 
  deframe()

# change all ordered factors to non-ordered factors.
# NOTE: this will (for some reason) remove variable labels from factor variables
df <- df |> 
  mutate(across(where(is.ordered), ~factor(., ordered = F)))

# Now assign the labels using the splice operator. Using the splice operator,
# labels are assigned via matching against the variable name, which means that
# variable order does not matter.
df <- df |> 
  labelled::set_variable_labels(!!!df_labels)
