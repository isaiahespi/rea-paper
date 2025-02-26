set.seed(1234)


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
  items = df,
  totals = T,
  missing = T,
  impute = "median"
)

# The item by scale correlations for each item, corrected for item overlap by
# replacing the item variance with the smc for that item.
# Note that, when SMC < 0, SMC were set to .0
item.cor.corrected <- conf.sum.scores$item.corrected

conf.mean.scores <- psych::scoreItems(
  keys = conf.keys,
  items = df,
  totals = F,
  missing = T,
  impute = "mean"
)



# head(conf.sum.scores$scores)
# conf.sum.scores$scores[,1] # conf.sum.az
# conf.sum.scores$scores[,2] # conf.sum.lcl
# conf.sum.scores$scores[,3] # trust.az
# conf.sum.scores$scores[,4] # trust.lcl
# conf.sum.scores$scores[,5] # distrust.az
# conf.sum.scores$scores[,6] # distrust.lcl
# 
# head(conf.mean.scores$scores)
# conf.mean.scores$scores[,1] # conf.sum.az
# conf.mean.scores$scores[,2] # conf.sum.lcl
# conf.mean.scores$scores[,3] # trust.az
# conf.mean.scores$scores[,4] # trust.lcl
# conf.mean.scores$scores[,5] # distrust.az
# conf.mean.scores$scores[,6] # distrust.lcl

# add confidence scale scores to data frame
df <- df |> 
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


# Delete unnecessary objects from global environment :::::::::::::::::::::::####

rm(conf.mean.scores, conf.keys)
