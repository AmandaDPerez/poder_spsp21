########################
### MEDIATION IN R#######
#### USING LAVAAN########
### CODE ALONG WITH ME###
########################

# This is a skeleton script.
# I have put basic instructions throughout

# In this activity, follow along with me
# practice typing code and executing it

# If you get stuck, the complete code is
# found in the "01b_mediation_code_along_ANSWERS.R"


# STEP 1: LOAD LIBRARIES
# To run the line of code you are on:
# press CTRL + Enter (Windows)
# press COMMAND + Enter (Mac)
# Or, highlight + Enter
library(kableExtra)
library(tidyverse)
library(knitr)
library(lavaan)
library(psych)

# STEP 2: LOAD DATA
# I simulated the below datafile
# aka it is FAKE DATA

cat_dat <- read.csv("cat_dat.csv") # The data is already uploaded and can be loaded

# Print first and last five observations

cat_dat %>%
  headTail()

# food = IV
# weight = MEDIATOR
# lazy = DV

# STEP 3: VISUALIZATION
# It's always a good idea to look at your data.

cat_dat %>%
  select(-id) %>% # telling r what columns we DO NOT want to see
  pairs.panels()


# STEP 4: CREATING OUR MEDIATION MODEL


mod1 <- "# a path IV -> Mediator
         weight ~ a * food

         # b path  Mediator -> DV
         lazy ~ b * weight

         # c prime path  IV -> DV
         lazy ~ cp * food

         # indirect and total effects
         ab := a * b
         total := cp + ab"


# Next, set random seed so results can be reproduced
set.seed(1234)

# STEP 5: RUNNING SEM
# Fit model
# You must specify bootstrapping in the sem() function
# Normally I run 10000, but it takes time.
# To be time efficient, we will only run 1000 today
csem1 <- sem(mod1, data = cat_dat, se = "bootstrap", bootstrap = 1000)


# STEP 6: SUMMARIZE MODEL
# standardized = TRUE adds standardized estimate to the model output.
summary(csem1, standardized = TRUE)

# Print all model parameters
parameterestimates(csem1, boot.ci.type = "bca.simple", standardized = TRUE)

# STEP 7: INTERPRET
# Every 1 degree increase in food intake was associated with an a = 3.216 (S.E. = 0.878) increase
# in weight units (lbs).

# Adjusting for food, every 1-unit (lb) increase in weight was
# associated with being b = 0.448 (S.E. = 0.152) more lazy.

# Increases in food consumption were associated with increases in laziness
# indirectly through increases in weight.

# Specifically, for every a = 3.216 unit increase in the association between food intake
# and weight, there was an ab = 1.440 (S.E. = 0.661) increase in how
# lazy the cat was. (INDIRECT EFFECT)
# Importantly, a bias-corrected bootstrapped confidence interval with 1,000
# samples was above zero, 95% CI [0.56, 3.028].

# Last, there was no strong evidence that food intake
# was associated with cat laziness
# independent of its association with
# weight, c' = 2.349 (S.E. = 0.167, p = 0.044).

#########################
#### PSYCH PACKAGE#######
#########################

## Test same model using psych package
# The syntax for mediation in psych doesn't have as steep a learning curve as lavaan
# but lavaan (and SEM in general) has more flexibility in specifying more involved models.
cat_dat %>%
  psych::mediate(y = "lazy", x = "food", m = "weight", n.iter = 1000) %>%
  print(short = TRUE)
