########################
###MEDIATION IN R#######
####USING LAVAAN########
###CODE ALONG WITH ME###
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
# hint: use read.csv

cat_dat <- read.csv("cat_dat.csv")

# Print first and last five observations
# hint: use headTail()

cat_dat %>%
  headTail()
  

#food = THE amount of food a cat intakes in a day
#weight = How much the cat weighs
#lazy = A score (1-5) on how lazy the cat is, 5 meaning very lazy

# STEP 3: VISUALIZATION
# It's always a good idea to look at your data.
# use select & pairs.panels
cat_dat %>%
  select(-id) %>% #select all EXCEPT id
  pairs.panels()


# STEP 4: CREATING OUR MEDIATION MODEL
#hint, copy & paste from thirst_dat example 

mod1 <- '# a path IV -> Mediator
         weight ~ a * food

         # b path  Mediator -> DV
         lazy ~ b * weight

         # c prime path  IV -> DV
         lazy ~ cp * food

         # indirect and total effects
         ab := a * b
         total := cp + ab'


# Next, set random seed so results can be reproduced
set.seed(1234)

# STEP 5: RUNNING SEM
# Fit model
# You must specify bootstrapping in the sem() function
# Normally I run 10000, but it takes time. 
# To be time efficient, we will only run 1000 today
# hint, use sem

csem1 <- sem(mod1, data = cat_dat, se = "bootstrap", bootstrap = 1000)


# STEP 6: SUMMARIZE MODEL
# standardized = TRUE adds standardized estimate to the model output
# hint, use summary & set standardized to TRUE
summary(csem1, standardized = TRUE)  


# Print all model parameters
# hint: use parameterestimates
parameterestimates(csem1, boot.ci.type = "bca.simple", standardized = TRUE)

# STEP 7: INTERPRET
#INTERPRET a
# Every 1 unit increase in food intake was associated with an a = 3.216 (SE 0.884) increase in weight units.

#INTERPRET b
# Adjusting for food, every 1 unit (lb) increase in wegiht was associated with being b = 0.448 (SE = 0.151) more lazy.

#DESCRIBE PATH
# Increased in food consumption were associated with increased in laziness INDIRECTLY through increases in weight.

#DESCRIBE ab
# Specifically, for every a = 3.216 unit increase in the association between food intake and weight, 
# there was an ab = 1.440 (SE 0.568) increase in how lazy the was (INDIRECT EFFECT).

#DESCRIBE CI
#Importantly, a bias-corrected bootstrapped confidence interval with 1,000 
#samples was above zero, 95% CI [0.508, 2.88]. 

#DESCRIBE C'
# Lastly, a partial mediation showed that food intake was associated with cat laziness 
# independent of its association with weight, c' = 2.349 (SE 1.122, p = 0.04).

#######################
####PSYCH PACKAGE#######
#########################

## Test same model using psych package
## hint, use mediate from psych package

cat_dat %>%
  mediate(y = "lazy", x = "food", m = "weight", n.iter = 1000) %>%
  print(short = TRUE)

