########################
###MEDIATION IN R#######
####USING LAVAAN########
########################

# STEP 1: LOAD LIBRARIES
# To run the line of code you are on:
# press CTRL + Enter (Windows)
# press COMMAND + Enter (Mac)
# Or, highlight CTRL/CMD + Enter
install.packages("lavaan") #only needs to be done once
install.packages("psych") #only needs to be done once
library(lavaan)
library(psych)

# STEP 2: LOAD DATA
# I took the data from Table 3.1 in Mackinnon (2008, p. 56)

thirst_dat <- read.csv("thirst_dat.csv") 

# Print first and last five observations

thirst_dat %>%
  headTail()

#room_temp = IV
#thirst = MEDIATOR
#consume = DV

# STEP 3: VISUALIZATION
# It's always a good idea to look at your data.

thirst_dat %>%
  select(room_temp, thirst, consume) %>% #telling r what columns we want to see
  pairs.panels()

# You can optionally remove the below "#" to run "help"
# "help" will tell you more about the function
# help("pairs.panels")

# STEP 4: CREATING OUR MEDIATION MODEL
# We will first run mediation with lavaan
# Below we write a model to test indirect effect using sem() 
# from lavaan (LAtent VAriable ANalysis)

# Quick Guide to lavaan syntax/symbols
# `~` = Regress onto 
# Within the regression models, I label coefficients with the astrix.
# `:=` = Define a new parameter. 
## Note when you define new parameter with :=, you can use the astrix to multiply values  

mod1 <- '# a path IV -> Mediator
         thirst ~ a * room_temp

         # b path  Mediator -> DV
         consume ~ b * thirst

         # c prime path  IV -> DV
         consume ~ cp * room_temp

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
fsem1 <- sem(mod1, data = thirst_dat, se = "bootstrap", bootstrap = 1000)


# STEP 6: SUMMARIZE MODEL
# standardized = TRUE adds standardized estimate to the model output.
summary(fsem1, standardized = TRUE)

# Print all model parameters
parameterestimates(fsem1, boot.ci.type = "bca.simple", standardized = TRUE)
help("parameterestimates")
# STEP 7: INTERPRET
#Every 1 degree increase in room temperature was associated with an a = 0.339 (S.E. = 0.099) increase 
#in thirstiness units. 

#Adjusting for room temperature, every 1-unit increase in thirstiness was 
#associated with drinking b = 0.451 (S.E. = 0.152) more deciliters of water.

#Increases in room temperature were associated with increases in water 
#drinking indirectly through increases in thirstiness.
#Specifically, for every a = 0.339 unit increase in the association between room 
#temperature and thirstiness, there was an ab = 0.153 (S.E. = 0.064) increase in deciliters of 
#water people drank. (INDIRECT EFFECT) 
#Importantly, a bias-corrected bootstrapped confidence interval with 1,000 
#samples was above zero, 95% CI [0.06, 0.32]. 

#Last, there was no sufficient evidence that room 
#temperature was associated with how many deciliters of water 
#people drank independent of its association with 
#thirstiness, c' = 0.208 (S.E. = 0.126).

#########################
####PSYCH PACKAGE#######
#########################

## Test same model using psych package
# The syntax for mediation in psych doesn't have as steep a learning curve as lavaan
# but lavaan (and SEM in general) has more flexibility in specifying more involved models.
thirst_dat %>%
  mediate(y = "consume", x = "room_temp", m = "thirst", n.iter = 1000) %>%
  print(short = FALSE)


### Sources
#MacKinnon, D. P. (2008). Introduction to statistical mediation analysis. New York, NY: Lawrence Erlbaum Associates.

#Revelle, W. (2017) How to use the psych package for mediation/moderation/regression analysis.

#Rosseel, Y. (2012). Lavaan: An R package for structural equation modeling and more. Version (BETA). Journal of statistical software, 48(2), 1-36.

#Rucker, D. D., Preacher, K. J., Tormala, Z. L., & Petty, R. E. (2011). Mediation analysis in social psychology: Current practices and new recommendations. Social and Personality Psychology Compass, 5(6), 359-371.

#http://www.stat.cmu.edu/~hseltman/PIER/SemAndLavaan.pdf

#https://nmmichalak.github.io/nicholas_michalak/blog_entries/2018/nrg01/nrg01.html
