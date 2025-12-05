# Author: Andrea Albertazzi

# Strategy estimation for "The Enemy of My Enemy"


install.packages("haven") # needed for importing stata datasets
library(haven)
install.packages("dplyr")
library(dplyr)
install.packages("stratEst")
library(stratEst)


setwd("your_path/strategy_estimation_manuscript")




##################################################################
#######################   CREATE DATA   ##########################
##################################################################
data_for_strategies <- read.table("your_path/data_for_strategies.txt", 
                   header = TRUE,    # first row has variable names
                   sep = "\t")       # tab-separated

data.SGA <- stratEst.data(data = data_for_strategies, choice = "choice",
                             input = c("choice","other_choice"),
                             input.lag = 1)

# Convert treatment to factor
data.SGA$treatment <- as.factor(data.SGA$treatment)
##################################################################
##################################################################


##################################################################
#####################   CREATE STRATEGIES   ######################
##################################################################
# Define 2TFT (2-Tits-for-Tat: defect if opponent defected in either of last 2 rounds)
twoTFT <- stratEst.strategy(choices = c("d","c"),
                            inputs  = c("cc","cd","dc","dd"),
                            num.states = 3,
                            prob.choices = c(0,1,  1,0,  1,0),
                            tr.inputs = c(
                              1, 2, 1, 2,
                              3, 2, 3, 2,
                              1, 2, 1, 2))

# Define Threshold strategies
#TH2 (cooperate first two rounds, then defect forever)
TH2 <- stratEst.strategy(choices = c("d","c"),
                         inputs = c("cc","cd","dc","dd"),
                         num.states = 3,
                         prob.choices = c(0,1, 0,1, 1,0),
                         tr.inputs = c(
                           2, 2, 2, 2,
                           3, 3, 3, 3,
                           3, 3, 3, 3))

#TH3 (cooperate first three rounds, then defect forever)
TH3 <- stratEst.strategy(choices = c("d","c"),
                         inputs = c("cc","cd","dc","dd"),
                         num.states = 4,
                         prob.choices = c(0,1, 0,1, 0,1, 1,0),
                         tr.inputs = c(
                           2, 2, 2, 2,
                           3, 3, 3, 3,
                           4, 4, 4, 4,
                           4, 4, 4, 4))

#TH4 (cooperate first four rounds, then defect forever)
TH4 <- stratEst.strategy(choices = c("d","c"),
                         inputs = c("cc","cd","dc","dd"),
                         num.states = 5,
                         prob.choices = c(0,1, 0,1, 0,1, 0,1, 1,0),
                         tr.inputs = c(
                           2, 2, 2, 2,
                           3, 3, 3, 3,
                           4, 4, 4, 4,
                           5, 5, 5, 5,
                           5, 5, 5, 5))

#TH5 (cooperate first five rounds, then defect forever)
TH5 <- stratEst.strategy(choices = c("d","c"),
                         inputs = c("cc","cd","dc","dd"),
                         num.states = 6,
                         prob.choices = c(0,1, 0,1, 0,1, 0,1, 0,1, 1,0),
                         tr.inputs = c(
                           2, 2, 2, 2,
                           3, 3, 3, 3,
                           4, 4, 4, 4,
                           5, 5, 5, 5,
                           6, 6, 6, 6,
                           6, 6, 6, 6))

#TH6 (cooperate first six rounds, then defect forever)
TH6 <- stratEst.strategy(choices = c("d","c"),
                         inputs = c("cc","cd","dc","dd"),
                         num.states = 7,
                         prob.choices = c(0,1, 0,1, 0,1, 0,1, 0,1, 0,1, 1,0),
                         tr.inputs = c(
                           2, 2, 2, 2,
                           3, 3, 3, 3,
                           4, 4, 4, 4,
                           5, 5, 5, 5,
                           6, 6, 6, 6,
                           7, 7, 7, 7,
                           7, 7, 7, 7))

#TH7 (cooperate first seven rounds, then defect forever)
TH7 <- stratEst.strategy(choices = c("d","c"),
                         inputs = c("cc","cd","dc","dd"),
                         num.states = 8,
                         prob.choices = c(0,1, 0,1, 0,1, 0,1, 0,1, 0,1, 0,1, 1,0),
                         tr.inputs = c(
                           2, 2, 2, 2,
                           3, 3, 3, 3,
                           4, 4, 4, 4,
                           5, 5, 5, 5,
                           6, 6, 6, 6,
                           7, 7, 7, 7,
                           8, 8, 8, 8,
                           8, 8, 8, 8))

#TH8 (cooperate first seven rounds, then defect forever)
TH8 <- stratEst.strategy(choices = c("d","c"),
                         inputs = c("cc","cd","dc","dd"),
                         num.states = 9,
                         prob.choices = c(0,1, 0,1, 0,1, 0,1, 0,1, 0,1, 0,1, 0,1, 1,0),
                         tr.inputs = c(
                           2, 2, 2, 2,
                           3, 3, 3, 3,
                           4, 4, 4, 4,
                           5, 5, 5, 5,
                           6, 6, 6, 6,
                           7, 7, 7, 7,
                           8, 8, 8, 8,
                           9, 9, 9, 9,
                           9, 9, 9, 9))


my.strategies <- c(strategies.DF2011, list("twoTFT" = twoTFT,
                                           "TH2" = TH2,
                                           "TH3" = TH3,
                                           "TH4" = TH4,
                                           "TH5" = TH5,
                                           "TH6" = TH6,
                                           "TH7" = TH7,
                                           "TH8" = TH8,
                                           DTFT  = strategies.FRD2012$DTFT,
                                           GRIM2 = strategies.FRD2012$GRIM2,
                                           GRIM3 = strategies.FRD2012$GRIM3,
                                           TF2T  = strategies.FRD2012$TF2T))
my.strategies$T2 <- NULL
names(my.strategies)[names(my.strategies) == "DTFT"] <- "STFT"
names(my.strategies)[names(my.strategies) == "ALLC"] <- "AC"
names(my.strategies)[names(my.strategies) == "ALLD"] <- "AD"
#####################################################################
#####################################################################






set.seed(123456789)

##################################################################
########################   ESTIMATIONS   #########################
##################################################################



######   SUPERGAMES 20-29   ####
# Estimate shares from FULL SET
model.full_set <- stratEst.model(
  data = data.SGA,
  strategies = my.strategies,
  sample.id = "treatment",
  se = "bootstrap",             # get standard errors from nonparametric block-bootstrap
  bs.samples = 10000,           # permutations
  quantiles = c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995),
  verbose = TRUE,
)

print(round(do.call(rbind, model.full_set$shares), 3))  # point estimates
print(round(model.full_set$shares.se, 3))              # SEs
print(round(model.full_set$shares.quantiles, 3))      # bootstrap quantiles
print(round(model.full_set$trembles, 3))
print(round(model.full_set$trembles.se, 3))
print(round(model.full_set$trembles.quantiles, 3)) 
stratEst.check(model.full_set, chi.tests=TRUE, verbose = TRUE, bs.samples=10000)  #returns log likelihood of the model, number of free model parameters, and values of three information criteria.
##################################################################


###############################################
# Estimate shares selecting strategies 
model.selected <- stratEst.model(
  data = data.SGA,
  strategies = my.strategies,
  select = "strategies",       # select strategies that best explain the data
  crit = "bic",                # according to Bayesian information criterion
  sample.id = "treatment",
  se = "bootstrap",             # get standard errors from nonparametric block-bootstrap
  bs.samples = 10000,           # permutations
  quantiles = c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995),
  verbose = TRUE,
)
print(round(do.call(rbind, model.selected$shares), 3))  # point estimates
print(round(model.selected$shares.se, 3))              # SEs
print(round(model.selected$shares.quantiles, 3))      # bootstrap quantiles
print(round(model.selected$trembles, 3))
print(round(model.selected$trembles.se, 3))
print(round(model.selected$trembles.quantiles, 3))
stratEst.check(model.selected, chi.tests=TRUE, verbose = TRUE, bs.samples=10000)  #returns log likelihood of the model, number of free model parameters, and values of three information criteria.
##################################################################





######   ALL SUPERGAMES   ####
#########################################################################
all_data_for_strategies <- read.table("C:/Users/tazze/Dropbox/Work/SAG_Enemy/Enemy_Andrea/Andrea_analisi/strategy_estimation_manuscript/all_data_for_strategies.txt", 
                                  header = TRUE,    # first row has variable names
                                  sep = "\t")       # tab-separated
all.data.SGA <- stratEst.data(data = all_data_for_strategies, choice = "choice",
                          input = c("choice","other_choice"),
                          input.lag = 1)
# Convert treatment to factor
all.data.SGA$treatment <- as.factor(all.data.SGA$treatment)



# Estimate shares from FULL SET
model.all_decisions <- stratEst.model(
  data = all.data.SGA,
  strategies = my.strategies,
  sample.id = "treatment",
  se = "bootstrap",             # get standard errors from nonparametric block-bootstrap
  bs.samples = 10000,           # permutations
  quantiles = c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995),
  verbose = TRUE,
)
print(round(do.call(rbind, model.all_decisions$shares), 3))  # point estimates
print(round(model.all_decisions$shares.se, 3))              # SEs
print(round(model.all_decisions$shares.quantiles, 3))      # bootstrap quantiles
print(round(model.all_decisions$trembles, 3))
print(round(model.all_decisions$trembles.se, 3))
stratEst.check(model.all_decisions, chi.tests=TRUE, verbose = TRUE, bs.samples=10000)  #returns log likelihood of the model, number of free model parameters, and values of three information criteria.



# Estimate shares selecting strategies (BIC)
model.selected.all.decision <- stratEst.model(
  data = all.data.SGA,
  strategies = my.strategies,
  select = "strategies",       # select strategies that best explain the data
  crit = "bic",                # according to Bayesian information criterion
  sample.id = "treatment",
  se = "bootstrap",             # get standard errors from nonparametric block-bootstrap
  bs.samples = 10000,           # permutations
  quantiles = c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995),
  verbose = TRUE,
)
print(round(do.call(rbind, model.selected.all.decision$shares), 3))  # point estimates
print(round(model.selected.all.decision$shares.se, 3))              # SEs
print(round(model.selected.all.decision$shares.quantiles, 3))      # bootstrap quantiles
print(round(model.selected.all.decision$trembles, 3))
print(round(model.selected.all.decision$trembles.se, 3))
stratEst.check(model.selected.all.decision, chi.tests=TRUE, verbose = TRUE, bs.samples=10000)  #returns log likelihood of the model, number of free model parameters, and values of three information criteria.




######  WITH TREMBLES AT THE STRATEGY LEVEL FOR SELECTED STRATEGIES  ####

selected.strategies <- c(my.strategies)
selected.strategies[c("AC", "TF2T", "twoTFT", "WSLS", "TH4", "TH5", "TH6", "TH7", "TH8")] <- NULL

set.seed(123456789)

selected.with.trembles <- stratEst.model(
  data = data.SGA,
  strategies = selected.strategies,
  sample.id = "treatment",
  sample.specific = c("shares","trembles"),
  r.trembles = "strategies",
  se = "bootstrap",             # get standard errors from nonparametric block-bootstrap
  bs.samples = 10000,           # permutations
  quantiles = c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995),
  verbose = TRUE,
)
print(round(do.call(rbind, selected.with.trembles$shares), 3))  # point estimates
print(round(selected.with.trembles$shares.se, 3))              # SEs
print(round(selected.with.trembles$shares.quantiles, 3))      # bootstrap quantiles
print(round(selected.with.trembles$trembles, 3))
print(round(selected.with.trembles$trembles.se, 3))
stratEst.check(selected.with.trembles, chi.tests=TRUE, verbose = TRUE, bs.samples=10000)  #returns log likelihood of the model, number of free model parameters, and values of three information criteria.
