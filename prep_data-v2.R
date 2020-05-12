# LAST EDITED 12/5/20
# subsets a dataframe that excludes those who did not use colour at above-chance level in the instructed blocks
 
# load(file="pu2dkids_data.RData") # load the first version of data, with 9 blocks for exp 1 & 2
# DATA_old <- DATA
# Qdata_old <- Qdata
# Sdata_old <- Sdata
# Sdata2_old <- Sdata2

load("pu2dkids_data-all.Rdata") # load current version, with 10 blocks for exp 1, 9 blocks for exp 2

# starting number
library(tidyverse)

preexc <- tapply(DATA$id, list(DATA$agegroup, DATA$taskV), function(x) length(unique(x)))
preexc
## counting no of unique IDs in each combination of age group and task V
## there are 28 kids in V1, 28 kids in V4 (experiments 1 and 2 respectively) before exclusions
## 22 adults in V1, 21 adults in V4

##############################################################################
##           ``  EXCLUSION CRITERIA BASED ON PERFORMANCE                    ## 
##############################################################################


#### Those who didn't switch in instructed block                          ####

## experiment 1
DATA_V1 <- DATA %>% filter(taskV == 'V1')

follthresh_v1 = qbinom(0.95, 64, prob = 0.5)/64 # 32 ambiguous trials,in the instructed block, 2 instr blocks


tmp = tapply(DATA_V1$followed, list(DATA_V1$id, DATA_V1$block, DATA_V1$cond, DATA_V1$respkey %in% c(77,88,188)), mean, na.rm = TRUE)[,9:10,2,'TRUE']
tmp = apply(tmp, 1, mean) # applies mean to all elements of each row

exclids1.1 = which(tmp < follthresh_v1)

table(tmp<follthresh_v1, tapply(DATA_V1$agegroup, list(DATA_V1$id), unique)) ## TRUE: kids 6, YA 1 to be excluded in exp 1

## experiment 2
DATA_V4 <- DATA %>% filter(taskV == 'V4')

follthresh_v4 = qbinom(0.95, 32, prob = 0.5)/32 #32 ambiguous trials,in the instructed block, 2 instr blocks
tmp = tapply(DATA_V4$followed, list(DATA_V4$id, DATA_V4$block, DATA_V4$cond, DATA_V4$respkey %in% c(77, 88, 188)), mean, na.rm = TRUE)[,9,2,'TRUE']
exclids1.2 = which(tmp < follthresh_v4)

table(tmp<follthresh_v4, tapply(DATA_V4$agegroup, list(DATA_V4$id), unique)) ## TRUE: kids 3, YA 3 to be excluded in exp 2


####             Total exlusions based on performance             ####

length(exclids1.1) # exp1, excluded because of instructed block
length(exclids1.2) # exp2, excluded because of instructed block

exclidsall = unique(names(c(exclids1.1, exclids1.2))) #ids of participants excluded

#### OUTPUT

DATA = subset(DATA, !(as.double(DATA$id) %in% as.double(exclidsall))) #subset data observations that ARE NOT the participants in exclusion list
DATA = subset(DATA, DATA$respkey %in% c(0, 77, 88, 188))
LEARN = subset(DATA, DATA$error==0)

postexc <- tapply(DATA$id, list(DATA$agegroup, DATA$taskV), function(x) length(unique(x)))

#### get all ids in data set and clean up 


ids = as.integer(levels(as.factor(LEARN$id)))
nids = length(ids)
kidids = tapply(DATA$id, DATA$agegroup, unique)$KIDS
nkids = length(kidids)
kidids_v1 = tapply(DATA$id, list(DATA$taskV, DATA$agegroup), unique)[['V1', 'KIDS']]
nkids_v1 = length(kidids_v1)
kidids_v4 = tapply(DATA$id, list(DATA$taskV, DATA$agegroup), unique)[['V4', 'KIDS']]
nkids_v4 = length(kidids_v4)
adultids = tapply(DATA$id, DATA$agegroup, unique)$YA
nadults = length(adultids)
adultids_v1 = tapply(DATA$id, list(DATA$taskV, DATA$agegroup), unique)[['V1', 'YA']]
nadults_v1 = length(adultids_v1)
adultids_v4 = tapply(DATA$id, list(DATA$taskV, DATA$agegroup), unique)[['V4', 'YA']]
nadults_v4 = length(adultids_v4)

# ids of exp 1 and exp 2 participants
ids_v1 = c(kidids_v1, adultids_v1)
ids_v4 = c(kidids_v4, adultids_v4)

trialspercond = apply(tapply(DATA$error, list(DATA$id, DATA$block, DATA$cond), length), c(1, 3), mean, na.rm = TRUE)

Qdata = subset(Qdata, Qdata$id %in% ids)
Sdata = subset(Sdata, Sdata$id %in% ids)
Sdata2 = subset(Sdata2, Sdata2$id %in% ids)
ages = tapply(DATA$age, DATA$id, mean)
expv = tapply(DATA$taskV, DATA$id, unique)

kidids2 = c(kidids, kidids_v1)


stroopids = c(tapply(Sdata$id, Sdata$id, function(x) unique(x)))
questids = tapply(Qdata$id, Qdata$id, function(x) unique(x))

#### calcualte some useful tables 

followed = tapply(DATA$followed, list(DATA$id, DATA$block, DATA$taskV, DATA$agegroup, DATA$cond, DATA$respkey != 0), mean, na.rm = TRUE)[,,,,2, 'TRUE']*100

followed_split = tapply(DATA$followed, list(DATA$id, DATA$block, DATA$agegroup, DATA$btrial>90, DATA$cond, DATA$respkey != 0), mean, na.rm = TRUE)[,,,,2, 'TRUE']*100

errors = tapply(DATA$error>0, list(DATA$id, DATA$block, DATA$taskV, DATA$agegroup, DATA$cond), mean, na.rm = TRUE)[,,,,1]*100
errors_split = tapply(DATA$error>0, list(DATA$id, DATA$block, DATA$agegroup, DATA$btrial>90, DATA$cond), mean, na.rm = TRUE)[,,,,1]*100
rts = tapply(LEARN$rt, list(LEARN$id, LEARN$block, LEARN$taskV, LEARN$agegroup, LEARN$cond), median, na.rm = TRUE)[,,,,1]
prekeys_late = tapply(DATA$prekey1>0, list(DATA$id, DATA$block, DATA$taskV, DATA$agegroup, DATA$cond), mean, na.rm = TRUE)[,,,,3]*100
prekeys_late_split = tapply(DATA$prekey1>0, list(DATA$id, DATA$block, DATA$agegroup, DATA$btrial>90, DATA$cond), mean, na.rm = TRUE)[,,,,3]*100

prekeys_noGo = tapply(DATA$prekey1>0, list(DATA$id, DATA$block, DATA$taskV, DATA$agegroup, DATA$cond), mean, na.rm = TRUE)[,,,,4]*100
ambiguity_costs = 
tapply(LEARN$rt, list(LEARN$id, LEARN$block, LEARN$taskV, LEARN$agegroup, LEARN$cond, LEARN$respkey != 0 ), median, na.rm = TRUE)[,,,,2,'TRUE'] - 
tapply(LEARN$rt, list(LEARN$id, LEARN$block, LEARN$taskV, LEARN$agegroup, LEARN$cond), median, na.rm = TRUE)[,,,,1]
congruency_costs = tapply(LEARN$rt, list(LEARN$id, LEARN$block, LEARN$taskV, LEARN$agegroup, LEARN$dimA, LEARN$cond), median, na.rm = TRUE)[,,,,,1]
congruency_costs = (congruency_costs[,,,,1] + congruency_costs[,,,,3])/2 - (congruency_costs[,,,,2] + congruency_costs[,,,,4])/2

allfoll = as.vector(na.omit(as.vector(apply(followed[c(kidids_v1, kidids_v4, adultids_v1, adultids_v4),7:8,,], c(1, 3, 4), mean))))
allerrs = as.vector(na.omit(as.vector(apply(errors[c(kidids_v1, kidids_v4, adultids_v1, adultids_v4),7:8,,], c(1, 3, 4), mean))))
allrts = as.vector(na.omit(as.vector(apply(rts[c(kidids_v1, kidids_v4, adultids_v1, adultids_v4),7:8,,], c(1, 3, 4), mean))))
allprelate = as.vector(na.omit(as.vector(apply(prekeys_late[c(kidids_v1, kidids_v4, adultids_v1, adultids_v4),7:8,,], c(1, 3, 4), mean))))
allpreno = as.vector(na.omit(as.vector(apply(prekeys_noGo[c(kidids_v1, kidids_v4, adultids_v1, adultids_v4),7:8,,], c(1, 3, 4), mean))))

allambigcosts = as.vector(na.omit(as.vector(apply(ambiguity_costs[c(kidids_v1, kidids_v4, adultids_v1, adultids_v4),7:8,,], c(1, 3, 4), mean))))
allcongcosts = as.vector(na.omit(as.vector(apply(congruency_costs[c(kidids_v1, kidids_v4, adultids_v1, adultids_v4),7:8,,], c(1, 3, 4), mean))))

names(allfoll) = names(allrts) = names(allerrs) = names(allprelate) = names(allpreno) = names(allambigcosts) = names(allcongcosts) = c(kidids_v1, kidids_v4, adultids_v1, adultids_v4)
group =   c(rep('CHN Exp.1', nkids_v1), rep('CHN Exp.2', nkids_v4), rep('ADLT Exp.1', nadults_v1), rep('ADLT Exp.2', nadults_v4))
