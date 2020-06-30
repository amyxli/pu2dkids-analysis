# LAST EDITED 30/6/20, AXL
# No plots, changed to be based on NS's code

## parts referenced in manuscript have been commented "(manuscript)"

# here, exp 1 and 2 are analysed separately

## load packages 
packages = c('car','sfsmisc', 'RColorBrewer', 'gdata', 'beeswarm', 'lme4', 'emmeans', 'pwr', 'tidyverse', 'lmerTest', 'MASS')
pksload = unlist(lapply(packages, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

if(sum(pksload) != length(packages)) {
  warning('SOME PACKAGES NOT LOADED')
}

#load some custom functions 
source('gen_functions-v1.R')

### settings for lmer
lcctrl = lmerControl(optimizer=c('bobyqa'), optCtrl = list(maxfun = 500000))

# colors for plotting 
colorset = rbind(brewer.pal(6, 'Blues')[c(3,6)], brewer.pal(6, 'Reds')[c(3, 6)]) 

# load data 


# note on condition coding: 
# 1 = standard
# 2 = ambiguous
# 3 = delayed Go 
# 4 = delayed noGo

# prep data 
source('prep_data-v2.R')

# dfs for exp 1 and 2, overwrite pre-exclusion dfs
DATA_V1 <- DATA %>% filter(taskV == 'V1')
DATA_V4 <- DATA %>% filter(taskV == 'V4')

####### overview of number of subjects in different experiments 


####                          without excluded participants                 #### 

# adults stats 

# mean age
mean((DATA %>% filter(agegroup=="YA") %>% dplyr::select(c(age, id, taskV)) %>% unique())$age)

#... in exp 1
mean((DATA %>% filter(agegroup=="YA") %>% dplyr::select(c(age, id, taskV)) %>% unique() %>% filter(taskV == "V1"))$age)
#... in exp 2
mean((DATA %>% filter(agegroup=="YA") %>% dplyr::select(c(age, id, taskV)) %>% unique() %>% filter(taskV == "V4"))$age)

# kids stats

# mean age
mean((DATA %>% filter(agegroup=="KIDS") %>% dplyr::select(c(age, id, taskV)) %>% unique())
     $age)

#... in exp 1
mean((DATA %>% filter(agegroup=="KIDS") %>% dplyr::select(c(age, id, taskV)) %>% unique() %>% 
        filter(taskV == "V1"))$age)
#... in exp 2
mean((DATA %>% filter(agegroup=="KIDS") %>% dplyr::select(c(age, id, taskV)) %>% unique() %>% 
        filter(taskV == "V4"))
     $age)

ages = tapply(DATA$age, DATA$id, mean)

##########################################################################################
#  Data frames

regular.cdf = data.frame(
  E = as.vector(errors), 
  RT = as.vector(rts),
  ID = rep(rownames(rts), prod(dim(rts)[2:4])),
  BLOCK = rep(as.numeric(colnames(rts)), each = dim(rts)[1], prod(dim(rts)[3:4])),
  EXP = rep(dimnames(rts)[[3]], each = prod(dim(rts)[1:2]), dim(rts)[4]), 
  GROUP = rep(dimnames(rts)[[4]], each = prod(dim(rts)[1:3])))

regular.cdf = subset(regular.cdf, !is.na(regular.cdf$E)) %>% arrange(ID)

regular.cdf$COND = NA
regular.cdf$COND[regular.cdf$BLOCK < 9] = 'LEARN' 
regular.cdf$COND[regular.cdf$BLOCK >= 9] = 'INSTR'
regular.cdf$COND = as.factor(regular.cdf$COND)

conflicts.cdf = data.frame(
  PREKEY_late = as.vector(prekeys_late), 
  PREKEY_NoGo = as.vector(prekeys_noGo),
  AMBIG_COSTS = as.vector(ambiguity_costs),
  CONG_COSTS = as.vector(congruency_costs),
  ID = rep(rownames(prekeys_late), prod(dim(prekeys_late)[2:4])),
  BLOCK = rep(as.numeric(colnames(prekeys_late)), each = dim(prekeys_late)[1], prod(dim(prekeys_late)[3:4])),
  EXP = rep(dimnames(prekeys_late)[[3]], each = prod(dim(prekeys_late)[1:2]), dim(prekeys_late)[4]), 
  GROUP = rep(dimnames(prekeys_late)[[4]], each = prod(dim(prekeys_late)[1:3])))

conflicts.cdf = subset(conflicts.cdf, !is.na(conflicts.cdf$PREKEY_late))
conflicts.cdf$COND = NA
conflicts.cdf$COND[conflicts.cdf$BLOCK < 9] = 'LEARN'
conflicts.cdf$COND[conflicts.cdf$BLOCK >= 9] = 'INSTR'
conflicts.cdf$COND = as.factor(conflicts.cdf$COND)

cthresh = qbinom(0.95, 64, prob = 0.5)/64 
tmp = tapply(DATA$followed, list(DATA$id, DATA$cond, DATA$block>6 & DATA$block<9, DATA$respkey %in% c(77, 88, 188)), mean, na.rm = TRUE)[,2,'TRUE','TRUE']

switchids = which(tmp > cthresh)
nswitchids = which(tmp <= cthresh)
nswitchers = length(switchids)

ambiguous.cdf = data.frame(
  COLOR = as.vector(followed), 
  ID = rep(rownames(followed), prod(dim(followed)[2:4])),
  AGE = rep(ages, prod(dim(followed)[2:4])),
  SWITCH = rep(rownames(followed) %in% names(switchids), prod(dim(followed)[2:4])),
  BLOCK = rep(as.numeric(colnames(followed)), each = dim(followed)[1], prod(dim(followed)[3:4])),
  EXP = rep(dimnames(followed)[[3]], each = prod(dim(followed)[1:2]), dim(followed)[4]), 
  GROUP = rep(dimnames(followed)[[4]], each = prod(dim(followed)[1:3])))


ambiguous.cdf = subset(ambiguous.cdf, !is.na(ambiguous.cdf$COLOR))
ambiguous.cdf$COND = NA
ambiguous.cdf$COND[ambiguous.cdf$BLOCK == 1] = 'RAND'
ambiguous.cdf$COND[ambiguous.cdf$BLOCK < 9 & ambiguous.cdf$BLOCK > 1] = 'LEARN'
ambiguous.cdf$COND[ambiguous.cdf$BLOCK >= 9] = 'INSTR'
ambiguous.cdf$COND = as.factor(ambiguous.cdf$COND)

#### SWITCH POINT ALIGNED 
cthresh = qbinom(0.95, 64, prob = 0.5)/64 
tmp = tapply(DATA$followed, list(DATA$id, DATA$cond, DATA$block>6 & DATA$block<9, DATA$respkey %in% c(77, 88, 188)), mean, na.rm = TRUE)[,2,'TRUE','TRUE']

switchids = which(tmp > cthresh)
nswitchids = which(tmp <= cthresh)
nswitchers = length(switchids)
nkidswitchers = length(which(ids %in% names(switchids) & ids %in% kidids))
nadultswitchers = length(which(ids %in% names(switchids) & ids %in% adultids))


tmp_switch = tapply(DATA$followed, list(DATA$id, DATA$miniblock, DATA$cond, DATA$respkey %in% c(0:200)), mean, na.rm = TRUE)[,,2, 'TRUE']
tmp_switch = tmp_switch[,1:16]
tmp = t(apply(tmp_switch - matrix(rowMeans(tmp_switch, na.rm = TRUE), ncol = 16, nrow = nids), 1, FUN = function(x) cumsum(x)))


switchpoints = apply(tmp, 1, FUN = function(x) {which.min(x)})
switchpoints[!(ids %in% names(switchids))] = sample(switchpoints[(ids %in% names(switchids))], nids- nswitchers, TRUE)

adult_switch = switchpoints[which(ids %in% names(switchids) & ids %in% adultids)]
kid_switch = switchpoints[which(ids %in% names(switchids) & ids %in% kidids)]


tmp_switch2 = cbind(NA, NA, NA, NA, NA, NA, NA, NA, tmp_switch, NA, NA, NA, NA, NA, NA, NA, NA)
switchpoints2 = switchpoints + 7
follow_aligned = sapply(1:nids, function(x) tmp_switch2[x, (switchpoints2[x]-5):(switchpoints2[x] + 7)])*100
follow_aligned_mean = follow_aligned_ses = matrix(NA, 4, 13)
follow_aligned_mean[1,] = apply(follow_aligned[,which(ids %in% names(switchids) & ids %in% kidids)], 1, mean, na.rm = TRUE)
follow_aligned_mean[2,] = apply(follow_aligned[,which(ids %in% names(nswitchids) & ids %in% kidids)], 1, mean, na.rm = TRUE)
follow_aligned_mean[3,] = apply(follow_aligned[,which(ids %in% names(switchids) & ids %in% adultids)], 1, mean, na.rm = TRUE)
follow_aligned_mean[4,] = apply(follow_aligned[,which(ids %in% names(nswitchids) & ids %in% adultids)], 1, mean, na.rm = TRUE)

follow_aligned_ses[1,] = apply(follow_aligned[,which(ids %in% names(switchids) & ids %in% kidids)], 1, std.error, na.rm = TRUE)
follow_aligned_ses[2,] = apply(follow_aligned[,which(ids %in% names(nswitchids) & ids %in% kidids)], 1, std.error, na.rm = TRUE)
follow_aligned_ses[3,] = apply(follow_aligned[,which(ids %in% names(switchids) & ids %in% adultids)], 1, std.error, na.rm = TRUE)
follow_aligned_ses[4,] = apply(follow_aligned[,which(ids %in% names(nswitchids) & ids %in% adultids)], 1, std.error, na.rm = TRUE)

follow_aligned_mean = follow_aligned_mean[,2:13]
follow_aligned_ses = follow_aligned_ses[,2:13]

follow_aligned = t(follow_aligned)
follow_aligned = follow_aligned[,2:13]
#follow_aligned[,4:9]
rownames(follow_aligned) = ids
colnames(follow_aligned) = c(-6:-1, 1:6)

switchpoint.cdf = data.frame(COLOR = as.vector(follow_aligned), 
                             ID = rep(rownames(follow_aligned), prod(dim(follow_aligned)[1:2])),
                             EXP = rep(rownames(follow_aligned), prod(dim(follow_aligned)[1:2])),
                             BLOCK = rep(as.numeric(colnames(follow_aligned)), each = dim(follow_aligned)[1]))


switchpoint.cdf$EXP = NA 
switchpoint.cdf$EXP[switchpoint.cdf$ID %in% c(adultids_v1, kidids_v1)]  = 'V1'
switchpoint.cdf$EXP[switchpoint.cdf$ID %in% c(adultids_v4, kidids_v4)]  = 'V4'

switchpoint.cdf$GROUP = NA 
switchpoint.cdf$GROUP[switchpoint.cdf$ID %in% kidids] = 'KIDS'
switchpoint.cdf$GROUP[switchpoint.cdf$ID %in% adultids] = 'YA'
switchpoint.cdf$GROUP = as.factor(switchpoint.cdf$GROUP)

switchpoint.cdf$AFTER = NA
switchpoint.cdf$AFTER[switchpoint.cdf$BLOCK < 0] = 'BEFORE'
switchpoint.cdf$AFTER[switchpoint.cdf$BLOCK > 0] = 'AFTER'

switchpoint.cdf$SWITCHED = NA
switchpoint.cdf$SWITCHED[switchpoint.cdf$ID %in% names(switchids)] = 'SWITCHER'
switchpoint.cdf$SWITCHED[!(switchpoint.cdf$ID %in% names(switchids))] = 'NOSWITCHER'

switchpoint.cdf$BLOCK = as.factor(switchpoint.cdf$BLOCK)
switchpoint.cdf$EXP = as.factor(switchpoint.cdf$EXP)
switchpoint.cdf$AFTER = as.factor(switchpoint.cdf$AFTER)
switchpoint.cdf$SWITCHED = as.factor(switchpoint.cdf$SWITCHED)

switchpoint.cdf = subset(switchpoint.cdf, switchpoint.cdf$BLOCK %in% -3:3)
switchpoint.cdf$BLOCK = droplevels(switchpoint.cdf$BLOCK)

#### QUESTIONNAIRE 
setdiff(ids, questids)
sum(setdiff(ids, questids) %in% kidids)
sum(setdiff(ids, questids) %in% adultids)

recog = recode(Qdata$recognised, 'yes' = 1, 'no' = 0)
names(recog) = Qdata$id

used = recode(Qdata$used, 'yes' = 1, 'no' = 0)
names(used) = Qdata$id

ctab = cbind(Qdata$left.up, Qdata$right.down, Qdata$left.down, Qdata$right.up) - 2

Qdata$correctreport = NA
Qdata$correctreport[Qdata$id %% 2 == 0] = apply(ctab[Qdata$id %% 2 == 0,], 1, function(x) mean(x == c(1, 1, 2, 2)) == 1)
Qdata$correctreport[Qdata$id %% 2 == 1] = apply(ctab[Qdata$id %% 2 == 1,], 1, function(x) mean(x == c(2, 2, 1, 1)) == 1)


wmscore = Qdata$wm.score
names(wmscore) = Qdata$id
stroop_rts = tapply(Sdata$resp.rt, list(Sdata$id, Sdata$congruent, Sdata$resp.corr, is.na(Sdata$trials.thisN)), mean)[,,'1', 'FALSE']*1000 
stroop_errors = 100 - tapply(Sdata$resp.corr, list(Sdata$id, Sdata$congruent, is.na(Sdata$trials.thisN)), mean, na.rm = TRUE)[,,'FALSE']*100 
stroop2_rts = tapply(Sdata2$Stimulus.RT, list(Sdata2$id, Sdata2$cond, Sdata2$Stimulus.ACC, Sdata2$Procedure.Block.), mean)[,,'0', 'BlockProc']
stroop2_errors = tapply(Sdata2$Stimulus.ACC, list(Sdata2$id, Sdata2$cond, Sdata2$Procedure.Block.), mean)[,, 'BlockProc']*100

stroop_rt_score = c(stroop_rts[,'neutral'] - stroop_rts[,'cong'], stroop2_rts[,'neutral'] - stroop2_rts[,'cong'])
stroop_error_score = c(stroop_errors[,'neutral'] - stroop_errors[,'cong'], stroop2_errors[,'neutral'] - stroop2_errors[,'cong'])
stroop_rt_score_interfere = c(stroop_rts[,'neutral'] - stroop_rts[,'incong'], stroop2_rts[,'neutral'] - stroop2_rts[,'incong'])

scores.cdf = data.frame(
  ID = names(allerrs), 
  WM = NA, 
  STROOP = NA, 
  E = allerrs, 
  RT = allrts, 
  PRELATE = allprelate, 
  PRENO = allpreno, 
  CONG = allcongcosts, 
  AMBIG = allambigcosts, 
  FOLL = allfoll, 
  FOLLbin = allfoll > (qbinom(0.95, 64, prob = 0.5)/64)*100, 
  RECOG = NA, 
  USED = NA, 
  GROUP = group)

WMmap = sapply(1:length(scores.cdf$ID), function(x) which(scores.cdf$ID[x] == names(wmscore)))
WMmap[unlist(lapply(WMmap, function(x) length(x)==0))] = NA
WMmap = unlist(WMmap)
scores.cdf$WM = wmscore[WMmap]

Smap = sapply(1:length(scores.cdf$ID), function(x) which(scores.cdf$ID[x] == names(stroop_rt_score)))
Smap[unlist(lapply(Smap, function(x) length(x)==0))] = NA
Smap = unlist(Smap)
scores.cdf$STROOP = stroop_rt_score[Smap]
scores.cdf$STROOP_INTER = stroop_rt_score_interfere[Smap]

Rmap = sapply(1:length(scores.cdf$ID), function(x) which(scores.cdf$ID[x] == names(recog)))

Rmap[unlist(lapply(Rmap, function(x) length(x)==0))] = NA
Rmap = unlist(Rmap)
scores.cdf$RECOG = recog[Rmap]

Umap = sapply(1:length(scores.cdf$ID), function(x) which(scores.cdf$ID[x] == names(used)))
Umap[unlist(lapply(Umap, function(x) length(x)==0))] = NA
Umap = unlist(Umap)
scores.cdf$USED = used[Umap]

CORRECTmap = sapply(1:length(scores.cdf$ID), function(x) which(scores.cdf$ID[x] == Qdata$id))
CORRECTmap[unlist(lapply(CORRECTmap, function(x) length(x)==0))] = NA
CORRECTmap = unlist(CORRECTmap)
scores.cdf$CORRECT = Qdata$correctreport[CORRECTmap]


scores.cdf$GROUPbin = NA
scores.cdf$GROUPbin[scores.cdf$GROUP == 'CHN Exp.1' | scores.cdf$GROUP == 'CHN Exp.2'] = 'KIDS'
scores.cdf$GROUPbin[scores.cdf$GROUP == 'ADLT Exp.1' | scores.cdf$GROUP == 'ADLT Exp.2'] = 'YA'
scores.cdf$GROUPbin = as.factor(scores.cdf$GROUPbin)

for (cvar in names(scores.cdf)[2:13]) {
  cvarz = paste(cvar, 'z', sep = '')
  scores.cdf[cvarz] = -as.vector(scale(scores.cdf[cvar]))
}

# recode vars where lower is 'better' (sign flip to all already above)
scores.cdf$FOLLz = -scores.cdf$FOLLz
scores.cdf$FOLLbinz = -scores.cdf$FOLLbinz
scores.cdf$WMz = -scores.cdf$WMz
scores.cdf$RECOGz = -scores.cdf$RECOGz
scores.cdf$USEDz = -scores.cdf$USEDz

scores.cdf$EXP = NA

scores.cdf$EXP[scores.cdf$ID %in% adultids_v1] = 'V1'
scores.cdf$EXP[scores.cdf$ID %in% kidids_v1] = 'V1'
scores.cdf$EXP[scores.cdf$ID %in% adultids_v4] = 'V4'
scores.cdf$EXP[scores.cdf$ID %in% kidids_v4] = 'V4'

scores.cdf$SWITCHED = ifelse(scores.cdf$ID %in% names(switchids), 1, 0)

######################## OVERALL ERRORS ---- ####################

clme = lmer(E ~ BLOCK*GROUP*EXP + (1+BLOCK|ID), data = regular.cdf, control = lcctrl, REML = FALSE)
# error as response variable
# random slopes and intercepts
# the term (1+BLOCK|ID) generates a vector-valued random effect (intercept and slope) for each of the n levels of the subject factor
summary(clme)

Anova(clme)
# BLOCK, GROUP SIG
emmeans(clme, list(pairwise ~ GROUP:EXP), adjust = "tukey")

# exp 1 (manuscript)
clme = lmer(E ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)


# exp 2 (manuscript)
clme = lmer(E ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)

emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey") 

# comparing exp 1 and 2
clme = lmer(E ~ BLOCK*GROUP*EXP + (1+BLOCK|ID), data = regular.cdf, control = lcctrl, REML = FALSE)
summary(clme)

###########  ERRORS IN BLOCKS 7 and 8 ----

clme = lmer(E ~ GROUP*EXP + (1|EXP), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
Anova(clme) # GROUP and EXP* sig
# summary(clme)
emmeans(clme, list(pairwise ~ GROUP:EXP), adjust = "tukey") 

# exp 2 (manuscript)
clme = lmer(E ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$BLOCK > 6 & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme) # GROUP 
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

tmp = apply(errors, c(2, 3, 4), mean, na.rm = TRUE) 
tmp.se = apply(errors, c(2, 3, 4), std.error)

# errors: test for interaction in pre/post instructions

## EXP 1 (manuscript)
clme = lmer(E ~ GROUP*COND + (1 + COND|ID), data = subset(regular.cdf, regular.cdf$BLOCK > 6 & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ COND | GROUP), adjust = "tukey")

## EXP 2 (manuscript)
clme = lmer(E ~ GROUP*COND + (1 + COND|ID), data = subset(regular.cdf, regular.cdf$BLOCK > 6 & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ COND | GROUP), adjust = "tukey")


# exp 1 - compare instructed blocks
t.test(
  regular.cdf$E[regular.cdf$COND == 'INSTR' & regular.cdf$GROUP == 'KIDS' & regular.cdf$EXP == 'V1'], 
  regular.cdf$E[regular.cdf$COND == 'INSTR' & regular.cdf$GROUP == 'YA' & regular.cdf$EXP == 'V1'])

# exp 2 - compare instructed block 9 (manuscript)
t.test(
  regular.cdf$E[regular.cdf$COND == 'INSTR' & regular.cdf$GROUP == 'KIDS' & regular.cdf$EXP == 'V4'], 
  regular.cdf$E[regular.cdf$COND == 'INSTR' & regular.cdf$GROUP == 'YA' & regular.cdf$EXP == 'V4'])


########### REACTION TIME ----

## exp 1: standard trials (manuscript)
clme = lmer(RT ~ BLOCK*GROUP + (1+BLOCK|GROUP/ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
summary(clme)
summary(rePCA(clme)) # take out GROUP as RE

clme = lmer(RT ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
summary(rePCA(clme))
summary(clme)

Anova(clme)


## exp 2: standard trials (manuscript)
clme = lmer(RT ~ BLOCK*GROUP + (1+BLOCK|GROUP/ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
summary(rePCA(clme)) # remove GROUP as RE

clme = lmer(RT ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
summary(rePCA(clme))  # do not remove ID as RE
summary(clme)
Anova(clme)

## exp 2, blocks 7 and 8
clme = lmer(RT ~ GROUP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V4' & regular.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ COND | GROUP), adjust = "tukey")

# RTs: test for interaction in pre/post instructions (manuscript)
## EXP 1
clme = lmer(RT ~ GROUP*COND + (1 + COND|ID), data = subset(regular.cdf, regular.cdf$BLOCK > 6 & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ COND | GROUP), adjust = "tukey")
## EXP 2
clme = lmer(RT ~ GROUP*COND + (1 + COND|ID), data = subset(regular.cdf, regular.cdf$BLOCK > 6 & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ COND | GROUP), adjust = "tukey")



t.test(allrts[kidids_v1], allrts[adultids_v1]) 
t.test(allrts[kidids_v4], allrts[adultids_v4]) 
t.test(allrts[kidids], allrts[adultids]) 
t.test(allrts[kidids_v1], allrts[kidids_v4]) 
t.test(allrts[adultids_v1], allrts[adultids_v4]) 


################################################################################################################################
#                                           task execution measures                                                           ##
################################################################################################################################

# response inhibition

# across both experiments
clme = lmer(PREKEY_late ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN'), control = lcctrl, REML = FALSE)
summary(clme)
Anova(clme) # sig group
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

clme = lmer(PREKEY_NoGo ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN'), control = lcctrl, REML = FALSE)
Anova(clme) # sig group
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

clme = lmer(AMBIG_COSTS ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN'), control = lcctrl, REML = FALSE)
Anova(clme) # sig block
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

clme = lmer(CONG_COSTS ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN'), control = lcctrl, REML = FALSE)
Anova(clme) # sig block
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

## EXP 1 (manuscript) LateGo and NoGo error rates
clme = lmer(PREKEY_late ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN' & conflicts.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")
Anova(clme)

clme = lmer(PREKEY_NoGo ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN' & conflicts.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme) # sig group
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")


## EXP 2 (manuscript) LateGo and NoGo error rates
clme = lmer(PREKEY_late ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(conflicts.cdf, conflicts.cdf$COND == 'LEARN' & conflicts.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")


################################################################################################################################
#                                       alternative strategy discovery                                                        ##
################################################################################################################################


## QUESTIONNAIRE MEASURES

t.test(scores.cdf$RECOG[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'], 
       scores.cdf$RECOG[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])

chisq.test(table(scores.cdf$RECOG[scores.cdf$EXP == 'V1'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V1']))

t.test(scores.cdf$USED[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'], 
       scores.cdf$USED[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])

chisq.test(table(scores.cdf$USED[scores.cdf$EXP == 'V1'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V1']))

t.test(scores.cdf$CORRECT[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'], 
       scores.cdf$CORRECT[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])
chisq.test(table(scores.cdf$CORRECT[scores.cdf$EXP == 'V1'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V1']))

## ----------- COLOUR USE ON AMBIGUOUS TRIALS

# exp 1
clme = lmer(COLOR ~ BLOCK*GROUP + (1+BLOCK|ID), # fit random intercept and random slope for BLOCK with ID as grouping factor
            data = (ambiguous.cdf %>% filter(EXP == "V1" & COND == "LEARN")), 
     control = lcctrl, REML = FALSE) 

summary(clme)
Anova(clme)

# check if including group improves model
clme_nogroup = lmer(COLOR ~ BLOCK + (1+BLOCK|ID), 
                    data = (ambiguous.cdf %>% filter(EXP == "V1" & COND == "LEARN")), 
                    control = lcctrl, REML = FALSE) 

anova(clme,clme_nogroup) # including GROUP does not improve model
summary(clme_nogroup)


# check if BLOCK is reasonable to include as a random effect
summary(rePCA(clme)) # looks like it is!

# exp 2
clme = lmer(COLOR ~ BLOCK*GROUP + (1+BLOCK|ID), data =
            (ambiguous.cdf %>% filter(EXP == "V4" & COND == "LEARN")),
            control = lcctrl, REML = FALSE)
summary(clme)
Anova(clme)

# check whether BLOCK is valid to include as a random effect (i.e., is model degenerate?)
summary(rePCA(clme)) # looks like it's not

# hence fit a model without BLOCK as random effect

clme = lmer(COLOR ~ BLOCK*GROUP + (1|ID), data = 
              (ambiguous.cdf %>% filter(EXP == "V4" & COND == "LEARN")),
            control = lcctrl, REML = FALSE)
summary(clme)
Anova(clme) # GROUP is not a significant predictor, in either the Anova() or summary()outputs.


# nested comparison: compare this model to one without GROUP as predictor
clme_nogroup = lmer(COLOR ~ BLOCK + (1|ID), data = 
                      (ambiguous.cdf %>% filter(EXP == "V4" & COND == "LEARN")),
                    control = lcctrl, REML = FALSE)

anova(clme, clme_nogroup)

# exp 1 and 2 collapsed
clme = lmer(COLOR ~ BLOCK*GROUP*EXP + (1+BLOCK|ID), data = (ambiguous.cdf %>% filter(COND == 'LEARN')), 
            control = lcctrl, REML = FALSE)
summary(clme)
Anova(clme) 

summary(rePCA(clme))  # model is not degenerate

# nested comparison with model without GROUP
clme_nogroup = lmer(COLOR ~ BLOCK*EXP + (1+BLOCK|ID), data = (ambiguous.cdf %>% filter(COND == 'LEARN')), 
                    control = lcctrl, REML = FALSE)
anova(clme, clme_nogroup) # including GROUP does not improve model


## Colour use in blocks 7 and 8
plot(y=ambiguous.cdf$COLOR, x=ambiguous.cdf$EXP)

clme = lmer(COLOR ~ BLOCK*GROUP*EXP + (1|EXP), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
summary(clme)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")
emmeans(clme, list(pairwise ~ GROUP:EXP), adjust = "tukey")

clme = lmer(COLOR ~ GROUP*EXP + (1|EXP), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
summary(clme) 


# experiment 1 (manuscript)
clme = lmer(COLOR ~ GROUP*BLOCK + (1|ID), data = (ambiguous.cdf %>% filter(EXP == "V1" & BLOCK > 6 & BLOCK < 9 & COND == "LEARN")), control = lcctrl, REML = FALSE)

summary(clme)
Anova(clme)

clme_nogroup = lmer(COLOR ~ BLOCK + (1|ID), data = (ambiguous.cdf %>% filter(EXP == "V1" & BLOCK > 6 & BLOCK < 9 & COND == "LEARN")), control = lcctrl, REML = FALSE)
anova(clme, clme_nogroup) # group not sig


# experiment 2 (manuscript)

clme = lmer(COLOR ~ GROUP*BLOCK + (1|ID), data = (ambiguous.cdf %>% filter(EXP == "V4" & BLOCK > 6 & BLOCK < 9 & COND == "LEARN")), control = lcctrl, REML = FALSE)
summary(clme)
Anova(clme)

clme_nogroup = lmer(COLOR ~ BLOCK + (1|ID), data = (ambiguous.cdf %>% filter(EXP == "V4" & BLOCK > 6 & BLOCK < 9 & COND == "LEARN")), control = lcctrl, REML = FALSE)

anova(clme, clme_nogroup) # group not sig

## Model for colour use blocks 7 - 8 without block
# EXP 1

clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6 & ambiguous.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")


# EXP 2
clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6 & ambiguous.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

tapply(ambiguous.cdf$COLOR, list(ambiguous.cdf$AGE, ambiguous.cdf$COND, ambiguous.cdf$BLOCK > 6), mean)[,'LEARN', 'TRUE']


################################################################################################################################
#                                             switched   analysis                                                             ##
################################################################################################################################

## proportion of subjects who switched
# EXP 1
tmp = tapply(ambiguous.cdf$COLOR, list(ambiguous.cdf$ID, ambiguous.cdf$BLOCK, ambiguous.cdf$GROUP, ambiguous.cdf$EXP), mean)[,,,'V1']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > (qbinom(0.95, 64, prob = 0.5)/64)*100
ctab = apply(tmp, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)

tmp = tapply(ambiguous.cdf$COLOR, list(ambiguous.cdf$ID, ambiguous.cdf$BLOCK, ambiguous.cdf$GROUP, ambiguous.cdf$EXP), mean)[,,,'V1']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > 75
ctab = apply(tmp, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)

# EXP 2
tmp = tapply(ambiguous.cdf$COLOR, list(ambiguous.cdf$ID, ambiguous.cdf$BLOCK, ambiguous.cdf$GROUP, ambiguous.cdf$EXP), mean)[,,,'V4']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > (qbinom(0.95, 64, prob = 0.5)/64)*100
ctab = apply(tmp, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)

tmp = tapply(ambiguous.cdf$COLOR, list(ambiguous.cdf$ID, ambiguous.cdf$BLOCK, ambiguous.cdf$GROUP, ambiguous.cdf$EXP), mean)[,,,'V4']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > 75
ctab = apply(tmp, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)

tmp = tapply(ambiguous.cdf$COLOR, list(ambiguous.cdf$ID, ambiguous.cdf$BLOCK, ambiguous.cdf$GROUP, ambiguous.cdf$EXP), mean)[,,,'V4']
tmp = apply(tmp[,7:8,], c(1, 3), mean, na.rm = TRUE) > 50
ctab = apply(tmp, 2, table)
chisq.test(ctab, simulate.p.value = TRUE)

## recognition, used, correct reports 

## EXP 1
t.test(scores.cdf$RECOG[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'], 
       scores.cdf$RECOG[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])

chisq.test(table(scores.cdf$RECOG[scores.cdf$EXP == 'V1'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V1']))

t.test(scores.cdf$USED[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'], 
       scores.cdf$USED[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])

chisq.test(table(scores.cdf$USED[scores.cdf$EXP == 'V1'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V1']))

t.test(scores.cdf$CORRECT[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'], 
       scores.cdf$CORRECT[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])
chisq.test(table(scores.cdf$CORRECT[scores.cdf$EXP == 'V1'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V1']))

# EXP 2
t.test(scores.cdf$RECOG[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'CHN Exp.2'], 
       scores.cdf$RECOG[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'ADLT Exp.2'])

chisq.test(table(scores.cdf$RECOG[scores.cdf$EXP == 'V4'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V4']))

t.test(scores.cdf$USED[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'CHN Exp.2'], 
       scores.cdf$USED[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'ADLT Exp.2'])

chisq.test(table(scores.cdf$USED[scores.cdf$EXP == 'V4'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V4']))

t.test(scores.cdf$CORRECT[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'CHN Exp.2'], 
       scores.cdf$CORRECT[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'ADLT Exp.2'])
chisq.test(table(scores.cdf$CORRECT[scores.cdf$EXP == 'V4'], scores.cdf$GROUPbin[scores.cdf$EXP == 'V4']))

### SWITCH POINT ALIGNED DATA 

## EXP 1

X = tapply(switchpoint.cdf$COLOR, list(switchpoint.cdf$ID, switchpoint.cdf$SWITCHED, switchpoint.cdf$BLOCK, switchpoint.cdf$GROUP, switchpoint.cdf$EXP), mean, na.rm = TRUE)[,,,,'V1']

ps_ya = sapply(1:5, function(x) t.test(X[,'SWITCHER',x,'YA'],X[,'SWITCHER',x+1,'YA'], paired = TRUE)$p.value)

ps_kids = sapply(1:5, function(x) t.test(X[,'SWITCHER',x,'KIDS'],X[,'SWITCHER',x+1,'KIDS'], paired = TRUE)$p.value)

p.adjust(ps_ya, method = 'holm')
p.adjust(ps_kids, method = 'holm')

cdf = subset(switchpoint.cdf, switchpoint.cdf$SWITCHED == 'SWITCHER' & switchpoint.cdf$EXP == 'V1' & !is.na(switchpoint.cdf$COLOR))

clme = lmer(COLOR ~ GROUP*AFTER + (1+AFTER|ID), data = cdf, control = lcctrl, REML = FALSE) #(manuscript)
Anova(clme)
emmeans(clme, list(pairwise ~ AFTER:GROUP), adjust = "tukey")


summary(subset(ambiguous.cdf, ambiguous.cdf$SWITCH == TRUE))

clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$SWITCH == TRUE & ambiguous.cdf$EXP == 'V1'))
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")


clme = lmer(COLOR ~ GROUP*SWITCHED*AFTER + (1 + AFTER|ID), data = subset(switchpoint.cdf, switchpoint.cdf$EXP == 'V1'))
Anova(clme)

X = tapply(switchpoint.cdf$COLOR, list(switchpoint.cdf$ID, switchpoint.cdf$SWITCHED, switchpoint.cdf$AFTER, switchpoint.cdf$GROUP, switchpoint.cdf$EXP), mean, na.rm = TRUE)[,,,,'V1']


t.test(X[,'SWITCHER', 'BEFORE', 'YA'], X[,'SWITCHER', 'BEFORE', 'KIDS'])
t.test(X[,'SWITCHER', 'AFTER', 'YA'], X[,'SWITCHER', 'AFTER', 'KIDS'])

t.test(X[,'SWITCHER', 'AFTER', 'YA'] - X[,'SWITCHER', 'BEFORE', 'YA'], X[,'SWITCHER', 'AFTER', 'KIDS'] - X[,'SWITCHER', 'BEFORE', 'KIDS'])

adult_switch_v1 = switchpoints[which(ids %in% names(switchids) & ids %in% adultids_v1)]
kid_switch_v1 = switchpoints[which(ids %in% names(switchids) & ids %in% kidids_v1)]
t.test(kid_switch_v1/2, adult_switch_v1/2)

## EXP 2
X = tapply(switchpoint.cdf$COLOR, list(switchpoint.cdf$ID, switchpoint.cdf$SWITCHED, switchpoint.cdf$BLOCK, switchpoint.cdf$GROUP, switchpoint.cdf$EXP), mean, na.rm = TRUE)[,,,,'V4']

ps_ya = sapply(1:5, function(x) t.test(X[,'SWITCHER',x,'YA'],X[,'SWITCHER',x+1,'YA'], paired = TRUE)$p.value)

ps_kids = sapply(1:5, function(x) t.test(X[,'SWITCHER',x,'KIDS'],X[,'SWITCHER',x+1,'KIDS'], paired = TRUE)$p.value)

p.adjust(ps_ya, method = 'holm') # manuscript
p.adjust(ps_kids, method = 'holm') # manuscript

cdf = subset(switchpoint.cdf, switchpoint.cdf$SWITCHED == 'SWITCHER' & switchpoint.cdf$EXP == 'V4' & !is.na(switchpoint.cdf$COLOR))

clme = lmer(COLOR ~ GROUP*AFTER + (1+AFTER|ID), data = cdf, control = lcctrl, REML = FALSE) # (manuscript)
Anova(clme)
emmeans(clme, list(pairwise ~ AFTER:GROUP), adjust = "tukey")

clme = lmer(COLOR ~ GROUP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$SWITCH == TRUE & ambiguous.cdf$EXP == 'V4'))
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")

clme = lmer(COLOR ~ GROUP*SWITCHED*AFTER + (1 + AFTER|ID), data = subset(switchpoint.cdf, switchpoint.cdf$EXP == 'V4'))
Anova(clme)

X = tapply(switchpoint.cdf$COLOR, list(switchpoint.cdf$ID, switchpoint.cdf$SWITCHED, switchpoint.cdf$AFTER, switchpoint.cdf$GROUP, switchpoint.cdf$EXP), mean, na.rm = TRUE)[,,,,'V4']

t.test(X[,'SWITCHER', 'BEFORE', 'YA'], X[,'SWITCHER', 'BEFORE', 'KIDS'])
t.test(X[,'SWITCHER', 'AFTER', 'YA'], X[,'SWITCHER', 'AFTER', 'KIDS'])

t.test(X[,'SWITCHER', 'AFTER', 'YA'] - X[,'SWITCHER', 'BEFORE', 'YA'], X[,'SWITCHER', 'AFTER', 'KIDS'] - X[,'SWITCHER', 'BEFORE', 'KIDS'])


adult_switch_v4 = switchpoints[which(ids %in% names(switchids) & ids %in% adultids_v4)]
kid_switch_v4 = switchpoints[which(ids %in% names(switchids) & ids %in% kidids_v4)]
t.test(kid_switch_v4/2, adult_switch_v4/2)

################################################################################################################################
#                                          cognitive control measures                                                         ##
################################################################################################################################

## WORKING MEMORY 

# Analysing differences in wm scores

t.test(wmscore[kidids_v1], wmscore[adultids_v1])
t.test(wmscore[kidids_v4], wmscore[adultids_v4])
t.test(wmscore[kidids_v1], wmscore[kidids_v4])
t.test(wmscore[adultids_v1], wmscore[adultids_v4])
t.test(wmscore[adultids], wmscore[kidids])


## STROOP 

# exp 1 
t.test(scores.cdf$STROOP[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'])

t.test(scores.cdf$STROOP[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])

t.test(scores.cdf$STROOP[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'], 
       scores.cdf$STROOP[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])

t.test(scores.cdf$STROOP_INTER[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'], 
       scores.cdf$STROOP_INTER[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])

t.test(scores.cdf$WM[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'CHN Exp.1'], 
       scores.cdf$WM[scores.cdf$EXP == 'V1' & scores.cdf$GROUP == 'ADLT Exp.1'])

# exp 2
t.test(scores.cdf$STROOP[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'CHN Exp.2'])
t.test(scores.cdf$STROOP[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'ADLT Exp.2'])

t.test(scores.cdf$STROOP[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'CHN Exp.2'], 
       scores.cdf$STROOP[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'ADLT Exp.2']) # children experienced larger facilitation effect of congruent stimuli than adults

t.test(scores.cdf$STROOP_INTER[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'CHN Exp.2'], 
       scores.cdf$STROOP_INTER[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'ADLT Exp.2'])  # adults did not experience significantly larger semantic interference (incongruent - neutral) than children

t.test(scores.cdf$WM[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'CHN Exp.2'], 
       scores.cdf$WM[scores.cdf$EXP == 'V4' & scores.cdf$GROUP == 'ADLT Exp.2'])

################################################################################################################################
#                                       variables in strategy switching                                                       ##
################################################################################################################################

## ------- switching to color use as a binary variable (whether or not colour used by blocks 7-8)
glm1 <- glm(SWITCHED ~ GROUPbin + EXP + E + AMBIG + CONG + PRENO + PRELATE + WM + STROOP + STROOP_INTER, 
            data=scores.cdf)
summary(glm1)

glm2 <- glm(SWITCHED ~ GROUPbin + EXP + E + AMBIG + CONG + PRENO + PRELATE + WM + STROOP, 
            data=scores.cdf) # model without stroop_score_interfere
summary(glm2)

# model comparison
anova(glm1, glm2, test="Chisq") # stroop_score_interfere is sig

scores.cdf$GROUPbin <- as.factor(scores.cdf$GROUPbin)

## stepwise AIC 

# omit na's for procedure
scores.cdf.no.na <- na.omit(scores.cdf) 

# model with all variables including experiment
tmpmod = glm(SWITCHED ~ GROUPbin + EXP + E + AMBIG + CONG + PRENO + PRELATE + WM + STROOP + STROOP_INTER, 
             data=scores.cdf.no.na)

# backwards
stepAIC(tmpmod, scope=tmpmod, direction="backward") 

tmpmod2 = glm(SWITCHED ~ 1, 
              data=scores.cdf.no.na)
# forwards
stepAIC(tmpmod2, scope=list(lower=tmpmod2, upper=tmpmod), direction="forward") 

# both
stepAIC(glm(SWITCHED ~ GROUPbin + STROOP_INTER, data=scores.cdf.no.na), scope=tmpmod, direction="both")

## ------ color use in blocks 7 and 8, and predictors, using LM

# create df for prop color use on ambig trials in last 2 correlated blocks (7&8)

tmp <- subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6) %>% 
  group_by(ID) %>% 
  summarise(COLOR = mean(COLOR)) # average blocks 7 & 8 color use proportions for each subject

scores.cdf <- right_join(scores.cdf,tmp)
scores.cdf.no.na <- na.omit(scores.cdf) 


# full model
tmpmod <- lm(COLOR ~ GROUPbin + EXP + E + AMBIG + CONG + PRENO + PRELATE + WM + STROOP + STROOP_INTER, 
  data=scores.cdf.no.na)

# backwards
stepAIC(tmpmod, scope=tmpmod, direction="backward") 

tmpmod2 = lm(COLOR ~ 1, 
              data=scores.cdf.no.na)
# forwards
stepAIC(tmpmod2, scope=list(lower=tmpmod2, upper=tmpmod), direction="forward") 

# both
stepAIC(lm(COLOR ~ GROUPbin + STROOP_INTER, data=scores.cdf.no.na), scope=tmpmod, direction="both")

