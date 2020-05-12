# LAST EDITED 12/5/20, AXL
# this is the latest version -- in progress
# currently checking code 
## reproducibility check - up to line 880
## functionality check - up to line 1036

# here, exp 1 and 2 are analysed separately

## load packages 
packages = c('car','sfsmisc', 'RColorBrewer', 'gdata', 'beeswarm', 'lme4', 'emmeans', 'pwr', 'tidyverse', 'lmerTest')
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
# regular trials 

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
regular.cdf$COND[regular.cdf$BLOCK == 1] = 'RAND' 

regular.cdf$COND = as.factor(regular.cdf$COND)


######################## OVERALL ERRORS ####################

clme = lmer(E ~ BLOCK*GROUP*EXP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN'), control = lcctrl, REML = FALSE)
# error as response variable
# random slopes and intercepts
# the term (1+BLOCK|ID) generates a vector-valued random effect (intercept and slope) for each of the n levels of the subject factor
summary(clme)

Anova(clme)
# BLOCK, GROUP SIG
emmeans(clme, list(pairwise ~ GROUP:EXP), adjust = "tukey")

# exp 1
clme = lmer(E ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
Anova(clme)

# exp 2
clme = lmer(E ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
Anova(clme)

# comparing exp 1 and 2
clme = lmer(E ~ BLOCK*GROUP*EXP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN'), control = lcctrl, REML = FALSE)
summary(clme)

###########  ERRORS IN BLOCKS 7 and 8

clme = lmer(E ~ GROUP*EXP + (1|EXP), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
Anova(clme) # GROUP and EXP* sig
# summary(clme)
emmeans(clme, list(pairwise ~ GROUP:EXP), adjust = "tukey") 
# significant:
# KIDS,V1 - YA,V1
# KIDS,V1 - YA,V4 
# YA,V1 - KIDS,V4 
# KIDS,V4 - YA,V4

tmp = apply(errors, c(2, 3, 4), mean, na.rm = TRUE) 
tmp.se = apply(errors, c(2, 3, 4), std.error)

## error plot

pdf('plots/Errors_main.pdf', width = 4, height = 4)

plot(tmp[,'V1', 'KIDS'], type = 'o', col = colorset[1,1], lty = 1, lwd = 2, 
     ylim = c(0, 65), ylab = '', xlab = '',   
     bty = 'n', cex.axis = 1.1, cex.lab = 1.2, lab = c(10, 6, 5), 
     xaxt = 'n')
lines(tmp[,'V1','YA'], type = 'o', lty = 1, lwd = 2, col = colorset[2,1])
se_bars(1:10, tmp[,'V1','KIDS'], tmp.se[,'V1','KIDS'], col = colorset[1,1])
se_bars(1:10, tmp[,'V1','YA'], tmp.se[,'V1','YA'], col = colorset[2,1])
axis(1, at = 1:10, labels = c(1:10), cex = 1.1)
text(1, -17, 'RAND', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
text(9.5, -17, 'INSTR', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
mtext(1, text = 'Block', line = 2.7, cex = 1.3)
mtext(2, text = 'Errors (%)', line = 2.5, cex = 1.3)

lines((1:10)+0.05, tmp[,'V4','KIDS'], type = 'o', lty = 1, lwd = 2, col = colorset[1,2])
lines((1:10)+0.05, tmp[,'V4','YA'], type = 'o', lty = 1, lwd = 2, col = colorset[2,2])
se_bars((1:10)+0.05, tmp[,'V4','KIDS'], tmp.se[,'V4','KIDS'], col = colorset[1,2])
se_bars(1:10+0.05, tmp[,'V4','YA'], tmp.se[,'V4','YA'], col = colorset[2,2])

legend('topright', legend = c('Children Exp. 1', 'Children Exp. 2', 'Adults Exp. 1', 'Adults Exp. 2'), 
       bty = 'n', col = t(colorset), lwd = 2, pch = 1, cex = 0.8)

dev.off()

pdf('plots/Errors_mean.pdf', width = 3, height = 4)

k = beeswarm(allerrs ~ group, col = t(colorset), pch = 1, corral = 'wrap',
             bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
             xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2, ylim = c(0, 50))

cmeans = tapply(allerrs, group, mean)
for (i in 1:4) {
  segments(i-0.3, cmeans[i], i+0.3, cmeans[i], lwd = 1.75)
}


text(1, -10, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(2, -10, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(1.5, -17, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)
text(3, -10, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(4, -10, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(3.5, -17, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)

mtext(2, text = 'Errors (%)', line = 2.5, cex = 1.3)
axis(2, at = seq(0, 50, 10), labels = seq(0, 50, 10), cex = 1.1)

dev.off()

t.test(allerrs[kidids_v1], allerrs[adultids_v1]) 
t.test(allerrs[kidids_v4], allerrs[adultids_v4]) 
t.test(allerrs[kidids], allerrs[adultids]) 
t.test(allerrs[kidids_v1], allerrs[kidids_v4]) 
t.test(allerrs[adultids_v1], allerrs[adultids_v4]) 


t.test(
  regular.cdf$E[regular.cdf$COND == 'instructed' & regular.cdf$GROUP == 'YA'], 
  regular.cdf$E[regular.cdf$COND == 'instructed' & regular.cdf$GROUP == 'KIDS']) # ERROR: not enough x observations

t.test(
  regular.cdf$E[regular.cdf$COND == 'instructed' & regular.cdf$GROUP == 'KIDS' & regular.cdf$EXP == 'V1'], 
  regular.cdf$E[regular.cdf$COND == 'instructed' & regular.cdf$GROUP == 'KIDS' & regular.cdf$EXP == 'V4'])

t.test(
  regular.cdf$E[regular.cdf$COND == 'instructed' & regular.cdf$GROUP == 'YA' & regular.cdf$EXP == 'V1'], 
  regular.cdf$E[regular.cdf$COND == 'instructed' & regular.cdf$GROUP == 'YA' & regular.cdf$EXP == 'V4'])

## RTs: to do - put in overleaf

clme = lmer(RT ~ GROUP*EXP + (1|EXP/ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
Anova(clme) # sig: group, cond, exp
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")
summary(clme)

clme = lmer(RT ~ GROUP*EXP + (1|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
Anova(clme)

clme = lmer(RT ~ GROUP*COND + (1 + COND|EXP/GROUP/ID), data = subset(regular.cdf, regular.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
Anova(clme) # sig: group, cond, group:cond
emmeans(clme, list(pairwise ~ COND | GROUP), adjust = "tukey")

## exp 1: standard trials
clme = lmer(RT ~ BLOCK*GROUP + (1+BLOCK|GROUP/ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
summary(clme)
summary(rePCA(clme)) # take out GROUP as RE

clme = lmer(RT ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN' & regular.cdf$EXP == 'V1'), control = lcctrl, REML = FALSE)
summary(rePCA(clme))
summary(clme)


Anova(clme)

## exp 2: standard trials
clme = lmer(RT ~ BLOCK*GROUP + (1+BLOCK|GROUP/ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN'& regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
summary(rePCA(clme)) # remove GROUP as RE

clme = lmer(RT ~ BLOCK*GROUP + (1+BLOCK|ID), data = subset(regular.cdf, regular.cdf$COND == 'LEARN'& regular.cdf$EXP == 'V4'), control = lcctrl, REML = FALSE)
# summary(rePCA(clme)) 
summary(clme)
Anova(clme)

tmp = apply(rts, c(2, 3, 4), mean, na.rm = TRUE)
tmp.se = apply(rts, c(2, 3, 4), std.error)

pdf('plots/RTs_main.pdf', width = 4, height = 4)
# tiff("RTs_main.tiff", units="in", width=4.5, height=4.5, res=300)

plot(tmp[,'V1', 'KIDS'], type = 'o', col = colorset[1,1], lty = 1, lwd = 2, 
     ylim = c(400, 1450), ylab = '', xlab = '',   
     bty = 'n', cex.axis = 1.1, cex.lab = 1.2, lab = c(10, 5, 5), 
     xaxt = 'n')
lines((1:9)+0.1, tmp[,'V4','KIDS'], type = 'o', lty = 1, lwd = 2, col = colorset[1,2])
lines(tmp[,'V1','YA'], type = 'o', lty = 1, lwd = 2, col = colorset[2,1])
lines((1:9)+0.1, tmp[,'V4','YA'], type = 'o', lty = 1, lwd = 2, col = colorset[2,2])

se_bars(1:9, tmp[,'V1','KIDS'], tmp.se[,'V1','KIDS'], col = colorset[1,1])
se_bars((1:9)+0.1, tmp[,'V4','KIDS'], tmp.se[,'V4','KIDS'], col = colorset[1,2])
se_bars(1:9, tmp[,'V1','YA'], tmp.se[,'V1','YA'], col = colorset[2,1])
se_bars(1:9+0.1, tmp[,'V4','YA'], tmp.se[,'V4','YA'], col = colorset[2,2])

axis(1, at = 1:9, labels = c(1:9), cex = 1.1)
text(1, 30, 'RAND', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
text(9, 30, 'INSTR', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
mtext(1, text = 'Block', line = 2.7, cex = 1.3)
mtext(2, text = 'RTs (ms)', line = 2.5, cex = 1.3)

legend('topright', legend = c('Children Exp. 1', 'Children Exp. 2', 'Adults Exp. 1', 'Adults Exp. 2'), 
       bty = 'n', col = t(colorset), lwd = 2, pch = 1, cex = 0.8)
dev.off()

pdf('plots/RTs_mean.pdf', width = 3, height = 4)
# tiff("RTs_mean.tiff", units="in", width=4.5, height=4.5, res=300)


k = beeswarm(allrts ~ group, col = t(colorset), pch = 1, corral = 'wrap',
             bty = 'n', cex.axis = 1.1, cex.lab = 1.2, ylim = c(400, 1450), 
             xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2)
cmeans = tapply(allrts, group, mean)
for (i in 1:4) {
  segments(i-0.3, cmeans[i], i+0.3, cmeans[i], lwd = 1.75)
}

text(1, 200, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(2, 200, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(1.5, 100, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)
text(3, 200, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(4, 200, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(3.5, 100, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)

mtext(2, text = 'RTs (ms)', line = 2.5, cex = 1.3)
axis(2, cex = 1.1)

dev.off()


t.test(allrts[kidids_v1], allrts[adultids_v1]) 
t.test(allrts[kidids_v4], allrts[adultids_v4]) 
t.test(allrts[kidids], allrts[adultids]) 
t.test(allrts[kidids_v1], allrts[kidids_v4]) 
t.test(allrts[adultids_v1], allrts[adultids_v4]) 


################################################################################################################################
#                                           task execution measures                                                           ##
################################################################################################################################

## Late Go trials

# start with combining across kid groups 
conflicts.cdf = data.frame(
  PREKEY_late = as.vector(prekeys_late), 
  PREKEY_NoGo = as.vector(prekeys_noGo),
  AMBIG_COSTS = as.vector(ambiguity_costs),
  CONG_COSTS = as.vector(congruency_costs),
  ID = rep(rownames(prekeys_late), prod(dim(prekeys_late)[2:4])),
  BLOCK = rep(as.numeric(colnames(prekeys_late)), each = dim(prekeys_late)[1], prod(dim(prekeys_late)[3:4])),
  EXP = rep(dimnames(prekeys_late)[[3]], each = prod(dim(prekeys_late)[1:2]), dim(prekeys_late)[4]), 
  GROUP = rep(dimnames(prekeys_late)[[4]], each = prod(dim(prekeys_late)[1:3])))

conflicts.cdf = subset(conflicts.cdf, !is.na(conflicts.cdf$PREKEY_late)) %>% arrange(EXP, GROUP, ID, BLOCK)
conflicts.cdf$COND = NA
conflicts.cdf$COND[conflicts.cdf$BLOCK < 9] = 'LEARN'
conflicts.cdf$COND[conflicts.cdf$BLOCK == 9] = 'INSTR'
conflicts.cdf$COND = as.factor(conflicts.cdf$COND)

# exp 1

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


pdf('plots/Prelate_mean.pdf', width = 3, height = 4)
# tiff("Prelate_mean.tiff", units="in", width=4.5, height=4.5, res=300)


k = beeswarm(allprelate ~ group, col = t(colorset), pch = 1, corral = 'wrap',
             bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
             xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2, ylim = c(0, 50))
cmeans = tapply(allprelate, group, mean)
for (i in 1:4) {segments(i-0.3, cmeans[i], i+0.3, cmeans[i], lwd = 1.75)}

text(1, -8, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(2, -8, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(1.5, -13.5, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)
text(3, -8, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(4, -8, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(3.5, -13.5, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)

mtext(2, text = 'Premature Responses (%)', line = 2.5, cex = 1.3)
axis(2, at = seq(0, 50, 10), labels = seq(0, 50, 10), cex = 1.1)

dev.off()

## No Go trials

pdf('plots/PreNoGo_mean.pdf', width = 3, height = 4)
# tiff("PreNoGo_mean.tiff", units="in", width=4.5, height=4.5, res=300)


k = beeswarm(allpreno ~ group, col = t(colorset), pch = 1, corral = 'wrap',
             bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
             xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2, ylim = c(0, 50))
cmeans = tapply(allpreno, group, mean)
for (i in 1:4) {segments(i-0.3, cmeans[i], i+0.3, cmeans[i], lwd = 1.75)}

text(1, -8, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(2, -8, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(1.5, -13.5, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)
text(3, -8, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(4, -8, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(3.5, -13.5, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)

mtext(2, text = 'Premature Responses (%)', line = 2.5, cex = 1.3)
axis(2, at = seq(0, 50, 10), labels = seq(0, 50, 10), cex = 1.1)

dev.off()

## Congruency costs

pdf('plots/CongCosts_mean.pdf', width = 3, height = 4)
# tiff("CongCosts_mean.tiff", units="in", width=4.5, height=4.5, res=300)

k = beeswarm(allcongcosts ~ group, col = t(colorset), pch = 1, corral = 'wrap',
             bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
             xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2, ylim = c(-200, 600))
cmeans = tapply(allcongcosts, group, mean)
for (i in 1:4) {segments(i-0.3, cmeans[i], i+0.3, cmeans[i], lwd = 1.75)}

text(1, -330, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(2, -330, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(1.5, -415, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)
text(3, -330, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(4, -330, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(3.5, -415, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)

mtext(2, text = 'Congruency Costs (ms)', line = 2.5, cex = 1.3)
axis(2, at = seq(-200, 600, 100), labels = seq(-200, 600, 100), cex = 1.1)
abline(h = 0) 
dev.off()

## Ambiguity costs

pdf('plots/AmbigCosts_mean.pdf', width = 3, height = 4)

k = beeswarm(allambigcosts ~ group, col = t(colorset), pch = 1, corral = 'wrap',
             bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
             xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2, ylim = c(-500, 1000))
cmeans = tapply(allambigcosts, group, mean)
for (i in 1:4) {segments(i-0.3, cmeans[i], i+0.3, cmeans[i], lwd = 1.75)}

text(1, -745, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(2, -745, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(1.5, -915, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)
text(3, -745, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(4, -746, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(3.5, -915, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)

mtext(2, text = 'Ambiguity Costs (ms)', line = 2.5, cex = 1.3)
axis(2, at = seq(-500, 1000, 500), labels = seq(-500, 1000, 500), cex = 1.1)
abline(h = 0) 
dev.off()



################################################################################################################################
#                                          cognitive control measures                                                         ##
################################################################################################################################


## WORKING MEMORY 
setdiff(ids, questids)

wmscore = Qdata$wm.score
names(wmscore) = Qdata$id

WMgroup = rep(NA, length(Qdata$id))
WMgroup[Qdata$id %in% adultids_v1] = 'Adults Exp.1'
WMgroup[Qdata$id %in% adultids_v4] = 'Adults Exp.2'
WMgroup[Qdata$id %in% kidids_v1] = 'Kids Exp.1'
WMgroup[Qdata$id %in% kidids_v4] = 'Kids Exp.2'


pdf('plots/WM_mean.pdf', width = 3, height = 4)
k = beeswarm(wmscore ~ WMgroup, col = t(colorset), pch = 1, corral = 'wrap',
             bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
             xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2, ylim = c(0, 15))
cmeans = tapply(wmscore, WMgroup, mean, na.rm = TRUE)
for (i in 1:4) {segments(i-0.3, cmeans[i], i+0.3, cmeans[i], lwd = 1.75)}

text(1, -2, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(2, -2, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(1.5, -4, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)
text(3, -2, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(4, -2, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(3.5, -4, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)

mtext(2, text = 'Working Memory Score', line = 2.5, cex = 1.3)
axis(2, at = seq(0, 15, 3), labels = seq(0, 15, 3), cex = 1.1)
dev.off()


t.test(wmscore[kidids_v1], wmscore[adultids_v1])
t.test(wmscore[kidids_v4], wmscore[adultids_v4])
t.test(wmscore[kidids_v1], wmscore[kidids_v4])
t.test(wmscore[adultids_v1], wmscore[adultids_v4])
t.test(wmscore[adultids], wmscore[kidids])


## STROOP TASK
setdiff(ids, stroopids)

sum(setdiff(ids, stroopids) %in% adultids)
sum(setdiff(ids, stroopids) %in% kidids)

stroop_rts = tapply(Sdata$resp.rt, list(Sdata$id, Sdata$congruent, Sdata$resp.corr, is.na(Sdata$trials.thisN)), mean)[,,'1', 'FALSE']*1000 # rts for correct responses only
stroop_errors = 1 - tapply(Sdata$resp.corr, list(Sdata$id, Sdata$congruent, is.na(Sdata$trials.thisN)), mean, na.rm = TRUE)[,,'FALSE']*100 

stroop_rt_score = (stroop_rts[,'neutral'] - stroop_rts[,'cong'])
stroop_rt_score_interfere = (stroop_rts[,'neutral'] - stroop_rts[,'incong'])
t.test(stroop_rts[,'cong'], stroop_rts[,'incong'])

## average color use in last two blocks
tmp = subset(ambiguous.cdf, COND == "LEARN" & BLOCK > 6) %>% group_by(ID, GROUP, EXP, AGE, SWITCH) %>% 
  summarise(color_familiar = mean(COLOR)/100) %>%
  rename(id = ID)

all.mx <- right_join(all.mx, tmp) # put into data frame that contains all measures

summary(lm(data=all.mx, color_familiar ~ neutral_incong))
summary(glm(data=all.mx, SWITCH ~ neutral_incong + cong_incong))


## Obtain data from Sdata2 (eprime version)

tmp=Sdata2 %>% dplyr::select(c(id,
                           Session,
                           ExperimentName,
                           Block))


Sdata2_df <- Sdata2 %>% 
  dplyr::select(c(id, 
                  ExperimentName,
                  Session,
                  Block,
                  eprime,
                  Trial,
                  cond,
                  word,
                  color,
                  Stimulus.RESP,
                  Stimulus.RT,
                  Stimulus.RTTime,
                  Stimulus.OnsetTime,
                  answer)) %>%
  mutate(
    correct = ifelse(Stimulus.RESP == answer, '1', '0')  # add variable for if response was correct
  ) %>%
  arrange(id, ExperimentName, Block, Trial)


Sdata2_df$correct = as.numeric(Sdata2_df$correct) # make numeric var as in Sdata to use tapply(Sdata$correct...)

# summary of rts from correct trials in eprime version (Sdata2), by condition
stroop_rts2 = tapply(Sdata2_df$Stimulus.RT, list(Sdata2_df$id, Sdata2_df$cond, Sdata2_df$correct), mean)[,,'1'] 

stroop_errors2 = 1 - tapply(Sdata2_df$correct, list(Sdata2_df$id, Sdata2_df$cond), mean, na.rm = TRUE)[,]*100 

stroop_rt_score2 = (stroop_rts2[,'neutral'] - stroop_rts2[,'cong'])
stroop_rt_score_interfere2 = (stroop_rts2[,'neutral'] - stroop_rts2[,'incong'])

# now combine data from non-eprime and eprime versions 
stroop_rt_score_interfere = c(stroop_rt_score_interfere, stroop_rt_score_interfere2)
stroop_rt_score = c(stroop_rt_score,stroop_rt_score2)

#plots
pdf('plots/Stroop_mean.pdf', width = 3, height = 4)
k = beeswarm(stroop_rt_score ~ Sgroup, col = t(colorset), pch = 1, corral = 'wrap',
             bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
             xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2, ylim = c(-100, 150))
cmeans = tapply(stroop_rt_score, Sgroup, mean)
for (i in 1:4) {segments(i-0.3, cmeans[i], i+0.3, cmeans[i], lwd = 1.75)}

text(1, -135, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(2, -135, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(1.5, -168, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)
text(3, -135, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(4, -135, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(3.5, -168, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)

mtext(2, text = 'Neutral RT - Cong. RT (ms)', line = 2.5, cex = 1.3)
axis(2, at = seq(-100, 150, 50), labels = seq(-100, 150, 50), cex = 1.1)
abline(h = 0)
dev.off()


################################################################################################################################
#                                       alternative strategy discovery                                                        ##
################################################################################################################################


## QUESTIONNAIRE MEASURES

setdiff(ids, questids)
sum(setdiff(ids, questids) %in% kidids)
sum(setdiff(ids, questids) %in% adultids)

recog = car::recode(Qdata$recognised, "'yes' = 1; 'no' = 0") # error
names(recog) = Qdata$id
cmeans = c(mean(recog[kidids_v1], na.rm = TRUE), 
           mean(recog[adultids_v1], na.rm = TRUE), 
           mean(recog[kidids_v4], na.rm = TRUE), 
           mean(recog[adultids_v4], na.rm = TRUE))*100

csums_v1 = c(sum(recog[kidids_v1] == 0, na.rm = TRUE), 
             sum(recog[kidids_v1] == 1, na.rm = TRUE), 
             sum(recog[adultids_v1]==0, na.rm = TRUE), 
             sum(recog[adultids_v1]==1, na.rm = TRUE)) 
ctab_v1 = matrix(csums_v1, 2, 2)
chisq.test(ctab_v1)

csums_v4 = c(sum(recog[kidids_v4] == 0, na.rm = TRUE), 
             sum(recog[kidids_v4] == 1, na.rm = TRUE), 
             sum(recog[adultids_v4]==0, na.rm = TRUE), 
             sum(recog[adultids_v4]==1, na.rm = TRUE)) 
ctab_v4 = matrix(csums_v4, 2, 2)

chisq.test(ctab_v4)

pdf('plots/Recognised_percent.pdf', width = 2.5, height = 4)
barplot(cmeans, col = as.vector(rbind(colorset[2,], colorset[1,])), 
        space = c(1, 0.1, 1, 0.1), ylim = c(0, 60), 
        cex.axis = 1.1, cex.lab = 1.2, border = NA, names.arg = '',
        ylab = '')
mtext(2, text = '% of Ps Reporting Discovery', line = 2.5, cex = 1.3)
dev.off()

used = car::recode(Qdata$used, "'yes' = 1; 'no' = 0")
names(used) = Qdata$id
cmeans = c(mean(used[kidids_v1], na.rm = TRUE), 
           mean(used[adultids_v1], na.rm = TRUE), 
           mean(used[kidids_v4], na.rm = TRUE), 
           mean(used[adultids_v4], na.rm = TRUE))*100

csums_v1 = c(sum(used[kidids_v1] == 0, na.rm = TRUE), 
             sum(used[kidids_v1] == 1, na.rm = TRUE), 
             sum(used[adultids_v1]==0, na.rm = TRUE), 
             sum(used[adultids_v1]==1, na.rm = TRUE)) 
ctab_v1 = matrix(csums_v1, 2, 2)
chisq.test(ctab_v1, simulate = TRUE)

csums_v4 = c(sum(used[kidids_v4] == 0, na.rm = TRUE), 
             sum(used[kidids_v4] == 1, na.rm = TRUE), 
             sum(used[adultids_v4]==0, na.rm = TRUE), 
             sum(used[adultids_v4]==1, na.rm = TRUE)) 
ctab_v4 = matrix(csums_v4, 2, 2)
chisq.test(ctab_v4)

pdf('plots/Used_percent.pdf', width = 2.5, height = 4)
# tiff("Used_percent.tiff", units="in", width=4.5, height=4.5, res=300)

barplot(cmeans, col = as.vector(colorset), 
        space = c(1, 0.1, 1, 0.1), ylim = c(0, 60), 
        cex.axis = 1.1, cex.lab = 1.2, border = NA, names.arg = '',
        ylab = '')
mtext(2, text = '% of Ps Reporting Rule Use', line = 2.5, cex = 1.3)
dev.off()


ctab = cbind(Qdata$left.up, Qdata$right.down, Qdata$left.down, Qdata$right.up) - 2

Qdata$correctreport = NA
Qdata$correctreport[Qdata$id %% 2 == 0] = 
  apply(ctab[Qdata$id %% 2 == 0,], 1, function(x) mean(x == c(1, 1, 2, 2)) == 1)
Qdata$correctreport[Qdata$id %% 2 == 1] = 
  apply(ctab[Qdata$id %% 2 == 1,], 1, function(x) mean(x == c(2, 2, 1, 1)) == 1)

yas = Qdata$correctreport[Qdata$recognised == 'yes' & Qdata$age > 15]
kids = Qdata$correctreport[Qdata$recognised == 'yes' & Qdata$age < 15]

t.test(yas, kids)

##########
## IN-TASK MEASURES

### Ambiguous trials
cthresh = qbinom(0.95, 64, prob = 0.5)/64 
tmp = tapply(DATA$followed, list(DATA$id, DATA$cond, DATA$block>6 & DATA$block<9, DATA$respkey %in% c(77, 88, 188)), mean, na.rm = TRUE)[,2,'TRUE','TRUE']

switchids = which(tmp > cthresh)
nswitchids = which(tmp <= cthresh)
nswitchers = length(switchids)

logfoll = -log(followed/100)


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
ambiguous.cdf$COND[ambiguous.cdf$BLOCK == 9] = 'INSTR'
ambiguous.cdf$COND = as.factor(ambiguous.cdf$COND)
ambiguous.cdf$ID = as.character(ambiguous.cdf$ID)

x = tapply(ambiguous.cdf$COLOR, list(ambiguous.cdf$ID, ambiguous.cdf$BLOCK, ambiguous.cdf$GROUP), mean)

xya = apply(x[,7:8, 'YA'], 1, mean, na.rm = TRUE)
xkids = apply(x[,7:8, 'KIDS'], 1, mean, na.rm = TRUE)

t.test(xya, mu = 50) #sig
t.test(xkids, mu = 50) #sig


x = tapply(ambiguous.cdf$COLOR, list(ambiguous.cdf$ID, ambiguous.cdf$BLOCK, ambiguous.cdf$GROUP, ambiguous.cdf$EXP), mean)

xya_v1 = apply(x[,7:8, 'YA', 'V1'], 1, mean, na.rm = TRUE)
xkids_v1 = apply(x[,7:8, 'KIDS', 'V1'], 1, mean, na.rm = TRUE)
xya_v4 = apply(x[,7:8, 'YA', 'V4'], 1, mean, na.rm = TRUE)
xkids_v4 = apply(x[,7:8, 'KIDS', 'V4'], 1, mean, na.rm = TRUE)


# more than 75%  - none sig
t.test(xkids>75, xya>75, var.equal = TRUE)
t.test(xkids_v1>75, xya_v1>75, var.equal = TRUE)
t.test(xkids_v4>75, xya_v4>75, var.equal = TRUE)

# mean 
t.test(xkids, xya, var.equal = TRUE)
t.test(xkids_v1, xya_v1, var.equal = TRUE)
t.test(xkids_v4, xya_v4, var.equal = TRUE)

t.test(xya_v1, mu = 50) #sig
t.test(xkids_v1, mu = 50) #sig

t.test(xya_v4, mu = 50) #sig
t.test(xkids_v4, mu = 50) #sig

## proportion of participants with p > .05

cthresh = qbinom(0.95, 64, prob = 0.5)/64 


# exp 1
tmp = tapply(DATA_V1$followed, 
             list(DATA_V1$id, DATA_V1$agegroup, DATA_V1$cond, 
                  DATA_V1$block>6 & DATA_V1$block<9, DATA_V1$respkey %in% c(77, 88, 188)), 
             mean, na.rm = TRUE)[,,2,'TRUE','TRUE']
nids_v1 = nkids_v1 + nadults_v1
ctab = table(tmp>=cthresh, matrix(c('KIDS', 'YA'), byrow = TRUE, nids_v1, 2))

ctab_prop = ctab/matrix(c(nkids_v1, nadults_v1), 2, 2, byrow = TRUE)
ctab_prop
chisq.test(ctab) 

# exp 2
tmp = tapply(DATA_V4$followed, 
             list(DATA_V4$id, DATA_V4$agegroup, DATA_V4$cond, 
                  DATA_V4$block>6 & DATA_V4$block<9, DATA_V4$respkey %in% c(77, 88, 188)), 
             mean, na.rm = TRUE)[,,2,'TRUE','TRUE']
nids_v4 = nkids_v4 + nadults_v4
ctab = table(tmp>=cthresh, matrix(c('KIDS', 'YA'), byrow = TRUE, nids_v4, 2))

ctab_prop = ctab/matrix(c(nkids_v4, nadults_v4), 2, 2, byrow = TRUE)
chisq.test(ctab) 

# hypothetical effect size calculations for power analysis 
ctab = table(tmp>=cthresh, matrix(c('KIDS', 'YA'), byrow = TRUE, nids, 2))
ctab[1,1] = ctab[1,1] + 3
ctab[2,1] = ctab[2,1] - 3
ctab[1,2] = ctab[1,2] - 5
ctab[2,2] = ctab[2,2] + 5
ctab_prop = ctab/matrix(c(nkids, nadults), 2, 2, byrow = TRUE)
effect_size = sqrt((chisq.test(ctab, correct = FALSE)$statistic^2)/nids)

ctab = matrix(c(50*0.6, 50*0.4, round(41*0.75), round(41*0.25)), ncol = 2)
ctab_prop = ctab/matrix(c(nkids, nadults), 2, 2, byrow = TRUE)
effect_size = sqrt((chisq.test(ctab, correct = FALSE)$statistic^2)/nids)

(ctab_prop[,1] - ctab_prop[,2])^2 

pwr.chisq.test(w = effect_size, N = nids, df = 1, sig.level = 0.05)

tmp = tapply(DATA$followed, list(DATA$id, DATA$agegroup, DATA$cond, DATA$block, DATA$taskV, DATA$respkey %in% c(77, 88, 188)), mean, na.rm = TRUE)[,,2,7:8,,'TRUE']
tmp = apply(tmp, c(1, 2, 4), mean, na.rm = TRUE)

ctab_v1 = table(tmp[,,'V1']>=cthresh, matrix(c('KIDS', 'YA'), byrow = TRUE, nids, 2))
ctab_prop_v1 = (ctab_v1/matrix(c(nkids_v1, nadults_v1), 2, 2, byrow = TRUE))*100
chisq.test(ctab_v1)

ctab_prop_v4 = (ctab_v4/matrix(c(nkids_v4, nadults_v4), 2, 2, byrow = TRUE))*100
chisq.test(ctab_v4)




#####################################################################################################################

## Create dataframe with ambiguous trials
ambiguous.cdf

ambiguous.cdf$COLOR2 <- scale(ambiguous.cdf$COLOR, center = TRUE, scale = TRUE)

basic.lm = lm(COLOR2 ~ BLOCK*GROUP, data = ambiguous.cdf)

summary(basic.lm)
ggplot(ambiguous.cdf, aes(x = AGE, y = COLOR2)) +
  geom_point() +
  geom_smooth(method = "lm")

plot(basic.lm, which = 2)

ggplot((ambiguous.cdf %>% dplyr::filter(COND == "LEARN" & GROUP == "YA")),
       aes(x = BLOCK, y = COLOR, colour = ID)) +
       geom_point(size = 2) +
       theme_classic() +
       theme (legend.position = "none") +
  geom_line()


## ----------- Colour use on ambiguous trials

# AXL.
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

# group sig in summary but not Anova() - there is discrepancy
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
Anova(clme) # discrepancy between summary and Anova

summary(rePCA(clme))  # model is not degenerate

# nested comparison with model without GROUP
clme_nogroup = lmer(COLOR ~ BLOCK*EXP + (1+BLOCK|ID), data = (ambiguous.cdf %>% filter(COND == 'LEARN')), 
                    control = lcctrl, REML = FALSE)
anova(clme, clme_nogroup) # including GROUP does not improve model
#########

## ----------- Colour use in blocks 7 and 8
plot(y=ambiguous.cdf$COLOR, x=ambiguous.cdf$EXP)

clme = lmer(COLOR ~ BLOCK*GROUP*EXP + (1|EXP), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
summary(clme)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")
emmeans(clme, list(pairwise ~ GROUP:EXP), adjust = "tukey")

clme = lmer(COLOR ~ GROUP*EXP + (1|EXP), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6), control = lcctrl, REML = FALSE)
summary(clme) # makes no real sense to include EXP here.

### AXL CHECKING UP TO HERE

# experiment 1
clme = lmer(COLOR ~ GROUP*BLOCK + (1|ID), data = (ambiguous.cdf %>% filter(EXP == "V1" & BLOCK > 6 & BLOCK < 9 & COND == "LEARN")), control = lcctrl, REML = FALSE)

summary(clme)
Anova(clme)

clme_nogroup = lmer(COLOR ~ BLOCK + (1|ID), data = (ambiguous.cdf %>% filter(EXP == "V1" & BLOCK > 6 & BLOCK < 9 & COND == "LEARN")), control = lcctrl, REML = FALSE)
anova(clme, clme_nogroup) # group not sig

# experiment 2
clme = lmer(COLOR ~ GROUP*BLOCK + (1|ID), data = (ambiguous.cdf %>% filter(EXP == "V4" & BLOCK > 6 & BLOCK < 9 & COND == "LEARN")), control = lcctrl, REML = FALSE)
summary(clme)
Anova(clme)

clme_nogroup = lmer(COLOR ~ BLOCK + (1|ID), data = (ambiguous.cdf %>% filter(EXP == "V4" & BLOCK > 6 & BLOCK < 9 & COND == "LEARN")), control = lcctrl, REML = FALSE)

anova(clme, clme_nogroup) # group not sig

tmp = apply(followed, c(2, 3, 4), mean, na.rm = TRUE)
tmp.se = apply(followed, c(2, 3, 4), std.error)

## Model for colour use blocks 7 - 8 without block

clme = lmer(COLOR ~ AGE*EXP + (1|ID), data = subset(ambiguous.cdf, ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6 & ambiguous.cdf$GROUP == 'KIDS'), control = lcctrl, REML = FALSE)
Anova(clme) #none sig

emmeans(clme, list(pairwise ~ AGE), adjust = "tukey")
emmeans(clme, list(pairwise ~ GROUP:EXP), adjust = "tukey")

tapply(ambiguous.cdf$COLOR, list(ambiguous.cdf$AGE, ambiguous.cdf$COND, ambiguous.cdf$BLOCK > 6), mean)[,'LEARN', 'TRUE']

### exp 1 and 2 separately

### exp 1
clme = lmer(COLOR ~ AGE + (1|ID), data = subset(ambiguous.cdf %>% filter(EXP == "V1"), ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6 & ambiguous.cdf$GROUP == 'KIDS'), control = lcctrl, REML = FALSE)
Anova(clme)

### exp 2
clme = lmer(COLOR ~ AGE + (1|ID), data = subset(ambiguous.cdf %>% filter(EXP == "V4"), ambiguous.cdf$COND == 'LEARN' & ambiguous.cdf$BLOCK > 6 & ambiguous.cdf$GROUP == 'KIDS'), control = lcctrl, REML = FALSE)
Anova(clme)


######
pdf('plots/Followed_main.pdf', width = 4, height = 4)
# tiff("Followed_main.tiff", units="in", width=4.5, height=4.5, res=300)

plot(tmp[,'V1', 'KIDS'], type = 'o', col = colorset[1,1], lty = 1, lwd = 2, 
     ylim = c(0, 100), ylab = '', xlab = '',   
     bty = 'n', cex.axis = 1.1, cex.lab = 1.2, lab = c(10, 5, 5), 
     xaxt = 'n')
lines((1:10)+0.1, tmp[,'V4','KIDS'], type = 'o', lty = 1, lwd = 2, col = colorset[1,2])
lines(tmp[,'V1','YA'], type = 'o', lty = 1, lwd = 2, col = colorset[2,1])
lines((1:10)+0.1, tmp[,'V4','YA'], type = 'o', lty = 1, lwd = 2, col = colorset[2,2])

se_bars(1:10, tmp[,'V1','KIDS'], tmp.se[,'V1','KIDS'], col = colorset[1,1])
se_bars((1:10)+0.1, tmp[,'V4','KIDS'], tmp.se[,'V4','KIDS'], col = colorset[1,2])
se_bars(1:10, tmp[,'V1','YA'], tmp.se[,'V1','YA'], col = colorset[2,1])
se_bars(1:10+0.1, tmp[,'V4','YA'], tmp.se[,'V4','YA'], col = colorset[2,2])

axis(1, at = 1:10, labels = c(1:10), cex = 1.1)
text(1, -24, 'RAND', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
text(9.5, -24, 'INSTR', srt = 0, xpd = TRUE, pos = 3, cex = 0.8)
mtext(1, text = 'Block', line = 2.7, cex = 1.3)
mtext(2, text = 'Color Use (%)', line = 2.5, cex = 1.3)

legend('topleft', legend = c('Children Exp. 1', 'Children Exp. 2', 'Adults Exp. 1', 'Adults Exp. 2'), 
       bty = 'n', col = t(colorset), lwd = 2, pch = 1, cex = 0.8)
abline(h = 50)
dev.off()

## Plot for color Use Blocks 7-8

pdf('plots/Followed_mean.pdf', width = 3, height = 4)
# tiff("Followed_mean", units="in", width=4.5, height=4.5, res=300)

k = beeswarm(allfoll ~ group, col = t(colorset), pch = 1, corral = 'wrap',
             bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
             xlab = '', ylab = '', labels = NA, cex = 0.9, axes = FALSE, lwd = 2, ylim = c(20, 100))
cmeans = tapply(allfoll, group, mean)
for (i in 1:4) {segments(i-0.3, cmeans[i], i+0.3, cmeans[i], lwd = 1.75)}

text(1, 5, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(2, 5, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(1.5, -5, 'Adults', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)
text(3, 5, 'Exp.1', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(4, 5, 'Exp.2', srt = 0, xpd = TRUE, pos = 3, cex = 0.7)
text(3.5, -5, 'Children', srt = 0, xpd = TRUE, pos = 3, cex = 1.1)

mtext(2, text = 'Color Use Blocks 7-8 (%)', line = 2.5, cex = 1.3)
axis(2, cex = 1.1)
dev.off()

################################################################################################################################
#                                           switch point analysis                                                             ##
################################################################################################################################

cthresh = qbinom(0.95, 64, prob = 0.5)/64 

# exp 1
tmp = tapply(DATA_V1$followed, list(DATA_V1$id, DATA_V1$cond, DATA_V1$block>6 & DATA_V1$block<9, DATA_V1$respkey %in% c(77, 88, 188)), mean, na.rm = TRUE)[,2,'TRUE','TRUE']

# participants who showed above chance responding in blocks 7 and 8

switchids_v1 = which(tmp > cthresh)# participants who switched  #
nswitchids_v1 = which(tmp <= cthresh) # participants who didn't switch
nswitchers_v1 = length(switchids_v1)
nkidswitchers_v1 = length(which(ids_v1 %in% names(switchids_v1) & ids_v1 %in% kidids_v1))
nadultswitchers_v1 = length(which(ids_v1 %in% names(switchids_v1) & ids_v1 %in% adultids_v1))

switchids = c(switchids_v1, switchids_v4)

################################################################################################################################
#                                                   build df with measures                                                    ##
################################################################################################################################

alltaskex_mx <- 
  right_join(
    right_join(
      right_join(
        rownames_to_column(as.data.frame(allambigcosts)),
        rownames_to_column(as.data.frame(allcongcosts))
      ),
      rownames_to_column(as.data.frame(allpreno))
    ),
    rownames_to_column(as.data.frame(allprelate))
  ) %>%
  rename("id" = "rowname")

## -----add switched data to all.mx df

alltaskex_mx$switched <- ifelse(alltaskex_mx$id %in% names(switchids), 1, 0)

all.mx <- right_join(alltaskex_mx, (DATA %>% dplyr::select(c(id, age, agegroup, taskV)) %>% unique())) 

tmp <- right_join(
    rownames_to_column(as.data.frame(stroop_rt_score_interfere)),
    rownames_to_column(as.data.frame(stroop_rt_score))
  ) # join stroop variables, 80 obs - 6 missing

# make tmp with id, stroop variables, and wmscore
tmp <- right_join(tmp, 
                  rownames_to_column(as.data.frame(wmscore))# 82 obs
                  ) %>%
  rename("id" = "rowname") 

# add to all.mx df and reorder variables
all.mx <- right_join(all.mx, tmp) %>% 
  dplyr::select(c(id, age, agegroup, taskV, allambigcosts, allcongcosts, wmscore, stroop_rt_score, stroop_rt_score_interfere, switched))

all.mx$switched = as.numeric(all.mx$switched)

## add error data for blocks 2 - 8
tmp <-  subset(regular.cdf, regular.cdf$COND == 'LEARN') %>% group_by(ID) %>% summarise(mean(E)) %>% rename(id = ID)
all.mx <-  right_join(all.mx, tmp)

# ## add Questionnaire data to all.mx
# Qdata$id = as.character(Qdata$id)
# all.mx= right_join(all.mx, Qdata)

################################################################################################################################
#                                       variables in strategy switching                                                       ##
################################################################################################################################

glm1 <- glm(switched ~ agegroup + taskV + `mean(E)` + allambigcosts + allcongcosts + allpreno + allprelate + wmscore + stroop_rt_score + stroop_rt_score_interfere, 
            data=all.mx)

summary(glm1)

glm2 <- glm(switched ~ agegroup + taskV +  `mean(E)` + allambigcosts + allcongcosts + allpreno + allprelate + wmscore + stroop_rt_score, 
            data=all.mx)
summary(glm2)

anova(glm1, glm2, test="Chisq")

all.mx$agegroup <- as.factor(all.mx$agegroup)
tmpmod = glm(switched ~ agegroup + taskV + allambigcosts + allcongcosts + wmscore + stroop_rt_score + stroop_rt_score_interfere, data=all.mx)
stepAIC(tmpmod, scope=tmpmod, direction="backward")

tmpmod = glm(switched ~ agegroup + taskV + allcongcosts + stroop_rt_score + stroop_rt_score_interfere, data=all.mx)
stepAIC(tmpmod, scope=tmpmod, direction="backward")

stepAIC(glm(switched ~ 1, data=all.mx), scope=tmpmod, direction="forward")

stepAIC(glm(switched ~ agegroup + stroop_rt_score_interfere, data=all.mx), scope=tmpmod, direction="both")

tmpmod = glm(switched ~ agegroup + allambigcosts + allcongcosts + wmscore + stroop_rt_score + stroop_rt_score_interfere, data=all.mx)
stepAIC(glm(switched ~ agegroup + allambigcosts + allcongcosts + wmscore + stroop_rt_score + stroop_rt_score_interfere, 
            data = all.mx %>% filter(taskV == "V1")), 
        scope=tmpmod, direction="backward")

stepAIC(glm(switched ~ agegroup + allambigcosts + allcongcosts + wmscore + stroop_rt_score + stroop_rt_score_interfere, 
            data = all.mx %>% filter(taskV == "V4")), 
        scope=tmpmod, direction="backward")

tmpmod = glm(switched ~ agegr2oup + allambigcosts + allcongcosts + wmscore + stroop_rt_score + stroop_rt_score_interfere, data=all.mx)


##########################################################################################

switchpoints_v1 = apply(tmp, 1, FUN = function(x) {which.min(x)}) #the switchpoint in miniblocks of each id
adult_switch_v1 = switchpoints_v1[which(ids_v1 %in% names(switchids_v1) & ids_v1 %in% adultids_v1)] # the switch points for adults who DID switch
kid_switch_v1 = switchpoints_v1[which(ids_v1 %in% names(switchids_v1) & ids_v1 %in% kidids_v1)] # the switch points for kids who DID switch
t.test(kid_switch_v1/2, adult_switch_v1/2) 

# exp 2

tmp = tapply(DATA_V4$followed, list(DATA_V4$id, DATA_V4$cond, DATA_V4$block>6 & DATA_V4$block<9, DATA_V4$respkey %in% c(77, 88, 188)), mean, na.rm = TRUE)[,2,'TRUE','TRUE']

switchids_v4 = which(tmp > cthresh)# participants who switched  #
nswitchids_v4 = which(tmp <= cthresh) # participants who didn't switch
nswitchers_v4 = length(switchids_v4)
nkidswitchers_v4 = length(which(ids_v4 %in% names(switchids_v4) & ids_v4 %in% kidids_v4))
nadultswitchers_v4 = length(which(ids_v4 %in% names(switchids_v4) & ids_v4 %in% adultids_v4))


tmp_switch_v4 = tapply(DATA_V4$followed, list(DATA_V4$id, DATA_V4$miniblock, DATA_V4$cond, DATA_V4$respkey %in% c(77, 88, 188)), mean, na.rm = TRUE)[,,2, 'TRUE']

tmp_switch_v4 = tmp_switch_v4[,1:16]

miniblock_diff_v4 = tmp_switch_v4 - matrix(rowMeans(tmp_switch_v4, na.rm = TRUE), ncol = 16, nrow = nids_v4)
CUsum = apply(miniblock_diff_v4, 1, FUN = function(x) cumsum(x))
tmp=t(CUsum)

switchpoints_v4 = apply(tmp, 1, FUN = function(x) {which.min(x)}) #the switchpoint in miniblocks of each id
adult_switch_v4 = switchpoints_v4[which(ids_v4 %in% names(switchids_v4) & ids_v4 %in% adultids_v4)] # the switch points for adults who DID switch
kid_switch_v4 = switchpoints_v4[which(ids_v4 %in% names(switchids_v4) & ids_v4 %in% kidids_v4)] # the switch points for kids who DID switch
t.test(kid_switch_v4/2, adult_switch_v4/2)

tmp_switch = cbind(tmp_switch_v1, tmp_switch_v4)
# to do: assign switchpoints by experiment

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

switchpoint.df = data.frame(COLOR = as.vector(follow_aligned), 
                            ID = rep(rownames(follow_aligned), prod(dim(follow_aligned)[1:2])),
                            EXP = rep(rownames(follow_aligned), prod(dim(follow_aligned)[1:2])),
                            BLOCK = rep(as.numeric(colnames(follow_aligned)), each = dim(follow_aligned)[1]))


switchpoint.df$EXP = NA 
switchpoint.df$EXP[switchpoint.df$ID %in% c(adultids_v1, kidids_v1)]  = 'V1'
switchpoint.df$EXP[switchpoint.df$ID %in% c(adultids_v4, kidids_v4)]  = 'V4'

switchpoint.df$GROUP = NA 
switchpoint.df$GROUP[switchpoint.df$ID %in% kidids] = 'KIDS'
switchpoint.df$GROUP[switchpoint.df$ID %in% adultids] = 'YA'
switchpoint.df$GROUP = as.factor(switchpoint.df$GROUP)

switchpoint.df$AFTER = NA
switchpoint.df$AFTER[switchpoint.df$BLOCK < 0] = 'BEFORE'
switchpoint.df$AFTER[switchpoint.df$BLOCK > 0] = 'AFTER'

switchpoint.df$SWITCHED = NA
switchpoint.df$SWITCHED[switchpoint.df$ID %in% names(switchids)] = 'SWITCHER'
switchpoint.df$SWITCHED[!(switchpoint.df$ID %in% names(switchids))] = 'NOSWITCHER'

clme = lmer(COLOR ~ GROUP*SWITCHED*AFTER*EXP + (1 + AFTER|ID), data = switchpoint.df)

Anova(clme) # group, switched, after, group:switched, switched:after, GROUP:SWITCHED:AFTER:EXP significant
emmeans(clme, list(pairwise ~ GROUP*EXP*AFTER:SWITCHED), adjust = "tukey")
emmeans(clme, list(pairwise ~ GROUP), adjust = "tukey")
# $`emmeans of GROUP`
# GROUP emmean    SE  df asymp.LCL asymp.UCL
# KIDS    56.3 0.821 Inf      54.6      57.9
# YA      61.2 0.958 Inf      59.4      63.1
# 
# Results are averaged over the levels of: SWITCHED, AFTER, EXP 
# Degrees-of-freedom method: asymptotic 
# Confidence level used: 0.95 
# 
# $`pairwise differences of GROUP`
# contrast  estimate   SE  df z.ratio p.value
# KIDS - YA    -4.99 1.26 Inf -3.955  0.0001 

tapply(switchpoint.df$COLOR, list(switchpoint.df$SWITCHED, switchpoint.df$AFTER, switchpoint.df$GROUP, switchpoint.df$EXP), mean, na.rm = TRUE)

X = tapply(switchpoint.df$COLOR, list(switchpoint.df$ID, switchpoint.df$SWITCHED, switchpoint.df$AFTER, switchpoint.df$GROUP, switchpoint.df$EXP), mean, na.rm = TRUE)

t.test(X[,'NOSWITCHER', 'BEFORE', 'YA', 'V1'], X[,'NOSWITCHER', 'BEFORE', 'KIDS', 'V1'])
t.test(X[,'NOSWITCHER', 'AFTER', 'YA', 'V1'], X[,'NOSWITCHER', 'AFTER', 'KIDS', 'V1'])

t.test(X[,'SWITCHER', 'BEFORE', 'YA', 'V1'], X[,'SWITCHER', 'BEFORE', 'KIDS', 'V1'])
t.test(X[,'SWITCHER', 'AFTER', 'YA', 'V1'], X[,'SWITCHER', 'AFTER', 'KIDS', 'V1']) #

t.test(X[,'SWITCHER', 'AFTER', 'YA', 'V1'] - X[,'SWITCHER', 'BEFORE', 'YA', 'V1'], X[,'SWITCHER', 'AFTER', 'KIDS', 'V1'] - X[,'SWITCHER', 'BEFORE', 'KIDS', 'V1'])

t.test(X[,'NOSWITCHER', 'BEFORE', 'YA', 'V4'], X[,'NOSWITCHER', 'BEFORE', 'KIDS', 'V4'])
t.test(X[,'NOSWITCHER', 'AFTER', 'YA', 'V4'], X[,'NOSWITCHER', 'AFTER', 'KIDS', 'V4']) # approaching

t.test(X[,'SWITCHER', 'BEFORE', 'YA', 'V4'], X[,'SWITCHER', 'BEFORE', 'KIDS', 'V4'])
t.test(X[,'SWITCHER', 'AFTER', 'YA', 'V4'], X[,'SWITCHER', 'AFTER', 'KIDS', 'V4'])


pdf('plots/Followed_aligned.pdf', width = 4, height = 4)
# tiff("Followed_align.tiff", units="in", width=4.5, height=4.5, res=300)


plot(follow_aligned_mean[1,], type = 'o', col = brewer.pal(6, 'Reds')[4], lty = 1, lwd = 2, 
     ylim = c(40, 100), ylab = '', xlab = '',   
     bty = 'n', cex.axis = 1.1, cex.lab = 1.2, lab = c(10, 5, 5), 
     xaxt = 'n')
abline(h = 50)
lines((1:12)+0.2, follow_aligned_mean[3,], type = 'o', lty = 1, lwd = 2, col = brewer.pal(6, 'Blues')[4])
lines(follow_aligned_mean[2,], type = 'o', pch = 2, lty = 1, lwd = 2, col = brewer.pal(6, 'Reds')[4])
lines((1:12)+0.2, follow_aligned_mean[4,], type = 'o', pch = 2, lty = 1, lwd = 2, col = brewer.pal(6, 'Blues')[4])

se_bars(1:12, follow_aligned_mean[1,], follow_aligned_ses[1,], col = brewer.pal(6, 'Reds')[4])
se_bars(1:12, follow_aligned_mean[2,], follow_aligned_ses[2,], col = brewer.pal(6, 'Reds')[4])
se_bars((1:12)+0.2, follow_aligned_mean[3,], follow_aligned_ses[3,], col = brewer.pal(6, 'Blues')[4])
se_bars((1:12)+0.2, follow_aligned_mean[4,], follow_aligned_ses[4,], col = brewer.pal(6, 'Blues')[4])

axis(1, at = 1:12, labels = c(-6:-1, 1:6), cex = 1.1)
mtext(1, text = 'Relative Block', line = 2.7, cex = 1.3)
mtext(2, text = 'Color Use (%)', line = 2.5, cex = 1.3)

legend('topleft', legend = c('Children No Switch', 'Adults No Switch', 'Children Switch', 'Adults Switch'), 
       bty = 'n', col = c(brewer.pal(6, 'Reds')[4],  brewer.pal(6, 'Blues')[4], brewer.pal(6, 'Reds')[4], brewer.pal(6, 'Blues')[4]), lwd = 2, pch = c(2, 2, 1, 1), cex = 0.8)

dev.off()


AdultsSwitch = sapply(1:16, function(x) mean(adult_switch <= x))*100
KidsSwitch = sapply(1:16, function(x) mean(kid_switch <= x))*100

pdf('plots/Followed_time.pdf', width = 3, height = 4) # this only outputs followed_time for participants who have switched
# tiff("Followed_time.tiff", units="in", width=4.5, height=4.5, res=300)

plot((1:16) + 0.1, AdultsSwitch-1, type = 'o', col = brewer.pal(6, 'Blues')[4], lty = 1, lwd = 2, 
     ylim = c(-1, 101), ylab = '', xlab = '',   
     bty = 'n', cex.axis = 1.1, cex.lab = 1.2, lab = c(10, 5, 5), 
     xaxt = 'n')
lines((1:16)-0.1, KidsSwitch+1, type = 'o', lty = 1, lwd = 2, col = brewer.pal(6, 'Reds')[4])
axis(1, at = seq(1, 16, by = 2), labels = seq(1, 8, by = 1), cex = 1.1)
mtext(1, text = 'Block', line = 2.7, cex = 1.3)
mtext(2, text = '% of Participants Switched ', line = 2.5, cex = 1.3)

legend('bottomright', legend = c('Children', 'Adults'), 
       bty = 'n', col = c(brewer.pal(6, 'Reds')[4],  brewer.pal(6, 'Blues')[4]), lwd = 2, pch = c(1, 1), cex = 0.8)
dev.off()




## CORRELATIONS BETWEEN VARS 
# task execution

# ambiguous.cdf 
all.mx <-
  right_join(
    all.mx,
  right_join(
    right_join(
      rownames_to_column(as.data.frame(stroop_rt_score_interfere)),
      rownames_to_column(as.data.frame(stroop_rt_score))
    ),
    rownames_to_column(as.data.frame(wmscore))) %>%
  rename("id" = "rowname"))

# # make one big df
all.mx <- right_join(allcc.mx, alltaskex_mx)
###

scores.df = data.frame(
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

WMmap = sapply(1:length(scores.df$ID), function(x) which(scores.df$ID[x] == names(wmscore))) #wmscore not found?
WMmap[unlist(lapply(WMmap, function(x) length(x)==0))] = NA
WMmap = unlist(WMmap)
scores.df$WM = wmscore[WMmap]

Smap = sapply(1:length(scores.df$ID), function(x) which(scores.df$ID[x] == names(stroop_rt_score)))
Smap[unlist(lapply(Smap, function(x) length(x)==0))] = NA
Smap = unlist(Smap)
scores.df$STROOP = stroop_rt_score[Smap]

Rmap = sapply(1:length(scores.df$ID), function(x) which(scores.df$ID[x] == names(recog)))
Rmap[unlist(lapply(Rmap, function(x) length(x)==0))] = NA
Rmap = unlist(Rmap)
scores.df$RECOG = recog[Rmap]

Umap = sapply(1:length(scores.df$ID), function(x) which(scores.df$ID[x] == names(used)))
Umap[unlist(lapply(Umap, function(x) length(x)==0))] = NA
Umap = unlist(Umap)
scores.df$USED = used[Umap]


scores.df$GROUPbin = NA
scores.df$GROUPbin[scores.df$GROUP == 'CHN Exp.1' | scores.df$GROUP == 'CHN Exp.2'] = 'KIDS'
scores.df$GROUPbin[scores.df$GROUP == 'ADLT Exp.1' | scores.df$GROUP == 'ADLT Exp.2'] = 'YA'
scores.df$GROUPbin = as.factor(scores.df$GROUPbin)

for (cvar in names(scores.df)[2:13]) {
  cvarz = paste(cvar, 'z', sep = '')
  scores.df[cvarz] = -as.vector(scale(scores.df[cvar]))
}

# recode vars where lower is 'better' (sign flip to all already above)
scores.df$FOLLz = -scores.df$FOLLz
scores.df$FOLLbinz = -scores.df$FOLLbinz
scores.df$WMz = -scores.df$WMz
scores.df$RECOGz = -scores.df$RECOGz
scores.df$USEDz = -scores.df$USEDz

scores_kids.df = subset(scores.df, scores.df$GROUPbin == 'KIDS')
scores_ya.df = subset(scores.df, scores.df$GROUPbin == 'YA')

summary(scores.df)
summary(scores_kids.df)
summary(scores_ya.df)

KT = as.table(cor(scores_kids.df[,2:13], use = 'pairwise.complete'))
YT = as.table(cor(scores_ya.df[,2:13], use = 'pairwise.complete'))

formatC(KT,digits=2, format="f") 
formatC(YT,digits=2, format="f") 


pmatya = pmatkids = matrix(NA, 12, 12)
for (i in 2:13) {
  for (j in 2:13) {
    pmatya[i-1, j-1] = cor.test(scores_ya.df[,i]*1, scores_ya.df[,j]*1)$p.value		
    pmatkids[i-1, j-1] = cor.test(scores_kids.df[,i]*1, scores_kids.df[,j]*1)$p.value		
  }
} ## error

colnames(pmatkids) = rownames(pmatkids) = colnames(KT)

colnames(pmatya) = rownames(pmatya) = colnames(KT)

(pmatkids) < .01

(pmatya*10) < .05

cor.test(scores_ya.df$FOLL, scores_ya.df$STROOP) # sig
cor.test(scores_ya.df$FOLLbin, scores_ya.df$STROOP) ## follbin ERROR
cor.test(scores_ya.df$FOLL, scores_ya.df$E) # ns

cor.test(scores_kids.df$FOLL, scores_kids.df$STROOP) # ns
cor.test(scores_kids.df$FOLL, scores_kids.df$E) #ns


cor.test(scores_ya.df$FOLLbin*1, scores_ya.df$RT)#ns
cor.test(scores_ya.df$FOLLbin*1, scores_ya.df$E)#ns

cor.test(scores_ya.df$FOLLbin*1, scores_ya.df$WM)#ns
cor.test(scores_ya.df$FOLLbin*1, scores_ya.df$STROOP)#ns

cor.test(scores_kids.df$FOLLbin*1, scores_kids.df$RT)#ns
cor.test(scores_kids.df$FOLLbin*1, scores_kids.df$E)#ns


cor.test(scores_kids.df$FOLLbin*1, scores_kids.df$WM)#ns
cor.test(scores_kids.df$FOLLbin*1, scores_kids.df$STROOP)#ns



#ctab = aggregate(scores.df[,16:27], list(scores.df$GROUPbin), mean, na.rm = TRUE)
#ctab_ses = aggregate(scores.df[,16:27], list(scores.df$GROUPbin), std.error)

cmeans = aggregate(scores.df[,c(2:7, 10:13)], list(scores.df$GROUPbin), mean, na.rm = TRUE)
csds = aggregate(scores.df[,c(2:7, 10:13)], list(scores.df$GROUPbin), sd, na.rm = TRUE)

mean_diff = cmeans[1,2:11] - cmeans[2,2:11]
pooled_sd = sqrt((csds[1,2:11]^2*(nkids-1) + csds[2,2:11]^2*(nadults-1))/(nkids + nadults - 2))

effect_size = mean_diff/pooled_sd

#recode WM, foll, follbin, recog and used, because in these variables higher is better 
# => (pos effect sizes reflect benefit forYA)
effect_size[,c(1, 7, 8, 9, 10)] = -effect_size[,c(1, 7, 8, 9, 10)] 
effect_size_CI = sqrt((nkids + nadults)/(nkids*nadults) + (effect_size^2)/((nkids+nadults)*2))*1.96





pdf('plots/Overview_effect_sizes.pdf', width = 4, height = 4)
par(mar=c(5.1,7.7,4.1,0.5))

plot(unlist(effect_size), 1:10, 
     type = 'p', col = c(rep('#459B76', 2), rep('#C66626', 4), rep('#C07CA5', 4)), 
     lwd = 2, xlim = c(-1, 2), ylim = c(0.5, 10.5), ylab = '', xlab = '', 
     bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
     xaxt = 'n', yaxt = 'n', pch = c(16), cex = 1.1)

abline(v = 0, lty = 2)
#se_bars(unlist(ctab[,c(2:7, 10:13)]), as.vector(matrix(c((1:10)-0.1, (1:10)+0.1), 2, 10, byrow = TRUE)), unlist(ctab_ses[,c(2:7, 10:13)]), 
#	col = c(rep('#459B76', 2*2), rep('#EEE461', 4*2), rep('#C07CA5', 4*2)), horiz = TRUE)

axis(1, at = seq(-1, 2, by = 0.5), labels = c('-1', '', '0', '', '+1', '', '+2'), cex = 0.9, mgp = c(3, 0.7, 0))
mtext(1, text = 'Effect Size', at = 0, line = 2, cex = 1)

text(-1.05, 1, 'Working memory', cex = 0.9, xpd = TRUE, pos = 2, col = '#459B76')
text(-1.05, 2, 'Stroop (neut.-cong.)', cex = 0.9, xpd = TRUE, pos = 2, col = '#459B76')

text(-1.05, 3, 'Errors', cex = 0.9, xpd = TRUE, pos = 2, col = '#C66626')
text(-1.05, 4, 'RTs', cex = 0.9, xpd = TRUE, pos = 2, col = '#C66626')
text(-1.05, 5, 'False alarms [delay]', cex = 0.9, xpd = TRUE, pos = 2, col = '#C66626')
text(-1.05, 6, 'False alarms [noGo]', cex = 0.9, xpd = TRUE, pos = 2, col = '#C66626')

text(-1.05, 7, 'Color use', cex = 0.9, xpd = TRUE, pos = 2, col = '#C07CA5')
text(-1.05, 8, 'Switch', cex = 0.9, xpd = TRUE, pos = 2, col = '#C07CA5')
text(-1.05, 9, 'Reported Discovery', cex = 0.9, xpd = TRUE, pos = 2, col = '#C07CA5')
text(-1.05, 10, 'Reported Use', cex = 0.9, xpd = TRUE, pos = 2, col = '#C07CA5')

dev.off()



pdf('plots/Overview.pdf', width = 4, height = 4)
par(mar=c(5.1,7.7,4.1,0.5))

plot(unlist(ctab[,c(2:7, 10:13)]), as.vector(matrix(c((1:10)-0.1, (1:10)+0.1), 2, 10, byrow = TRUE)), 
     type = 'p', col = c(rep('#459B76', 2*2), rep('#EEE461', 4*2), rep('#C07CA5', 4*2)), 
     lwd = 2, xlim = c(-0.8, 0.8), ylim = c(0.5, 10.5), ylab = '', xlab = '', 
     bty = 'n', cex.axis = 1.1, cex.lab = 1.2, 
     xaxt = 'n', yaxt = 'n', pch = c(16, 17), cex = 1.1)
abline(v = 0, lty = 2)
se_bars(unlist(ctab[,c(2:7, 10:13)]), as.vector(matrix(c((1:10)-0.1, (1:10)+0.1), 2, 10, byrow = TRUE)), unlist(ctab_ses[,c(2:7, 10:13)]), 
        col = c(rep('#459B76', 2*2), rep('#EEE461', 4*2), rep('#C07CA5', 4*2)), horiz = TRUE)

axis(1, at = seq(-0.8, 0.8, by = 0.4), labels = seq(-0.8, 0.8, by = 0.4), cex = 1.1)
mtext(1, text = 'z Score ', line = 2.7, cex = 1.3)

text(-0.85, 1, 'Working memory', cex = 0.9, xpd = TRUE, pos = 2, col = '#459B76')
text(-0.85, 2, 'Stroop (neut.-cong.)', cex = 0.9, xpd = TRUE, pos = 2, col = '#459B76')

text(-0.85, 3, 'Errors', cex = 0.9, xpd = TRUE, pos = 2, col = '#EEE461')
text(-0.85, 4, 'RTs', cex = 0.9, xpd = TRUE, pos = 2, col = '#EEE461')
text(-0.85, 5, 'False alarms [delay]', cex = 0.9, xpd = TRUE, pos = 2, col = '#EEE461')
text(-0.85, 6, 'False alarms [noGo]', cex = 0.9, xpd = TRUE, pos = 2, col = '#EEE461')

text(-0.85, 7, 'Color use', cex = 0.9, xpd = TRUE, pos = 2, col = '#C07CA5')
text(-0.85, 8, 'Switch', cex = 0.9, xpd = TRUE, pos = 2, col = '#C07CA5')
text(-0.85, 9, 'Reported Discovery', cex = 0.9, xpd = TRUE, pos = 2, col = '#C07CA5')
text(-0.85, 10, 'Reported Use', cex = 0.9, xpd = TRUE, pos = 2, col = '#C07CA5')

dev.off()

scores_cond.df = data.frame(
  Z = c(scores.df$WMz, scores.df$STROOPz, scores.df$Ez, scores.df$RTz, scores.df$PRELATEz, scores.df$PRENOz, scores.df$CONGz, scores.df$AMBIGz, scores.df$FOLLz, scores.df$FOLLbinz, scores.df$RECOGz, scores.df$USEDz), 
  ID = rep(scores.df$ID, 12), 
  GROUP = rep(scores.df$GROUPbin, 12), 
  COND = as.factor(rep(colnames(ctab)[2:13], each = nids)))

scores_cond.df = subset(scores_cond.df, scores_cond.df$COND != 'AMBIGz' & scores_cond.df$COND != 'CONGz')

clme = lmer(Z ~ COND*GROUP + (1|ID), data = scores_cond.df)
Anova(clme)
emmeans(clme, list(pairwise ~ GROUP|COND), adjust = "tukey")


CLD(clme)

ccols = rep(NA, nids)
ccols[scores.df$GROUPbin == 'YA'] = brewer.pal(6, 'Blues')[4]
ccols[scores.df$GROUPbin == 'KIDS'] = brewer.pal(6, 'Reds')[4]

plot(scores.df$WM, scores.df$FOLL, col = ccols, pch = 16)


cor(scores_kids.df[,2:13], use = 'pairwise.complete')
cor(scores_ya.df[,2:13], use = 'pairwise.complete')

flex_score = tapply(DATA$followed, list(DATA$id, DATA$block > 6 & DATA$block > 9, DATA$cond, DATA$resp %in% c(77, 88, 188)), mean)[,,2,'TRUE']
rt_score = tapply(DATA$rt, list(DATA$id, DATA$block > 6 & DATA$block > 9, DATA$cond, DATA$error == 0), mean)[,,1,'TRUE']

late_pre_score = tapply(DATA$prekey1>0, list(DATA$id, DATA$block > 6 & DATA$block > 9, DATA$cond), mean, na.rm = TRUE)[,,3]
nogo_pre_score = tapply(DATA$prekey1>0, list(DATA$id, DATA$block > 6 & DATA$block > 9, DATA$cond), mean, na.rm = TRUE)[,,4]

rt_costs = 
  tapply(DATA$rt, list(DATA$id, DATA$block, DATA$agegroup2, DATA$cond, DATA$rt > 200), mean, na.rm = TRUE)[,,,3, 'TRUE'] - 
  tapply(DATA$rt, list(DATA$id, DATA$block, DATA$agegroup2, DATA$cond, DATA$rt > 200), mean, na.rm = TRUE)[,,,1, 'TRUE']


wmidmap = unlist(lapply(Qdata$id, function(x) which(names(flex_score) %in% x)))
stroopidmap = unlist(lapply(names(stroop_rt_score), function(x) which(names(flex_score) %in% x)))

cdf = data.frame(FLEX = flex_score, RT = rt_score, LATE = late_pre_score, NOGO = nogo_pre_score, ID = as.factor(names(flex_score)))
cdf$AGEGROUP[cdf$ID %in% kidids2] = 'KIDS'
cdf$AGEGROUP[cdf$ID %in% adultids] = 'YA'
cdf$AGEGROUP = as.factor(cdf$AGEGROUP)

cdf$WM = NA
cdf$WM[wmidmap] = wmscore

cdf$STROOP = NA
cdf$STROOP[stroopidmap] = stroop_rt_score

caov = aov(FLEX ~ AGEGROUP*STROOP, data = cdf)
summary(caov)

cor.test(cdf$FLEX[cdf$AGEGROUP == 'YA'], cdf$STROOP[cdf$AGEGROUP == 'YA'])
cor.test(cdf$FLEX[cdf$AGEGROUP == 'KIDS'], cdf$STROOP[cdf$AGEGROUP == 'KIDS'])

cor.test(cdf$RT[cdf$AGEGROUP == 'YA'], cdf$STROOP[cdf$AGEGROUP == 'YA'])
cor.test(cdf$RT[cdf$AGEGROUP == 'KIDS'], cdf$STROOP[cdf$AGEGROUP == 'KIDS'])

caov = aov(WM ~ AGEGROUP*STROOP, data = cdf)
summary(caov)


caov = aov(LATE ~ AGEGROUP*STROOP, data = cdf)
summary(caov)

caov = aov(NOGO ~ AGEGROUP*STROOP, data = cdf)
summary(caov)


allidx = sapply(Qdata$id, function(x) which(ids == x))

allfoll = c(rowMeans(followed[kidids,c(8,8), 'KIDS']), rowMeans(followed[kidids_v1,c(8,8), 'KIDS_V1']), rowMeans(followed[adultids,c(8,8), 'YA']))*100


cor.test(wmscore[WMgroup == 'Kids'], allerrs[kidids[kididx]])
cor.test(wmscore[WMgroup == 'Kids V1'], allerrs[kidids_v1[kidv1idx]])
cor.test(wmscore[WMgroup == 'Adults'], allerrs[adultids[adultidx]])


cor.test(wmscore[WMgroup == 'Kids'], allrts[kidids[kididx]])
cor.test(wmscore[WMgroup == 'Kids V1'], allrts[kidids_v1[kidv1idx]])
cor.test(wmscore[WMgroup == 'Adults'], allrts[adultids[adultidx]])

cor.test(wmscore[WMgroup == 'Kids'], allfoll[kidids[kididx]])
cor.test(wmscore[WMgroup == 'Kids V1'], allfoll[kidids_v1[kidv1idx]])
cor.test(wmscore[WMgroup == 'Adults'], allfoll[adultids[adultidx]])

cor.test(c(wmscore[WMgroup == 'Kids'], wmscore[WMgroup == 'Kids V1']), c(allfoll[kidids[kididx]], allfoll[kidids_v1[kidv1idx]]))


cor.test(wmscore[group == 'Kids'], followed[kidids[kididx],8, 'KIDS'])
cor.test(wmscore[group == 'Kids V1'], followed[kidids_v1[kidv1idx],8, 'KIDS_V1'])

cor.test(wmscore[group == 'Adults'], followed[adultids[adultidx],8, 'YA'])

plot(wmscore[group == 'Adults'], followed[adultids[adultidx],8, 'YA'], col = colorset[2,2], pch = 16,
     bty = 'n', cex.axis = 1.1, cex.lab = 1.2, xlab = 'WM Score', ylab = '% followed')

x = cbind(followed[as.character(ids[allidx]),8, 'KIDS_V1'], followed[as.character(ids[allidx]),8, 'KIDS'], followed[as.character(ids[allidx]),8, 'YA'])

plot(wmscore, rowMeans(x, na.rm = TRUE))

abline(lm(followed[adultids[adultidx],8, 'YA']~wmscore[group == 'Adults']), lwd = 2, col = colorset[2,2])


tmp = apply(followed, c(2, 3), mean, na.rm = TRUE)*100
tmp.se = apply(followed, c(2, 3), std.error)*100

plot(tmp[,'KIDS'], type = 'l', col = colorset[2,1], lty = 1, lwd = 2, ylab = '% followed', xlab = 'Block',   
     bty = 'n', ylim = c(20, 100), cex.axis = 1.1, cex.lab = 1.2, lab = c(10, 5, 5), 
     xaxt = 'n')
lines(tmp[,'YA'], type = 'l', lty = 1, lwd = 2, col = colorset[2,2])
lines((1:9)+0.1, tmp[,'KIDS_V1'], type = 'l', lty = 1, lwd = 2, col = colorset[1,1])
se_bars(1:9, tmp[,'KIDS'], tmp.se[,'KIDS'], col = colorset[2,1])
se_bars((1:9)+0.1, tmp[,'KIDS_V1'], tmp.se[,'KIDS_V1'], col = colorset[1,1])
se_bars(1:9, tmp[,'YA'], tmp.se[,'YA'], col = colorset[2,2])

axis(1, at = 1:9, labels = c('', 2:8, ''), cex = 1.1)
mtext(3, text = 'Ambiguous Trials', font = 2, cex = 1.2)
text(1, -5, 'Rand', srt = 45, xpd = TRUE, pos = 3)
text(9, -5, 'Instr', srt = 45, xpd = TRUE, pos = 3)

abline(h = 50, lty = 2)


#allfoll = c(followed[kidids,8, 'KIDS'], followed[kidids_v1,8, 'KIDS_V1'], followed[adultids,8, 'YA'])*100
group =   as.factor(c(rep('Kids', nkids), rep('Kids V1', nkids_v1), rep('Adults', nadults)))
k = beeswarm(allfoll ~ group, ylab = 'followed (%)', col = c(colorset[2,2], colorset[2,1], colorset[1,1]), pch = 16,
             bty = 'n', cex.axis = 1.1, cex.lab = 1.2, xlab = 'Group')

t.test(followed[kidids,8, 'KIDS'], followed[kidids_v1,8, 'KIDS_V1'])
t.test(followed[adultids,8, 'YA'], followed[kidids_v1,8, 'KIDS_V1'])
t.test(followed[adultids,8, 'YA'], followed[kidids,8, 'KIDS'])

t.test(followed[adultids,8, 'YA'], c(followed[kidids,8, 'KIDS'], followed[kidids_v1,8, 'KIDS_V1']))

a = lm(allfoll~group)
b = lm(allfoll~allerrs)
c = lm(allfoll~allerrs+group)

anova(a,c)
summary(c)

b = lm(allfoll~allerrs)$res

t.test(b[group == 'Adults'], b[group == 'Kids V1'])

plot(allfoll, allerrs, col = group)

allfoll_adj = allfoll/(100-allerrs)
group =   as.factor(c(rep('Kids', nkids), rep('Kids V1', nkids_v1), rep('Adults', nadults)))
k = beeswarm(allfoll_adj ~ group, ylab = 'followed (%)', col = c(colorset[2,2], colorset[2,1], colorset[1,1]), pch = 16,
             bty = 'n', cex.axis = 1.1, cex.lab = 1.2, xlab = 'Group')

t.test(allfoll_adj[group == 'Kids'], allfoll_adj[group == 'Adults'])
t.test(allfoll_adj[group == 'Kids V1'], allfoll_adj[group == 'Adults'])
t.test(c(allfoll_adj[group == 'Kids'], allfoll_adj[group == 'Kids V1']), allfoll_adj[group == 'Adults'])



# 0.65625 is binomial threshold for .05 
tmp = apply(followed>=follthresh, c(2, 3), mean, na.rm = TRUE)*100
tmp.se = apply(followed>=follthresh, c(2, 3), std.error)*100

plot((1:9)-0.1, tmp[,'KIDS'], type = 'l', col = colorset[2,1], lty = 1, lwd = 2, ylab = '% subjs p <.05', xlab = 'Block',   
     bty = 'n', ylim = c(0, 100), cex.axis = 1.1, cex.lab = 1.2, lab = c(10, 5, 5), 
     xaxt = 'n')
lines(tmp[,'YA'], type = 'l', lty = 1, lwd = 2, col = colorset[2,2])
lines((1:9)+0.1, tmp[,'KIDS_V1'], type = 'l', lty = 1, lwd = 2, col = colorset[1,1])
se_bars((1:9)-0.1, tmp[,'KIDS'], tmp.se[,'KIDS'], col = colorset[2,1])
se_bars((1:9)+0.1, tmp[,'KIDS_V1'], tmp.se[,'KIDS_V1'], col = colorset[1,1])
se_bars(1:9, tmp[,'YA'], tmp.se[,'YA'], col = colorset[2,2])
abline(h = 5, lty = 2)
axis(1, at = 1:9, labels = c('', 2:8, ''), cex = 1.1)
mtext(3, text = 'Ambiguous Trials', font = 2, cex = 1.2)
text(1, -35, 'Rand', srt = 45, xpd = TRUE, pos = 3)
text(9, -35, 'Instr', srt = 45, xpd = TRUE, pos = 3)




# zscore and do interaction 
cols = c(colorset[2,2], colorset[2,1], colorset[2,1])
names(cols) = c('Adults', 'Kids', 'Kids V1')
cdf = data.frame(z = c(scale(allerrs), scale(allrts), scale(allfoll)), COND = gl(3, nids), ID = as.factor(rep(names(allfoll), 3)), GROUP = as.factor(rep(group , 3)))
cdf$z2 = ceiling(c(rank(allerrs, ties.method = "random"), rank(allrts, ties.method = "random"), rank(allfoll, ties.method = "random"))/4)

table(cdf$z2)
cdf$CONDxGROUP = as.factor(paste(as.character(cdf$GROUP), as.character(cdf$COND), sep = '_'))

cdf$CONDxGROUP = factor(cdf$CONDxGROUP,levels(cdf$CONDxGROUP)[c(1,4,7, 2, 5, 8, 3, 6, 9)])

beeswarm(z2~COND, data = cdf, pwcol = cols[cdf$GROUP], pch = 16)


clme = lmer(z ~ GROUP*COND * (1| ID), data = cdf)

Anova(clme)
t.test(cdf$z[cdf$CONDxGROUP == 'Adults_1'], cdf$z[cdf$CONDxGROUP == 'Kids_1'])
t.test(cdf$z[cdf$CONDxGROUP == 'Adults_2'], cdf$z[cdf$CONDxGROUP == 'Kids_2'])
t.test(cdf$z[cdf$CONDxGROUP == 'Adults_3'], cdf$z[cdf$CONDxGROUP == 'Kids_3'])

cdf$GROUP*cdf$COND


allfoll = c(followed[kidids,8, 'KIDS'], followed[adultids,8, 'YA'])*100
group =   c(rep('Kids', nkids), rep('Adults', nadults))
beeswarm(allfoll ~ group, ylab = 'Color use (%)', col = colorset[2,1:2], pch = 16,
         bty = 'n', cex.axis = 1.1, cex.lab = 1.2, xlab = 'Age Group')




tapply(allfoll>75, group, mean)

# knowledge, stroop and WM data 
recog = Qdata$recognised
recog = Recode(recog, "'yes' = 1; 'no' = 0") #+ rnorm(length(recog), m = 0, sd = 0.05)
agegroup = Qdata$age>15
agegroup = as.factor(Recode(agegroup, "FALSE = 'Kids'; TRUE = 'Adults'"))

barplot(c(mean(recog[agegroup == 'Adults']), mean(recog[agegroup == 'Kids']))*100, 
        col = colorset[2,1:2], border = NA)

table(recog, agegroup)

Qkididx = which(sapply(Qdata$id, function(x) (x %in% kidids)))
Qkidids = unlist(lapply(Qdata$id, function(x) which(kidids == x)))

Qadultidx = which(sapply(Qdata$id, function(x) (x %in% adultids)))
Qadultids = unlist(lapply(Qdata$id, function(x) which(adultids == x)))

cor.test(wmscore[Qkididx], followed[kidids[Qkidids], 8, 'KIDS'])
cor.test(wmscore[Qadultidx], followed[adultids[Qadultids], 8, 'YA'])


