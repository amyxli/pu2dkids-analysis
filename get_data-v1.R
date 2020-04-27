# get_data-v1.0.R. LAST:
## EDITED 22/1/20
## this version of the script retains 10 blocks from experiment 1, where block 9 and 10 are the 2 instructed blocks
## changes of interest from 97-152

#####################################################################################################################
##                                                 DATA FROM MAIN TASK                                             ## 
#####################################################################################################################

# get list of files 
rm(list=ls())

datadir = 'data/pu2d_kids/'
files = dir(datadir, pattern='main.txt', recursive = TRUE)
files = files[setdiff(1:length(files), grep('training', files))]
files = files[!duplicated(lapply(strsplit(files, c('/')), function(x) x[length(x)]))]


# conds: 
# 1 = standard
# 2 = ambiguous
# 3 = delayed Go 
# 4 = delayed noGo
# 5 = No Stimulus

# read first file to initialize data structure 
DATA = read.table(paste(datadir, files[1], sep=''), fill = TRUE, header = TRUE, sep = ',', nrows = 1)
DATA$id = NA
DATA$date = as.POSIXct('1999-05-20 15:38:03 CEST')


#loop over remaining files, appening the data to the bottom each time 
for (i in 1:length(files)) {
	tmp = read.table(paste(datadir, files[i], sep=''), fill = TRUE, header = FALSE, skip = 1, sep = ',')
	
	if (dim(tmp)[2] == 1) {
		tmp = read.table(paste(datadir, files[i], sep=''), fill = TRUE, header = FALSE, skip = 1, sep = '\t')
	}
	
	cdate = file.info(paste(datadir, files[i], sep = ''))$mtime
	tmp$date = as.POSIXct(cdate)
	# stuff below is just to handle exceptions 
	excl = apply(tmp, 2, function(x) mean(is.na(x))>0.8)
	tmp = tmp[,!excl]
	colnames(tmp)  = colnames(DATA)
	tmp = subset(tmp, tmp$id!='*****' & tmp$id!='break and restart')
	DATA = rbind(DATA, tmp)
	}

DATA_pre_get_exc <- DATA

# exclude subjects with inclomplete data 
DATA = subset(DATA, !is.na(DATA$id))
DATA$id = as.numeric(DATA$id)
DATA$id[DATA$id < 30] = DATA$id[DATA$id < 30] + 500
DATA$id = as.character(DATA$id)

tapply(as.numeric(DATA$id), DATA$age, function(x) length(unique(x))) 
aborted = c(203, 216, 311)
DATA = subset(DATA, !(DATA$id %in% aborted))

DATA_backup = DATA

# first, cut off trialing trials to get precise condition distributions  
cDATA = subset(DATA, DATA$btrial < 169)

# get task stats to sort into versions

# 1. no trials per block & number of blocks 
trialsperblock = apply(tapply(cDATA$btrial, list(cDATA$id, cDATA$block), length), 1, mean, na.rm = TRUE)

numblocks = tapply(cDATA$block, list(cDATA$id), max)
# 2. timing of conditions (RSIs)
#timing = round(apply(tapply(cDATA$framedur, list(cDATA$id, cDATA$block, cDATA$cond), median), c(1, 3), mean, na.rm = TRUE), digits = -2)
timing = round(tapply(cDATA$framedur, list(cDATA$id, cDATA$cond), function(x) mean(x[x>50])), digits = -2)

# 3. no of trials per condition  
trialspercond = apply(tapply(cDATA$error, list(cDATA$id, cDATA$block, cDATA$cond), length), c(1, 3), mean, na.rm = TRUE)

randomblock = (apply(tapply(cDATA$dimB, list(cDATA$id, cDATA$dimA, cDATA$block, cDATA$cond), function(x) abs(mean(x) - round(mean(x))))[,,,1], c(1, 3), mean, na.rm = TRUE)>0.1)*1

# concat full characteristics and find unique combinations, i.e. task versions 
idtask = cbind(trialsperblock, numblocks, timing, trialspercond)


versions = unique(idtask)
versionnames = c('V1', 'V1b', 'V1c', 'V2', 'V3', 'V4', 'V4')

DATA$taskversion = NULL

for (ctask in 1:dim(versions)[1]) {
	cids = which(apply(idtask, 1, function(x) mean(x == versions[ctask,], na.rm = TRUE)) == 1)
    DATA$taskversion[DATA$id %in% names(cids)] = versionnames[ctask]	
}
DATA$taskversion = as.factor(DATA$taskversion)

framedur = tapply(DATA$framedur, list(DATA$cond, DATA$taskversion), max)
soa = tapply(DATA$soa, list(DATA$cond, DATA$taskversion), mean)
rsi = tapply(DATA$rsi, list(DATA$cond, DATA$taskversion), mean)


DATA$taskV = car::recode(DATA$taskversion, "'V1b' = 'V1'") 

DATA$agegroup = DATA$age > 15
DATA$agegroup[DATA$agegroup == TRUE] = 'YA'
DATA$agegroup[DATA$agegroup == FALSE] = 'KIDS'

tapply(DATA$id, DATA$age>15, function(x) length(unique(x))) 
tapply(DATA$id, list(DATA$age>15, DATA$taskV), function(x) length(unique(x))) 

tapply(DATA$id, list(DATA$age>15, DATA$taskversion), function(x) length(unique(x))) 


### add some useful variables to DATA 

DATA$ambig = (DATA$cond ==2)
congruencymap = c(2,1,2,1,0)
DATA$incong = congruencymap[DATA$dimA]
DATA$half = DATA$block > 4
DATA$id = as.character(DATA$id)
# get keymapping for each individual subject 
# check if subj follows keymapping for at least 50% of trials, separtely for each key (truekeys)
# check if often neighboring key was pressed and corrrect if necessary
mapping1 = c(188, 88, 88, 188)
mapping2 = c(77, 88, 88, 77)
DATA$followed = NA
cidx = DATA$cond == 2 & as.numeric(DATA$id) <1000
DATA$followed[cidx] = (DATA$respkey[cidx] == mapping1[DATA$dimB[cidx]])
cidx = DATA$cond == 2 & as.numeric(DATA$id) >1000
DATA$followed[cidx] = (DATA$respkey[cidx] == mapping2[DATA$dimB[cidx]])

# select relevant task versions (V1 and V4)
DATA = subset(DATA, (DATA$taskV == 'V1' | DATA$taskV == 'V4'))

DATA_backup1 <- DATA %>% dplyr::filter(taskversion == 'V1b') %>% dplyr::filter(block == "9" | block == "10"| block == "11") %>% dplyr::filter (btrial == 1)

DATA$block[DATA$taskversion == 'V1b'] = car::recode(DATA$block[DATA$taskversion == 'V1b'], "9 = 12; 10 = 9") 

DATA$block[DATA$taskversion == 'V1b'] = car::recode(DATA$block[DATA$taskversion == 'V1b'],  "11 = 10")
## V1b was version of task where participants were tested with 11 blocks
# keep blocks 10 and 11 (instructed)
# need to drop block 9, and recode 11 = 10, 10 = 9

DATA_backup2 <- DATA %>% dplyr::filter(taskversion == 'V1b') %>% dplyr::filter(block == "9" | block == "10"| block == "11" |block == "12") %>% dplyr::filter (btrial == 1)
# check new DATA by comparing DATA_backup2 against DATA_backup1

DATA$miniblock = ceiling(DATA$btrial/84) + (DATA$block- 1)*2
DATA = subset(DATA, DATA$block < 11) # keep blocks 1 to 10 (V1 should have 10 blocks, and V2 should have 9 blocks)

DATA = subset(DATA, DATA$cond < 5) # keep only conditions 1 to 4 (remove no stimulus cond)
DATA = subset(DATA, !is.na(DATA$id))
DATA$taskversion = droplevels(DATA$taskversion)
DATA$taskV = droplevels(DATA$taskV)


#####################################################################################################################
##                                         QUESTIONNAIRE DATA - in prog                                            ## 
#####################################################################################################################

#read questionnaire 

Qdatadir = 'data/pu2d_kids/all_questionnaires'
files = dir(Qdatadir, pattern='.xlsx', recursive = TRUE)
files = files[grep('Q', files)]

for (cf in files) {
  tmp = gdata::read.xls(paste(Qdatadir, '/', cf, sep = ''), sheet = 1, header = TRUE)
  if (cf == files[1]) Qdata = tmp[,1:13]
  else Qdata = rbind(Qdata, tmp[,1:13])
}

Qdata$sex = car::recode(Qdata$sex, "1 = 'f'; 0 = 'm'")
Qdata$handedness = car::recode(Qdata$handedness, "1 = 'right'; 0 = 'left'")
Qdata$recognised = car::recode(Qdata$recognised, "1 = 'yes'; 0 = 'no'")
Qdata$used = car::recode(Qdata$used, "1 = 'yes'; 0 = 'no'")

Qdata = Qdata[sort(Qdata$id, index.return = TRUE)$ix,]

Qdata$left.up[Qdata$left.up == '0'] = 'red' 
Qdata$left.up[Qdata$left.up == '1'] = 'green' 
Qdata$left.up = as.factor(Qdata$left.up)

Qdata$right.up[Qdata$right.up == '0'] = 'red' 
Qdata$right.up[Qdata$right.up == '1'] = 'green' 
Qdata$right.up = as.factor(Qdata$right.up)

Qdata$left.down[Qdata$left.down == '0'] = 'red' 
Qdata$left.down[Qdata$left.down == '1'] = 'green' 
Qdata$left.down = as.factor(Qdata$left.down)
Qdata$right.down[Qdata$right.down == '0'] = 'red' 
Qdata$right.down[Qdata$right.down == '1'] = 'green' 
Qdata$right.down = as.factor(Qdata$right.down)

# a check of ids
length(Qdata$id)
length(unique(DATA$id))

# Qdata id's not in DATA
QnotinDATA <- Qdata$id[!Qdata$id %in% as.integer(unique(DATA$id))] 
### [1] 150 151 152 153 154 155 203 216 250 251 252 253 254 255 256 257 311

# check if these missing ids were because of exclusion from DATA
# due to incomplete data
(subset(DATA_pre_get_exc, DATA_pre_get_exc$id %in% QnotinDATA))$id %>% unique()

## DATA id's not in Qdata
as.integer(unique(DATA$id))[!as.integer(unique(DATA$id)) %in% Qdata$id] 
###  [1] 1101 1102 1103 1104 1105 1106 1201 1202 3202 3205

unique(subset(DATA, DATA$taskversion == "V1b")$id)
# [1] "1101" "1102" "1103" "1104" "1105" "1106" "1201" "1202" "1203" "3101"
# [11] "3201" "3202" "3204" "3205" "3206"

## sort this out!

#####################################################################################################################
##                                                 STROOP DATA                                                     ## 
#####################################################################################################################

# Stroop data for Kids 
Sdatadir = 'data/pu2d_kids/'
files = dir(Sdatadir, pattern='.csv', recursive = TRUE)
files = files[grep('troop', files)]

files = paste(datadir, files, sep = '')

Sdata = read.table(files[1], fill = TRUE, header = TRUE, sep = ',')

for (i in 2:length(files)) {
  tmp = read.table(files[i], fill = TRUE, header = TRUE, sep = ',')	
  Sdata = rbind(Sdata, tmp)
}

# stroop from V1 is stored in different file 
tmp = gdata::read.xls('data/pu2d_kids/V1/stroop/Stroop_combined_raw_data_matched.xlsx')
names(tmp) == names(Sdata)

Sdata = rbind(Sdata, tmp)
Sdata$id = Sdata$participant
Sdata$participant = NULL
Sdata$id[Sdata$id < 30] = Sdata$id[Sdata$id < 30] + 500


# read in stroop data from version 1b 
files = dir(Sdatadir, pattern='.txt', recursive = TRUE)
files = files[grep('troop', files)]
files = files[grep('V1b', files)]
#files = files[-grep('~', files)]
files = paste(datadir, files, sep = '')

Sdata2 = read.table(files[1], skip = 1, header = TRUE, fill = TRUE, sep = '\t', fileEncoding='UTF-16')	
Sdata2$Clock.Information = NULL
Sdata2$Attribute1 = NULL
#tmp = read.xls(files[3], pattern = 'Experiment')	

for (i in 2:length(files)) {
  #tmp = read.xls(files[i], pattern = 'Experiment')	
  tmp = read.table(files[i], skip = 1, header = TRUE, fill = TRUE, sep = '\t', fileEncoding='UTF-16')	
  tmp$Clock.Information = NULL
  tmp$Attribute1 = NULL
  
  Sdata2 = rbind(Sdata2, tmp)
}
Sdata2$id = Sdata2$Subject
Sdata2$Subject = NULL
Sdata2$word = as.character(Sdata2$word)
Sdata2$word[Sdata2$word == 'blau'] = 'blue'
Sdata2$word[Sdata2$word == 'rot'] = 'red'
Sdata2$word = as.factor(Sdata2$word)
Sdata2$cond = as.factor(car::recode(Sdata2$cond, "0 = 'Practice'; 1 = 'cong'; 2 = 'incong'; 3 = 'neutral'"))

Sdata2old = Sdata2

Sdata$text = as.character(Sdata$text)
Sdata$text[Sdata$text == 'blau'] = 'blue'
Sdata$text[Sdata$text == 'gelb'] = 'yellow'
Sdata$text = as.factor(Sdata$text)

tmp = tapply(as.character(Sdata$text)==as.character(Sdata$letterColor), Sdata$congruent, mean)
tapply(as.character(Sdata$text)=='xxxx', Sdata$congruent, mean)


summary(Sdata2)
summary(Sdata2old)


# check ids

# DATA id's not in Sdata and Sdata2
as.integer(unique(DATA$id))[!as.integer(unique(DATA$id)) %in% unique(c(Sdata2$id, Sdata$id))]

# [1] 1101 1102 1103 1104 1105 1106 1201 1202 3101 3202  320  323  502
# 10 missing from V1b
# 320 and 323 are V4 child
# 502 V4 YA - program failed? raw files are blank

## SAVE 320 as csv


####
## check the program version

### assign eprime = 1 if stroop program was eprime
Sdata <- 
  Sdata %>%
  mutate(eprime = ifelse(Sdata$id %in% (DATA %>% dplyr::filter(taskversion =="V1b"))$id, 1, 0))

Sdata2  <- 
  Sdata2 %>%
  mutate(eprime = ifelse(Sdata2$id %in% (DATA %>% dplyr::filter(taskversion =="V1b"))$id, 1, 0))

## save the data

save(DATA, Qdata, Sdata, Sdata2, file = "pu2dkids_data-all.Rdata")

                                    