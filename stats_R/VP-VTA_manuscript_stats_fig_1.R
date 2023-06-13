
###### enter python env ####
Sys.setenv(RETICULATE_PYTHON = "C:/Users/Dakota/anaconda3/envs/spyder-env-seaborn-update")

pd <- import("pandas")


#%%-- Import dependencies ####
# library(lme4)
library(reticulate)
library(lmerTest)
library(emmeans)

#%% -- Set Paths ####

#- NOTE: manually change working directory in RStudio to source file location! (up top session -> set working directory -> to source file location)
# https://statisticsglobe.com/set-working-directory-to-source-file-location-automatically-in-rstudio

#-Note: To read .pickles, pandas version in R environment has to match pandas version of .pkl created!

pathWorking= getwd()

pathOutput= paste(pathWorking,'/_output', sep="")
#get rid of space introduced by paste()
gsub(" ", "", pathOutput)


pathInput= paste(pathWorking,'/_input', sep="")
#get rid of space introduced by paste()
gsub(" ", "", pathInput)



# __________________________________________________ ####

## ----- FIGURE 1D Stats A- PE Prob---------------------------------------------------------####


#0%%-- Clear vars between tests ####
# #clear workspace (R environment) # Except paths, Python packages (pandas)
rm(list = setdiff(ls(), c("pathWorking", "pathOutput", "pathInput", "pd")))


#%% Figure 1D Stats A -- Compare DS vs NS PE Ratio--##

##1%%-- Load data from .pkl ####
pathData= paste(pathInput,'/fig1d.pkl', sep="")

df <- pd$read_pickle(pathData)


###### summarize data
summary(df)

#verify dtypes imported properly
sapply(df, class) 


#%%-- Subset data ## 
#Remove missing/invalid observations 
# #-- Currently trainDayThisPhase is 0 both on last day of Early and last day of Late. Make distinct (bc coded as distinct factor)
# df[(df$trainDayThisPhase== 0) & (df$trainPhase=='early'), 'trainDayThisPhase']= as.factor(c(99))

# to change the Factor column, unfactorize first, change, and then refactorize
df$trainDayThisPhase= as.character(df$trainDayThisPhase)

df[(df$trainDayThisPhase== 0) & (df$trainPhase=='early'), 'trainDayThisPhase']= (c(999))

df$trainDayThisPhase= as.factor(df$trainDayThisPhase)


#only include the late trainPhase (when NS is present)

df_Sub_A= df[df$trainPhase=='late',]

#since we've dropped levels(categories) from the factor(categorical) variable trainDayThisPhase, drop accordingly for stats to work out
# droplevels(df_Sub_A$trainDayThisPhase)
# droplevels(df_Sub_A$trainPhase)


#2%%-- Run LME ####

model= lmerTest::lmer('trialTypePEProb10s  ~ trialType * trainDayThisPhase + (1|subject)', data=df_Sub_A)


model_anova<- anova(model)


#3%%-- Run Follow-up post-hoc tests ####

#-- Pairwise comparisons (t test) between TrialOutcome
#- Viz interaction plot & save
figName= "vp-vta_fig1D_stats_A_interactionPlot.pdf"
setwd(pathOutput)
pdf(file=figName)

emmip(model, trialType ~ trainDayThisPhase)

dev.off()
setwd(pathWorking)

#- Pairwise T- tests
EMM <- emmeans(model, ~ trialType | trainDayThisPhase)   # where treat has 2 levels
tPairwise= pairs(EMM, adjust = "sidak")   # adjustment is ignored - only 1 test per group
summary(tPairwise, by = NULL, adjust = "sidak")   # all are in one group now


#4%%-- Save output to variables between tests  ####
# trying to keep code mostly generalizable and just save custom names at end
# all the results into descriptive variables between tests
fig1D_stats_A_0_description= "Figure 1D: Late Training DS vs NS PE Ratio"
fig1D_stats_A_1_model= model
fig1D_stats_A_2_model_anova= model_anova
fig1D_stats_A_3_model_post_hoc_pairwise= tPairwise 


#5%%-- Save output to File ####
# B
setwd(pathOutput)


sink("vp-vta_fig1D_stats_A_LateTraining_DSvsNS_PE_Ratio.txt")
'------------------------------------------------------------------------------'
'0)---- Description --: '
print(fig1D_stats_A_0_description)
'------------------------------------------------------------------------------'
print('1)---- LME:')
print(summary(fig1D_stats_A_1_model))
'------------------------------------------------------------------------------'
print('2)---- ANOVA of LME:')
print(fig1D_stats_A_2_model_anova)
'------------------------------------------------------------------------------'
print('3)---- Posthoc pairwise:')
print(fig1D_stats_A_3_model_post_hoc_pairwise, by=NULL, adjust='sidak')
'---- END ---------------------------------------------------------------------'
sink()  # returns output to the console

setwd(pathWorking)


# __________________________________________________ ####

#%% FIGURE 1D Stats B -- Learning DS PE Ratio--####

#0%%-- Clear vars between tests ####
# #clear workspace (R environment) # Except paths, Python packages (pandas)
rm(list = setdiff(ls(), c("pathWorking", "pathOutput", "pathInput", "pd")))

#1%%-- Load data from .pkl ####

pathData= paste(pathInput,'/fig1d.pkl', sep="")


df <- pd$read_pickle(pathData)

#%%-- Subset data ## 
#Remove missing/invalid observations 
#include only DS PE Ratios, across both phases
df_Sub_B= df[df$trialType=='DStime',]

#since we've dropped levels(categories) from the factor(categorical) variable trainDayThisPhase, drop accordingly for stats to work out
# droplevels(df_Sub_A$trainDayThisPhase)
# droplevels(df_Sub_A$trainPhase)


#2%%-- Run LME ##
#-- This isn't good because trainDayThisPhase==0 in both early and late. ----$$$$
model= lmerTest::lmer('trialTypePEProb10s  ~ trainDayThisPhase + (1|subject)', data=df_Sub_B)


model_anova<- anova(model)


#3%%-- Run Follow-up post-hoc tests ####

# #-- Pairwise comparisons (t test) between TrialOutcome
# #- Viz interaction plot & save
# figName= "vp-vta_fig1D_stats_B_interactionPlot.pdf"
# setwd(pathOutput)
# pdf(file=figName)
# 
# emmip(model, trialType ~ trainDayThisPhase)
# 
# dev.off()
# setwd(pathWorking)

#- Pairwise T- tests
EMM <- emmeans(model, ~  trainDayThisPhase)   # where treat has 2 levels
tPairwise= pairs(EMM, adjust = "sidak")   # adjustment is ignored - only 1 test per group
summary(tPairwise, by = NULL, adjust = "sidak")   # all are in one group now


#4%%-- Save output to variables between tests  ####
# trying to keep code mostly generalizable and just save custom names at end
# all the results into descriptive variables between tests
fig1D_stats_B_0_description= "Figure 1D:  Learning- DS PE Ratio early & late"
fig1D_stats_B_1_model= model
fig1D_stats_B_2_model_anova= model_anova
fig1D_stats_B_3_model_post_hoc_pairwise= tPairwise 


#5%%-- Save output to File ####
# B
setwd(pathOutput)


sink("vp-vta_fig1D_stats_B_Learning_DS_PE_Ratio.txt")
'------------------------------------------------------------------------------'
'0)---- Description --: '
print(fig1D_stats_B_0_description)
'------------------------------------------------------------------------------'
print('1)---- LME:')
print(summary(fig1D_stats_B_1_model))
'------------------------------------------------------------------------------'
print('2)---- ANOVA of LME:')
print(fig1D_stats_B_2_model_anova)
'------------------------------------------------------------------------------'
print('3)---- Posthoc pairwise:')
print(fig1D_stats_B_3_model_post_hoc_pairwise, by=NULL, adjust='sidak')
'---- END ---------------------------------------------------------------------'
sink()  # returns output to the console

setwd(pathWorking)


# __________________________________________________ ####



## ----- FIGURE 1_Supplement Stats A- PE Latency---------------------------------------------------------####

#0%%-- Clear vars between tests ####
# #clear workspace (R environment) # Except paths, Python packages (pandas)
rm(list = setdiff(ls(), c("pathWorking", "pathOutput", "pathInput", "pd")))

#1%%-- Load data from .pkl ####

pathData= paste(pathInput,'/fig1d_supplement_latency.pkl', sep="")

df <- pd$read_pickle(pathData)


###### summarize data
summary(df)

#verify dtypes imported properly
sapply(df, class) 


#%%-- Subset data ## 
#Remove missing/invalid observations 
#only include the late trainPhase (when NS is present)

df_Sub_A= df[df$trainPhase=='late',]

#since we've dropped levels(categories) from the factor(categorical) variable trainDayThisPhase, drop accordingly for stats to work out
# droplevels(df_Sub_A$trainDayThisPhase)
# droplevels(df_Sub_A$trainPhase)


#2%%-- Run LME ####

model= lmerTest::lmer('eventLatency  ~ trialType * trainDayThisPhase + (1|subject)', data=df_Sub_A)


model_anova<- anova(model)


#3%%-- Run Follow-up post-hoc tests ####

#-- Pairwise comparisons (t test) between TrialOutcome
#- Viz interaction plot & save
figName= "vp-vta_fig1_supplement_stats_A_interactionPlot.pdf"
setwd(pathOutput)
pdf(file=figName)

emmip(model, trialType ~ trainDayThisPhase)

dev.off()
setwd(pathWorking)

#- Pairwise T- tests
EMM <- emmeans(model, ~ trialType | trainDayThisPhase)   # where treat has 2 levels
tPairwise= pairs(EMM, adjust = "sidak")   # adjustment is ignored - only 1 test per group
summary(tPairwise, by = NULL, adjust = "sidak")   # all are in one group now


#4%%-- Save output to variables between tests  ####
# trying to keep code mostly generalizable and just save custom names at end
# all the results into descriptive variables between tests
fig1D_stats_A_0_description= "Figure 1 supplement: Late Training DS vs NS PE Latency"
fig1D_stats_A_1_model= model
fig1D_stats_A_2_model_anova= model_anova
fig1D_stats_A_3_model_post_hoc_pairwise= tPairwise 


#5%%-- Save output to File ####
# B
setwd(pathOutput)


sink("vp-vta_fig1_Supplement_stats_A_LateTraining_DSvsNS_PE_Latency.txt")
'------------------------------------------------------------------------------'
'0)---- Description --: '
print(fig1D_stats_A_0_description)
'------------------------------------------------------------------------------'
print('1)---- LME:')
print(summary(fig1D_stats_A_1_model))
'------------------------------------------------------------------------------'
print('2)---- ANOVA of LME:')
print(fig1D_stats_A_2_model_anova)
'------------------------------------------------------------------------------'
print('3)---- Posthoc pairwise:')
print(fig1D_stats_A_3_model_post_hoc_pairwise, by=NULL, adjust='sidak')
'---- END ---------------------------------------------------------------------'
sink()  # returns output to the console

setwd(pathWorking)



# __________________________________________________ ####

#%% -- Figure 1_ Supplement Stats Stats A.2 - # Number of Training Days to End ####

#0%%-- Clear vars between tests ####
# #clear workspace (R environment) # Except paths, Python packages (pandas)
rm(list = setdiff(ls(), c("pathWorking", "pathOutput", "pathInput", "pd")))


#1%%-- Load/subset data from .pkl ####

pathData= paste(pathInput,'/fig1d.pkl', sep="")

df <- pd$read_pickle(pathData)


#- Subset Final Session (Training End)
df_Sub_A_finalSes= df[(df$trainDayThisPhase==0) & (df$trainPhase=='late'),]


#- SUbset to one observation per file
df_Sub_A_finalSes= df_Sub_A_finalSes[df_Sub_A_finalSes$trialType=='DStime',]


# Descriptive stats of train day
fig1D_stats_A_4_daysToTrainEnd= summary(df_Sub_A_finalSes$trainDay)





