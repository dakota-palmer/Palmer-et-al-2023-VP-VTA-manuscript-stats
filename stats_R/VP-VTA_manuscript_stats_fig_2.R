
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


#%% --- FIGURE 2B STATS -------------------------------------------- ## 

#%% Figure 2B Stats A -- Compare DS vs NS AUC on special sessions with NS (stage >5)--####

#0%%-- Clear vars between tests ####
# #clear workspace (R environment) # Except paths, Python packages (pandas)
rm(list = setdiff(ls(), c("pathWorking", "pathOutput", "pathInput", "pd")))


#1%%-- Load data from .pkl ####

pathData= paste(pathInput,'/fig2b.pkl', sep="")


df <- pd$read_pickle(pathData)


###### summarize data
summary(df)

#verify dtypes imported properly
sapply(df, class) 


#%%-- Subset data ## 
#Remove missing/invalid observations 
#-can only do stat comparison for DS vs NS in stages/sessions where NS auc is present
#so subset to stages >=5

#would need to convert to int and back to categorical for math comparison <5, so just exclude =='1'
df_Sub_A= df[df$stage!="1",]


#2%%-- Run LME ####

library(lmerTest)

model= lmerTest::lmer('periCueBlueAuc ~ trialType * sesSpecialLabel + (1|subject)', data=df_Sub_A)


model_anova<- anova(model)


#3%%-- Run Follow-up post-hoc tests ####

#- Signifcant interaction term, want to follow-up and estimate main effects

#emmeans package useful for post-hoc
library(emmeans)


#-- Pairwise comparisons (t test) between TrialType for each sesSpecialLabel 
#workaround for sidak correction with only 2 groups:

#- Viz interaction plot & save
figName= "vp-vta_fig2B_stats_A_interactionPlot.pdf"
setwd(pathOutput)
pdf(file=figName)

emmip(model, trialType ~ sesSpecialLabel)

dev.off()
setwd(pathWorking)


#- Pairwise T- tests
EMM <- emmeans(model, ~ trialType | sesSpecialLabel)   # where treat has 2 levels
tPairwise= pairs(EMM, adjust = "sidak")   # adjustment is ignored - only 1 test per group
print(tPairwise, by = NULL, adjust = "sidak")   # all are in one group now

#- T-Test vs. null
# EMM <- emmeans(model, ~ trialType | sesSpecialLabel)   # where treat has 2 levels
# this way multiple comparisons correction per session
EMM <- emmeans(model, ~ sesSpecialLabel | trialType)   # where treat has 2 levels

t= test(EMM, adjust = "sidak")   # adjustment is ignored - only 1 test per group
print(t, by = NULL, adjust = "sidak")   # all are in one group now



#4%%-- Save output to variables between tests  ####
# trying to keep code mostly generalizable and just save custom names at end
# all the results into descriptive variables between tests

#naming scheme : figure_stats_{count/identifier of stats goal}_{count/identifier of stats chronology}_{descriptor of stats test}
# trying to make names for good alphanumeric sorting / legibility later

fig2B_stats_A_0_description= "DS vs NS AUC on special Sessions with NS"
fig2B_stats_A_1_model= model
fig2B_stats_A_2_model_anova= model_anova
fig2B_stats_A_3_model_post_hoc_pairwise= tPairwise 
fig2B_stats_A_3_model_post_hoc_t= t 


##5%%-- Save output to file ####
setwd(pathOutput)

# use sink to write console output to text file

# Fig2B_A
sink("vp-vta_fig2B_stats_A_DSvsNS.txt")
'------------------------------------------------------------------------------'
'0)---- Description --: '
print(fig2B_stats_A_0_description)
'------------------------------------------------------------------------------'
print('1)---- LME:')
print(summary(fig2B_stats_A_1_model))
'------------------------------------------------------------------------------'
print('2)---- ANOVA of LME:')
print(fig2B_stats_A_2_model_anova)
'------------------------------------------------------------------------------'
print('3)---- Posthoc pairwise:')
print(fig2B_stats_A_3_model_post_hoc_pairwise, by=NULL, adjust='sidak')
'------------------------------------------------------------------------------'
print('3b)---- Posthoc t vs. null:')
print(fig2B_stats_A_3_model_post_hoc_t, adjust='sidak')
'---- END ---------------------------------------------------------------------'
sink()  # returns output to the console

setwd(pathWorking)


# __________________________________________________ ####

#%% Figure 2B Stats B-- Compare DS vs Null/0 AUC on first session (no NS) --####

#0%%-- Clear vars between tests ####
# #clear workspace (R environment) # Except paths, Python packages (pandas)
rm(list = setdiff(ls(), c("pathWorking", "pathOutput", "pathInput", "pd")))


#1%%-- Load data from .pkl ####

pathData= paste(pathInput,'/fig2b.pkl', sep="")
df <- pd$read_pickle(pathData)


###### summarize data
summary(df)

#verify dtypes imported properly
sapply(df, class) 


#%%-- Subset data ## 
#Remove missing/invalid observations 


#-- subset data ##
# subset stage 1
df_Sub_B= df[df$stage==1,]

# subset DS trials only
df_Sub_B= df_Sub_B[df_Sub_B$trialType== 'aucDSblue',]

#-  aggregate so that stats run on subject means 
df_Sub_B <- aggregate(periCueBlueAuc ~ subject + trialType, data = df_Sub_B, mean) # Equivalent


#2%%-- Run model ####

model= lm('periCueBlueAuc ~ subject', data=df_Sub_B)


model_anova= anova(model)


t= t.test(df_Sub_B$periCueBlueAuc)

#4%%-- Save output to variables between tests  ####

fig2B_stats_B_0_description= "DS AUC vs 0 on first training day (no NS)"
fig2B_stats_B_1_t= t 

#5%%-- Save output to file

setwd(pathOutput)

# use sink to write console output to text file
# Fig2B_B
sink("vp-vta_fig2B_stats_B_firstSes_DSvs0.txt")
'------------------------------------------------------------------------------'
'0)---- Description --: '
print(fig2B_stats_B_0_description)
'------------------------------------------------------------------------------'
print('1)---- One sample T-Test:')
print(fig2B_stats_B_1_t, by=NULL, adjust='sidak')
'---- END ---------------------------------------------------------------------'
sink()  # returns output to the console


# __________________________________________________ ####

#%% Figure 2B Stats 2 -- 'Learning' across sesssions; Compare DS AUC across all special sessions --####

#0%%-- Clear vars between tests ####
# #clear workspace (R environment) # Except paths, Python packages (pandas)
rm(list = setdiff(ls(), c("pathWorking", "pathOutput", "pd")))


#1%%-- Load data from .pkl ####

pathData <- "C:\\Users\\Dakota\\Documents\\GitHub\\FP-analysis\\python\\_output\\fig2b.pkl"

df <- pd$read_pickle(pathData)


###### summarize data
summary(df)

#verify dtypes imported properly
sapply(df, class) 


#%%-- Subset data ## 
# #Remove missing/invalid observations - Only for NS bc want to keep comparisons between sessions for DS
df_Sub_B= df[!is.nan(df$periCueBlueAuc),]


#-- subset DS trials for 'learning' across sessions
df_Sub_B_DS= df_Sub_B[df_Sub_B$trialType =="aucDSblue",]

df_Sub_B_NS= df_Sub_B[df_Sub_B$trialType =="aucNSblue",]

#2%%-- LME ####
model_pooled= lmerTest::lmer('periCueBlueAuc ~ trialType * sesSpecialLabel + (1|subject)', data=df_Sub_B)
model_anova_pooled= anova(model_pooled)


model_DS= lmerTest::lmer('periCueBlueAuc ~ sesSpecialLabel + (1|subject)', data=df_Sub_B_DS)
model_anova_DS= anova(model_DS)

model_NS= lmerTest::lmer('periCueBlueAuc ~ sesSpecialLabel + (1|subject)', data=df_Sub_B_NS)
model_anova_NS= anova(model_NS)

#3%%--Posthoc pairwise comparisons (t test) ####

#- Viz interaction plot & save
figName= "vp-vta_fig2b_stats_C_interactionPlot.pdf"
setwd(pathOutput)
pdf(file=figName)

# emmip(model_pooled, ~ sesSpecialLabel)
emmip(model_pooled, trialType ~ sesSpecialLabel)


dev.off()
setwd(pathWorking)


#- pooled version
emmip(model_pooled, trialType ~ sesSpecialLabel)


# EMM <- emmeans(model_pooled, ~ trialType | sesSpecialLabel)   # where treat has 2 levels
EMM <- emmeans(model_pooled, ~ sesSpecialLabel|trialType)   # where treat has 2 levels

# tPairwise_pooled= pairs(EMM, by=NULL, adjust= "sidak")
tPairwise_pooled= pairs(EMM, adjust= "sidak")


print(tPairwise_pooled, adjust="sidak")


# t_pooled= test(EMM, by=NULL, adjust="sidak")
t_pooled= test(EMM, null= 0, adjust="sidak")

print(t_pooled, adjust= "sidak")


#4%%-- Save output to variables between tests  ####
fig2B_stats_C_0_description= "DS & NS AUC: Learning/Changes across sessions"
fig2B_stats_C_1_model= model_pooled
fig2B_stats_C_2_model_anova= model_anova_pooled
fig2B_stats_C_3_model_post_hoc_pairwise= tPairwise_pooled
fig2B_stats_C_3_model_post_hoc_t= t_pooled


#5%%-- Save output to File #### 

setwd(pathOutput)

# use sink to write console output to text file
# write everything to one file #removing print() calls doesn't seem to clean up anyway

# # Fig2B_A
# sink("vp-vta_fig2B_stats_A_DSvsNS.txt")
# '------------------------------------------------------------------------------'
# '0)---- Description --: '
# print(fig2B_stats_A_0_description)
# '------------------------------------------------------------------------------'
# print('1)---- LME:')
# print(summary(fig2B_stats_A_1_model))
# '------------------------------------------------------------------------------'
# print('2)---- ANOVA of LME:')
# print(fig2B_stats_A_2_model_anova)
# '------------------------------------------------------------------------------'
# print('3)---- Posthoc pairwise:')
# print(fig2B_stats_A_3_model_post_hoc_pairwise)
# '---- END ---------------------------------------------------------------------'
# sink()  # returns output to the console


# # Fig2B_B
# sink("vp-vta_fig2B_stats_B_firstSes_DSvs0.txt")
# '------------------------------------------------------------------------------'
# '0)---- Description --: '
# print(fig2B_stats_B_0_description)
# '------------------------------------------------------------------------------'
# print('1)---- One sample T-Test:')
# print(fig2B_stats_B_1_t)
# '---- END ---------------------------------------------------------------------'
# sink()  # returns output to the console


# Fig2B_C
setwd(pathOutput)


sink("vp-vta_fig2B_stats_C_Learning_DS.txt")
'------------------------------------------------------------------------------'
'0)---- Description --: '
print(fig2B_stats_C_0_description)
'------------------------------------------------------------------------------'
print('1)---- LME:')
print(summary(fig2B_stats_C_1_model))
'------------------------------------------------------------------------------'
print('2)---- ANOVA of LME:')
print(fig2B_stats_C_2_model_anova)
'------------------------------------------------------------------------------'
print('3)---- Posthoc pairwise:')
print(fig2B_stats_C_3_model_post_hoc_pairwise, adjust='sidak')
'------------------------------------------------------------------------------'
print('3)---- Posthoc T vs null,:')
print(fig2B_stats_C_3_model_post_hoc_t, by=NULL, adjust='sidak')
'---- END ---------------------------------------------------------------------'

sink()  # returns output to the console

setwd(pathWorking)

# __________________________________________________ ####


## ---- FIGURE 2D --------------------------------------------------------##

#%% Figure 2D Stats A -- Compare PE vs no PE AUC DS #### 


#0%%-- Clear vars between tests ####
# #clear workspace (R environment) # Except paths, Python packages (pandas)# #clear workspace (R environment) # Except paths, Python packages (pandas)
rm(list = setdiff(ls(), c("pathWorking", "pathOutput", "pathInput", "pd")))


#1%%-- Load data from .pkl ####

pathData= paste(pathInput,'/fig2d.pkl', sep="")

df <- pd$read_pickle(pathData)


###### summarize data
summary(df)

#verify dtypes imported properly
sapply(df, class) 

#%% Figure 2D Stats A -- Compare PE vs no PE AUC DS 

#%%-- Subset data ## 
#Remove missing/invalid observations 
df_Sub_A= df

#2%%-- Run LME ####

model= lmerTest::lmer('periCueBlueAuc ~ trialOutcome + (1|subject)', data=df_Sub_A)


model_anova<- anova(model)


#3%%-- Run Follow-up post-hoc tests ####

#- Signifcant interaction term, want to follow-up and estimate main effects


#-- Pairwise comparisons (t test) between TrialOutcome


#- Pairwise T- tests
EMM <- emmeans(model, ~ trialOutcome)   # where treat has 2 levels
tPairwise= pairs(EMM, adjust = "sidak")   # adjustment is ignored - only 1 test per group
print(tPairwise, adjust = "sidak")   # all are in one group now

#-  T- tests vs null
EMM <- emmeans(model, ~ trialOutcome)   # where treat has 2 levels
t= test(EMM, adjust = "sidak")   # adjustment is ignored - only 1 test per group
print(t, by = NULL, adjust = "sidak")   # all are in one group now



#4%%-- Save output to variables between tests  ####
# trying to keep code mostly generalizable and just save custom names at end
# all the results into descriptive variables between tests
fig2D_stats_A_0_description= "Figure 2D: PE vs no PE AUC , stage 7"
fig2D_stats_A_1_model= model
fig2D_stats_A_2_model_anova= model_anova
fig2D_stats_A_3_model_post_hoc_pairwise= tPairwise 
fig2D_stats_A_3_model_post_hoc_t= t 



#5%%-- Save output to File ####
# Fig2D_A
setwd(pathOutput)


sink("vp-vta_fig2D_stats_A_PEvsNoPE_DS.txt")
'------------------------------------------------------------------------------'
'0)---- Description --: '
print(fig2D_stats_A_0_description)
'------------------------------------------------------------------------------'
print('1)---- LME:')
print(summary(fig2D_stats_A_1_model))
'------------------------------------------------------------------------------'
print('2)---- ANOVA of LME:')
print(fig2D_stats_A_2_model_anova)
'------------------------------------------------------------------------------'
print('3)---- Posthoc pairwise:')
print(fig2D_stats_A_3_model_post_hoc_pairwise, adjust='sidak')
'------------------------------------------------------------------------------'
print('3)---- Posthoc t:')
print(fig2D_stats_A_3_model_post_hoc_t, adjust='sidak')

'---- END ---------------------------------------------------------------------'
sink()  # returns output to the console

setwd(pathWorking)


