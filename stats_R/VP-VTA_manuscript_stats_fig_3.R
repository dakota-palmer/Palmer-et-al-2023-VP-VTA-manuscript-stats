
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

#%- fig 3 Stats-- Encoding Model Kernel AUC ####
#0%%-- Clear vars between tests ####
# #clear workspace (R environment) # Except paths, Python packages (pandas)
rm(list = setdiff(ls(), c("pathWorking", "pathOutput", "pathInput", "pd")))


#1%%-- Load data from .pkl ####

pathData= paste(pathInput,'/fig3_encodingModel.pkl', sep="")

df <- pd$read_pickle(pathData)


###### summarize data
summary(df)

#verify dtypes imported properly
sapply(df, class)


#- Subset to one kernel auc value per eventType per subject
# df= df(df$timeLock==0)
df= df[df$timeShift== 0.025,]


#2%%-- Run model ####

model= lmerTest::lmer('betaAUCpostEvent ~ eventType + (1|subject)', data=df)
model_anova<- anova(model)


# -- Interaction plot
#- Viz interaction plot & save
figName= "vp-vta_fig3_encoding_stats_interactionPlot.pdf"
setwd(pathOutput)
pdf(file=figName)

emmip(model, ~ eventType)


# 3%%-- Posthoc tests ####

# -- T Test compare AUCs vs null of 0 
#%% -- Stat comparison of AUC Kernels vs. null of 0 and comparison between two

EMM <- emmeans(model, ~ eventType)   # where treat has 2 levels
tPairwise= pairs(EMM, adjust = "sidak")   # adjustment is ignored - only 1 test per group
summary(tPairwise, by = NULL, adjust = "sidak")   # all are in one group now

tPairwise= tPairwise

# for active proportion, check if each level significantly different from 0.5 (chance)
t= test(EMM, null=0, adjust='sidak')


#4%%-- Save output to variables between tests  ####
# trying to keep code mostly generalizable and just save custom names at end
# all the results into descriptive variables between tests


fig3_stats_EncodingModel_A_0_description= "Figure 3: Encoding model, Post-Event Kernel AUCs"
fig3_stats_EncodingModel_A_1_model= model
fig3_stats_EncodingModel_A_2_model_anova= model_anova
fig3_stats_EncodingModel_A_3_model_post_hoc_pairwise= tPairwise#tPairwiseSig
fig3_stats_EncodingModel_A_3_model_post_hoc_t= t#tSig

#Report stat test for comparing the two kernels within-subject

model_B= lmerTest::lmer('betaAUCpostEvent ~ eventType + (1|subject)', data=df)
model_anova_B<- anova(model)

#5%%-- Save output ####

#- move to output directory prior to saving
setwd(pathOutput)



#------Pooled

sink("vp-vta_fig3_stats_EncodingModel_A_AUC.txt")
'------------------------------------------------------------------------------'
'0)---- Description --: '
print(fig3_stats_EncodingModel_A_0_description)
'------------------------------------------------------------------------------'
print('1)---- LME:')
print(summary(fig3_stats_EncodingModel_A_1_model))
'------------------------------------------------------------------------------'
print('2)---- ANOVA of LME:')
print(fig3_stats_EncodingModel_A_2_model_anova)
'------------------------------------------------------------------------------'
print('3)---- Posthoc T :') # Make sure for posthocs the summary is printed with pval correction
print(fig3_stats_EncodingModel_A_3_model_post_hoc_t, by = NULL, adjust = "sidak")


'---- END ---------------------------------------------------------------------'
sink()  # returns output to the console


# __________________________________________________ ####



## %- fig 3 Stats-- Encoding Model Kernel Time series ####


#0%%-- Clear vars between tests ####
# #clear workspace (R environment) # Except paths, Python packages (pandas)
rm(list = setdiff(ls(), c("pathWorking", "pathOutput", "pathInput", "pd")))


#1%%-- Load data from .pkl ####

pathData= paste(pathInput,'/fig3_encodingModel.pkl', sep="")

df <- pd$read_pickle(pathData)


###### summarize data
summary(df)

#verify dtypes imported properly
sapply(df, class)


#- Subset to one kernel auc value per eventType per subject
# df= df(df$timeLock==0)


#2%%-- Run model ####

model= lmerTest::lmer('beta ~ eventType * timeShift + (1|subject)', data=df)
model_anova<- anova(model)


# -- Interaction plot

# 3%%-- Posthoc tests ####

# -- T Test compare AUCs vs null of 0
#%% -- Stat comparison of AUC Kernels vs. null of 0 and comparison between two

EMM <- emmeans(model, ~  eventType | timeShift)   # where treat has 2 levels
tPairwise= pairs(EMM, adjust = "sidak")   # adjustment is ignored - only 1 test per group
summary(tPairwise, by = NULL, adjust = "sidak")   # all are in one group now

tPairwise= tPairwise

# # convert to df with summary to extract data / subset by significant p.value
tPairwiseDF= summary(tPairwise, by = NULL, adjust = "sidak")
# 

indSig= which(tPairwiseDF[,'p.value']<=pAlpha)

tPairwiseSig= tPairwiseDF[indSig,]

# for active proportion, check if each level significantly different from 0 (chance)
# only corrects for 2 tests
# t= test(EMM, null=0, adjust='sidak')

# want to do posthoc correction for each timebin, so 
t= test(EMM, null=0, by='eventType', adjust='sidak')


# lots of values here (comparison at every time bin) make a viz or subset of only those below "significance" p value threshold
indSig= which(t$p.value<=pAlpha)

tSig= t[indSig,]


#4%%-- Save output to variables between tests  ####
# trying to keep code mostly generalizable and just save custom names at end
# all the results into descriptive variables between tests


fig3_stats_EncodingModel_B_0_description= "Figure 3: Encoding model, Time Series Stats"
fig3_stats_EncodingModel_B_1_model= model
fig3_stats_EncodingModel_B_2_model_anova= model_anova
fig3_stats_EncodingModel_B_3_model_post_hoc_pairwise= tPairwiseSig
fig3_stats_EncodingModel_B_3_model_post_hoc_t= tSig


#3.5%% -- Correlation between predicted vs. actual GCaMP ####

#%%-- Load data from CSV ####

pathData= paste(pathInput,'/fig3_df_predictedMean.csv', sep="")


dat = read.csv(pathData, header = TRUE)

corr= cor.test(dat$y, dat$yPredicted)


#5%%-- Save output ####

#- move to output directory prior to saving
setwd(pathOutput)

#------Pooled

sink("vp-vta_fig3_stats_EncodingModel_B_TimeSeries.txt")
'------------------------------------------------------------------------------'
'0)---- Description --: '
print(fig3_stats_EncodingModel_B_0_description)
'------------------------------------------------------------------------------'
print('1)---- LME:')
print(summary(fig3_stats_EncodingModel_B_1_model))
'------------------------------------------------------------------------------'
print('2)---- ANOVA of LME:')
print(fig3_stats_EncodingModel_B_2_model_anova)
'------------------------------------------------------------------------------'
print('3)---- Posthoc T, Only Significant time bins :') # Make sure for posthocs the summary is printed with pval correction
print(fig3_stats_EncodingModel_B_3_model_post_hoc_t, by = NULL, adjust = "sidak")


'---- END ---------------------------------------------------------------------'
sink()  # returns output to the console




#6%%-- viz ####
# Viz stats output of "significant" comparions by time bin

p=''

p= ggplot()+
  
  scale_colour_brewer(palette="Dark2")+
  
  geom_point(data= t, aes(x=timeShift, y=p.value, color=eventType, shape=eventType, size=1))+

  # geom_line(data=df, inherit.aes=FALSE, aes(x=timeShift, y=beta, color=eventType, alpha=0.2))+
  
  geom_hline(yintercept=pAlpha, color='red', size=2, alpha=0.6)+
  geom_vline(xintercept= t$timeShift[t$timeShift==5.0], color='grey', size=2, alpha=0.6)+
  
  # manual latency
  # geom_vline(xintercept= t$timeShift[t$timeShift==2.75], color='purple', size=2, alpha=0.6)+
  geom_vline(xintercept= t$timeShift[t$timeShift==2.05], color='purple', size=2, alpha=0.6)+
  # geom_vline(xintercept=2.05, color='purple', size=2, alpha=0.6)+
  
  
  show(p)


#- Viz kernels at "significant" time bins


p=''

p= ggplot()+
  
  scale_colour_brewer(palette="Dark2")+
  
  geom_point(data= tSig, aes(x=timeShift, y=p.value, color=eventType, shape=eventType, size=1))+
  
  # geom_line(data=df, inherit.aes=FALSE, aes(x=timeShift, y=beta, color=eventType, alpha=0.2))+
  
  geom_hline(yintercept=pAlpha, color='red', size=2, alpha=0.6)+
  geom_vline(xintercept= t$timeShift[t$timeShift==5.0], color='grey', size=2, alpha=0.6)+
  
  # manual latency
  # geom_vline(xintercept= t$timeShift[t$timeShift==2.75], color='purple', size=2, alpha=0.6)+
  geom_vline(xintercept= t$timeShift[t$timeShift==2.05], color='purple', size=2, alpha=0.6)+
  # geom_vline(xintercept=2.05, color='purple', size=2, alpha=0.6)+
  
  
  # show(p)

#- use plotly for interactive plots
library(plotly)
ggplotly(p)

#- viz kernels at significant tbins for kernels

#manual subsetting instead of looping/programmatic subsetting here

# separte dfs for separate plotting

tSig_DS= tSig[tSig$eventType=='DStime',]

tSig_PE= tSig[tSig$eventType=='PEcue',]

indSig_DS= which((df$timeShift %in% tSig_DS$timeShift) & (df$eventType=='DStime'))

indSig_PE= which((df$timeShift %in% tSig_PE$timeShift) & (df$eventType=='PEcue'))

dfSig_DS= df[indSig_DS,]
dfSig_PE= df[indSig_PE,]


indSig_all= c(indSig_DS, indSig_PE)

dfSig= df[indSig_all,]

# 
# p= ggplot()+
#   
#   scale_colour_brewer(palette="Dark2")+
#   # 
#   # geom_point(data= dfSig_DS, aes(x=timeShift, y=beta, color=eventType, shape=subject, size=1))+
#   # 
#   # geom_point(data= dfSig_PE, aes(x=timeShift, y=beta, color=eventType, shape=subject, size=1))+
#   
#   geom_point(data= dfSig, aes(x=timeShift, y=beta, color=eventType, shape=subject, size=1))+
#   
#   # geom_line(data= dfSig, aes(x=timeShift, y=beta, color=eventType, size=1))+
#   
#   # geom_line(data=df, inherit.aes=FALSE, aes(x=timeShift, y=beta, color=eventType, alpha=0.2))+
#   # 
#   geom_hline(yintercept=pAlpha, color='black', size=1, alpha=0.6)
#   
#   
#   # geom_hline(yintercept=pAlpha, color='red', size=2, alpha=0.6)+
#   # geom_vline(xintercept= t$timeShift[t$timeShift==5.0], color='grey', size=2, alpha=0.6)+
#   # 
#   # # manual latency
#   # # geom_vline(xintercept= t$timeShift[t$timeShift==2.75], color='purple', size=2, alpha=0.6)+
#   # geom_vline(xintercept= t$timeShift[t$timeShift==2.05], color='purple', size=2, alpha=0.6)
#   # # geom_vline(xintercept=2.05, color='purple', size=2, alpha=0.6)+
#   
# ggplotly(p)


#convert to numeric x axis
dfSig$timeShift= as.numeric(as.character(dfSig$timeShift))

p= ggplot(data= dfSig)+

  scale_colour_brewer(palette="Dark2")+
    
  facet_grid(eventType~.)+
  
  geom_point(data= dfSig, aes(x=timeShift, y=beta, color=eventType, shape=subject))+
  
  stat_summary(fun=mean, geom='line', aes(x=timeShift,y=beta, group=eventType), colour='black', size=2, alpha=0.8)+
  
    
  # geom_line(data= dfSig, aes(x=timeShift, y=beta, color=eventType, size=1))+
  
  # 
  geom_hline(yintercept=pAlpha, color='black', size=1, alpha=0.6)


# geom_hline(yintercept=pAlpha, color='red', size=2, alpha=0.6)+
# geom_vline(xintercept= t$timeShift[t$timeShift==5.0], color='grey', size=2, alpha=0.6)+
# 
# # manual latency
# # geom_vline(xintercept= t$timeShift[t$timeShift==2.75], color='purple', size=2, alpha=0.6)+
# geom_vline(xintercept= t$timeShift[t$timeShift==2.05], color='purple', size=2, alpha=0.6)
# # geom_vline(xintercept=2.05, color='purple', size=2, alpha=0.6)+

ggplotly(p)


# __________________________________________________ ####


# %- fig 3 Stats-- Latency Correlation ####

#0%%-- Clear vars between tests ####
# #clear workspace (R environment) # Except paths, Python packages (pandas)
rm(list = setdiff(ls(), c("pathWorking", "pathOutput", "pathInput", "pd")))


#1%%-- Load data from .pkl ####

pathData= paste(pathInput,'/fig3_latencyCorr.pkl', sep="")


df <- pd$read_pickle(pathData)


###### summarize data
summary(df)

#verify dtypes imported properly
sapply(df, class)


#- Subset to one kernel auc value per eventType per subject

#%- Drop invalid observations
# possible that some latencies don't have rho values, drop these (e.g. if subject has no trials beyond certain latency, real fp signal will be excluded)
# df_Sub_A= df[!is.na(df$periCueRho),]


# df_Sub_A= na.omit(df[,c('periCueRho','subject','timeLock')])

# should be rows with nan pvalRhoBlue
df_Sub_A= na.omit(df)


#2%%-- Run model ####

model= lmerTest::lmer('periCueRho ~ latencyOrder * timeLock + (1|subject)', data=df_Sub_A)
model_anova<- anova(model)


# -- Interaction plot

# 3%%-- Posthoc tests ####
pAlpha= 0.050


# --  Pairwise t tests between levels
#%%

EMM <- emmeans(model, ~  latencyOrder | timeLock)   # where treat has 2 levels
tPairwise= pairs(EMM, adjust = "sidak")   # adjustment is ignored - only 1 test per group
summary(tPairwise, by = NULL, adjust = "sidak")   # all are in one group now

# convert to df with summary to extract data / subset by significant p.value
tPairwiseDF= summary(tPairwise, by = NULL, adjust = "sidak")

indSig= which(tPairwiseDF[,'p.value']<=pAlpha)

tPairwiseSig= tPairwiseDF[indSig,]


#  t test- check if each level significantly different from null hypothesis (chance)
EMM <- emmeans(model, ~  timeLock | latencyOrder)   # where treat has 2 levels

t= test(EMM, null=0, adjust='sidak')

print(t,by = NULL, adjust = "sidak")   # all are in one group now

# lots of values here (comparison at every time bin) make a viz or subset of only those below "significance" p value threshold
indSig= which(t$p.value<=pAlpha)

tSig= t[indSig,]



# lots of values here. make a viz or subset of only those below "significance" p value threshold
pAlpha= 0.050

indSig= which(t$p.value<=pAlpha)

tSig= t[indSig,]

library(ggplot2)




#4%%-- Save output to variables between tests  ####
# trying to keep code mostly generalizable and just save custom names at end
# all the results into descriptive variables between tests


fig3_stats_latencyCorrelation_A_0_description= "Figure 3: Encoding model, Post-Event Kernel AUCs"
fig3_stats_latencyCorrelation_A_1_model= model
fig3_stats_latencyCorrelation_A_2_model_anova= model_anova
fig3_stats_latencyCorrelation_A_3_model_post_hoc_pairwise= tPairwiseSig
fig3_stats_latencyCorrelation_A_3_model_post_hoc_t= tSig


#5%%-- Save output ####

#- move to output directory prior to saving
setwd(pathOutput)

#------Pooled

sink("vp-vta_fig3_stats_latencyCorrelation_A.txt")
'------------------------------------------------------------------------------'
'0)---- Description --: '
print(fig3_stats_latencyCorrelation_0_description)
'------------------------------------------------------------------------------'
print('1)---- LME:')
print(summary(fig3_stats_latencyCorrelation_A_1_model))
'------------------------------------------------------------------------------'
print('2)---- ANOVA of LME:')
print(fig3_stats_latencyCorrelation_A_2_model_anova)
'------------------------------------------------------------------------------'
print('3)---- Posthoc T, Only Significant time bins :') # Make sure for posthocs the summary is printed with pval correction
print(fig3_stats_latencyCorrelation_A_3_model_post_hoc_t, by = NULL, adjust = "sidak")


'---- END ---------------------------------------------------------------------'
sink()  # returns output to the console


setwd(workingDir)

#6%%-- viz ####
# Viz stats output of "significant" comparions by time bin


p=''

p= ggplot()+
  
  scale_colour_brewer(palette="Dark2")+
  
  geom_point(data= t, aes(x=timeLock, y=p.value, color=latencyOrder, shape=latencyOrder, size=1))+
  
  # scale_color_hue(l=40, c=35)+
  
  # geom_line(inherit.aes=FALSE, data=df, aes(x=timeLock, y=periCueRho, color=latencyOrder)+
  #   scale_colour_brewer(palette="Set2"))+
  
  # geom_line(data=df, aes(x=timeLock, y=periCueRho, color=latencyOrder)+
  # #             scale_colour_brewer(palette="Set2"))+
  # # 
  # geom_line(data=df, aes(x=timeLock, y=periCueRho, color=latencyOrder)+
  #             scale_colour_brewer(palette="Set2")
#           )
# # 
# geom_line(data=df, aes(x=timeLock, y=periCueRho, color=latencyOrder, alpha=0.2)+
#   scale_colour_manual(l=30))
geom_line(data=df, inherit.aes=FALSE, aes(x=timeLock, y=periCueRho, color=latencyOrder, alpha=0.2))+
  
  geom_hline(yintercept=pAlpha, color='red', size=2, alpha=0.6)+
  geom_vline(xintercept= t$timeLock[t$timeLock==5.0], color='grey', size=2, alpha=0.6)+
  
  # manual latency
  # geom_vline(xintercept= t$timeLock[t$timeLock==2.75], color='purple', size=2, alpha=0.6)+
  geom_vline(xintercept= t$timeLock[t$timeLock==2.05], color='purple', size=2, alpha=0.6)+
  # geom_vline(xintercept=2.05, color='purple', size=2, alpha=0.6)+
  
  
  show(p)



# Check the actual data for these "Significant" time points. Was the correlation itself significant?

indSig= which(df$timeLock %in% tSig$timeLock)

dfSig= df[indSig,]

# clearly "significant" late time bins seem to be outliers. not consistent between all subjects tho maybe shared between a couple

# plot "Significant" data
dfPlot= dfSig[dfSig$latencyOrder=='rhoBlue',]

  # double "sig"-- correlation pval < alpha and different from 0 in this time bin
dfPlot2= dfPlot[dfPlot$pvalBlue<= pAlpha,]

p=''

p= ggplot()+
  
  # geom_boxplot(data= dfPlot, aes(x=timeLock, y=pvalBlue), colour='gray', alpha=0.5)+
  
  geom_boxplot(data= dfPlot, aes(x=timeLock, y=pvalBlue), colour='gray', alpha=0.5)+
  # 
  # stat_summary(data= dfPlot,
  #   fun= median,
  #   geom = 'line',
  #   aes(x=timeLock, y=pvalBlue), colour='black')+
  # 
  
  # # stat_summary(fun=median, geom='line', aes(x=dfPlot$timeLock, y=dfPlot$pvalBlue), colour='black', alpha=0.5, size=2)+
  # geom_line(fun=median, data=dfPlot, aes(x=timeLock, y=pvalBlue), colour='black', alpha=.8, size=2)+
  # 
  # 
  # stat_summary(fun=median, data= dfPlot,  geom='line', aes(x=timeLock, y=pvalBlue), colour='black', alpha=0.5, size=2)+
  
  
  geom_line(data=dfPlot, aes(x=timeLock, y=pvalBlue, color=subject, alpha=0.2))+
  
  geom_point(data=dfPlot, aes(x=timeLock, y=pvalBlue, shape=subject, alpha=0.6))+
  
  # geom_point(data=dfPlot2, aes(x=timeLock, y=pvalBlue, shape=subject), colour="purple", size=2)+
  geom_point(data=dfPlot2, aes(x=timeLock, y=pvalBlue, colour=subject), shape=1, size=3, stroke=3, alpha=0.6)+
  
    
  geom_hline(yintercept=pAlpha, colour='red', size=2, alpha=0.6)+
  
  # geom_vline(xintercept= df$timeLock[df$timeLock==2.05], colour='purple', size=2, alpha=0.6)+
  geom_vline(xintercept= 2.05, colour='purple', size=2, alpha=0.6)+
  
  
  
  # geom_vline(xintercept= t$timeLock[t$timeLock==5.0], color='grey', size=2, alpha=0.6)+
  
  # manual latency
  # geom_vline(xintercept= t$timeLock[t$timeLock==2.75], color='purple', size=2, alpha=0.6)+
  
  show(p)


# # ggplot(data=df, x= 'timeLock', y='periCueRho', color='latencyOrder')
# #   geom_line()
#   # geom_point()
# 
# p= ggplot(data= df, aes(x= timeLock, y=periCueRho, color=latencyOrder, group=subject)) 
# p= p+ geom_line()
# show(p)
# 
# # p= p+ stat_summary(fun=mean, group=latencyOrder, geom='line')
# # p= p+ geom_line(group=subject)
# 
# # p= p+ stat_summary(fun=mean, geom='line')
# 
# # p= p+ stat_summary(fun=mean)
# 
# # geom_errorbar(aes(ymin=len-se, ymax=len+se),
# #               width=.2,                    # Width of the error bars
# #               position=position_dodge(.9))
# 
# p= ggplot(data= df, aes(x= timeLock, y=periCueRho, color=latencyOrder)) 
# 
# p= p+ stat_summary(fun=mean,  geom='line')
# 
# 
# p= p+ geom_line(aes(x= timeLock, y=periCueRho, group=latencyOrder)) 
# 
# show(p)
# 
# 
# # try
# # rm(p)
# p=''
# 
# 
# p= ggplot(data=df, aes(x=timeLock, y=periCueRho))+
#    # stat_summary(fun=mean, geom="line")+
#    # stat_summary(fun=mean, geom="line", aes(group = 1))
#   # stat_summary(fun=mean, geom="line", aes(group = latencyOrder, color= latencyOrder))+
#   
#   # stat_summary(fun=mean, geom="line", aes(group = subject, color= latencyOrder, alpha=0.5))+
#   
#   #individual subjects
#   # having issues with this specifically
#     # geom_line(aes(group=subject, color=latencyOrder,alpha= 0.8, size=0.5))
#   # geom_line(aes(group=subject, color= latencyOrder))
# # 
# # geom_point(aes(group=subject, color= subject, shape=subject, alpha= 0.5))+
# # 
# # geom_line(aes(group=subject, color= subject, alpha= 0.9))+
# 
#   #individual subj + color 
# 
# # geom_point(aes(group=subject, color= latencyOrder, shape=subject, alpha= 0.5))+
#   
# # geom_line(aes(group=subject, color= latencyOrder), alpha= 0.7)+
#   
# #alpha on line seems to add fill, just make size different for subj?
# geom_line(aes(group=subject, color= latencyOrder), size= 0.5)+
# 
#     
#     
# # stat_summary(fun=mean, geom="line", aes(group = subject, color= subject))+
# 
# 
#   
#   #   # SEM
#   # stat_summary(fun = mean_se, geom = "ribbon",
#   #            alpha = 0.3, aes(group= latencyOrder, color= latencyOrder))+
# 
# 
# 
#    # stat_summary(fun=mean, colour="red", geom="line", aes(group = subject))
#    
#    
# show(p)
# 
# 
# summary(t)$p.value < pAlpha
#   
# summary(t[t$p.value<pAlpha,])
# 
# timeLockSig= t[t$p.value < pAlpha, 'timeLock']
# 
# # plot "significant" time epochs
# p=''
# 
# p= ggplot(data=t, aes(x=timeLock, y=p.value, color=latencyOrder))+
#   geom_point()+
#   geom_hline(yintercept=pAlpha, color='black', size=2, alpha=0.5)+
#   geom_vline(xintercept= t$timeLock[t$timeLock==5.0], color='grey', size=2, alpha=0.5)
# 
# show(p)

# 
# # #- manual subplot real with highlighted significant timebins
# p=''
# 
# 
# df_Sub= df[df$latencyOrder=='rhoBlue',]
# 
# t_Sub= t[t$latencyOrder=='rhoBlue',]
# 
# p= ggplot()+
#   
#   geom_hline(yintercept=pAlpha, color='black', size=2, alpha=0.5)+
#   geom_vline(xintercept= t_Sub$timeLock[t_Sub$timeLock==5.0], color='grey', size=2, alpha=0.5)+
#   
#   geom_vline(xintercept= mean(df$poxDSrelMean), color='red', size=2, alpha=0.5)+
#   
#   
#   geom_line(data=df_Sub, aes(x=timeLock, y=periCueRho, color=latencyOrder))+
# 
#   geom_point(data= t_Sub, aes(x=timeLock, y=p.value, colour='black'))+
# 
# show(p)
# 
# 
# # df_Sub= df[df$latencyOrder=='rhoBlue',]
# # 
# # 
# # p=''
# # 
# # # tSig= t[t$p.value<pAlpha,]
# # tSig= t[t$p.value<pAlpha,'timeLock']
# # 
# # 
# # p= ggplot()+
# #   # geom_point(data= t, aes(x=timeLock, y=p.value, color=latencyOrder))+
# #   # geom_hline(yintercept=pAlpha, color='black', size=2, alpha=0.5)+
# #   # geom_vline(xintercept= t$timeLock[t$timeLock==5.0], color='grey', size=2, alpha=0.5)+
# #   
# #   # geom_ribbon(data=t[t$p.value<pAlpha,], aes(x=t$timeLock[t$p.value<pAlpha], ymin=0, ymax=1), fill='purple', alpha=0.9)+
# #   
# #   # geom_ribbon(data=t[t$p.value<pAlpha,], aes(x=t$timeLock[t$p.value<pAlpha], ymin=0, ymax=1), fill='purple', alpha=0.9)+
# #   
# #   # geom_ribbon(aes(x=t$timeLock[t$p.value<pAlpha], ymin=0, ymax=1), fill='purple', alpha=0.9)#+
# #   
# #   geom_ribbon(data= tSig, aes(timeLock, ymin=0.0, ymax=1.0))
# # 
# # 
# #   
# #   # geom_ribbon(data=t, aes(x=t$timeLock[t$p.value<pAlpha], ymin=0, ymax=1), color='grey', alpha=0.5)+
# #   
# #   
# #   # geom_line(data=df_Sub, aes(x=timeLock, y=periCueRho, color=latencyOrder))
# # 
# # 
## show(p)

# __________________________________________________ ####



# __________________________________________________ ####


# %- Supplemental fig 3 Stats-- Lick Count Correlation ####

#0%%-- Clear vars between tests ####
# #clear workspace (R environment) # Except paths, Python packages (pandas)
rm(list = setdiff(ls(), c("pathWorking", "pathOutput", "pathInput", "pd")))


#1%%-- Load data from .pkl ####

pathData= paste(pathInput,'/supplement_Fig3_lickCount_PE_corr.pkl', sep="")


df <- pd$read_pickle(pathData)


###### summarize data
summary(df)

#verify dtypes imported properly
sapply(df, class)


#- Subset to one kernel auc value per eventType per subject

#%- Drop invalid observations
# possible that some latencies don't have rho values, drop these (e.g. if subject has no trials beyond certain latency, real fp signal will be excluded)
# df_Sub_A= df[!is.na(df$periCueRho),]


# df_Sub_A= na.omit(df[,c('periCueRho','subject','timeLock')])

# should be rows with nan pvalRhoBlue
df_Sub_A= na.omit(df)


#2%%-- Run model ####

model= lmerTest::lmer('periCueRho ~ latencyOrder * timeLock + (1|subject)', data=df_Sub_A)
model_anova<- anova(model)


# -- Interaction plot

# 3%%-- Posthoc tests ####
pAlpha= 0.050


# --  Pairwise t tests between levels
#%%

EMM <- emmeans(model, ~  latencyOrder | timeLock)   # where treat has 2 levels
tPairwise= pairs(EMM, adjust = "sidak")   # adjustment is ignored - only 1 test per group
summary(tPairwise, by = NULL, adjust = "sidak")   # all are in one group now

# convert to df with summary to extract data / subset by significant p.value
tPairwiseDF= summary(tPairwise, by = NULL, adjust = "sidak")

indSig= which(tPairwiseDF[,'p.value']<=pAlpha)

tPairwiseSig= tPairwiseDF[indSig,]


#  t test- check if each level significantly different from null hypothesis (chance)
EMM <- emmeans(model, ~  timeLock | latencyOrder)   # where treat has 2 levels

t= test(EMM, null=0, adjust='sidak')

print(t,by = NULL, adjust = "sidak")   # all are in one group now

# lots of values here (comparison at every time bin) make a viz or subset of only those below "significance" p value threshold
indSig= which(t$p.value<=pAlpha)

tSig= t[indSig,]



# lots of values here. make a viz or subset of only those below "significance" p value threshold
pAlpha= 0.050

indSig= which(t$p.value<=pAlpha)

tSig= t[indSig,]

library(ggplot2)




#4%%-- Save output to variables between tests  ####
# trying to keep code mostly generalizable and just save custom names at end
# all the results into descriptive variables between tests


fig3_Supplement_stats_lickCorrelation_A_0_description= "Figure 3: Encoding model, Post-Event Kernel AUCs"
fig3_Supplement_stats_lickCorrelation_A_1_model= model
fig3_Supplement_stats_lickCorrelation_A_2_model_anova= model_anova
fig3_Supplement_stats_lickCorrelation_A_3_model_post_hoc_pairwise= tPairwiseSig
fig3_Supplement_stats_lickCorrelation_A_3_model_post_hoc_t= tSig


#5%%-- Save output ####

#- move to output directory prior to saving
setwd(pathOutput)

#------Pooled

sink("vp-vta_fig3_Supplement_stats_lickCorrelation_A.txt")
'------------------------------------------------------------------------------'
'0)---- Description --: '
print(fig3_Supplement_stats_lickCorrelation_0_description)
'------------------------------------------------------------------------------'
print('1)---- LME:')
print(summary(fig3_Supplement_stats_lickCorrelation_A_1_model))
'------------------------------------------------------------------------------'
print('2)---- ANOVA of LME:')
print(fig3_Supplement_stats_lickCorrelation_A_2_model_anova)
'------------------------------------------------------------------------------'
print('3)---- Posthoc T, Only Significant time bins :') # Make sure for posthocs the summary is printed with pval correction
print(fig3_Supplement_stats_lickCorrelation_A_3_model_post_hoc_t, by = NULL, adjust = "sidak")


'---- END ---------------------------------------------------------------------'
sink()  # returns output to the console


setwd(workingDir)

#6%%-- viz ####
# Viz stats output of "significant" comparions by time bin


p=''

p= ggplot()+
  
  scale_colour_brewer(palette="Dark2")+
  
  geom_point(data= t, aes(x=timeLock, y=p.value, color=latencyOrder, shape=latencyOrder, size=1))+
  
  # scale_color_hue(l=40, c=35)+
  
  # geom_line(inherit.aes=FALSE, data=df, aes(x=timeLock, y=periCueRho, color=latencyOrder)+
  #   scale_colour_brewer(palette="Set2"))+
  
  # geom_line(data=df, aes(x=timeLock, y=periCueRho, color=latencyOrder)+
  # #             scale_colour_brewer(palette="Set2"))+
  # # 
  # geom_line(data=df, aes(x=timeLock, y=periCueRho, color=latencyOrder)+
  #             scale_colour_brewer(palette="Set2")
#           )
# # 
# geom_line(data=df, aes(x=timeLock, y=periCueRho, color=latencyOrder, alpha=0.2)+
#   scale_colour_manual(l=30))
geom_line(data=df, inherit.aes=FALSE, aes(x=timeLock, y=periCueRho, color=latencyOrder, alpha=0.2))+
  
  geom_hline(yintercept=pAlpha, color='red', size=2, alpha=0.6)+
  geom_vline(xintercept= t$timeLock[t$timeLock==5.0], color='grey', size=2, alpha=0.6)+
  
  # manual latency
  # geom_vline(xintercept= t$timeLock[t$timeLock==2.75], color='purple', size=2, alpha=0.6)+
  geom_vline(xintercept= t$timeLock[t$timeLock==2.05], color='purple', size=2, alpha=0.6)+
  # geom_vline(xintercept=2.05, color='purple', size=2, alpha=0.6)+
  
  
  show(p)



# Check the actual data for these "Significant" time points. Was the correlation itself significant?

indSig= which(df$timeLock %in% tSig$timeLock)

dfSig= df[indSig,]

# clearly "significant" late time bins seem to be outliers. not consistent between all subjects tho maybe shared between a couple

# plot "Significant" data
dfPlot= dfSig[dfSig$latencyOrder=='rhoBlue',]

# double "sig"-- correlation pval < alpha and different from 0 in this time bin
dfPlot2= dfPlot[dfPlot$pvalBlue<= pAlpha,]

p=''

p= ggplot()+
  
  # geom_boxplot(data= dfPlot, aes(x=timeLock, y=pvalBlue), colour='gray', alpha=0.5)+
  
  geom_boxplot(data= dfPlot, aes(x=timeLock, y=pvalBlue), colour='gray', alpha=0.5)+
  # 
  # stat_summary(data= dfPlot,
  #   fun= median,
  #   geom = 'line',
  #   aes(x=timeLock, y=pvalBlue), colour='black')+
  # 
  
  # # stat_summary(fun=median, geom='line', aes(x=dfPlot$timeLock, y=dfPlot$pvalBlue), colour='black', alpha=0.5, size=2)+
  # geom_line(fun=median, data=dfPlot, aes(x=timeLock, y=pvalBlue), colour='black', alpha=.8, size=2)+
  # 
  # 
# stat_summary(fun=median, data= dfPlot,  geom='line', aes(x=timeLock, y=pvalBlue), colour='black', alpha=0.5, size=2)+


geom_line(data=dfPlot, aes(x=timeLock, y=pvalBlue, color=subject, alpha=0.2))+
  
  geom_point(data=dfPlot, aes(x=timeLock, y=pvalBlue, shape=subject, alpha=0.6))+
  
  # geom_point(data=dfPlot2, aes(x=timeLock, y=pvalBlue, shape=subject), colour="purple", size=2)+
  geom_point(data=dfPlot2, aes(x=timeLock, y=pvalBlue, colour=subject), shape=1, size=3, stroke=3, alpha=0.6)+
  
  
  geom_hline(yintercept=pAlpha, colour='red', size=2, alpha=0.6)+
  
  # geom_vline(xintercept= df$timeLock[df$timeLock==2.05], colour='purple', size=2, alpha=0.6)+
  geom_vline(xintercept= 2.05, colour='purple', size=2, alpha=0.6)+
  
  
  
  # geom_vline(xintercept= t$timeLock[t$timeLock==5.0], color='grey', size=2, alpha=0.6)+
  
  # manual latency
  # geom_vline(xintercept= t$timeLock[t$timeLock==2.75], color='purple', size=2, alpha=0.6)+
  
  show(p)


#%% END ####

