#First part of this code is from Fang and Nik
#Will note with "#F-N" at start and end of their sections to acknowledge their contribution
#Looks like this:
"#F-N #(indicates start of section)
#some code and/or comments
#over several lines
#F-N" #(indicates end of section)

#First install all the appropriate packages
#F-N
install.packages("reshape")
install.packages("ggplot2")
install.packages("lme4")
install.packages("sjPlot")
install.packages("Hmisc")
library(sjPlot)
library(psych)
library(Hmisc)
library(lme4)
library(ggplot2)
library(reshape)
#F-N

#load script file "2_helperFUNfinal.R" (I used "open with RStudio" on Mac)
#run ""2_helperFUNfinal.R"
#or
#Did this as below code refers to Nik's pathway to file
path_to_helper_functions = file.choose() #choose "2_helperFUNfinal.R" 

#F-N
#path_to_helper_functions = "~/Win7/Desktop/helperFUNfinal.r"
#F-N

#F-N
source(path_to_helper_functions) # loading the helper functions using the path specified on the path_to_helper_functions variable #F-N
#My note: Don't know if these last 2 steps are actually needed, maybe redundant, did them anyway
#TLD <- read.csv("M://TangoLyricsData.csv")
#F-N

#Instead of above did this first
TLD <- read.csv(file.choose())
#and choose file TangoLyricsData.csv

#Glitch: apparently when saving the .xlxs file "TangoLyricsData" as .csv file it added 180 blank "observations"
#consisting of 180 empty rows at end of data, which messes up the data
#Went into TangoLyricsData.csv file and deleted those 180 rows; seems to be fine now.
#Also note that since splitting out compadre-compadrito-compadrón into 3 separate
#terms the total # of observations changed from 2400 to 2460
#But verify that it shows "2460 obs. of 7 variables" in Global Environment window

#F-N
#-----------------------------------------------------------------------#
#                 Descriptive statistics and Visualisation              #
#-----------------------------------------------------------------------#


##Descriptive tables
#ROWS = VARIABLES YOU WANT (USUALLY ONLY CONTINUES). COLUMNS = statistic to apply
round(psych::describe(TLD[,c("Termfreq", "DPfreq")]),2)  

summary(TLD[,c("Gender","Term","Variation", "DP")])  


##Comparing each level of the categorical variables.
by(TLD[,"DPfreq"], TLD$Gender, psych::describe) 
by(TLD[,"DPfreq"], TLD$Variation, psych::describe)
by(TLD[,"DPfreq"], TLD$Affect, psych::describe)
#F-N
#Note: for some reason above seems to work with .csv file but not .xlxs

#F-N
## set these variables as factors
TLD$DP = factor(TLD$DP)
TLD$Gender = factor(TLD$Gender)
TLD$Variation = factor(TLD$Variation)
TLD$Term = factor(TLD$Term) 
TLD$Affect = factor(TLD$Affect)
#I added Affect as a factor, but probably won't use it in formulas--just wanted to have it available.

##Checking any potential correlation amongst your continuous variables.
#Remember that continuous variable means scale or numeric variable
#all others are categorical/nominal
cor.test(TLD$DPfreq, TLD$Termfreq)
##high corelation between DPfreq and Termfreq! Looks good!
#F-N

#Checking for correlation between categorical x variables:
chisq.test(TLD$Variation, TLD$Gender)
#results:
#Pearson's Chi-squared test with Yates' continuity correction
#data:  TLD$Variation and TLD$Gender
#X-squared = 1.3009, df = 1, p-value = 0.2541
#So, Variation and Gender are not significantly correlated, so no multicollinearity.
#I think.
#Now on the other combinations:
chisq.test(TLD$Variation, TLD$Affect)
#Results:
#Pearson's Chi-squared test
#data:  TLD$Variation and TLD$Affect
#X-squared = 0, df = 2, p-value = 1
chisq.test(TLD$Variation, TLD$DP)
#Pearson's Chi-squared test
#data:  TLD$Variation and TLD$DP
#X-squared = 0, df = 29, p-value = 1
chisq.test(TLD$Variation, TLD$Term)
#Pearson's Chi-squared test
#data:  TLD$Variation and TLD$Term
#X-squared = 2460, df = 81, p-value < 2.2e-16
#This makes sense, as 40 of the terms must be Lunfardo and 42 must be Standard

#Now for Gender:
chisq.test(TLD$Gender, TLD$Affect)
#Pearson's Chi-squared test
#data:  TLD$Gender and TLD$Affect
#X-squared = 0, df = 2, p-value = 1
chisq.test(TLD$Gender, TLD$DP)
#Pearson's Chi-squared test
#data:  TLD$Gender and TLD$DP
#X-squared = 0, df = 29, p-value = 1
chisq.test(TLD$Gender, TLD$Term)
#Pearson's Chi-squared test
#data:  TLD$Gender and TLD$Term
#X-squared = 2460, df = 81, p-value < 2.2e-16
#Again, this makes complete sense due to the 40/42 split between female and male

#Finally, Affect just to make sure.
chisq.test(TLD$Affect, TLD$DP)
#Pearson's Chi-squared test
#data:  TLD$Affect and TLD$DP
#X-squared = 4845.4, df = 58, p-value < 2.2e-16
chisq.test(TLD$Affect, TLD$Term)
#Pearson's Chi-squared test
#data:  TLD$Affect and TLD$Term
#X-squared = 0, df = 162, p-value = 1
#Also totally logical!! The DPs have been assigned and affect, so high correlation.
#The terms have not.
#All of the above shows that I do not have problems with multicollinearity--woohoo!!
#And, perhaps this is overkill. But I want to be extra careful since I have
#a limited understanding of stats and do not want to be caught out in errors because I didn't
#check and re-check everything!

##------Just for S & G-------------##
#This table is for checking on possible correlations within data.
#it makes it obvious that there ARE correlations in my data
#and it's pretty
library(tabplot)
tableplot(TLD)
tableplot(TLD,sortCol = "DP")
#ok, these are very cool, colorful charts!
#They do show a correlation between lunfardo and female, 
#and the affects of the DPs (maybe?)
#not sure on 2nd statement
#trying again with different sort criteria
tableplot(TLD,sortCol = "Variation")
#the above is interesting- note it appears there are more DPs with lunfardo
tableplot(TLD,sortCol = "Gender")
#and more DPs with female
tableplot(TLD,sortCol = "Affect")
tableplot(TLD,sortCol = "DPfreq")
#interesting: if I sort by continuous variables it converts them to log
tableplot(TLD,sortCol = "Termfreq")
#shows that as term freq goes up, so does dpfreq, which is to be expected.
tableplot(TLD,sortCol = "Term")
#Ok, that was fun. But not really necessary
#I guess I could make some cool art out of it :-) Paper my walls or something.
#---------------------------------------

#Back to the serious stuff
#F-N
###--Plotting
##Plotting an overview figure/inspecting for anything blatantly weird.
## note that we only need to check continuous variables #F-N
pairs.panels(TLD[,c(2,5)]) 
#F-N

#Note that histograms of Termfreq and DPfreq from above plotting show definite right-tail skew,
#and the scatterplot of DPfreq on y axis and Termfreq on x axis show definite pattern
#This is first hint of non-normality and heteroscedasticity
#Further EDA shows that this is most likely due to zero-inflation in data.


#F-N
## plotting some descriptives statistics
# boxplot for continuous VS categorical variables: Gender
plot(TLD[,"DPfreq"]~TLD[,"Gender"], ylab="DPfreq", xlab="Gender", main = "Boxplot of DPfreq conditional on Gender",border="grey30", 
     frame.plot=F, staplewex=0, outwex=2, range = 1.5, boxwex=.75, 
     boxfill="grey50", whisklty=1, whiskcol="grey40", boxcol="grey40",
     outcol="grey40", outpch=20, outcex=.5, staplecol="white")
#F-N

#I added boxplot for the other variable, and did the same with all of the boxplots:
# boxplot for continuous VS categorical variables: Variation
plot(TLD[,"DPfreq"]~TLD[,"Variation"], ylab="DPfreq", xlab="Variation", main = "Boxplot of DPfreq conditional on Variation", border="grey30", 
     frame.plot=F, staplewex=0, outwex=2, range = 1.5, boxwex=.75, 
     boxfill="grey50", whisklty=1, whiskcol="grey40", boxcol="grey40",
     outcol="grey40", outpch=20, outcex=.5, staplecol="white")

#F-N
# boxplot for only one continuous variable: "DPfreq"
boxplot(TLD[,"DPfreq"], main=names(TLD["DPfreq"]), border="grey30", 
        frame.plot=F, staplewex=0, outwex=2, range = 1.5, boxwex=.75, 
        boxfill="grey50", whisklty=1, whiskcol="grey40", boxcol="grey40",
        outcol="grey40", outpch=20, outcex=.5, staplecol="white")
#F-N

#Added boxplot for the other variable:
# boxplot for only one continuous variable: "Termfreq"
boxplot(TLD[,"Termfreq"], main=names(TLD["Termfreq"]), border="grey30", 
        frame.plot=F, staplewex=0, outwex=2, range = 1.5, boxwex=.75, 
        boxfill="grey50", whisklty=1, whiskcol="grey40", boxcol="grey40",
        outcol="grey40", outpch=20, outcex=.5, staplecol="white")

#F-N
# Violin plots for gender
ggplot(TLD, aes(x=Gender, y=DPfreq,)) + 
  geom_violin(trim=F, fill="gray")+
  geom_boxplot(width=0.1)+
  theme_classic()+
  stat_summary(fun.data="mean_sdl"
               ,geom="crossbar", width=0.0005 )
#stat_summary(fun.data=mean_sdl
#           ,geom="pointrange", color="red")
#F-N

#Added boxplot for the other variable:
# Violin plots for variation
ggplot(TLD, aes(x=Variation, y=DPfreq)) + 
  geom_violin(trim=F, fill="gray")+
  geom_boxplot(width=0.1)+
  theme_classic()+
  stat_summary(fun.data="mean_sdl"
               ,geom="crossbar", width=0.0005 )
#stat_summary(fun.data=mean_sdl
#            ,geom="pointrange", color="red")

#F-N
# histogram for 1 continuous: "DPfreq"
hist(TLD[,"DPfreq"],main="DPfreq", xlab="", probability = T, border = F, col = "grey60", breaks = 20); lines(
  density(TLD[,"DPfreq"], na.rm = T),type="l", lwd=3,lty=1, col="grey25")
#F-N

#I added histogram for continuous: "Termfreq"
hist(TLD[,"Termfreq"],main="Termfreq", xlab="", probability = T, border = F, col = "grey60", breaks = 20); lines(
  density(TLD[,"Termfreq"], na.rm = T),type="l", lwd=3,lty=1, col="grey25")

#Again, the "DPfreq" histogram indicates zero-inflation and the "Termfreq" histogram
#looks suspiciously like it will lead to heteroscedasticity.

#F-N
#explore the structure of the data (nested or crossed relationships between the categorical variables)
#for dp
table(TLD$DP, TLD$Gender) # nested
table(TLD$DP, TLD$Variation) # nested
table(TLD$DP, TLD$Variation, TLD$Gender) # nested - nested
#adding Affect (mine):
table(TLD$DP, TLD$Affect) #crossed

# for term
table(TLD$Term, TLD$Gender) # crossed 
table(TLD$Term, TLD$Variation) # crossed
table(TLD$Term, TLD$Variation, TLD$Gender) # crossed - crossed
#Adding Affect (mine):
table(TLD$Term, TLD$Affect) #nested

table(TLD$Term, TLD$DP) # nested
table(TLD$Gender, TLD$Variation) #nested
#F-N
#adding affect
table(TLD$Term,TLD$DP, TLD$Affect) #nested
table(TLD$Gender, TLD$Variation,TLD$Affect) #nested
#F-N
#-----------------------------------------------------------------------#
#                 Analyses and Visualisations                           #
#-----------------------------------------------------------------------#

############################### Gender #################################

plotDat = fot.fun(data = TLD, measurevar ="DPfreq", groupvars = c("Gender") )
(gend_plot = ggplot(plotDat, aes(y = DPfreq, x=Gender))+
    geom_line(aes(group=1), size=1, colour = "#333333")+
    ylab("DPfreq")+
    #scale_y_continuous(breaks = c(610,630,650,670))+
    ggtitle("DP Frequency of nouns for different genders") +
    geom_text(aes(label=round(DPfreq,2)), vjust=.5,hjust=1.5, colour="black",position=position_dodge(.9))+
    #scale_linetype_discrete(name=NULL, labels=c("Left Hand", "Right Hand"))+
    geom_errorbar(aes(ymin=DPfreq-se, ymax=DPfreq+se, linetype=NULL),
                  width=.03, size=0.6, colour = "#333333")+
    theme(axis.text = element_text(size=12, colour = "#993333", angle=25),
          legend.position = c(0.6, 0.9),
          axis.title = element_text(size = 14, colour = "#333333"), axis.line.x = element_line(size = 0.7),
          axis.line.y = element_line(size = 0.7, colour = "#333333"), axis.ticks = element_line(size = 0.7))+
    theme(
      panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
      #panel.grid.minor = theme_blank(), 
      #panel.grid.major = theme_blank(),
      plot.background = element_rect(fill = "transparent",colour = NA)
    ) )

############################## Variation ############################## 

plotDat = fot.fun(data = TLD, measurevar ="DPfreq", groupvars = c("Variation") )
(varia_plot = ggplot(plotDat, aes(y = DPfreq, x=Variation))+
    geom_line(aes(group=1), size=1, colour = "#333333")+
    ylab("DPfreq")+
    #scale_y_continuous(breaks = c(610,630,650,670))+
    ggtitle("DP Frequency of gendered nouns in different variations") +
    geom_text(aes(label=round(DPfreq,2)), vjust=.5,hjust=1.5, colour="black",position=position_dodge(.9))+
    #scale_linetype_discrete(name=NULL, labels=c("Left Hand", "Right Hand"))+
    geom_errorbar(aes(ymin=DPfreq-se, ymax=DPfreq+se, linetype=NULL),
                  width=.03, size=0.6, colour = "#333333")+
    theme(axis.text = element_text(size=12, colour = "#993333", angle=25),
          legend.position = c(0.6, 0.9),
          axis.title = element_text(size = 14, colour = "#333333"), axis.line.x = element_line(size = 0.7),
          axis.line.y = element_line(size = 0.7, colour = "#333333"), axis.ticks = element_line(size = 0.7))+
    theme(
      panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
      #panel.grid.minor = theme_blank(), 
      #panel.grid.major = theme_blank(),
      plot.background = element_rect(fill = "transparent",colour = NA)
    ))


# show gender plot and variation plot in the same window
multiplot(gend_plot, varia_plot, cols = 2)

############### Gender and variation ###############

plotDat = fot.fun(data = TLD, measurevar ="DPfreq", groupvars = c("Gender", "Variation") )
(gend_var_plot = ggplot(plotDat, aes(DPfreq, x=Variation, linetype=Gender))+
    geom_line(aes(group=Gender), size=1, colour = "#333333")+
    ylab("DPfreq")+
    #scale_y_continuous(breaks = c(610,630,650,670))+
    ggtitle("DP Frequency of different gendererd nouns in different variations") +
    geom_text(aes(label=round(DPfreq,2)), vjust=.5, colour="black",position=position_dodge(.6))+
    #scale_linetype_discrete(name=NULL, labels=c("Left Hand", "Right Hand"))+
    geom_errorbar(aes(ymin=DPfreq-se, ymax=DPfreq+se, linetype=NULL),
                  width=.03, size=0.6, colour = "#333333")+
    theme(axis.text = element_text(size=12, colour = "#993333", angle=45),
          legend.position = c(0.6, 0.9),
          axis.title = element_text(size = 14, colour = "#333333"), axis.line.x = element_line(size = 0.7),
          axis.line.y = element_line(size = 0.7, colour = "#333333"), axis.ticks = element_line(size = 0.7))+
    theme(
      panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
      #panel.grid.minor = theme_blank(), 
      #panel.grid.major = theme_blank(),
      plot.background = element_rect(fill = "transparent",colour = NA)
    ) )

# show gender plot and variation plot in the same window
multiplot(gend_var_plot, varia_plot, cols =2)

############## Termfreq ####################

plotDat = fot.fun(data = TLD, measurevar ="DPfreq", groupvars = c("Termfreq") )
ggplot(plotDat, aes(DPfreq, x=Termfreq))+
  geom_line(aes(group=1), size=1, colour = "#333333")+
  #facet_wrap(~Gender)+
  ylab("DPfreq")+
  #scale_y_continuous(breaks = c(610,630,650,670))+
  ggtitle("Relation between Termfreq and DPfreq") +
  #geom_text(aes(label=round(DPfreq,2)), vjust=.5, colour="black",position=position_dodge(.9))+
  #scale_linetype_discrete(name=NULL, labels=c("Left Hand", "Right Hand"))+
  geom_errorbar(aes(ymin=DPfreq-se, ymax=DPfreq+se, linetype=NULL),
                width=.03, size=0.6, colour = "#333333")+
  theme(axis.text = element_text(size=12, colour = "#993333", angle=45),
        legend.position = c(0.6, 0.9),
        axis.title = element_text(size = 14, colour = "#333333"), axis.line.x = element_line(size = 0.7),
        axis.line.y = element_line(size = 0.7, colour = "#333333"), axis.ticks = element_line(size = 0.7))+
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
    #panel.grid.minor = theme_blank(), 
    #panel.grid.major = theme_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA)
  ) 

############## 2 - way int gender ~  Termfreq ####################

plotDat = fot.fun(data = TLD, measurevar ="DPfreq", groupvars = c("Gender",  "Termfreq") )
ggplot(plotDat, aes(DPfreq, x=Termfreq, linetype=Gender))+
  geom_line(aes(group=Gender), size=1, colour = "#333333")+
  #facet_wrap(~Gender)+
  ylab("DPfreq")+
  #scale_y_continuous(breaks = c(610,630,650,670))+
  ggtitle("Relation between Termfreq and DPfreq for different gendered nouns") +
  #geom_text(aes(label=round(DPfreq,2)), vjust=.5, colour="black",position=position_dodge(.9))+
  #scale_linetype_discrete(name=NULL, labels=c("Left Hand", "Right Hand"))+
  geom_errorbar(aes(ymin=DPfreq-se, ymax=DPfreq+se, linetype=NULL),
                width=.03, size=0.6, colour = "#333333")+
  theme(axis.text = element_text(size=12, colour = "#993333", angle=45),
        legend.position = c(0.6, 0.9),
        axis.title = element_text(size = 14, colour = "#333333"), axis.line.x = element_line(size = 0.7),
        axis.line.y = element_line(size = 0.7, colour = "#333333"), axis.ticks = element_line(size = 0.7))+
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
    #panel.grid.minor = theme_blank(), 
    #panel.grid.major = theme_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA)
  ) 

############## 2 - way int Variation ~  Termfreq ####################

plotDat = fot.fun(data = TLD, measurevar ="DPfreq", groupvars = c("Termfreq",  "Variation") )
ggplot(plotDat, aes(DPfreq, x=Termfreq, linetype=Variation))+
  geom_line(aes(group=Variation), size=1, colour = "#333333")+
  #facet_wrap(~Gender)+
  ylab("DPfreq")+
  #scale_y_continuous(breaks = c(610,630,650,670))+
  ggtitle("Relation between Termfreq and DPfreq for gendered nouns in different variations") +
  #geom_text(aes(label=round(DPfreq,2)), vjust=.5, colour="black",position=position_dodge(.9))+
  #scale_linetype_discrete(name=NULL, labels=c("Left Hand", "Right Hand"))+
  geom_errorbar(aes(ymin=DPfreq-se, ymax=DPfreq+se, linetype=NULL),
                width=.03, size=0.6, colour = "#333333")+
  theme(axis.text = element_text(size=12, colour = "#993333", angle=45),
        legend.position = c(0.6, 0.9),
        axis.title = element_text(size = 14, colour = "#333333"), axis.line.x = element_line(size = 0.7),
        axis.line.y = element_line(size = 0.7, colour = "#333333"), axis.ticks = element_line(size = 0.7))+
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
    #panel.grid.minor = theme_blank(), 
    #panel.grid.major = theme_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA)
  ) 

############## 3- way int gender ~ variation ~ Termfreq ####################

plotDat = fot.fun(data = TLD, measurevar ="DPfreq", groupvars = c("Gender", "Variation", "Termfreq") )
ggplot(plotDat, aes(DPfreq, x=Termfreq, linetype=Variation))+
  geom_line(aes(group=Variation), size=1, colour = "#333333")+
  facet_wrap(~Gender)+
  ylab("DPfreq")+
  #scale_y_continuous(breaks = c(610,630,650,670))+
  ggtitle("Relation between DP Frequency and Term Frequency by Gender and by Variation") +
  #geom_text(aes(label=round(DPfreq,2)), vjust=.5, colour="black",position=position_dodge(.9))+
  #scale_linetype_discrete(name=NULL, labels=c("Left Hand", "Right Hand"))+
  geom_errorbar(aes(ymin=DPfreq-se, ymax=DPfreq+se, linetype=NULL),
                width=.03, size=0.6, colour = "#333333")+
  theme(axis.text = element_text(size=12, colour = "#993333", angle=45),
        legend.position = c(0.6, 0.9),
        axis.title = element_text(size = 14, colour = "#333333"), axis.line.x = element_line(size = 0.7),
        axis.line.y = element_line(size = 0.7, colour = "#333333"), axis.ticks = element_line(size = 0.7))+
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
    #panel.grid.minor = theme_blank(), 
    #panel.grid.major = theme_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA)
  ) 

########################  models   ###############################
#fit linear mixed effects models
##############

# model by dp
#meaning that dp is the random effect
mod_dp <- lmer(DPfreq ~ Variation + Gender + Termfreq +
                 Variation : Gender +
                 (1 | DP), data=TLD)

sjt.lmer(mod_dp, show.header = TRUE, string.est = "Estimate", 
         string.ci = "Conf. Int.", string.p = "p-value",show.se = T,
         string.dv = "Response", string.pred = "Coefficients",
         p.numeric=T, show.aic = TRUE, separate.ci.col = T,
         show.r2 = T,show.dev =T,show.re.var = T)

summary(mod_dp)

## Intercept term
# It's the mean of the combination of the "leading" groups e.g., the mean of the Lunfardo,females. 
#(it's not exactly that because you also have a continuous variable which hasn't levels)
# model by term
## VariationStandard term
# The variationStandard (for females) it's by -.36 DPfreq lower than the variationFunfardo (for females)
## GenderMale
# The GenderMale (for VariationStandard) is by -.26 DPreq lower than the GenderFemales (for VariationStandard)
## TermFreq 
# For each +1 value of Termfreq, DPfreq increases by 0.025
## VariationStandard:GenderMale
# The effect of the VariationStandard for females on the DPfreq is by .23 stronger than from the same effect for males (the variation VS gender plot might help here)


# model by term
#here term is the random effect
mod_term <- lmer(DPfreq ~ Variation + Gender + Termfreq +
                   Variation : Gender +
                   (1 | Term), data=TLD)

summary(mod_term)

sjt.lmer(mod_term, show.header = TRUE, string.est = "Estimate", 
         string.ci = "Conf. Int.", string.p = "p-value",show.se = T,
         string.dv = "Response", string.pred = "Coefficients",
         p.numeric=T, show.aic = TRUE, separate.ci.col = T,
         show.r2 = T,show.dev =T,show.re.var = T)




# model by by DP and by term
model_DP_Term <- lmer(DPfreq~Termfreq+Variation*Gender + 
             (1|DP) + (1|Term),data=TLD, REML=F)
summary(model_DP_Term)



sjt.lmer(model_DP_Term, show.header = TRUE, string.est = "Estimate", 
         string.ci = "Conf. Int.", string.p = "p-value",show.se = T,
         string.dv = "Response", string.pred = "Coefficients",
         p.numeric=T, show.aic = TRUE, separate.ci.col = T,
         show.r2 = T,show.dev =T,show.re.var = T)
#F-N

#F-N
############################ model comparison ####################################  
anova(mod_dp, mod_term, model_DP_Term)


hist(resid(model_DP_Term), breaks = 20) # normality assumption for the residuals. Looks good.
#F-N

#just for fun, install rcompanion then run
library(rcompanion)
plotNormalHistogram(residuals(model_DP_Term))
#This just shows a normal curve line on the histogram

#F-N
#model_DP_Term is the best-fit model
#choose model_DP_Term as the final solution
#report stats

sjt.lmer(model_DP_Term, show.header = TRUE, string.est = "Estimate", 
         string.ci = "Conf. Int.", string.p = "p-value",show.se = T,
         string.dv = "Response", string.pred = "Coefficients",
         p.numeric=T, show.aic = TRUE, separate.ci.col = T,
         show.r2 = T,show.dev =T,show.re.var = T)
#F-N
#This is the end of #F-N code; the rest is mine
#But without Fang and Nik I would never have made it this far!!!
#FANG AND NIK ARE AWESOME!!!!

#Note that above (sjt.lmer etc. etc.) generates a table in APA style, ready to include in paper
#run the summary to get the t-values to report, if using this model
summary(model_DP_Term)

##-----------TWEAKING----------------------##
#So, while both #F-N's and my histograms showed normality in the residuals,
#I want to double check for homoscedasticity (aka homogeneity of variance)
plot(fitted(model_DP_Term),residuals(model_DP_Term))
#The plot shows definite heteroscedasticity: there is a clear pattern in the residuals
qqnorm(residuals(model_DP_Term))
qqline(residuals(model_DP_Term),col="red")
#the qqnorm plot with red line shows that in reality there is only normality 
#in the residuals for values between -2.5 and 1;
#values < -2.5 or > 1 dip and surge from the line respectively
#But what is most concerning is the heteroscedasticity
#which I attribute to the data being zero-inflated.
#I attempted to transform the data using Sqrt, Log10, and a Tukey transformation
#These improved the qqnorm line a bit, but did not handle the heteroscedasticity

##------trying Zuur, et. al's zero-inflated code------------##
#pg 278 in "Mixed Effects Models and Extensions in Ecology with R" (Zuur, et. al. 2009)
#will cite properly in thesis
#Substituted my variables in their code for "ParasiteCod" example

# I already have my factors from before:
#TLD$DP = factor(TLD$DP)
#TLD$Gender = factor(TLD$Gender)
#TLD$Variation = factor(TLD$Variation)
#TLD$Term = factor(TLD$Term)

#This is graphic that helps evaluate for zero-inflation
I1= is.na(TLD$DPfreq)|is.na(TLD$Termfreq)|is.na(TLD$DP)|is.na(TLD$Gender)|
  is.na(TLD$Variation)|is.na(TLD$Term)
TLD2=TLD[!I1, ]
plot(table(TLD2$DPfreq), ylab= "Frequencies", xlab="Observed DPfreqs")
#Ok, that actually worked. It shows that, for example there are >1500 0s in the data
#and only a few at DPfreqs of 5 and above.

##-------------------------------------------------##
##      DEALING WITH THE ZERO-INFLATED DATA        ##
##-------------------------------------------------##

#There is a new package for this: glmmTMB
#ARTICLE JUST CAME OUT JULY 2, 2018 so how lucky is that?!! (cite Bolker)
#Make sure have installed these, but don't re-install!
library(glmmTMB)
library(bbmle)
library(ggplot2)

#Double-checking my understanding of what terms mean:
#offset is needed in poisson I guess?
# "Poisson models handle exposure variables by using simple algebra to change the dependent variable from a rate into a count."
# "If the rate is count/exposure, multiplying both sides of the equation by exposure 
#moves it to the right side of the equation.  When both sides of the equation are then logged, 
#the final model contains ln(exposure) as a term that is added to the regression coefficients. 
#This logged variable, ln(exposure), is called the offset variable."
#found here: https://www.theanalysisfactor.com/the-exposure-variable-in-poission-regression-models/ 
#accessed 4 aug 2018
#so, yes, it is needed for the poisson model

#So now, fit model_DP_freq using glmmTMB as poisson first, then try negative binomial 1 & 2
#essentially I'm ending up doing a ZINB model.
#Poisson is not appropriate for this data anyway, as it isn't time-based, but showing it here
#to show the steps I went through to get there
fit_zipoisson <- glmmTMB(DPfreq~(Variation*Gender)+offset(log(Termfreq))+(1|DP)+(1|Term),
                         data=TLD,ziformula=~1,family=poisson)

summary(fit_zipoisson)
#Results do look good, though, as far as p-values are concerned
plot(residuals(fit_zipoisson))
plot(fitted(fit_zipoisson), residuals(fit_zipoisson))
#So, the plot still shows a mess for heterogeneity

#Next is to apply negative binomial formulas to see what works best
fit_zinbinom1=update(fit_zipoisson,family=nbinom1)
summary(fit_zinbinom1)#
plot(fitted(fit_zinbinom1), residuals(fit_zinbinom1))
#Not much change in residuals here. Keep trying

#Reminder that my orig formula is: (DPfreq~Termfreq + (Variation*Gender)+(1|DP)+(1|Term)
fit_zinbinom1_bs = update(fit_zinbinom1,. ~ (Variation*Gender)+Termfreq+(1|DP)+(1|Term))
summary(fit_zinbinom1_bs)
plot(fitted(fit_zinbinom1_bs), residuals(fit_zinbinom1_bs))

#Just double-checking:
plotNormalHistogram(residuals(fit_zinbinom1_bs))
qqnorm(residuals(fit_zinbinom1_bs))
qqline(residuals(fit_zinbinom1_bs),col="red")

#This will compare the models
AICtab(fit_zipoisson,fit_zinbinom1,fit_zinbinom1_bs)
#The AIC shows fit_zinbinom1 at lowest value
#THEREFORE, fit_zinbinom1 is the best model
#in spite of fact that fit_zinbinom_bs has better pvalues
#And although the VariationStandard:GenderMale interaction is not significant
#that's ok--rest of the numbers are great!! (Can't have everything!)
#Run summary again just to check:
summary(fit_zinbinom1)

#Ok, now re-wrote the model taking out the poisson bit as it's not needed
fit2_zinbinom1 = glmmTMB(DPfreq~(Variation*Gender)+offset(log(Termfreq))+(1|DP)+(1|Term),data=TLD,ziformula=~1,family=nbinom1)
summary(fit2_zinbinom1)
#gives same results as fit_zinbiniom1 model

##--------------------------------------------------------##
##             What finally worked: DHARMa                ##
##--------------------------------------------------------##
# I found this page: https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
#This put my mind to rest about heteroscedasticity and zero-inflation
#Did my zero-inflation fit actually work? 
#-----YES!!!!!----#
#What DHARMa does is give you a plot of residuals and other EDA tools that allow you to determine
#if you really do have normality, homoscedasticity, etc. in a ZINB-fit model.
#Apparently the other packages (lme4, etc.) are not accurate for these models.
#Regardless,
#Will cite and praise to the sky for this wonderful thing!!
#install package DHARMa
library(DHARMa)
library(glmmTMB) #I think I already ran this

#First tried this with the original model: "model_DP_Term"
simulationOutput= simulateResiduals(fittedModel =model_DP_Term, n=250)
# n is the number of tries the program will attempt to simulate the output of the original model
#The article recommends at least 250, and maybe 1000 at the most

plot(residuals(simulationOutput))
#These residuals are still weird, but when compared with original model:
plot(residuals(model_DP_Term))
#the simulation model residual plot is mostly just a more spread out plot of same
#However, this is revealing:
plot(simulationOutput)
#It really shows where the model fails for assumptions

#ok, I think I'm on to something here...
plotResiduals(TLD$Termfreq,simulationOutput$scaledResiduals)
plotResiduals(TLD$Variation,simulationOutput$scaledResiduals)
plotResiduals(TLD$Gender,simulationOutput$scaledResiduals)
plotResiduals(TLD$DP,simulationOutput$scaledResiduals)
plotResiduals(TLD$Term,simulationOutput$scaledResiduals)
plotResiduals(TLD$Affect,simulationOutput$scaledResiduals)
#I followed article suggestion to do above; just is more EDA that confirms suspicions
#Formal goodness of fit tests on scaled residuals

testUniformity(simulationOutput = simulationOutput)
summary(simulationOutput)
testOverdispersion(simulationOutput = simulationOutput)
#I don't really know how to interpret the above histogram, but I think it's not good
testZeroInflation(simulationOutput = simulationOutput)
#Above definitely confirms zero-inflation!

##Simulation options
simulationOutput=simulateResiduals(fittedModel = model_DP_Term , refit = F)

plot(residuals(simulationOutput))
plot(residuals(model_DP_Term))
plot(simulationOutput)
simulationOutput= simulateResiduals(fittedModel =model_DP_Term, n=250)

#This part is checking the groups, or factors
simulationOutput = recalculateResiduals(simulationOutput, group = TLD$Variation)
plot(simulationOutput)
simulationOutput = recalculateResiduals(simulationOutput, group = TLD$Gender)
plot(simulationOutput)
simulationOutput = recalculateResiduals(simulationOutput, group = TLD$DP)
plot(simulationOutput)
simulationOutput = recalculateResiduals(simulationOutput, group = TLD$Termfreq)
plot(simulationOutput)
simulationOutput = recalculateResiduals(simulationOutput, group = TLD$DPfreq)
plot(simulationOutput)
simulationOutput = recalculateResiduals(simulationOutput, group = TLD$Term)
plot(simulationOutput)

#Back to original
simulationOutput= simulateResiduals(fittedModel =model_DP_Term, n=250)
plot(simulationOutput)
testUniformity(simulationOutput = simulationOutput)

#ok, actually looks like I have underdispersion
#but above is not considered as "rigorous" as this next bit:
testDispersion(simulationOutput)
simulationOutput2=simulateResiduals(fittedModel = model_DP_Term, refit = T, n=20)
testDispersion(simulationOutput2)
########
#Now try using the zero-inflation model "fit2_zinbinom1" that was chosen as best model from that process
simulationOutput3=simulateResiduals(fittedModel = fit2_zinbinom1, n=250)
plot(simulationOutput3)
#SUCCESS!!
# The residual QQ plot shows a normal line--no deviation
#The residual vs. predicted values match as closely as they're gonna get!
#I finally have homogeneity!! Or at least as close as it's gonna get
#and generally good enough to say that my p-values are valid
testUniformity(simulationOutput3)
#Bingo!!
#results of above:
#One-sample Kolmogorov-Smirnov test
#data:  simulationOutput$scaledResiduals
#D = 0.017756, p-value = 0.42
#alternative hypothesis: two-sided
#p-value is not significant: means that there is homogeneity of variance!!

testZeroInflation(simulationOutput3)
testDispersion(simulationOutput3)
#Not sure what above two do for me, but ran them anyway

#Ok, these two I don't need--don't apply to my data--they just made pretty colors
testTemporalAutocorrelation(simulationOutput = simulationOutput3)
testSpatialAutocorrelation(simulationOutput = simulationOutput3, x = TLD$x, y=TLD$y)

# Whoever wrote DHARMa, I love you!! and I love the folks who wrote glmmTMB!!

#Final thing: create APA table for write-up
library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(fit2_zinbinom1, show.se = T)

#After running above, it gave me the APA table, but also this message:
#There were 12 warnings (use warnings() to see them)
#So I did, and got 12 total exactly like #1.
#Warning messages:
#  1: In stri_length(string) :
#  invalid UTF-8 byte sequence detected; perhaps you should try calling stri_enc_toutf8()
#It didn't seem to affect the table, so idk. Not worrying about it for now.
#The point is--this all worked!
#My work is done here.

#Ok, maybe one more thing. I'm going to rename the fit model since the "fit2_zinbinom1" came directly from Bolker
TLD_zinb_model = fit2_zinbinom1
summary(TLD_zinb_model)
simulationOutput4=simulateResiduals(fittedModel = TLD_zinb_model, n=250)
plot(simulationOutput4)
tab_model(TLD_zinb_model, show.se = TRUE)
# Final model is: TLD_zinb_model



##-----CITATIONS & ACKNOWLEDGMENTS--------##

# First of all, let me be clear:

#----FANG JACKSON-YANG and NIKOLAOS THEODOROPOULOS are ROCK STARS!!!----#

#I could not have done this if Fang had not pointed me to LME models in the first place,
#and Nik's plots and explanation of the results helped me to make sense of it all
#I'll write up a suitable (proper) statement in the thesis, but here I just want to say again:
##----YOU ROCK!!!---##


#List of packages to cite:
#First cite R
citation("DHARMa")
citation("glmmTMB")
citation("reshape")
citation("ggplot2")
citation("lme4")
citation("sjPlot")
citation("sjmisc")
citation("sjlabelled")
citation("Hmisc")
citation("rcompanion")
citation("bbmle")
citation("ggplot2")
citation("plyr")
citation("grid")
citation("psych")

#Others that are checked in the Packages window--don't know if/when used or if are normal
# part of R. Find out how/if to cite
citation("lattice") #For ex: I used this in my code exploration but didn't end up using in code
citation("datasets")
citation("Formula")
citation("graphics")
citation("grDevices")
citation("Hmisc")
citation("Matrix")
citation("methods")
citation("stats")
citation("stats4")
citation("survival")
citation("utils")
