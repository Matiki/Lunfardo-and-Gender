#This is the script to run the anovas on the data
#First run load scripts for "1_Elaine TangoLyricsData analysis.R"
#and "2_helperFUNfinal.R"
#The "alternative hypothesis" ("alt") model is TLD_zinb_model:
#TLD_zinb_model = glmmTMB(DPfreq~(Variation*Gender)+offset(log(Termfreq))+(1|DP)+(1|Term),data=TLD,ziformula=~1,family=nbinom1)
#Then create null hypothesis models and run anovas against each.
TLD.nullVAR =glmmTMB(DPfreq~Gender+offset(log(Termfreq))+(1|DP)+(1|Term),data=TLD,ziformula=~1,family=nbinom1)
summary(TLD.nullVAR)
anova(TLD.nullVAR,TLD_zinb_model)
#Variation is significant for DPfreq at p = 0.001981

TLD.nullGEN = glmmTMB(DPfreq~Variation+offset(log(Termfreq))+(1|DP)+(1|Term),data=TLD,ziformula=~1,family=nbinom1)
summary(TLD.nullGEN)
anova(TLD.nullGEN,TLD_zinb_model)
#Gender is significant for DPfreq at p=0.007588

#Check on the interaction between Variation and Gender
TLD.nullInteraction = glmmTMB(DPfreq~Variation+Gender+offset(log(Termfreq))+(1|DP)+(1|Term),data=TLD,ziformula=~1,family=nbinom1)
summary(TLD.nullInteraction)
anova(TLD.nullInteraction, TLD_zinb_model)
#Interaction of Variation and Gender is NOT significant for DPfreq, p = 0.1099
#But it is close to 10%, so don't disregard entirely

#Generate APA results tables
tab_model(TLD_zinb_model, show.se = TRUE)
tab_model(TLD.nullGEN, show.se = TRUE) #This is with Variation included
tab_model(TLD.nullVAR,show.se = TRUE) #And this with Gender
tab_model(TLD.nullInteraction, show.se = TRUE)
#It's somewhat confusing if you look at formulas:
#TLD.nullGEN has Variation in the formula, but it's NULL for GEN, etc.

#If need to see intercepts (coefficients) in "tidy" list, use new package broom.mixed
#"tidy" command
library(broom.mixed)
tidy(TLD_zinb_model)


#Still really not sure if I report the estimates and standard errors from
#TLD_zinb_model or from the 2 null models...may need to ask.
summary(TLD_zinb_model)
#go over all this script as it may have random crap in it--like this note!

########################
#Ok, found this--really cool and will help me explain interaction
# and/or marginal effects or predictions of DPfreqs by variables
library(lme4)
library(ggeffects)
#Shows predicted incidents of DPfreq classed by Variation (the 1st listed variable)
#Note: x is the predictor variable 
#This is MEM: marginal effects at the mean
#Don't need to run this
#pr1 <- ggpredict(TLD_zinb_model, c("Variation", "Gender"))
#plot(pr1)

#can change colors with the palettes from display.brewer.all
#default palette is "Set1"
#Using "Dark2" for Gender and "Set1" for Variation

#But ggaverage gives AME (average marginal effects) which seems to be a better predictor:
#pr1AVE <- ggaverage(TLD_zinb_model, terms = c("Variation", "Gender"))
#summary(pr1AVE)
#plot(pr1AVE)
#And corresponds better to the formula outcomes: Male is a better predictor of standard
# and female of lunfardo.
#so will add AME for all models and use in thesis
#keeping the MEM models recorded here just for the sake of and renaming to avoid confusion
pr1 <- ggaverage(TLD_zinb_model, terms = c("Variation", "Gender"), x.as.factor = TRUE)
summary(pr1)
plot(pr1, color= "Dark2")
#for back up, as stated:
#pr1MEM <-  ggpredict(TLD_zinb_model, c("Variation", "Gender"))

#Shows predicted incidents of DPfreq classed by Gender, since now Gender is first
pr2 <- ggaverage(TLD_zinb_model, c("Gender","Variation"), x.as.factor = TRUE)
plot(pr2)
#pr2MEM <- ggpredict(TLD_zinb_model, c("Gender","Variation"))

#Side by side (gender as facet):
pr3 <- ggaverage(TLD_zinb_model, terms = c("Variation", "Gender"), x.as.factor = TRUE)
#pr3MEM <- ggpredict(TLD_zinb_model, terms = c("Variation", "Gender"))
plot(pr3, facet=TRUE, color="Dark2")
#this is really pr1 (same formula) with facets added to the plot. Same with pr4/pr2

#Side by side (variation as facet):
pr4 <- ggaverage(TLD_zinb_model, terms = c("Gender", "Variation"), x.as.factor = TRUE)
#pr4MEM <- ggpredict(TLD_zinb_model, terms = c("Gender", "Variation"))
plot(pr4, facet=TRUE)
#Note that this model shows that MS is just BARELY a better predictor than FS


#Ok, here's where affect might come in:
#(don't need to run again, it's just here to show that I checked it)
#TLD_Affect <- glmmTMB(DPfreq~(Variation*Gender)+Affect+offset(log(Termfreq))+(1|DP)+(1|Term),data=TLD,ziformula=~1,family=nbinom1)
#pr7 <- ggpredict(TLD_Affect, terms = c("Variation", "Gender", "Affect"))
#plot(pr7)
#actually, no. If do
#summary(TLD_Affect)
#it still shows no significance for intercepts for affects--makes sense!!
#And if you look at above plot, the predicted incidences by variation 
#are the same as plots with original formula w/out Affect

#WOW!! These show facets by the 30 DPs!! Exactly what I wanted!!
#First by variation, genders side by side
pr5 <- ggaverage(TLD_zinb_model, terms = c("Variation", "Gender", "DP"), x.as.factor = TRUE)
#pr5MEM <- ggpredict(TLD_zinb_model, terms = c("Variation", "Gender", "DP"))
plot(pr5, color="Dark2")

#Then by gender, variations side by side
pr6 <- ggaverage(TLD_zinb_model, terms = c("Gender", "Variation", "DP"), x.as.factor = TRUE)
#pr6MEM <- ggpredict(TLD_zinb_model, terms = c("Gender", "Variation", "DP"))
plot(pr6)

#if do summary(pr6) then get the x predicted, and low and high conf. levels 
#for EACH Dp, split by EACH variation!!
#these are marginal effects at the mean (MEM) 
#See vignette at https://cran.r-project.org/web/packages/ggeffects/vignettes/marginaleffects.html
# for what ggeffects does and what it tells you.
#see "R: Get marginal effects from model terms" for definitions of marginal effects

#For one variable, with DPs:
pr7 <- ggaverage(TLD_zinb_model, terms = c("Variation", "DP"), x.as.factor = TRUE)
#pr7MEM <- ggpredict(TLD_zinb_model, terms = c("DP", "Variation"))
summary(pr7)
plot(pr7)
plot(pr7, facet=TRUE)
#summary is more useful as it gives CI and x predicted in numbers
#plot is ok, basically it's like pr5 and pr6 but just for Lunfardo and Standard
#"plot(pr7") 
#This puts red "female" and blue "male" plot lines together, which is interesting
#It's easier to read the DPs, and clearly shows that female gender predicts more than male
#but "plot(pr7, facet = TRUE)" makes it hard to read the DPs. 
#Pr 5 & pr6 are more visually appealing with respect to seeing all the DPs
#Conclusion: could use pr7 & pr8 for #s and pr5 & pr6 for visual plots

pr8 <- ggaverage(TLD_zinb_model, terms = c("Gender", "DP"), x.as.factor = TRUE)
#pr8MEM <- ggpredict(TLD_zinb_model, terms = c("DP", "Gender"))
summary(pr8)
plot(pr8)
#same as above plot, but note did not use "facet= TRUE" but could
#Actually, pr7 & pr8 are the most useful

#This would give my the AME by data subsets (FL, FS, ML, MS)
#But I can't figure out how to get it to show the other 20 rows!
pr9 <- ggaverage(TLD_zinb_model, terms = c("DP", "Gender", "Variation"), x.as.factor = TRUE)
#pr9MEM <- ggpredict(TLD_zinb_model, terms = c("DP", "Gender", "Variation"))
summary(pr9)
#but it's actually the same as pr5 and pr6, which are technically arranged in right order
#but this one is easier to find the DPs

#This gives me the AME by term--Hooray!
pr10 <- ggaverage(TLD_zinb_model, terms= c("Term", "DP"), x.as.factor = TRUE)
#pr10MEM <- ggpredict(TLD_zinb_model, terms= c("DP", "Term"))
summary(pr10)
#Also this gives me the pretty beaded-curtain plot :-)

pr11 <- ggaverage(TLD_zinb_model, terms = c("DP", "Term"), x.as.factor = TRUE)
#This plot is the red "beads"
plot(pr11)
summary(pr11)

#This is to predict JUST Variation on DPfreqs
pr12 <- ggaverage(TLD_zinb_model, terms = "Variation", x.as.factor = TRUE)
summary(pr12)
plot(pr12)

#This is to predict JUST Gender on DPfreqa
pr13 <- ggaverage(TLD_zinb_model, terms = "Gender", x.as.factor = TRUE)
summary(pr13)
plot(pr13)

#This gives me AMEs for just the terms. So, "piba" as AME of 21.58%, so that's its
#predictor value for creating ANY discourse prosody for every 1 unit increase of "Term"
pr14 <- ggaverage(TLD_zinb_model, terms = "Term")
> plot(pr14)
> summary(pr14)
#VERY USEFUL AND IMPORTANT!!(although plots are unreadable)

#This one gives top 10 DPs (per AME) with top 10 terms (per AME)
pr15 <- ggaverage(TLD_zinb_model, terms = c("DP [LB,O, V, U, LG, QMG, G, B,T, QMB]", "Term [piba,mina, milonga_milonguita_milonguera,china,pebeta, paica, taita, mujer, papusa, percanta]"), x.as.factor = TRUE)
#Remember to run ColorPalette.R to get more colors on plots
plot(pr15, facet = TRUE, color = pal(15))

pr16 <- ggaverage(TLD_zinb_model, terms = "DP", x.as.factor = TRUE)
summary(pr16)
plot(pr16)

#This gives me the AMEs for each term in a set. This one is for FL:
TLDFL <- ggaverage(TLD_zinb_model, terms = c("DP [LB]", "Term [milonga_milonguita_milonguera,mina, pebeta, paica,muneca, piba, china, flor,percanta, garaba, papusa,
bacana, loca, madam, callejera, galleguita, griseta, grela, pipistrela, bataclana]"), x.as.factor = TRUE)
plot(TLDFL, color = rainbow(20)) #colors are what it says
#other colors: plot(TLDFL, color = topo.colors(20)) #colors are blues to greens to yellows

#Here is how to export data from these models to Excel
#That way I get all the rows
library(openxlsx)
write.xlsx(pr1, "c:pr1.xlsx")
write.xlsx(pr2, "c:pr2.xlsx")
write.xlsx(pr3, "c:pr3.xlsx")
write.xlsx(pr4, "c:pr4.xlsx")
write.xlsx(pr5, "c:pr5.xlsx")
write.xlsx(pr6, "c:pr6.xlsx")
write.xlsx(pr7, "c:pr7.xlsx")
write.xlsx(pr8, "c:pr8.xlsx")
write.xlsx(pr9, "c:pr9.xlsx")
write.xlsx(pr10, "c:pr10.xlsx")
write.xlsx(pr11, "c:pr11.xlsx")
write.xlsx(pr12, "c:pr12.xlsx")
write.xlsx(pr13, "c:pr13.xlsx")
write.xlsx(pr14,"c:pr14.xlsx")
write.xlsx(pr15,"c:pr15.xlsx")
write.xlsx(pr16,"c:pr16.xlsx")
write.xlsx(TLDFL,"c:TLDFL.xlsx")

