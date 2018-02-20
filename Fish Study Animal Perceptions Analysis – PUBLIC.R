# ------------------------------------------------------------------------------------------------------------
# ----- SETS WORKING DIRECTORY ------
# ------------------------------------------------------------------------------------------------------------

# set working directory
#Path = "C:\\Users\\steve\\Desktop\\Mercy For Animals\\Fish Welfare Issues Research Study"
Path = "" 
setwd(Path)
getwd()

library(dplyr)
library(reshape2)
library(plotrix)
library(picante)
library(ggplot2)

options(scipen=999)


# ------------------------------------------------------------------------------------------------------------
# ----- IMPORT DATA ------
# ------------------------------------------------------------------------------------------------------------

df = read.csv("fishStudy3Perceptions_cleanData (Public).csv", stringsAsFactors = FALSE,  na.strings = c("","NA"))


# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------- ANALYSIS -------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
colnames(df)

# CALCULATE MEANS FOR INTELLIGENCE 
intelligence = df[,c("intelligentChimps", "intelligentPigs","intelligentChickens","intelligentCows", "intelligentDogs","intelligentFish", "intelligentPlants")] %>% summarise_each(funs(mean))
physicalPain = df[,c("phsicalPainChimps", "phsicalPainPigs","phsicalPainChickens","phsicalPainCows", "phsicalPainDogs","phsicalPainFish", "phsicalPainPlants")] %>% summarise_each(funs(mean))
mentalPain = df[,c("mentalPainChimps", "mentalPainPigs","mentalPainChickens","mentalPainCows", "mentalPainDogs","mentalPainFish", "mentalPainPlants")] %>% summarise_each(funs(mean))
pleasure = df[,c("pleasureChimps", "pleasurePigs","pleasureChickens","pleasureCows", "pleasureDogs","pleasureFish", "pleasurePlants")] %>% summarise_each(funs(mean))

# se's 
intelligence.se = df[,c("intelligentChimps", "intelligentPigs","intelligentChickens","intelligentCows", "intelligentDogs","intelligentFish", "intelligentPlants")] %>% summarise_each(funs(std.error))
physicalPain.se = df[,c("phsicalPainChimps", "phsicalPainPigs","phsicalPainChickens","phsicalPainCows", "phsicalPainDogs","phsicalPainFish", "phsicalPainPlants")] %>% summarise_each(funs(std.error))
mentalPain.se = df[,c("mentalPainChimps", "mentalPainPigs","mentalPainChickens","mentalPainCows", "mentalPainDogs","mentalPainFish", "mentalPainPlants")] %>% summarise_each(funs(std.error))
pleasure.se = df[,c("pleasureChimps", "pleasurePigs","pleasureChickens","pleasureCows", "pleasureDogs","pleasureFish", "pleasurePlants")] %>% summarise_each(funs(std.error))

# change colnames of mean and se df's
colnames = c("chimps", "pigs", "chickens", "cows", "dogs", "fish", "plants")
colnames(intelligence) = colnames
colnames(physicalPain) = colnames
colnames(mentalPain) = colnames
colnames(pleasure) = colnames
colnames(intelligence.se) = colnames
colnames(physicalPain.se) = colnames
colnames(mentalPain.se) = colnames
colnames(pleasure.se) = colnames

# change rownames of mean and se df's
rownames(intelligence) = "intelligence.m"
rownames(physicalPain) = "physicalPain.m"
rownames(mentalPain) = "mentalPain.m"
rownames(pleasure) = "pleasure.m"
rownames(intelligence.se) = "intelligence.se"
rownames(physicalPain.se) = "physicalPain.se"
rownames(mentalPain.se) = "mentalPain.se"
rownames(pleasure.se) = "pleasure.se"

# combine mean df's to one df
perceptionMeans = rbind(intelligence, physicalPain, mentalPain, pleasure, intelligence.se, physicalPain.se, mentalPain.se, pleasure.se)

perceptionMeans[9,] = perceptionMeans[5,]*1.96
perceptionMeans[10,] = perceptionMeans[6,]*1.96
perceptionMeans[11,] = perceptionMeans[7,]*1.96
perceptionMeans[12,] = perceptionMeans[8,]*1.96

rownames = c("intelligence.m","physicalPain.m","mentalPain.m","pleasure.m",
             "intelligence.se","physicalPain.se","mentalPain.se","pleasure.se",
             "intelligence.ci","physicalPain.ci","mentalPain.ci","pleasure.ci")

rownames(perceptionMeans) = rownames

# CORRELATE PERCEPTIONS WITH MEAT CONSUMPTION
# ------------------------------------------------------------------------------------------------------------
colnames(df)
dietCols = c("dietFruitNum", "dietVegNum","dietEggsNum","dietDairyNum","dietAllMeatNum","dietBeefNum","dietPorkNum",
"dietChickenNum","dietTurkeyNum","dietFishNum","dietShellfishNum") 

intelligenceCols = c("intelligentChimps", "intelligentPigs","intelligentChickens","intelligentCows", "intelligentDogs","intelligentFish", "intelligentPlants")
physicalCols  = c("phsicalPainChimps", "phsicalPainPigs","phsicalPainChickens","phsicalPainCows", "phsicalPainDogs","phsicalPainFish", "phsicalPainPlants")
mentalCols = c("mentalPainChimps", "mentalPainPigs","mentalPainChickens","mentalPainCows", "mentalPainDogs","mentalPainFish", "mentalPainPlants")
pleasureCols = c("pleasureChimps", "pleasurePigs","pleasureChickens","pleasureCows", "pleasureDogs","pleasureFish", "pleasurePlants")

df.cor = subset(df, select = c(dietCols, intelligenceCols, physicalCols, 
                                    mentalCols, pleasureCols))

# cor.table.pearson = cor.table(df.cor[complete.cases(df.cor),], cor.method="pearson")
# cor.table.pearson.df = as.data.frame(cor.table.pearson)

cor.table.spearman = cor.table(df.cor[complete.cases(df.cor),], cor.method="spearman")
cor.table.spearman.df = as.data.frame(cor.table.spearman)


# # Holm correction for pearson correlations
# cor.table.pearson.df$P.dietFruitNumHolm = p.adjust(cor.table.pearson.df$P.dietFruitNum, method = "holm", n = 308)
# cor.table.pearson.df$P.dietVegNumHolm = p.adjust(cor.table.pearson.df$P.dietVegNum, method = "holm", n = 308)
# cor.table.pearson.df$P.dietEggsNumHolm = p.adjust(cor.table.pearson.df$P.dietEggsNum, method = "holm", n = 308)
# cor.table.pearson.df$P.dietDairyNumHolm = p.adjust(cor.table.pearson.df$P.dietDairyNum, method = "holm", n = 308)
# cor.table.pearson.df$P.dietAllMeatNumHolm = p.adjust(cor.table.pearson.df$P.dietAllMeatNum, method = "holm", n = 308)
# cor.table.pearson.df$P.dietBeefNumHolm = p.adjust(cor.table.pearson.df$P.dietBeefNum, method = "holm", n = 308)
# cor.table.pearson.df$P.dietPorkNumHolm = p.adjust(cor.table.pearson.df$P.dietPorkNum, method = "holm", n = 308)
# cor.table.pearson.df$P.dietChickenNumHolm = p.adjust(cor.table.pearson.df$P.dietChickenNum, method = "holm", n = 308)
# cor.table.pearson.df$P.dietTurkeyNumHolm = p.adjust(cor.table.pearson.df$P.dietTurkeyNum, method = "holm", n = 308)
# cor.table.pearson.df$P.dietFishNumHolm = p.adjust(cor.table.pearson.df$P.dietFishNum, method = "holm", n = 308)
# cor.table.pearson.df$P.dietShellfishNumHolm = p.adjust(cor.table.pearson.df$P.dietShellfishNum, method = "holm", n = 308)

# Holm correction for spearman correlations
cor.table.spearman.df$P.dietFruitNumHolm = p.adjust(cor.table.spearman.df$P.dietFruitNum, method = "holm", n = 308)
cor.table.spearman.df$P.dietVegNumHolm = p.adjust(cor.table.spearman.df$P.dietVegNum, method = "holm", n = 308)
cor.table.spearman.df$P.dietEggsNumHolm = p.adjust(cor.table.spearman.df$P.dietEggsNum, method = "holm", n = 308)
cor.table.spearman.df$P.dietDairyNumHolm = p.adjust(cor.table.spearman.df$P.dietDairyNum, method = "holm", n = 308)
cor.table.spearman.df$P.dietAllMeatNumHolm = p.adjust(cor.table.spearman.df$P.dietAllMeatNum, method = "holm", n = 308)
cor.table.spearman.df$P.dietBeefNumHolm = p.adjust(cor.table.spearman.df$P.dietBeefNum, method = "holm", n = 308)
cor.table.spearman.df$P.dietPorkNumHolm = p.adjust(cor.table.spearman.df$P.dietPorkNum, method = "holm", n = 308)
cor.table.spearman.df$P.dietChickenNumHolm = p.adjust(cor.table.spearman.df$P.dietChickenNum, method = "holm", n = 308)
cor.table.spearman.df$P.dietTurkeyNumHolm = p.adjust(cor.table.spearman.df$P.dietTurkeyNum, method = "holm", n = 308)
cor.table.spearman.df$P.dietFishNumHolm = p.adjust(cor.table.spearman.df$P.dietFishNum, method = "holm", n = 308)
cor.table.spearman.df$P.dietShellfishNumHolm = p.adjust(cor.table.spearman.df$P.dietShellfishNum, method = "holm", n = 308)

# CLEANING COR TABLE
# ------------------------------------------------------------------------------------------------------------
cor.table.spearman.df.clean = cor.table.spearman.df[-c(1:11),-c(12:40, 52:79)]
cor.table.spearman.df.clean.rOnly = cor.table.spearman.df.clean[,1:11]
cor.table.spearman.df.clean.pHolmOnly = cor.table.spearman.df.clean[,23:33]


# SIGNIFICANCE TESTING WELFARE ISSUES
# ------------------------------------------------------------------------------------------------------------
# intelligence ratings
df.intelligence = subset(df, select = c(intelligentChimps, intelligentPigs, intelligentChickens, 
                                        intelligentCows, intelligentDogs,intelligentFish, 
                                        intelligentPlants))
df.intelligence.long = melt(df.intelligence)

ttest.int = pairwise.t.test(df.intelligence.long$value, df.intelligence.long$variable, p.adjust = "holm", paired=T)
ttest.int.export = as.data.frame(ttest.int$p.value)

# physical pain ratings
df.physical = subset(df, select = c(phsicalPainChimps, phsicalPainPigs, phsicalPainChickens, 
                                        phsicalPainCows, phsicalPainDogs,phsicalPainFish, 
                                        phsicalPainPlants))
df.physical.long = melt(df.physical)

ttest.physical = pairwise.t.test(df.physical.long$value, df.physical.long$variable, p.adjust = "holm", paired=T)
ttest.physical.export = as.data.frame(ttest.physical$p.value)

# mental pain ratings
df.mental = subset(df, select = c(mentalPainChimps, mentalPainPigs, mentalPainChickens, 
                                    mentalPainCows, mentalPainDogs,mentalPainFish, 
                                    mentalPainPlants))
df.mental.long = melt(df.mental)

ttest.mental = pairwise.t.test(df.mental.long$value, df.mental.long$variable, p.adjust = "holm", paired=T)
ttest.mental.export = as.data.frame(ttest.mental$p.value)

# pleasure ratings
df.pleasure = subset(df, select = c(pleasureChimps, pleasurePigs, pleasureChickens, 
                                  pleasureCows, pleasureDogs,pleasureFish, 
                                  pleasurePlants))
df.pleasure.long = melt(df.pleasure)

ttest.pleasure = pairwise.t.test(df.pleasure.long$value, df.pleasure.long$variable, p.adjust = "holm", paired=T)
ttest.pleasure.export = as.data.frame(ttest.pleasure$p.value)


# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------- EXPORT RESULTS -------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------

write.csv(perceptionMeans, "perceptionMeans.csv")

write.csv(ttest.int.export, "ttest.int.export.csv")
write.csv(ttest.physical.export, "ttest.physical.export.csv")
write.csv(ttest.mental.export, "ttest.mental.export.csv")
write.csv(ttest.pleasure.export, "ttest.pleasure.export.csv")

write.csv(cor.table.spearman.df.clean, "diet.cor.table.spearman.all.csv")
write.csv(cor.table.spearman.df.clean.rOnly, "diet.cor.table.spearman.rOnly.csv")
write.csv(cor.table.spearman.df.clean.pHolmOnly, "diet.cor.table.spearman.pHolmOnly.csv")


# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------- CORRELATION PLOT -------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------

# prepare correlations df for plot
# ------------------------------------------------------------------------------------------------------------
corsPlot = cor.table.spearman.df.clean.rOnly
corsPlot$X = rownames(corsPlot)
corsPlot = corsPlot[-c(1,7,8,14,15,21,22,28),]

rowOrder = c("intelligentDogs", "phsicalPainDogs", "mentalPainDogs", "pleasureDogs",
             "intelligentPigs", "phsicalPainPigs", "mentalPainPigs", "pleasurePigs",
             "intelligentCows", "phsicalPainCows", "mentalPainCows", "pleasureCows",
             "intelligentChickens", "phsicalPainChickens", "mentalPainChickens", "pleasureChickens",
             "intelligentFish", "phsicalPainFish", "mentalPainFish", "pleasureFish")

corsPlot = corsPlot[match(rowOrder, corsPlot$X),]

corsPlotMelt = melt(corsPlot)

# prepare p-value df for plot
# ------------------------------------------------------------------------------------------------------------
pvaluePlot = cor.table.spearman.df.clean.pHolmOnly
pvaluePlot$X = rownames(pvaluePlot)
pvaluePlot = pvaluePlot[-c(1,7,8,14,15,21,22,28),]

pvaluePlot = pvaluePlot[match(rowOrder, pvaluePlot$X),]

pvaluePlottMelt = melt(pvaluePlot)

pvaluePlottMelt$star = ""
pvaluePlottMelt$star[pvaluePlottMelt$value<=0.20] = "*"
pvaluePlottMelt$star[pvaluePlottMelt$value<=0.10] = "**"
pvaluePlottMelt$star[pvaluePlottMelt$value<=0.05] = "***"

# plot
# ------------------------------------------------------------------------------------------------------------

animalLabels = c("intelligentPigs" = "Pigs IN",
                 "phsicalPainPigs" = "Pigs PP",
                 "mentalPainPigs" = "Pigs MP",
                 "pleasurePigs" = "Pigs PL",
                 "intelligentChickens" = "Chickens IN",
                 "phsicalPainChickens" = "Chickens PP",
                 "mentalPainChickens" = "Chickens MP",
                 "pleasureChickens" = "Chickens PL",
                 "intelligentCows" = "Cows IN",
                 "phsicalPainCows" = "Cows PP",
                 "mentalPainCows" = "Cows MP",
                 "pleasureCows" = "Cows PL",
                 "intelligentDogs" = "Dogs IN",
                 "phsicalPainDogs" = "Dogs PP",
                 "mentalPainDogs" = "Dogs MP",
                 "pleasureDogs" = "Dogs PL",
                 "intelligentFish" = "Fish IN",
                 "phsicalPainFish" = "Fish PP",
                 "mentalPainFish" = "Fish MP",
                 "pleasureFish" = "Fish PL")

dietLabels = c("r.dietFruitNum" = "Fruit", 
               "r.dietVegNum" = "Veg",
               "r.dietEggsNum" = "Eggs",
               "r.dietDairyNum" = "Dairy",
               "r.dietAllMeatNum" = "All Meat",
               "r.dietBeefNum" = "Beef",
               "r.dietPorkNum" = "Pork",
               "r.dietChickenNum" = "Chicken",
               "r.dietTurkeyNum" = "Turkey",
               "r.dietFishNum" = "Fish",
               "r.dietShellfishNum" = "Shellfish")


caption = expression(paste(italic("* = p < 0.20   ** = p < 0.10   *** = p < 0.05")))
subtitle= expression(paste("Spearman's ", italic("r")))

p = ggplot(corsPlotMelt, aes(x=X, y=variable)) + 
  geom_tile(aes(fill=value)) + xlab("") + ylab("") +
  scale_fill_gradient2(low="darkred", mid= "white", high="steelblue4", name="Legend") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(limits=corsPlot$X, labels=animalLabels) +
  scale_y_discrete(labels=dietLabels) +
  labs(title = "Correlation between diet and perceptions of animal\nintelligence and ability to feel pain and pleasure", 
       subtitle = subtitle,
       caption = caption) +
  geom_text(aes(label=pvaluePlottMelt$star), color="black", size=5)
p

p = p + theme(axis.text = element_text(size = 18, family = "Trebuchet MS"),
              plot.title = element_text(size = 25, face = "bold", family = "Trebuchet MS"),
              plot.subtitle = element_text(size = 18, face="bold", family = "Trebuchet MS"),
              plot.caption = element_text(size = 12, family = "Trebuchet MS"),
              legend.text = element_text(size = 15, family = "Trebuchet MS"),
              legend.title = element_text(size = 15, face="bold",  family = "Trebuchet MS"))
p = p + theme(plot.margin = unit(c(1,2,1,1), "cm"))
p

ggsave(filename='dietAndPerceptionsPlot.png', plot=p, width=30, height=25, dpi=150, units='cm')



