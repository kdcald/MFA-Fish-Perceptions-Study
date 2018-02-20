# ------------------------------------------------------------------------------------------------------------
# ----- SETS WORKING DIRECTORY ------
# ------------------------------------------------------------------------------------------------------------

# set working directory
Path = "" 
setwd(Path)
getwd()

library(dplyr)


# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------- DATA CLEANING AND PROCESSING -------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------
# ----- IMPORT DATA ------
# ------------------------------------------------------------------------------------------------------------

df.raw = read.csv("fishStudy3Perceptions_rawDataPublic.csv", stringsAsFactors = FALSE,  na.strings = c("","NA"))


# ------------------------------------------------------------------------------------------------------------
# ----- SUBSETTING AND RENAMING COLUMNS------
# ------------------------------------------------------------------------------------------------------------
colnames(df.raw)

# remove unwanted columns
df.subs = subset(df.raw, select = -c(
  Collector.ID,
  IP.Address,                                                                                                                                                                                                                                                                                                                                                                                    
  Email.Address,                                                                                                                                                                                                                                                                                                                                                                                 
  First.Name,                                                                                                                                                                                                                                                                                                                                                                                   
  Last.Name,
  Custom.Data
))

# new columns names
colnames(df.subs)
colNamesClean = c("startDate", "endDate", 
                  "intelligentChimps", "phsicalPainChimps", "mentalPainChimps", "pleasureChimps",
                  "intelligentPigs", "phsicalPainPigs", "mentalPainPigs", "pleasurePigs",
                  "intelligentChickens", "phsicalPainChickens", "mentalPainChickens", "pleasureChickens",
                  "intelligentCows", "phsicalPainCows", "mentalPainCows", "pleasureCows",
                  "intelligentDogs", "phsicalPainDogs", "mentalPainDogs", "pleasureDogs",
                  "intelligentFish", "phsicalPainFish", "mentalPainFish", "pleasureFish",
                  "intelligentPlants", "phsicalPainPlants", "mentalPainPlants", "pleasurePlants",
                  "dietFruit", "dietVeg", "dietEggs", "dietDairy", "dietAllMeat", "dietBeef", 
                  "dietPork", "dietChicken", "dietTurkey", "dietFish","dietShellfish", "currentDiet",
                  "currentDietOther", "gender", "age", "ethnicityBlack", "ethnicityAsian", 
                  "ethnicityWhite", "ethnicityIndian", "ethnicityMiddleEast", "ethnicityLatino", 
                  "ethnicityNativeAmerican", "ethnicityPacific","ethnicityNotAnswer", "ethnicityOther", 
                  "education", "state", "income", "religion", "religionOther", "comments")
length(colNamesClean)
                  

# renaming columns
colnames(df.subs) = colNamesClean

# removing first row
df.subs = df.subs[-1,]


# ------------------------------------------------------------------------------------------------------------
# ----- CLEANING VARIABLES ------
# ------------------------------------------------------------------------------------------------------------

# CONVERT PERCETION RATINGS TO NUMERIC
# ------------------------------------------------------------------------------------------------------------
# columns to change to numeric
cols.num <- c("intelligentChimps", "phsicalPainChimps", "mentalPainChimps", "pleasureChimps",
              "intelligentPigs", "phsicalPainPigs", "mentalPainPigs", "pleasurePigs",
              "intelligentChickens", "phsicalPainChickens", "mentalPainChickens", "pleasureChickens",
              "intelligentCows", "phsicalPainCows", "mentalPainCows", "pleasureCows",
              "intelligentDogs", "phsicalPainDogs", "mentalPainDogs", "pleasureDogs",
              "intelligentFish", "phsicalPainFish", "mentalPainFish", "pleasureFish",
              "intelligentPlants", "phsicalPainPlants", "mentalPainPlants", "pleasurePlants")

# function to change from char to num
df.subs[cols.num] <- sapply(df.subs[cols.num],as.numeric)
sapply(df.subs, class)

# DIET
# ------------------------------------------------------------------------------------------------------------
# string not matching characters
# convert to factor and then access it via number

# I am creating factor levels like this because there are some strange characters in the levels caused by SurveyMonkey that are causing problems downstream
dietLevels = c(unique(df.subs$dietAllMeat)[8], #never
                unique(df.subs$dietAllMeat)[6], #A few times a year
                unique(df.subs$dietAllMeat)[2], #about once per month
                unique(df.subs$dietAllMeat)[5], #about once per week
                unique(df.subs$dietAllMeat)[3], #several times per week 
                unique(df.subs$dietAllMeat)[1], #about once per day
                unique(df.subs$dietAllMeat)[4]) #several times per day

df.subs$dietFruit = factor(df.subs$dietFruit, levels = dietLevels)  
df.subs$dietVeg = factor(df.subs$dietVeg, levels = dietLevels)                     
df.subs$dietEggs = factor(df.subs$dietEggs, levels = dietLevels)                     
df.subs$dietDairy = factor(df.subs$dietDairy, levels = dietLevels)                     
df.subs$dietAllMeat = factor(df.subs$dietAllMeat, levels = dietLevels)
df.subs$dietBeef = factor(df.subs$dietBeef, levels = dietLevels)                     
df.subs$dietPork = factor(df.subs$dietPork, levels = dietLevels)                     
df.subs$dietChicken = factor(df.subs$dietChicken, levels = dietLevels)                     
df.subs$dietTurkey = factor(df.subs$dietTurkey, levels = dietLevels)                     
df.subs$dietFish = factor(df.subs$dietFish, levels = dietLevels)                     
df.subs$dietShellfish = factor(df.subs$dietShellfish, levels = dietLevels) 




dietLevelsFactor = c(levels(df.subs$dietAllMeat)[1], #never = 0
                    levels(df.subs$dietAllMeat)[2], #A few times a year = 3/365
                    levels(df.subs$dietAllMeat)[3], #about once per month = (1*12)/365
                    levels(df.subs$dietAllMeat)[4], #about once per week = (1*52)/365
                    levels(df.subs$dietAllMeat)[5], #several times per week = (3*52)/365
                    levels(df.subs$dietAllMeat)[6], #about once per day = 1
                    levels(df.subs$dietAllMeat)[7]) # several times per day = 2.5


# Create numeric diet columns
# fruit 
table(df.subs$dietFruitNum, exclude=NULL)
table(df.subs$dietFruit, exclude=NULL)
df.subs$dietFruitNum[df.subs$dietFruit==dietLevelsFactor[1]] = 0
df.subs$dietFruitNum[df.subs$dietFruit==dietLevelsFactor[2]] = 3/365
df.subs$dietFruitNum[df.subs$dietFruit==dietLevelsFactor[3]] = (1*12)/365
df.subs$dietFruitNum[df.subs$dietFruit==dietLevelsFactor[4]] = (1*52)/365
df.subs$dietFruitNum[df.subs$dietFruit==dietLevelsFactor[5]] = (3*52)/365
df.subs$dietFruitNum[df.subs$dietFruit==dietLevelsFactor[6]] = 1
df.subs$dietFruitNum[df.subs$dietFruit==dietLevelsFactor[7]] = 2.5

# Veg 
table(df.subs$dietVegNum, exclude=NULL)
table(df.subs$dietVeg, exclude=NULL)
df.subs$dietVegNum[df.subs$dietVeg==dietLevelsFactor[1]] = 0
df.subs$dietVegNum[df.subs$dietVeg==dietLevelsFactor[2]] = 3/365
df.subs$dietVegNum[df.subs$dietVeg==dietLevelsFactor[3]] = (1*12)/365
df.subs$dietVegNum[df.subs$dietVeg==dietLevelsFactor[4]] = (1*52)/365
df.subs$dietVegNum[df.subs$dietVeg==dietLevelsFactor[5]] = (3*52)/365
df.subs$dietVegNum[df.subs$dietVeg==dietLevelsFactor[6]] = 1
df.subs$dietVegNum[df.subs$dietVeg==dietLevelsFactor[7]] = 2.5

# Eggs 
table(df.subs$dietEggsNum, exclude=NULL)
table(df.subs$dietEggs, exclude=NULL)
df.subs$dietEggsNum[df.subs$dietEggs==dietLevelsFactor[1]] = 0
df.subs$dietEggsNum[df.subs$dietEggs==dietLevelsFactor[2]] = 3/365
df.subs$dietEggsNum[df.subs$dietEggs==dietLevelsFactor[3]] = (1*12)/365
df.subs$dietEggsNum[df.subs$dietEggs==dietLevelsFactor[4]] = (1*52)/365
df.subs$dietEggsNum[df.subs$dietEggs==dietLevelsFactor[5]] = (3*52)/365
df.subs$dietEggsNum[df.subs$dietEggs==dietLevelsFactor[6]] = 1
df.subs$dietEggsNum[df.subs$dietEggs==dietLevelsFactor[7]] = 2.5

# Dairy 
table(df.subs$dietDairyNum, exclude=NULL)
table(df.subs$dietDairy, exclude=NULL)
df.subs$dietDairyNum[df.subs$dietDairy==dietLevelsFactor[1]] = 0
df.subs$dietDairyNum[df.subs$dietDairy==dietLevelsFactor[2]] = 3/365
df.subs$dietDairyNum[df.subs$dietDairy==dietLevelsFactor[3]] = (1*12)/365
df.subs$dietDairyNum[df.subs$dietDairy==dietLevelsFactor[4]] = (1*52)/365
df.subs$dietDairyNum[df.subs$dietDairy==dietLevelsFactor[5]] = (3*52)/365
df.subs$dietDairyNum[df.subs$dietDairy==dietLevelsFactor[6]] = 1
df.subs$dietDairyNum[df.subs$dietDairy==dietLevelsFactor[7]] = 2.5

# AllMeat 
table(df.subs$dietAllMeatNum, exclude=NULL)
table(df.subs$dietAllMeat, exclude=NULL)
df.subs$dietAllMeatNum[df.subs$dietAllMeat==dietLevelsFactor[1]] = 0
df.subs$dietAllMeatNum[df.subs$dietAllMeat==dietLevelsFactor[2]] = 3/365
df.subs$dietAllMeatNum[df.subs$dietAllMeat==dietLevelsFactor[3]] = (1*12)/365
df.subs$dietAllMeatNum[df.subs$dietAllMeat==dietLevelsFactor[4]] = (1*52)/365
df.subs$dietAllMeatNum[df.subs$dietAllMeat==dietLevelsFactor[5]] = (3*52)/365
df.subs$dietAllMeatNum[df.subs$dietAllMeat==dietLevelsFactor[6]] = 1
df.subs$dietAllMeatNum[df.subs$dietAllMeat==dietLevelsFactor[7]] = 2.5

# Beef 
table(df.subs$dietBeefNum, exclude=NULL)
table(df.subs$dietBeef, exclude=NULL)
df.subs$dietBeefNum[df.subs$dietBeef==dietLevelsFactor[1]] = 0
df.subs$dietBeefNum[df.subs$dietBeef==dietLevelsFactor[2]] = 3/365
df.subs$dietBeefNum[df.subs$dietBeef==dietLevelsFactor[3]] = (1*12)/365
df.subs$dietBeefNum[df.subs$dietBeef==dietLevelsFactor[4]] = (1*52)/365
df.subs$dietBeefNum[df.subs$dietBeef==dietLevelsFactor[5]] = (3*52)/365
df.subs$dietBeefNum[df.subs$dietBeef==dietLevelsFactor[6]] = 1
df.subs$dietBeefNum[df.subs$dietBeef==dietLevelsFactor[7]] = 2.5

# Pork 
table(df.subs$dietPorkNum, exclude=NULL)
table(df.subs$dietPork, exclude=NULL)
df.subs$dietPorkNum[df.subs$dietPork==dietLevelsFactor[1]] = 0
df.subs$dietPorkNum[df.subs$dietPork==dietLevelsFactor[2]] = 3/365
df.subs$dietPorkNum[df.subs$dietPork==dietLevelsFactor[3]] = (1*12)/365
df.subs$dietPorkNum[df.subs$dietPork==dietLevelsFactor[4]] = (1*52)/365
df.subs$dietPorkNum[df.subs$dietPork==dietLevelsFactor[5]] = (3*52)/365
df.subs$dietPorkNum[df.subs$dietPork==dietLevelsFactor[6]] = 1
df.subs$dietPorkNum[df.subs$dietPork==dietLevelsFactor[7]] = 2.5

# Chicken 
table(df.subs$dietChickenNum, exclude=NULL)
table(df.subs$dietChicken, exclude=NULL)
df.subs$dietChickenNum[df.subs$dietChicken==dietLevelsFactor[1]] = 0
df.subs$dietChickenNum[df.subs$dietChicken==dietLevelsFactor[2]] = 3/365
df.subs$dietChickenNum[df.subs$dietChicken==dietLevelsFactor[3]] = (1*12)/365
df.subs$dietChickenNum[df.subs$dietChicken==dietLevelsFactor[4]] = (1*52)/365
df.subs$dietChickenNum[df.subs$dietChicken==dietLevelsFactor[5]] = (3*52)/365
df.subs$dietChickenNum[df.subs$dietChicken==dietLevelsFactor[6]] = 1
df.subs$dietChickenNum[df.subs$dietChicken==dietLevelsFactor[7]] = 2.5

# Turkey 
table(df.subs$dietTurkeyNum, exclude=NULL)
table(df.subs$dietTurkey, exclude=NULL)
df.subs$dietTurkeyNum[df.subs$dietTurkey==dietLevelsFactor[1]] = 0
df.subs$dietTurkeyNum[df.subs$dietTurkey==dietLevelsFactor[2]] = 3/365
df.subs$dietTurkeyNum[df.subs$dietTurkey==dietLevelsFactor[3]] = (1*12)/365
df.subs$dietTurkeyNum[df.subs$dietTurkey==dietLevelsFactor[4]] = (1*52)/365
df.subs$dietTurkeyNum[df.subs$dietTurkey==dietLevelsFactor[5]] = (3*52)/365
df.subs$dietTurkeyNum[df.subs$dietTurkey==dietLevelsFactor[6]] = 1
df.subs$dietTurkeyNum[df.subs$dietTurkey==dietLevelsFactor[7]] = 2.5

# Fish 
table(df.subs$dietFishNum, exclude=NULL)
table(df.subs$dietFish, exclude=NULL)
df.subs$dietFishNum[df.subs$dietFish==dietLevelsFactor[1]] = 0
df.subs$dietFishNum[df.subs$dietFish==dietLevelsFactor[2]] = 3/365
df.subs$dietFishNum[df.subs$dietFish==dietLevelsFactor[3]] = (1*12)/365
df.subs$dietFishNum[df.subs$dietFish==dietLevelsFactor[4]] = (1*52)/365
df.subs$dietFishNum[df.subs$dietFish==dietLevelsFactor[5]] = (3*52)/365
df.subs$dietFishNum[df.subs$dietFish==dietLevelsFactor[6]] = 1
df.subs$dietFishNum[df.subs$dietFish==dietLevelsFactor[7]] = 2.5


# Shellfish 
table(df.subs$dietShellfishNum, exclude=NULL)
table(df.subs$dietShellfish, exclude=NULL)
df.subs$dietShellfishNum[df.subs$dietShellfish==dietLevelsFactor[1]] = 0
df.subs$dietShellfishNum[df.subs$dietShellfish==dietLevelsFactor[2]] = 3/365
df.subs$dietShellfishNum[df.subs$dietShellfish==dietLevelsFactor[3]] = (1*12)/365
df.subs$dietShellfishNum[df.subs$dietShellfish==dietLevelsFactor[4]] = (1*52)/365
df.subs$dietShellfishNum[df.subs$dietShellfish==dietLevelsFactor[5]] = (3*52)/365
df.subs$dietShellfishNum[df.subs$dietShellfish==dietLevelsFactor[6]] = 1
df.subs$dietShellfishNum[df.subs$dietShellfish==dietLevelsFactor[7]] = 2.5


# GENDER
# ------------------------------------------------------------------------------------------------------------
table(df.subs$gender, exclude=NULL)

# AGE
# ------------------------------------------------------------------------------------------------------------
table(df.subs$age, exclude=NULL)
class(df.subs$age)
df.subs$age = as.numeric(df.subs$age)


# EDUCATION
# ------------------------------------------------------------------------------------------------------------
table(df.subs$education, exclude=NULL)
class(df.subs$education)
df.subs$education = as.factor(df.subs$education)
levels(df.subs$education)
df.subs$education = factor(df.subs$education, levels =c("Less than 12th grade, no diploma",
                                                        "High school diploma (or equivalent)",
                                                        "Some education past high school, no degree",
                                                        "Bachelor’s degree",
                                                        "Associate’s degree or other non-Bachelor degree",
                                                        "Graduate or professional degree"))                          

# CURRENT DIET
# ------------------------------------------------------------------------------------------------------------
table(df.subs$currentDiet, exclude=NULL)

df.subs$currentDietShort = NA
df.subs$currentDietShort[df.subs$currentDiet=="Atkins Diet – eat low-carbohydrate, high protein foods"] = "Atkins"
df.subs$currentDietShort[df.subs$currentDiet=="Meat Reduction Diet – A diet attempting to reduce meat consumption, example Meatless Mondays"] = "Meat Reduction"
df.subs$currentDietShort[df.subs$currentDiet=="Mediterranean Diet – A diet mimicking the traditional dietary patterns of southern Italy"] = "Mediterranean"
df.subs$currentDietShort[df.subs$currentDiet=="No specific diet – A diet with no specific preferences or exclusions"] = "No specific diet"
df.subs$currentDietShort[df.subs$currentDiet=="Other (please specify)"] = "Other"
df.subs$currentDietShort[df.subs$currentDiet=="Paleolithic Diet – consists of fish, meats, eggs, vegetables, fruit, fungi, roots, and nuts"] = "Paleolithic"
df.subs$currentDietShort[df.subs$currentDiet=="Pescatarian Diet – eat fish, egg, and milk products, but no other meat (including chicken)"] = "Pescatarian"
df.subs$currentDietShort[df.subs$currentDiet=="Vegan Diet – eat no meat (including fish or chicken), milk products, egg, or other animal products"] = "Vegan"
df.subs$currentDietShort[df.subs$currentDiet=="Vegetarian Diet – eat egg and milk products, but no meat (including fish or chicken)"] = "Vegetarian"
table(df.subs$currentDietShort, exclude=NULL)


# INCOME
# ------------------------------------------------------------------------------------------------------------
table(df.subs$income, exclude=NULL)
df.subs$income = as.factor(df.subs$income)
df.subs$income = factor(df.subs$income, levels =c("Less than $24,999",
                                                        "$25,000 to $49,999",
                                                        "$50,000 to $74,999",
                                                        "$75,000 to $99,999",
                                                        "$100,000 or more",
                                                        "Prefer not to answer"))                          
levels(df.subs$income)


# STATE
# ------------------------------------------------------------------------------------------------------------
table(df.subs$state, exclude = NULL)

# RELIGION
# ------------------------------------------------------------------------------------------------------------
table(df.subs$religion, exclude = NULL)
table(df.subs$religionOther, exclude = NULL)

# ETHNICITY
# ------------------------------------------------------------------------------------------------------------

# TO DO: ETHNICITY COLUMNS HAVE ALL NA'S - NEED TO DISTINGUISH BETWEEN ACTUAL NA'S AND 0'S
# "ethnicityBlack", "ethnicityAsian"         
# "ethnicityWhite", "ethnicityIndian"        
# "ethnicityMiddleEast", "ethnicityLatino"        
# "ethnicityNativeAmerican", "ethnicityPacific"       
# "ethnicityNotAnswer", "ethnicityOther"         


# ------------------------------------------------------------------------------------------------------------
# ----- REMOVING INCOMPLETE CASES ------
# ------------------------------------------------------------------------------------------------------------

df = df.subs[!is.na(df.subs$gender),]

# ------------------------------------------------------------------------------------------------------------
# ----- EXPORT CLEAN DATASET ------
# ------------------------------------------------------------------------------------------------------------

write.csv(df, "fishStudyPerceptions_cleanData.csv", row.names=FALSE)





