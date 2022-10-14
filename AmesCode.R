# A4: Modeling Assignment 2: Building Linear Regression Models

library(ggplot2)
library(knitr)
library(reshape)
library(imputeTS)
library(corrplot)
library(gridExtra)
library(lessR)
library(rstatix)
library(dplyr)

setwd("C:/Users/aa0843/Documents/MSDS 410/Assignment 7 (Modeling 3)")

mydata <- read.csv(file="ames_housing_data.csv",head=TRUE,sep=",")


# (1) Preparing the Categorical Variables

# EDA & Defining Sample Population
str(mydata)
head(mydata)
names(mydata)
summary(mydata)

# Code property type variables
mydata$SubClass_coded <- recode(mydata$SubClass,
                                "20"="1-STORY 1946 & NEWER ALL STYLES",
                                "30"="1-STORY 1945 & OLDER",
                                "40"="1-STORY W/FINISHED ATTIC ALL AGES",
                                "45"="1-1/2 STORY - UNFINISHED ALL AGES",
                                "50"="1-1/2 STORY FINISHED ALL AGES",
                                "60"="2-STORY 1946 & NEWER",
                                "70"="2-STORY 1945 & OLDER",
                                "75"="2-1/2 STORY ALL AGES",
                                "80"="SPLIT OR MULTI-LEVEL",
                                "85"="SPLIT FOYER",
                                "90"="DUPLEX - ALL STYLES AND AGES",
                                "120"="1-STORY PUD (Planned Unit Development) - 1946 & NEWER",
                                "150"="1-1/2 STORY PUD - ALL AGES",
                                "160"="2-STORY PUD - 1946 & NEWER",
                                "180"="PUD - MULTILEVEL - INCL SPLIT LEV/FOYER",
                                "190"="2 FAMILY CONVERSION - ALL STYLES AND AGES")

mydata$Zoning_coded <- recode(mydata$Zoning,
                              "A (agr)"="Agriculture",
                              "C (all)"="Commercial",
                              "FV"="Floating Village Residential",
                              "I (all)"="Industrial",
                              "RH"="Residential High Density",
                              "RL"="Residential Low Density",
                              "RP"="Residential Low Density Park ",
                              "RM"="Residential Medium Density")


# Plots of different property types
ggplot(mydata, aes(x=HouseStyle, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of Sale Price by Zoning Classification") + 
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  scale_x_discrete(label=function(x) abbreviate(x, minlength=15)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels=scales::dollar_format())

ggplot(mydata, aes(x=HouseStyle)) + 
  geom_bar(color="black") +
  labs(title="Number of Properties by Zoning Classification") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  scale_x_discrete(label=function(x) abbreviate(x, minlength=15)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(mydata, aes(x=GrLivArea, y=SalePrice)) + 
  geom_point(color="blue", shape=16) +
  ggtitle("Scatter Plot of Above Ground SQFT vs Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(mydata, aes(x=GrLivArea)) + 
  labs(title="Number of Properties by Above Ground SQFT") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_histogram(color="black")

ggplot(mydata, aes(x=BldgType, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of Sale Price by Dwelling Type") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels=scales::dollar_format())

ggplot(mydata, aes(x=BldgType)) + 
  geom_bar(color="black") +
  labs(title="Number of Properties by Dwelling Type") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  scale_x_discrete(label=function(x) abbreviate(x, minlength=15)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


# Drop conditions
# Remove non-residential (agriculture, commercial, industrial)
table(mydata$Zoning)
subdat1 <- filter(mydata, Zoning %in% c("FV", "RH", "RL", "RP", "RM"))
table(subdat1$Zoning)
summary(subdat1$Zoning)

# Remove non-single-family homes - will actually keep in
# table(subdat1$BldgType)
# subdat2 <- filter(subdat1, !BldgType %in% c("2fmCon", "Duplex"))
# table(subdat2$BldgType)

# Will keep sq ft above 4000 this time for now (might need to remove as outliers); also keeping non-normal sale conditions

# Waterfall summary of drop conditions
drop_cnt <- c(0, nrow(mydata) - nrow(subdat1))

droptab <- data.frame(drop_conditions=c("Before dropping data", "Non-residential zoning"),
                      drop_cnt = drop_cnt[1:2],
                      cume_drop_cnt = cumsum(drop_cnt),
                      final_row_cnt = c(nrow(mydata), nrow(subdat1)))
kable(droptab)

str(subdat1)

# extract numeric data
nums <- unlist(lapply(subdat1, is.numeric))  
datanum <- subdat1[, nums]

# check for missing data
kable(sapply(datanum, function(x) sum(is.na (x))))

# remove columns with many missing data 
subdat3 <- subdat1[, !(colnames(subdat1) %in% c( "GarageYrBlt","MasVnrArea"))]

# LotFrontage also has a lot of NAs but will leave in and fill with 0s since that seems like why its NA
subdat3["LotFrontage"][is.na(subdat3["LotFrontage"])] <- 0
# impute missing data with means
subdat4 <- na_mean(subdat3)

# double check for missing data including categorical data
kable(sapply(subdat4, function(x) sum(is.na (x))))

# Ordinal variables - replace NAs/blanks with 0 
subdat4["BsmtQual"][is.na(subdat4["BsmtQual"])] <- 0
subdat4["BsmtQual"][subdat4["BsmtQual"] == ""] <- 0
subdat4["BsmtCond"][is.na(subdat4["BsmtCond"])] <- 0
subdat4["FireplaceQu"][is.na(subdat4["FireplaceQu"])] <- 0
subdat4["GarageQual"][is.na(subdat4["GarageQual"])] <- 0
subdat4["GarageCond"][is.na(subdat4["GarageCond"])] <- 0
subdat4["PoolQC"][is.na(subdat4["PoolQC"])] <- 0
subdat4["HeatingQC"][is.na(subdat4["HeatingQC"])] <- 0
subdat4["ExterQual"][is.na(subdat4["ExterQual"])] <- 0
subdat4["ExterCond"][is.na(subdat4["ExterCond"])] <- 0

# Add calculated variables
subdat4$TotalFloorSF <- subdat4$FirstFlrSF + subdat4$SecondFlrSF
subdat4$HouseAge <- subdat4$YrSold - subdat4$YearBuilt
subdat4$QualityIndex <- subdat4$OverallQual * subdat4$OverallCond
subdat4$logSalePrice <- log(subdat4$SalePrice)
subdat4$price_sqft <- subdat4$SalePrice/subdat4$TotalFloorSF
subdat4$TotalBath <- subdat4$FullBath + subdat4$HalfBath + subdat4$BsmtFullBath + subdat4$BsmtHalfBath

str(subdat4)

# overview of outliers
meltData <- melt(subdat4)
ggplot(meltData, aes(factor(variable), value)) + 
  geom_boxplot() + facet_wrap(~variable, scale="free")

# Focus on key subset of variables
outliervar <- subdat4[, (colnames(subdat4) %in% c("OverallQual", "OverallCond","GrLivArea","TotRmsAbvGrd","Fireplaces",
                                                  "GarageCars","GarageArea","SalePrice","HouseAge","QualityIndex",
                                                  "TotalBath"))]
meltData2 <- melt(outliervar)
ggplot(meltData2, aes(factor(variable), value)) + 
  geom_boxplot() + facet_wrap(~variable, scale="free")

# Scatter plots of key variables
plot1 <- ggplot(subdat4, aes(x=OverallQual, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  geom_smooth(method=lm, se=FALSE)
plot2 <- ggplot(subdat4, aes(x=OverallCond, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  geom_smooth(method=lm, se=FALSE)
plot3 <- ggplot(subdat4, aes(x=GrLivArea, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  geom_smooth(method=lm, se=FALSE)
plot4 <- ggplot(subdat4, aes(x=TotRmsAbvGrd, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  geom_smooth(method=lm, se=FALSE)
plot5 <- ggplot(subdat4, aes(x=Fireplaces, y=SalePrice))+ 
  geom_point(color="blue", size=2) +
  geom_smooth(method=lm, se=FALSE)
plot6 <- ggplot(subdat4, aes(x=GarageCars, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  geom_smooth(method=lm, se=FALSE)
plot7 <- ggplot(subdat4, aes(x=GarageArea, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  geom_smooth(method=lm, se=FALSE)
plot8 <- ggplot(subdat4, aes(x=HouseAge, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  geom_smooth(method=lm, se=FALSE)
plot9 <- ggplot(subdat4, aes(x=QualityIndex, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  geom_smooth(method=lm, se=FALSE)
plot10 <- ggplot(subdat4, aes(x=TotalBath, y=SalePrice)) + 
  geom_point(color="blue", size=2) +
  geom_smooth(method=lm, se=FALSE)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10, ncol=3)

# Taking a deeper look into specific extreme outlier values
subdat4[is_extreme(subdat4$OverallQual),"OverallQual"] # none
subdat4[is_extreme(subdat4$OverallCond),"OverallCond"] # probably valid ratings - very old houses
subdat4[is_extreme(subdat4$GrLivArea),"GrLivArea"] # might later remove those above 4000 sq ft
out1 <- subdat4[is_extreme(subdat4$TotRmsAbvGrd),"TotRmsAbvGrd"]
subdat4[is_extreme(subdat4$Fireplaces),"Fireplaces"] # none
subdat4[is_extreme(subdat4$GarageCars),"GarageCars"] # none
out2 <- subdat4[is_extreme(subdat4$GarageArea),"GarageArea"]
subdat4[is_extreme(subdat4$HouseAge),"HouseAge"] # none
subdat4[is_extreme(subdat4$QualityIndex),"QualityIndex"] # probably valid
subdat4[is_extreme(subdat4$TotalBath),"TotalBath"] # probably valid as they're duplex with many bedrooms too
subdat4[is_extreme(subdat4$SalePrice),"SalePrice"] # leave for now - may need transformation

# Adjust extreme outliers down to next highest value
newval1 <- max(subdat4$TotRmsAbvGrd[subdat4$TotRmsAbvGrd < min(out1)])
newval2 <- max(subdat4$GarageArea[subdat4$GarageArea < min(out2)])

subdat4[subdat4$TotRmsAbvGrd %in% out1,"TotRmsAbvGrd"] <- newval1
subdat4[subdat4$GarageArea %in% out2,"GarageArea"] <- newval2

# Remove outliers found after fitting models
subdat4 <- subdat4[-c(182,2162,1488,2163),]

# Boxplot of potential categorical predictors
plot1 <- ggplot(subdat4, aes(x=reorder(Neighborhood, SalePrice, FUN=median), y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  scale_x_discrete(label=function(x) abbreviate(x, minlength=20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels=scales::dollar_format()) +
  xlab("Neighborhood")

plot2 <- ggplot(subdat4, aes(x=reorder(HouseStyle, SalePrice, FUN=median), y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  scale_x_discrete(label=function(x) abbreviate(x, minlength=10)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels=scales::dollar_format()) +
  xlab("House Style")

grid.arrange(plot1, plot2, ncol=2)

plot3 <- ggplot(subdat4, aes(x=reorder(Zoning_coded, SalePrice, FUN=median), y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  scale_x_discrete(label=function(x) abbreviate(x, minlength=10)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels=scales::dollar_format()) +
  xlab("Zoning")

plot4 <- ggplot(subdat4, aes(x=reorder(BldgType, SalePrice, FUN=median), y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels=scales::dollar_format()) +
  xlab("Building Type")

plot5 <- ggplot(subdat4, aes(x=reorder(SaleCondition, SalePrice, FUN=median), y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  scale_x_discrete(label=function(x) abbreviate(x, minlength=20)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels=scales::dollar_format()) +
  xlab("Sale Condition")

grid.arrange(plot3, plot4, plot5, ncol=2)


# Look at ordinal variables sale price distribution (focus on those with high correlation)
p1 <- ggplot(subdat4, aes(x=reorder(ExterQual, SalePrice, FUN = median), y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  scale_x_discrete(label=function(x) abbreviate(x, minlength=15)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels=scales::dollar_format()) +
  xlab("Exterior Quality")

p2 <- ggplot(subdat4, aes(x=reorder(HeatingQC, SalePrice, FUN = median), y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  scale_x_discrete(label=function(x) abbreviate(x, minlength=15)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels=scales::dollar_format()) +
  xlab("Heating Quality")

p3 <- ggplot(subdat4, aes(x=reorder(BsmtQual, SalePrice, FUN = median), y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  scale_x_discrete(label=function(x) abbreviate(x, minlength=15)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels=scales::dollar_format()) +
  xlab("Basement Quality")

p4 <- ggplot(subdat4, aes(x=reorder(KitchenQual, SalePrice, FUN = median), y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  scale_x_discrete(label=function(x) abbreviate(x, minlength=15)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels=scales::dollar_format()) +
  xlab("Kitchen Quality")

grid.arrange(p1, p2, p3, p4, ncol=2)

# Summary stats
neighsum <- do.call(data.frame, aggregate(subdat4$SalePrice, list(subdat4$Neighborhood), 
                                          function(x) c(count = length(x), min = min(x), q25 = quantile(x, .25), 
                                                        mean = mean(x), median = median(x), 
                                                        q75 = quantile(x, .75), max = max(x), sd = sd(x))))
colnames(neighsum) <- c("Category", "count", "min", "q25", "mean", "median", "q75", "max", "sd")
kable(neighsum %>% mutate_if(is.numeric, round) %>% arrange(desc(mean)))

salecondsum <- do.call(data.frame, aggregate(subdat4$SalePrice, list(subdat4$SaleCondition), 
                                             function(x) c(count = length(x), min = min(x), q25 = quantile(x, .25), 
                                                           mean = mean(x), median = median(x), 
                                                           q75 = quantile(x, .75), max = max(x), sd = sd(x))))
colnames(salecondsum) <- c("Category", "count", "min", "q25", "mean", "median", "q75", "max", "sd")
kable(salecondsum %>% mutate_if(is.numeric, round) %>% arrange(desc(mean)))

extsum <- do.call(data.frame, aggregate(subdat4$SalePrice, list(subdat4$ExterQual), 
                                             function(x) c(count = length(x), min = min(x), q25 = quantile(x, .25), 
                                                           mean = mean(x), median = median(x), 
                                                           q75 = quantile(x, .75), max = max(x), sd = sd(x))))
colnames(extsum) <- c("Category", "count", "min", "q25", "mean", "median", "q75", "max", "sd")
kable(extsum %>% mutate_if(is.numeric, round) %>% arrange(desc(mean)))

bsmtsum <- do.call(data.frame, aggregate(subdat4$SalePrice, list(subdat4$BsmtQual), 
                                             function(x) c(count = length(x), min = min(x), q25 = quantile(x, .25), 
                                                           mean = mean(x), median = median(x), 
                                                           q75 = quantile(x, .75), max = max(x), sd = sd(x))))
colnames(bsmtsum) <- c("Category", "count", "min", "q25", "mean", "median", "q75", "max", "sd")
kable(bsmtsum %>% mutate_if(is.numeric, round) %>% arrange(desc(mean)))

kitchsum <- do.call(data.frame, aggregate(subdat4$SalePrice, list(subdat4$KitchenQual), 
                                          function(x) c(count = length(x), min = min(x), q25 = quantile(x, .25), 
                                                        mean = mean(x), median = median(x), 
                                                        q75 = quantile(x, .75), max = max(x), sd = sd(x))))
colnames(kitchsum) <- c("Category", "count", "min", "q25", "mean", "median", "q75", "max", "sd")
kable(kitchsum %>% mutate_if(is.numeric, round) %>% arrange(desc(mean)))


# Combine Neighborhoods
subdat4$neighg <- ifelse(subdat4$Neighborhood %in% c("NoRidge", "StoneBr", "NridgHt", "GrnHill", "Veenker", "Timber", "Somerst"), 1,
                         ifelse(subdat4$Neighborhood %in% c("Crawfor", "ClearCr", "CollgCr", "Blmngtn", "Greens", "Gilbert", "NWAmes"), 2,
                                ifelse(subdat4$Neighborhood %in% c("SawyerW", "Mitchel", "NAmes", "Blueste", "NPkVill", "Sawyer", "Landmrk"), 3,
                                       ifelse(subdat4$Neighborhood %in% c("SWISU", "Edwards", "BrkSide", "OldTown", "IDOTRR", "BrDale", "MeadowV"), 4, 0))))

subdat4$neigh1 <- ifelse(subdat4$neighg == 1,1,0)
subdat4$neigh2 <- ifelse(subdat4$neighg == 2,1,0)
subdat4$neigh3 <- ifelse(subdat4$neighg == 3,1,0)
subdat4$neigh4 <- ifelse(subdat4$neighg == 4,1,0)

neighgsum <- do.call(data.frame, aggregate(subdat4$SalePrice, list(subdat4$neighg), 
                                           function(x) c(count = length(x), min = min(x), q25 = quantile(x, .25), 
                                                         mean = mean(x), median = median(x), 
                                                         q75 = quantile(x, .75), max = max(x), sd = sd(x))))
colnames(neighgsum) <- c("Category", "count", "min", "q25", "mean", "median", "q75", "max", "sd")
kable(neighgsum %>% mutate_if(is.numeric, round) %>% arrange(desc(mean)))

modelneigh <- lm(logSalePrice ~ neigh1 + neigh2 + neigh3, subdat4)
summary(modelneigh)


# rough regression of categ vars
summary(lm(logSalePrice ~ Neighborhood, subdat4)) # strong 58% R^2 
summary(lm(logSalePrice ~ HouseStyle, subdat4)) # not that strong but let's try combining 1 story vs 2 story
summary(lm(logSalePrice ~ Zoning_coded, subdat4)) # 15% R^2
summary(lm(logSalePrice ~ BldgType, subdat4)) 
summary(lm(logSalePrice ~ SaleCondition, subdat4)) # 13% R^2
summary(lm(logSalePrice ~ KitchenQual, subdat4)) # strong 45% R^2
summary(lm(logSalePrice ~ ExterQual, subdat4)) # strong 47% R^2
summary(lm(logSalePrice ~ HeatingQC, subdat4)) # 25%
summary(lm(logSalePrice ~ BsmtQual, subdat4)) # strong 49%

# Code HouseStyle into 1 vs 2 story
subdat4$Story2_dum <- ifelse(subdat4$HouseStyle %in% c("2.5Fin","2.5Unf", "2Story", "SFoyer", "SLvl"), 1, 0)
summary(lm(logSalePrice ~ Story2_dum, subdat4)) 
# only slightly better. will leave out

subdat4$ExterEx_dum <- ifelse(subdat4$ExterQual == "Ex", 1, 0) # R^2 is only at 14% with just Excellent
subdat4$ExterGd_dum <- ifelse(subdat4$ExterQual == "Gd", 1, 0)
summary(lm(logSalePrice ~ ExterEx_dum + ExterGd_dum, subdat4)) # strong 47% R^2

subdat4$HeatEx_dum <- ifelse(subdat4$HeatingQC == "Ex", 1, 0) # R^2 is at 23% with just Excellent; only 1% bump if add Gd
summary(lm(logSalePrice ~ HeatEx_dum, subdat4)) # will drop since not as strong as others

subdat4$BsmtEx_dum <- ifelse(subdat4$BsmtQual == "Ex", 1, 0) # R^2 is only at 25% with just Excellent
subdat4$BsmtGd_dum <- ifelse(subdat4$BsmtQual == "Gd", 1, 0)
summary(lm(logSalePrice ~ BsmtEx_dum + BsmtGd_dum, subdat4)) # strong 47% R^2

# Exterior Quality
subdat4$ExterQualgrp <- ifelse(subdat4$ExterQual == "Ex", "Ex", 
                              ifelse(subdat4$ExterQual == "Gd", "Gd", "Low"))

Extergsum <- do.call(data.frame, aggregate(subdat4$SalePrice, list(subdat4$ExterQualgrp), 
                                          function(x) c(count = length(x), min = min(x), q25 = quantile(x, .25), 
                                                        mean = mean(x), median = median(x), 
                                                        q75 = quantile(x, .75), max = max(x), sd = sd(x))))
colnames(Extergsum) <- c("Category", "count", "min", "q25", "mean", "median", "q75", "max", "sd")
kable(Extergsum %>% mutate_if(is.numeric, round) %>% arrange(desc(mean)))


# Basement Quality
subdat4$BsmtQualgrp <- ifelse(subdat4$BsmtQual == "Ex", "Ex", 
                         ifelse(subdat4$BsmtQual == "Gd", "Gd", "Low"))

Bsmtgsum <- do.call(data.frame, aggregate(subdat4$SalePrice, list(subdat4$BsmtQualgrp), 
                                         function(x) c(count = length(x), min = min(x), q25 = quantile(x, .25), 
                                                       mean = mean(x), median = median(x), 
                                                       q75 = quantile(x, .75), max = max(x), sd = sd(x))))
colnames(Bsmtgsum) <- c("Category", "count", "min", "q25", "mean", "median", "q75", "max", "sd")
kable(Bsmtgsum %>% mutate_if(is.numeric, round) %>% arrange(desc(mean)))

# Combine Kitchen QUality
subdat4$Kitgrp <- ifelse(subdat4$KitchenQual == "Ex", "Ex", 
                       ifelse(subdat4$KitchenQual == "Gd", "Gd", 
                              ifelse(subdat4$KitchenQual %in% c("TA", "Fa", "Po"), "Low", 0)))

subdat4$KitEx_dum <- ifelse(subdat4$KitchenQual == "Ex", 1, 0)
subdat4$KitGd_dum <- ifelse(subdat4$KitchenQual == "Gd", 1, 0)
subdat4$KitLw_dum <- ifelse(subdat4$KitchenQual %in% c("TA", "Fa", "Po"), 1, 0)

Kitgsum <- do.call(data.frame, aggregate(subdat4$SalePrice, list(subdat4$Kitgrp), 
                                         function(x) c(count = length(x), min = min(x), q25 = quantile(x, .25), 
                                                       mean = mean(x), median = median(x), 
                                                       q75 = quantile(x, .75), max = max(x), sd = sd(x))))
colnames(Kitgsum) <- c("Category", "count", "min", "q25", "mean", "median", "q75", "max", "sd")
kable(Kitgsum %>% mutate_if(is.numeric, round) %>% arrange(desc(mean)))

modelkit <- lm(logSalePrice ~ KitEx_dum + KitGd_dum, subdat4)
summary(modelkit)



# (2) The Predictive Modeling Framework
# Set the seed on the random number generator so you get the same split every time that you run the code.
set.seed(123)
subdat4$u <- runif(n=dim(subdat4)[1],min=0,max=1);

# Define these two variables for later use;
# subdat4$QualityIndex <- subdat4$OverallQual*subdat4$OverallCond; # already in
subdat4$TotalSqftCalc <- subdat4$BsmtFinSF1+subdat4$BsmtFinSF2+subdat4$GrLivArea;

# Create train/test split;
train.df <- subset(subdat4, u<0.70);
test.df  <- subset(subdat4, u>=0.70);

# Check your data split. The sum of the parts should equal the whole.
# Do your totals add up?
totlen <- dim(subdat4)[1]
trainlen <- dim(train.df)[1]
testlen <- dim(test.df)[1]
trainlen + testlen

# Summary table of counts in each data set
split <- data.frame(rbind(totlen, trainlen, testlen))
split <- cbind(split, c(totlen/totlen*100, trainlen/totlen*100, testlen/totlen*100))
colnames(split) <- c("# Observations", "% Observations")
rownames(split) <- c("Total", "Training", "Test")
kable(split %>% mutate_if(is.numeric, round, 3))



# (3) Model Identification by Automated Variable Selection
# Focus on numeric data
dat4num <- subdat4[, !sapply(subdat4, is.character)]
str(dat4num)      

# check collinearity between area variables
areas<- subdat4[,c("TotalSqftCalc","GrLivArea","FirstFlrSF","SecondFlrSF","LotArea","LotFrontage","GarageArea","BsmtFinSF1","BsmtFinSF2","TotalBsmtSF","SalePrice")]
mcor <- cor(areas)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black",tl.cex=0.8, addCoef.col="black", number.cex=0.8)

# trim down num variables and check corr again
drop.list <- c('SID','PID','SubClass','OverallQual','OverallCond','YearBuilt', # dropping after first check (part of calc vars)
               'YearRemodel','BsmtFinSF1', 'BsmtFinSF2', 'FirstFlrSF', 'SecondFlrSF', 'GrLivArea',
               'BsmtFullBath', 'BsmtHalfBath', 'FullBath', 'HalfBath', 'MoSold', 'YrSold', 'u', 
               'MiscVal', 'price_sqft', # dropping after 2nd check
               'BsmtCond_code', 'GarageQual_code', 'GarageCond_code', 'PoolQC_code', # dropping after 3rd check
               'ExterCond_code', 'BsmtUnfSF', 'LowQualFinSF', 'KitchenAbvGr', 'ThreeSsnPorch',
               'PoolArea', 'EnclosedPorch', 'neighg', 'TotalBsmtSF', 'TotRmsAbvGrd', 'GarageArea', 
               'FireplaceQu_code', 'TotalFloorSF', "Story2_dum", "HeatEx_dum",
               'neigh4', 'KitLw_dum')# drop baseline of interpretation
dat5num <- dat4num[, !(names(dat4num) %in% drop.list)]
str(dat5num)
mcor <- cor(dat5num)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black",tl.cex=0.8, addCoef.col="black", number.cex=0.7)

# drop num vars from above, non-coded categorical vars, and SalePrice from clean training data
train.clean <-train.df[, !(names(subdat4) %in% drop.list)]
train.clean <- train.clean[, !sapply(train.clean, is.character)]
train.clean <- train.clean[, !(names(train.clean) %in% "SalePrice")]

str(train.clean)

# Check no missing values/NAs
kable(sapply(train.clean, function(x) sum(is.na (x))))


# Define the upper model as the FULL model
upper.lm <- lm(logSalePrice ~ .,data=train.clean);
summary(upper.lm)

e# Define the lower model as the Intercept model
lower.lm <- lm(logSalePrice ~ 1,data=train.clean);
summary(lower.lm)

# Need a SLR to initialize stepwise selection
sqft.lm <- lm(logSalePrice ~ TotalSqftCalc,data=train.clean);
summary(sqft.lm)

# Note: There is only one function for classical model selection in R - stepAIC();
# stepAIC() is part of the MASS library.
# The MASS library comes with the BASE R distribution, but you still need to load it;
library(MASS)

# Call stepAIC() for variable selection

forward.lm <- stepAIC(object=lower.lm,scope=list(upper=upper.lm,lower=lower.lm),
                      direction=c('forward'));
summary(forward.lm)

backward.lm <- stepAIC(object=upper.lm,direction=c('backward'));
summary(backward.lm)

stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1),
                       direction=c('both'));
summary(stepwise.lm)

junk.lm <- lm(logSalePrice ~ OverallQual + OverallCond + QualityIndex + GrLivArea + TotalSqftCalc, data=train.df)
summary(junk.lm)

# check assumptions (not requested in Q3)
par(mfrow=c(2,2))
plot(forward.lm)
plot(backward.lm)
plot(stepwise.lm)
par(mfrow=c(1,1))

library(car)
vif_f <- data.frame(sort(vif(forward.lm),decreasing=TRUE))
colnames(vif_f) <- "VIF"
kable(vif_f)

vif_b <- data.frame(sort(vif(backward.lm),decreasing=TRUE))
colnames(vif_b) <- "VIF"
kable(vif_b)

vif_s <- data.frame(sort(vif(stepwise.lm),decreasing=TRUE))
colnames(vif_s) <- "VIF"
kable(vif_s)

vif_j <- data.frame(sort(vif(junk.lm),decreasing=TRUE))
colnames(vif_j) <- "VIF"
kable(vif_j)

# Summary stats of models
adjrsq <- c(summary(forward.lm)$adj.r.squared, summary(backward.lm)$adj.r.squared, 
            summary(stepwise.lm)$adj.r.squared, summary(junk.lm)$adj.r.squared)
aicval <- c(AIC(forward.lm), AIC(backward.lm), AIC(stepwise.lm), AIC(junk.lm))
bicval <- c(BIC(forward.lm), BIC(backward.lm), BIC(stepwise.lm), BIC(junk.lm))
MSE <- c(anova(forward.lm)['Residuals', 'Mean Sq'], anova(backward.lm)['Residuals', 'Mean Sq'], 
         anova(stepwise.lm)['Residuals', 'Mean Sq'], anova(junk.lm)['Residuals', 'Mean Sq'])
MAE <- c(sum(abs(forward.lm$residuals))/anova(forward.lm)['Residuals', 'Df'],
         sum(abs(backward.lm$residuals))/anova(backward.lm)['Residuals', 'Df'],
         sum(abs(stepwise.lm$residuals))/anova(stepwise.lm)['Residuals', 'Df'],
         sum(abs(junk.lm$residuals))/anova(junk.lm)['Residuals', 'Df'])


models_sum <- data.frame(model=c("Forward", "Backward", "Stepwise", "Junk"),
                         adj_r_squared = round(adjrsq,2), AIC = round(aicval,2), BIC = round(bicval,2),
                         MSE = round(MSE,3), MAE = round(MAE,3))
kable(models_sum)



# (4) Predictive Accuracy
# Predictions on Test Data
forward.test <- predict(forward.lm, newdata=test.df)
backward.test <- predict(backward.lm, newdata=test.df)
stepwise.test <- predict(stepwise.lm, newdata=test.df)
junk.test <- predict(junk.lm, newdata=test.df)

MSE_fwd <- mean((test.df$logSalePrice-forward.test)^2)
MSE_bwd <- mean((test.df$logSalePrice-backward.test)^2)
MSE_stp <- mean((test.df$logSalePrice-stepwise.test)^2)
MSE_jnk <- mean((test.df$logSalePrice-junk.test)^2)

MAE_fwd <- mean(abs(test.df$logSalePrice-forward.test))
MAE_bwd <- mean(abs(test.df$logSalePrice-backward.test))
MAE_stp <- mean(abs(test.df$logSalePrice-stepwise.test))
MAE_jnk <- mean(abs(test.df$logSalePrice-junk.test))

modeltest_sum <- data.frame(model=c("Forward", "Backward", "Stepwise", "Junk"),
                            MSE = round(c(MSE_fwd, MSE_bwd, MSE_stp, MSE_jnk),3), 
                            MAE = round(c(MAE_fwd, MAE_bwd, MAE_stp, MAE_jnk),3))
kable(modeltest_sum)

# MAPE (not asked in Q)
# Training Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train.clean$logSalePrice;
MAPE <- mean(forward.pct)
MAPE
backward.pct <- abs(backward.lm$residuals)/train.clean$logSalePrice;
MAPE <- mean(backward.pct)
MAPE
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean$logSalePrice;
MAPE <- mean(stepwise.pct)
MAPE
junk.pct <- abs(junk.lm$residuals)/train.clean$logSalePrice;
MAPE <- mean(junk.pct)
MAPE


# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.df$logSalePrice-forward.test)/test.df$logSalePrice;
MAPE <- mean(forward.testPCT)
MAPE
backward.testPCT <- abs(test.df$logSalePrice-backward.test)/test.df$logSalePrice;
MAPE <- mean(backward.testPCT)
MAPE
stepwise.testPCT <- abs(test.df$logSalePrice-stepwise.test)/test.df$logSalePrice;
MAPE <- mean(stepwise.testPCT)
MAPE
junk.testPCT <- abs(test.df$logSalePrice-junk.test)/test.df$logSalePrice;
MAPE <- mean(junk.testPCT)
MAPE


# (5) Operational Validation
# Assign Prediction Grades training data;
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0,0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

forward.trainTable <- table(forward.PredictionGrade)
kable(forward.trainTable/sum(forward.trainTable))


junk.PredictionGrade <- ifelse(junk.pct<=0.10,'Grade 1: [0,0.10]',
                                  ifelse(junk.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(junk.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

junk.trainTable <- table(junk.PredictionGrade)
kable(junk.trainTable/sum(junk.trainTable))


# Assign Prediction Grades test data;
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0,0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

forward.testTable <-table(forward.testPredictionGrade)
kable(forward.testTable/sum(forward.testTable))


junk.testPredictionGrade <- ifelse(junk.testPCT<=0.10,'Grade 1: [0,0.10]',
                                      ifelse(junk.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(junk.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

junk.testTable <-table(junk.testPredictionGrade)
kable(junk.testTable/sum(junk.testTable))



# Test if taking the exponent results in different predictions
# Training Data
# for comparison : forward.pct <- abs(forward.lm$residuals)/train.clean$logSalePrice
fwdexp.pct <- abs(train.df$SalePrice - exp(forward.lm$fitted.values))/train.df$SalePrice
fwdexp.PredictionGrade <- ifelse(fwdexp.pct<=0.10,'Grade 1: [0,0.10]',
                                  ifelse(fwdexp.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(fwdexp.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

fwdexp.trainTable <- table(fwdexp.PredictionGrade)
kable(fwdexp.trainTable/sum(fwdexp.trainTable))

junkexp.pct <- abs(train.df$SalePrice - exp(junk.lm$fitted.values))/train.df$SalePrice;
junkexp.PredictionGrade <- ifelse(junkexp.pct<=0.10,'Grade 1: [0,0.10]',
                               ifelse(junkexp.pct<=0.15,'Grade 2: (0.10,0.15]',
                                      ifelse(junkexp.pct<=0.25,'Grade 3: (0.15,0.25]',
                                             'Grade 4: (0.25+]')
                               )					
)

junkexp.trainTable <- table(junkexp.PredictionGrade)
kable(junkexp.trainTable/sum(junkexp.trainTable))



# Test Data
# from earlier: forward.test <- predict(forward.lm, newdata=test.df)
# for comparison: forward.testPCT <- abs(test.df$logSalePrice-forward.test)/test.df$logSalePrice;
fwdexp.testPCT <- abs(test.df$SalePrice - exp(forward.test))/test.df$SalePrice
fwdexp.PredictionGrade <- ifelse(fwdexp.testPCT<=0.10,'Grade 1: [0,0.10]',
                                  ifelse(fwdexp.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(fwdexp.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

fwdexp.testTable <- table(fwdexp.PredictionGrade)
kable(fwdexp.testTable/sum(fwdexp.testTable))


junkexp.testPCT <- abs(test.df$SalePrice - exp(junk.test))/test.df$SalePrice
junkexp.PredictionGrade <- ifelse(junkexp.testPCT<=0.10,'Grade 1: [0,0.10]',
                               ifelse(junkexp.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                      ifelse(junkexp.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                             'junktest.testPCT 4: (0.25+]')
                               )					
)

junkexp.testTable <- table(junkexp.PredictionGrade)
kable(junkexp.testTable/sum(junkexp.testTable))



# (6) Cleaning Up the Final Model
par(mfrow=c(2,2))
plot(forward.lm)
par(mfrow=c(1,1))

# removed influential points and reran everything above

# Check if some variables can be dropped

model1 <- lm(logSalePrice ~ TotalSqftCalc + HouseAge + QualityIndex + 
               GarageCars + neigh1 + neigh2 + BsmtEx_dum + LotArea + BedroomAbvGr + Fireplaces +   
               LotFrontage + OpenPorchSF + ScreenPorch + ExterGd_dum + ExterEx_dum + neigh3 + 
               KitEx_dum + KitGd_dum + BsmtGd_dum, data = train.clean)
summary(model1)

# Need TotalSqftCalc even though small coeff
# Maybe don't need LotFrontage or LotArea, OpenPorchSF or ScreenPorch
# Don't need BsmtGd_dum but leave in excellent dummy? or remove that too
# Maybe take out KitGd_dum and ExterGd_dum but leave in excellent dummy? Can that be done?

model2 <- lm(logSalePrice ~ TotalSqftCalc + HouseAge + QualityIndex + 
               GarageCars + neigh1 + neigh2 + neigh3 + BedroomAbvGr + Fireplaces + 
                KitEx_dum, data = train.clean)
summary(model2)



.8995-.8915 # remove lot area, lot frontage, porch ones
.8915-.8887 # remove basement quality
.8887-.8861 # remove exterior quality
.8861 - .8835 # remove kitchen good

.8915-.8869
.8951-.8915
.897-.8951
.8915 - .8887
.8894- .8872 # good exterior
.8872-.8843 # good kitchen
.8843-.88 # fireplaces - dont remove
.8866-.8835 # lot area

.8995-.8835 # model1-model2 adj r-sq
.9004-.8841 # model1-model2 r-sq
.1364-.1267 # model2-model1 resid std error


# check for interaction

# neighborhood + sqft
Plot(TotalSqftCalc, SalePrice, by=neighg, fit=TRUE, data=subdat4)

train.df$neigh1sqft <- train.df$neigh1 * train.df$TotalSqftCalc
train.df$neigh2sqft <- train.df$neigh2 * train.df$TotalSqftCalc
train.df$neigh3sqft <- train.df$neigh3 * train.df$TotalSqftCalc

modeltest1 <- lm(logSalePrice ~ TotalSqftCalc + HouseAge + QualityIndex + 
                  GarageCars + neigh1 + neigh2 + neigh3 + BedroomAbvGr + Fireplaces + 
                  KitEx_dum + neigh1sqft + neigh2sqft + neigh3sqft, data = train.df)
summary(modeltest1)
anova(modeltest1, model2)

# neighborhood + age
ggplot(subdat4, aes(x=HouseAge, y=SalePrice, colour = factor(neighg))) +
  geom_point(shape=16) +
  labs(color="Neighborhood") +
  #   labs(title="SalePrice vs Neighborhood", color="Neighborhood") +
  #   scale_color_manual(labels = c("No", "Low", "High"), values = c("blue", "orange", "red")) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

Plot(HouseAge, SalePrice, by=neighg, fit=TRUE, data=subdat4)

train.df$neigh1age <- train.df$neigh1 * train.df$HouseAge
train.df$neigh2age <- train.df$neigh2 * train.df$HouseAge
train.df$neigh3age <- train.df$neigh3 * train.df$HouseAge

test.df$neigh1age <- test.df$neigh1 * test.df$HouseAge
test.df$neigh2age <- test.df$neigh2 * test.df$HouseAge
test.df$neigh3age <- test.df$neigh3 * test.df$HouseAge

modeltest2 <- lm(logSalePrice ~ TotalSqftCalc + HouseAge + QualityIndex + 
                  GarageCars + neigh1 + neigh2 + neigh3 + BedroomAbvGr + Fireplaces + 
                  KitEx_dum + neigh1sqft + neigh2sqft + neigh3sqft + 
                  neigh1age + neigh2age + neigh3age, data = train.df)
summary(modeltest2)
anova(modeltest2, modeltest1)

# neighborhood + quality index
Plot(QualityIndex, SalePrice, by=neighg, fit=TRUE, data=subdat4)

train.df$neigh1qi <- train.df$neigh1 * train.df$QualityIndex
train.df$neigh2qi <- train.df$neigh2 * train.df$QualityIndex
train.df$neigh3qi <- train.df$neigh3 * train.df$QualityIndex

test.df$neigh1qi <- test.df$neigh1 * test.df$QualityIndex
test.df$neigh2qi <- test.df$neigh2 * test.df$QualityIndex
test.df$neigh3qi <- test.df$neigh3 * test.df$QualityIndex

modeltest3 <- lm(logSalePrice ~ TotalSqftCalc + HouseAge + QualityIndex + 
                   GarageCars + neigh1 + neigh2 + neigh3 + BedroomAbvGr + Fireplaces + 
                   KitEx_dum + neigh1sqft + neigh2sqft + neigh3sqft + 
                   neigh1age + neigh2age + neigh3age + neigh1qi + neigh2qi + neigh3qi, data = train.df)
summary(modeltest3)
anova(modeltest3, modeltest2)

# neighborhood + garage cars
Plot(GarageCars, SalePrice, by=neighg, fit=TRUE, data=subdat4)

train.df$neigh1cars <- train.df$neigh1 * train.df$GarageCars
train.df$neigh2cars <- train.df$neigh2 * train.df$GarageCars
train.df$neigh3cars <- train.df$neigh3 * train.df$GarageCars

test.df$neigh1cars <- test.df$neigh1 * test.df$GarageCars
test.df$neigh2cars <- test.df$neigh2 * test.df$GarageCars
test.df$neigh3cars <- test.df$neigh3 * test.df$GarageCars

modeltest4 <- lm(logSalePrice ~ TotalSqftCalc + HouseAge + QualityIndex + 
                   GarageCars + neigh1 + neigh2 + neigh3 + BedroomAbvGr + Fireplaces + 
                   KitEx_dum + neigh1sqft + neigh2sqft + neigh3sqft + 
                   neigh1age + neigh2age + neigh3age + 
                   neigh1qi + neigh2qi + neigh3qi +
                   neigh1cars + neigh2cars + neigh3cars, data = train.df)
summary(modeltest4)
anova(modeltest4, modeltest3)

# neighborhood + bedrooms
Plot(BedroomAbvGr, SalePrice, by=neighg, fit=TRUE, data=subdat4)

train.df$neigh1bd <- train.df$neigh1 * train.df$BedroomAbvGr
train.df$neigh2bd <- train.df$neigh2 * train.df$BedroomAbvGr
train.df$neigh3bd <- train.df$neigh3 * train.df$BedroomAbvGr

test.df$neigh1bd <- test.df$neigh1 * test.df$BedroomAbvGr
test.df$neigh2bd <- test.df$neigh2 * test.df$BedroomAbvGr
test.df$neigh3bd <- test.df$neigh3 * test.df$BedroomAbvGr

modeltest5 <- lm(logSalePrice ~ TotalSqftCalc + HouseAge + QualityIndex + 
                   GarageCars + neigh1 + neigh2 + neigh3 + BedroomAbvGr + Fireplaces + 
                   KitEx_dum + neigh1sqft + neigh2sqft + neigh3sqft + 
                   neigh1age + neigh2age + neigh3age + 
                   neigh1qi + neigh2qi + neigh3qi +
                   neigh1cars + neigh2cars + neigh3cars +
                   neigh1bd + neigh2bd + neigh3bd, data = train.df)
summary(modeltest5)
anova(modeltest5, modeltest4)

# neighborhood + fireplaces - not significant
Plot(Fireplaces, SalePrice, by=neighg, fit=TRUE, data=subdat4)

train.df$neigh1fire <- train.df$neigh1 * train.df$Fireplaces
train.df$neigh2fire <- train.df$neigh2 * train.df$Fireplaces
train.df$neigh3fire <- train.df$neigh3 * train.df$Fireplaces

modeltest6 <- lm(logSalePrice ~ TotalSqftCalc + HouseAge + QualityIndex + 
                   GarageCars + neigh1 + neigh2 + neigh3 + BedroomAbvGr + Fireplaces + 
                   KitEx_dum + neigh1sqft + neigh2sqft + neigh3sqft + 
                   neigh1age + neigh2age + neigh3age + 
                   neigh1qi + neigh2qi + neigh3qi +
                   neigh1cars + neigh2cars + neigh3cars +
                   neigh1bd + neigh2bd + neigh3bd +
                   neigh1fire + neigh2fire + neigh3fire, data = train.df)
summary(modeltest6)
anova(modeltest6, modeltest5)


# excellent kitchen + sq ft - not sig
subdat4$Kitgrp2 <- ifelse(subdat4$KitchenQual == "Ex", "Ex", "Non-Ex")

Plot(TotalSqftCalc, SalePrice, by=Kitgrp2, fit=TRUE, data=subdat4)

train.df$kitsqft <- train.df$KitEx_dum * train.df$TotalSqftCalc

modeltest6 <- lm(logSalePrice ~ TotalSqftCalc + HouseAge + QualityIndex + 
                   GarageCars + neigh1 + neigh2 + neigh3 + BedroomAbvGr + Fireplaces + 
                   KitEx_dum + neigh1sqft + neigh2sqft + neigh3sqft + 
                   neigh1age + neigh2age + neigh3age + 
                   neigh1qi + neigh2qi + neigh3qi +
                   neigh1cars + neigh2cars + neigh3cars +
                   neigh1bd + neigh2bd + neigh3bd +
                   kitsqft, data = train.df)
summary(modeltest6)
anova(modeltest6, modeltest5)

# excellent kitchen + age - not sig
Plot(HouseAge, SalePrice, by=Kitgrp2, fit=TRUE, data=subdat4)

train.df$kitage <- train.df$KitEx_dum * train.df$HouseAge

modeltest6 <- lm(logSalePrice ~ TotalSqftCalc + HouseAge + QualityIndex + 
                   GarageCars + neigh1 + neigh2 + neigh3 + BedroomAbvGr + Fireplaces + 
                   KitEx_dum + neigh1sqft + neigh2sqft + neigh3sqft + 
                   neigh1age + neigh2age + neigh3age + 
                   neigh1qi + neigh2qi + neigh3qi +
                   neigh1cars + neigh2cars + neigh3cars +
                   neigh1bd + neigh2bd + neigh3bd +
                   kitage, data = train.df)
summary(modeltest6)
anova(modeltest6, modeltest5)

# excellent kitchen + quality index - not sig
Plot(QualityIndex, SalePrice, by=Kitgrp2, fit=TRUE, data=subdat4)

train.df$kitqi <- train.df$KitEx_dum * train.df$QualityIndex

modeltest6 <- lm(logSalePrice ~ TotalSqftCalc + HouseAge + QualityIndex + 
                   GarageCars + neigh1 + neigh2 + neigh3 + BedroomAbvGr + Fireplaces + 
                   KitEx_dum + neigh1sqft + neigh2sqft + neigh3sqft + 
                   neigh1age + neigh2age + neigh3age + 
                   neigh1qi + neigh2qi + neigh3qi +
                   neigh1cars + neigh2cars + neigh3cars +
                   neigh1bd + neigh2bd + neigh3bd +
                   kitqi, data = train.df)
summary(modeltest6)
anova(modeltest6, modeltest5)

# excellent kitchen + garage cars - not sig
Plot(GarageCars, SalePrice, by=Kitgrp2, fit=TRUE, data=subdat4)

train.df$kitcars <- train.df$KitEx_dum * train.df$GarageCars

modeltest6 <- lm(logSalePrice ~ TotalSqftCalc + HouseAge + QualityIndex + 
                   GarageCars + neigh1 + neigh2 + neigh3 + BedroomAbvGr + Fireplaces + 
                   KitEx_dum + neigh1sqft + neigh2sqft + neigh3sqft + 
                   neigh1age + neigh2age + neigh3age + 
                   neigh1qi + neigh2qi + neigh3qi +
                   neigh1cars + neigh2cars + neigh3cars +
                   neigh1bd + neigh2bd + neigh3bd +
                   kitcars, data = train.df)
summary(modeltest6)
anova(modeltest6, modeltest5)

# excellent kitchen + bedrooms 
Plot(BedroomAbvGr, SalePrice, by=Kitgrp2, fit=TRUE, data=subdat4)

train.df$kitbd <- train.df$KitEx_dum * train.df$BedroomAbvGr

test.df$kitbd <- test.df$KitEx_dum * test.df$BedroomAbvGr

modeltest6 <- lm(logSalePrice ~ TotalSqftCalc + HouseAge + QualityIndex + 
                   GarageCars + neigh1 + neigh2 + neigh3 + BedroomAbvGr + Fireplaces + 
                   KitEx_dum + neigh1sqft + neigh2sqft + neigh3sqft + 
                   neigh1age + neigh2age + neigh3age + 
                   neigh1qi + neigh2qi + neigh3qi +
                   neigh1cars + neigh2cars + neigh3cars +
                   neigh1bd + neigh2bd + neigh3bd +
                   kitbd, data = train.df)
summary(modeltest6)
anova(modeltest6, modeltest5)

# excellent kitchen + fireplaces - not sig
Plot(Fireplaces, SalePrice, by=Kitgrp2, fit=TRUE, data=subdat4)

train.df$kitfire <- train.df$KitEx_dum * train.df$Fireplaces

modeltest7 <- lm(logSalePrice ~ TotalSqftCalc + HouseAge + QualityIndex + 
                   GarageCars + neigh1 + neigh2 + neigh3 + BedroomAbvGr + Fireplaces + 
                   KitEx_dum + neigh1sqft + neigh2sqft + neigh3sqft + 
                   neigh1age + neigh2age + neigh3age + 
                   neigh1qi + neigh2qi + neigh3qi +
                   neigh1cars + neigh2cars + neigh3cars +
                   neigh1bd + neigh2bd + neigh3bd +
                   kitbd +
                   kitfire, data = train.df)
summary(modeltest7)
anova(modeltest7, modeltest6)

# excellent kitchen + neigb - not stat sig
Plot(neighg, SalePrice, by=Kitgrp2, fit=TRUE, data=subdat4)

train.df$kitneigh1 <- train.df$KitEx_dum * train.df$neigh1
train.df$kitneigh2 <- train.df$KitEx_dum * train.df$neigh2
train.df$kitneigh3 <- train.df$KitEx_dum * train.df$neigh3

modeltest7 <- lm(logSalePrice ~ TotalSqftCalc + HouseAge + QualityIndex + 
                   GarageCars + neigh1 + neigh2 + neigh3 + BedroomAbvGr + Fireplaces + 
                   KitEx_dum + neigh1sqft + neigh2sqft + neigh3sqft + 
                   neigh1age + neigh2age + neigh3age + 
                   neigh1qi + neigh2qi + neigh3qi +
                   neigh1cars + neigh2cars + neigh3cars +
                   neigh1bd + neigh2bd + neigh3bd +
                   kitbd +
                   kitfire +
                   kitneigh1 + kitneigh2 + kitneigh3, data = train.df)
summary(modeltest7)
anova(modeltest7, modeltest6)

# move forward with  but clean out not significant for all interactions
summary(modeltest6)
# remove sq ft

modelfin <- lm(logSalePrice ~ TotalSqftCalc + HouseAge + QualityIndex + 
                   GarageCars + neigh1 + neigh2 + neigh3 + BedroomAbvGr + Fireplaces + 
                   KitEx_dum + 
                   neigh1age + neigh2age + neigh3age + 
                   neigh1qi + neigh2qi + neigh3qi +
                   neigh1cars + neigh2cars + neigh3cars +
                   neigh1bd + neigh2bd + neigh3bd +
                   kitbd, data = train.df)

summary(modelfin)


# Validate final model

par(mfrow=c(2,2))
plot(modelfin)
par(mfrow=c(1,1))

vif_m2 <- data.frame(sort(vif(modelfin),decreasing=TRUE))
colnames(vif_m2) <- "VIF"
kable(vif_m2)
# fine to ignore for interaction terms?

# Summary stats of models
adjrsq <- c(summary(forward.lm)$adj.r.squared, summary(backward.lm)$adj.r.squared, 
            summary(stepwise.lm)$adj.r.squared, summary(junk.lm)$adj.r.squared, summary(modelfin)$adj.r.squared)
aicval <- c(AIC(forward.lm), AIC(backward.lm), AIC(stepwise.lm), AIC(junk.lm), AIC(modelfin))
bicval <- c(BIC(forward.lm), BIC(backward.lm), BIC(stepwise.lm), BIC(junk.lm), BIC(modelfin))
MSE <- c(anova(forward.lm)['Residuals', 'Mean Sq'], anova(backward.lm)['Residuals', 'Mean Sq'], 
         anova(stepwise.lm)['Residuals', 'Mean Sq'], anova(junk.lm)['Residuals', 'Mean Sq'],
         anova(modelfin)['Residuals', 'Mean Sq'])
MAE <- c(sum(abs(forward.lm$residuals))/anova(forward.lm)['Residuals', 'Df'],
         sum(abs(backward.lm$residuals))/anova(backward.lm)['Residuals', 'Df'],
         sum(abs(stepwise.lm$residuals))/anova(stepwise.lm)['Residuals', 'Df'],
         sum(abs(junk.lm$residuals))/anova(junk.lm)['Residuals', 'Df'],
         sum(abs(modelfin$residuals))/anova(modelfin)['Residuals', 'Df'])

models_sum <- data.frame(model=c("Forward", "Backward", "Stepwise", "Junk", "Final Model"),
                         adj_r_squared = round(adjrsq,2), AIC = round(aicval,2), BIC = round(bicval,2),
                         MSE = round(MSE,3), MAE = round(MAE,3))
kable(models_sum)



# Predictive Accuracy
# Predictions on Test Data
modelfin.test <- predict(modelfin, newdata=test.df)

MSE_m2 <- mean((test.df$logSalePrice-modelfin.test)^2)

MAE_m2 <- mean(abs(test.df$logSalePrice-modelfin.test))

modeltest_sum <- data.frame(model=c("Forward", "Backward", "Stepwise", "Junk", "Final Model"),
                            MSE = round(c(MSE_fwd, MSE_bwd, MSE_stp, MSE_jnk, MSE_m2),3), 
                            MAE = round(c(MAE_fwd, MAE_bwd, MAE_stp, MAE_jnk, MAE_m2),3))
kable(modeltest_sum)

# MAPE (not asked in Q)
# Training Data
# Abs Pct Error
m2.pct <- abs(modelfin$residuals)/train.clean$logSalePrice;
MAPE <- mean(m2.pct)
MAPE


# Test Data
# Abs Pct Error
m2.testPCT <- abs(test.df$logSalePrice-modelfin.test)/test.df$logSalePrice;
MAPE <- mean(m2.testPCT)
MAPE

# Operational Validation
# Assign Prediction Grades training data;
m2.PredictionGrade <- ifelse(m2.pct<=0.10,'Grade 1: [0,0.10]',
                                  ifelse(m2.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(m2.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

m2.trainTable <- table(m2.PredictionGrade)
kable(m2.trainTable/sum(m2.trainTable))


# Assign Prediction Grades test data;
m2.testPredictionGrade <- ifelse(m2.testPCT<=0.10,'Grade 1: [0,0.10]',
                                      ifelse(m2.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(m2.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

m2.testTable <-table(m2.testPredictionGrade)
kable(m2.testTable/sum(m2.testTable))



# Test if taking the exponent results in different predictions
# Training Data
# for comparison : forward.pct <- abs(forward.lm$residuals)/train.clean$logSalePrice
m2exp.pct <- abs(train.df$SalePrice - exp(modelfin$fitted.values))/train.df$SalePrice
m2exp.PredictionGrade <- ifelse(m2exp.pct<=0.10,'Grade 1: [0,0.10]',
                                 ifelse(m2exp.pct<=0.15,'Grade 2: (0.10,0.15]',
                                        ifelse(m2exp.pct<=0.25,'Grade 3: (0.15,0.25]',
                                               'Grade 4: (0.25+]')
                                 )					
)

m2exp.trainTable <- table(m2exp.PredictionGrade)
kable(m2exp.trainTable/sum(m2exp.trainTable))


# Test Data
# from earlier: forward.test <- predict(forward.lm, newdata=test.df)
# for comparison: forward.testPCT <- abs(test.df$logSalePrice-forward.test)/test.df$logSalePrice;
modelfin.test <- predict(modelfin, newdata=test.df)
m2exp.testPCT <- abs(test.df$SalePrice - exp(modelfin.test))/test.df$SalePrice
m2exp.PredictionGrade <- ifelse(m2exp.testPCT<=0.10,'Grade 1: [0,0.10]',
                                 ifelse(m2exp.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                        ifelse(m2exp.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                               'Grade 4: (0.25+]')
                                 )					
)

m2exp.testTable <- table(m2exp.PredictionGrade)
kable(m2exp.testTable/sum(m2exp.testTable))

mse_train <- mean((train.df$SalePrice - exp(modelfin$fitted.values))^2)
mse_test<- mean((test.df$SalePrice - exp(modelfin.test))^2)
rmse_train <- sqrt(mse_train)
rmse_test <- sqrt(mse_test)
rmse_train
rmse_test

avgprice <- mean(subdat4$SalePrice)
cv <- rmse_test/avgprice
cv

coeff_exp <- exp(modelfin$coefficients)
pct_delta <- exp(modelfin$coefficients) - 1
delta <- (coeff_exp-1)*avgprice
kable(data.frame(pct_of_price = round(pct_delta*100,2), unit_change = round(delta,2)))

      