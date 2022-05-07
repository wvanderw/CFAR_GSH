#00_DataWrangling_210915
##Wade VanderWright & Anthony Iliou

####Load packages####
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(tidyr )
library(car)
library(here)

####Load data####
#GSH and CFAR sheet
setwd(here('2_Data'))

GSHR <- read.csv(here("2_Data", "GSHR.csv"), header = T)

#GSA sheet
GSA <- read.csv()








####Clean the GSH sheet####

GSHRclean <- GSHR %>% select(-Collector_FirstnameLastname, -DateCollected_yymmdd, -TimeTaken_min, -Notes)
GSHRna <- GSHRclean %>% drop_na(GS1_proportionTL)
# line 8 removes unnecessary information columns, creating a new dataframe
#line 10 removes columns with NA values in total length, therefore creating a dataframe
# with only sharks measured in it
GSHm <- GSHRna %>% rowwise() %>% mutate("MeanGSH" = mean(c(GS1_proportionTL, GS2_proportionTL, GS3_proportionTL, GS4_proportionTL, GS5_proportionTL, GS6_proportionTL, GS7_proportionTL), na.rm = TRUE))
# calculate average gill slit height across rows, omitting na values for gs6/7 if present
GSHsd <- GSHm %>% rowwise() %>% mutate("sdGSH" = sd(c(GS1_proportionTL, GS2_proportionTL, GS3_proportionTL, GS4_proportionTL, GS5_proportionTL, GS6_proportionTL, GS7_proportionTL), na.rm = TRUE))
# adds in another row for sd of gill slit height, omitting na values once again 6/7
caudal_fin <- function(height, area) {
  aspect_ratio <- (height^2 / area)
  return(aspect_ratio)
}
# this creates a function called caudal fin that calculated height squared divided by area
# now to apply this to GSHsd function and create GSHall
GSHall <- GSHsd %>%
  rowwise() %>% 
  mutate("CaudalAspect_proportion_TL" = caudal_fin(TailHeight_proportionTL, TailArea_proportionTL))
## NOTE ##
# These values are all in proportion of total length, but is a ratio, so does not contain units
# STOP HERE, KEEP GSHALL UNEDITED

T <- subset(GSHall, !is.na(BodyType))
#Omit the NAs from the new body type column

T$BodyType = as.factor(T$BodyType)
#Convert body type from numeric to categorical for later graphing purposes

Linear <- lm(MeanGSH ~ CaudalAspect_proportion_TL, GSHall)
summary(Linear)
#32% of the change in GSH can be attributed to increasing caudal aspect ratio
A <- T
lm_anova <- lm(CaudalAspect_proportion_TL ~ BodyType, A)
anova
anova_pairs <- emmeans(lm_anova, ~ BodyType)
pairs(anova_pairs, adjust = 'tukey')
#Running tests to see which gill slit height means are different in the body type groups

GSHall <- GSH
A <- GSHall
A <- A %>% 
  rowwise() %>% 
  mutate(log10_MeanGSH = log10(MeanGSH)) %>% 
  mutate(log10_CFAR = log10(CaudalAspect_proportion_TL))

Linear2 <- lm(log10_MeanGSH ~ log10_CFAR, A)
summary(Linear2)
confint(Linear2)


LM2<- lm(log10_MeanGSH ~ log10_CFAR * Order, A)
summary(LM2)

coef(LM2)
confint(LM2)

model_int <- coef(LM2)[1]
model_slope <- coef(LM2)[2]
model_ints <- c(model_int, model_int + coef(LM2)[3:9])
model_slopes <- c(model_slope, model_slope + coef(LM2)[10:16])
model_Coef1 <- cbind(model_ints, model_slopes)
model_LowerCI_slope <- confint(LM2)[2, 1]
model_UpperCI_slope <- confint(LM2)[2, 2]
model_LowerCI_slopes <- c(model_LowerCI_slope, model_LowerCI_slope +  confint(LM2)[10:16, 1])
model_UpperCI_slopes <- c(model_UpperCI_slope, model_UpperCI_slope + confint(LM2)[10:16, 2])
model_Coef1 <- cbind(model_Coef1, model_LowerCI_slopes, model_UpperCI_slopes)
model_LowerCI_intercept <- confint(LM2)[1, 1]
model_UpperCI_intercept <- confint(LM2)[1, 2]
model_LowerCI_intercepts <- c(model_LowerCI_intercept, model_LowerCI_intercept + confint(LM2)[3:9, 1])
model_UpperCI_intercepts <- c(model_UpperCI_intercept, model_UpperCI_intercept + confint(LM2)[3:9, 2])
model_Coef1 <- cbind(model_Coef1, model_LowerCI_intercepts, model_UpperCI_intercepts)
model_Coef1 <- as.data.frame(model_Coef1)

rownames(model_Coef1) <- c(unique(A$Order))

write.csv(model_Coef1, 'LM2.csv', row.names = T)

A %>% ggplot(aes(x = log10_CFAR, y = log10_MeanGSH, color = Order)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) + facet_wrap(~ Order) + xlab("CFAR")
# Graphing linear relationship between CFAR and GSH
Orecto2 <- Orecto %>% filter(Species != "typus")

#Removing the whale shark from the orectolobiformes data set, outlier\


## before doing this, must filter out 6/7 gilled sharks as this only works for 5 gill sharks##

test <- Orecto
test <- test %>% subset(select = -c(TotalLengthReported_cm, ForkLengthInImage_proportionTL, TotalLengthInImage_proportionTL, TailHeight_proportionTL, TailArea_proportionTL, BodyType, MeanGSH, sdGSH, CaudalAspect_proportion_TL))
test$Max <-apply(test[8:12],1,max)
#replace "test" with df that has been filtered for 5 gill sharks
test2 <- test %>% mutate(GS1_ratio = GS1_proportionTL/Max, GS2_ratio = GS2_proportionTL/Max, GS3_ratio = GS3_proportionTL/Max, GS4_ratio = GS4_proportionTL/Max, GS5_ratio = GS5_proportionTL/Max)
#creates new column with standardized values of gill slits to be tested, replace test with df

A$TotalLengthReported_cm = as.numeric(A$TotalLengthReported_cm)
# total length was for some reason a character, change to numeric

B <- A %>% mutate("MeanGSH_cm" = MeanGSH*TotalLengthReported_cm)

Car <- B %>% filter(Order == "Carcharhiniformes")
Hetero <- B %>% filter(Order == "Heterodontiformes")
Lamni <- B %>% filter(Order == "Lamniformes")
Orecto <- B %>% filter(Order == "Orectolobiformes")
Squali <- B %>% filter(Order == "Squaliformes")
lm_Car <- lm(MeanGSH_cm ~ CaudalAspect_proportion_TL, Car)
lm_Hetero <- lm(MeanGSH_cm ~ CaudalAspect_proportion_TL, Hetero)
lm_Lamni <- lm(MeanGSH_cm ~ CaudalAspect_proportion_TL, Lamni)
lm_Orecto <- lm(MeanGSH_cm ~ CaudalAspect_proportion_TL, Orecto)
lm_Squali <- lm(MeanGSH_cm ~ CaudalAspect_proportion_TL, Squali)
#rerun these models properly this time, with mean gsh in centimeters

lm_all <- lm(MeanGSH_cm ~ CaudalAspect_proportion_TL, B)

Type1 <- B %>% filter(BodyType == 1)
Type2 <- B %>% filter(BodyType == 2)
Type3 <- B %>% filter(BodyType == 3)
Type4 <- B %>% filter(BodyType == 4)
lm_Type1 <- lm(MeanGSH_cm ~ CaudalAspect_proportion_TL, Type1)
lm_Type2 <- lm(MeanGSH_cm ~ CaudalAspect_proportion_TL, Type2)
lm_Type3 <- lm(MeanGSH_cm ~ CaudalAspect_proportion_TL, Type3)
lm_Type4 <- lm(MeanGSH_cm ~ CaudalAspect_proportion_TL, Type4)

write.csv(df_Coef, "BodyTypeCoef.csv", row.names = FALSE)
write.csv(df_Coef1, "OrderCoef.csv", row.names = FALSE)