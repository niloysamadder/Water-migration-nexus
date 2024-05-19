## Attaching Required Libraries

library (readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(car)
library(Hmisc)
library(stargazer)
library(mice)
library(VIM)
library(cowplot)
library(ggstatsplot)
library(ggpubr)
library(naniar)
library(finalfit)
library(remotes)


## Loading the final dataset for water-migration relationship across the countries

WMNCA <- read_excel("D:\\HSE\\R Projects\\Missing Data Imputation of Panel Data\\Final Dataset_Water-Migration.xlsx",
                    sheet = "Analysis", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric",
                                                      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                      "numeric", "numeric", "numeric", "numeric", "numeric"))

view(WMNCA)

## Loading the final dataset for water-migration relationship across the countries with additional variables for missing data analysis


WMNMDA <- read_excel("D:\\HSE\\R Projects\\Missing Data Imputation of Panel Data\\Final Dataset_Water-Migration.xlsx",
                     sheet = "WMNMDA", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric",
                                                     "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                     "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                     "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                     "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                     "numeric", "numeric", "numeric", "numeric", "numeric"))

view(WMNMDA)

# The following two datasets are same as the above two datsets
# The only difference is that the dependent variables have been downscaled to avoid error due to the large difference in the scale of dependent and independent variables

WMNCA1 <- read_excel("D:\\HSE\\R Projects\\Missing Data Imputation of Panel Data\\Final Dataset_Water-Migration.xlsx",
                     sheet = "Analysis1", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric",
                                                        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                        "numeric", "numeric", "numeric", "numeric"))

view(WMNCA1)

WMNMDA1 <- read_excel("D:\\HSE\\R Projects\\Missing Data Imputation of Panel Data\\Final Dataset_Water-Migration.xlsx",
                      sheet = "WMNMDA1", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric",
                                                       "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                       "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                       "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                       "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                       "numeric", "numeric", "numeric", "numeric", "numeric"))

view(WMNMDA1)

## Missing Data Analysis


# Determining the percentage of missing data in all variables
pMiss <- function(x){sum(is.na(x))/length(x)*100}
pm <- apply(WMNCA,2,pMiss)
apply(WMNCA,1,pMiss)

write.csv(pm, file = "Percentage of missing data_final.csv")

# Identifying the missing data patterns

pattern <- md.pattern(WMNCA)

write.csv(pattern, file = "Pattern of missing data_final.csv")



library(naniar)
gg_miss_var(WMNCA)
res<-summary(aggr(WMNCA, sortVar=TRUE))$combinations


# Little's MCAR Test

install.packages("remotes")
remotes::install_github("njtierney/naniar", force = TRUE)

r <- mcar_test(WMNCA1)
r

write.csv(r, file = "MCAR Test.csv")

r1 <- mcar_test(WMNMDA1)
r1

write.csv(r1, file = "MCAR Test with additional variables.csv")

## Testing whether the Missingness of the dataset is MAR

# Loading the recoded dataset with dummy variables

mar <- read_excel("D:\\HSE\\R Projects\\Missing Data Imputation of Panel Data\\Final Dataset_Water-Migration.xlsx",
                  sheet = "MAR", col_types = c("numeric", "numeric", "numeric", "numeric",
                                               "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                               "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                               "numeric"))
View(mar)

# Creating a dataset for regressing WS_missing

mar_ws <- as.matrix(mar[-c(7, 12:17)])

View(mar_ws)


# A vector of p values for missingnes in WS

p_WS <- rep(NA, ncol(mar_ws))

# Perform Logistic Regression for WS_missing

for(j in 1:ncol(mar_ws)) {
  s_WS <- summary(glm(mar$WS_missing ~ mar_ws[, j]),
                  family = binomial)
  p_WS[j] <- s_WS$coefficients[2,4]
}

p_WS

s_WS


## Logistic regression using finalfit package to check for MAR

# Checking the missingness of Water Stress
dependent_WS = "WS_missing"
explanatory_WS = c("NI", "NE", "IMS_BS", "PD", "GDPPC", "HDI", "WUE", "NRI", "TPASDW", "TAPND")

MAR_WS <- mar %>% 
  finalfit(dependent_WS, explanatory_WS, metrics = TRUE)

MAR_WS

write.csv(MAR_WS, file = "MAR check for Water Stress.csv", row.names = TRUE, col.names = TRUE)


# Checking the missingness of Human Development Index

dependent_HDI = "HDI_missing"
explanatory_HDI = c("NI", "NE", "IMS_BS", "PD", "GDPPC", "WS", "WUE", "NRI", "TPASDW", "TAPND")

MAR_HDI <- mar %>% 
  finalfit(dependent_HDI, explanatory_HDI, metrics = TRUE)

write.csv(MAR_HDI, file = "MAR check for Human Development Index.csv", row.names = TRUE, col.names = TRUE)


# Checking the missingness of Water Use Efficiency

dependent_WUE = "WUE_missing"
explanatory_WUE = c("NI", "NE", "IMS_BS", "PD", "GDPPC", "HDI" ,"WS", "NRI", "TPASDW", "TAPND")

MAR_WUE <- mar %>% 
  finalfit(dependent_WUE, explanatory_WUE, metrics = TRUE)

write.csv(MAR_WUE, file = "MAR check for Water Use Efficiency.csv", row.names = TRUE, col.names = TRUE)

# Checking the missingness of National Rainfall Index

dependent_NRI = "NRI_missing"
explanatory_NRI = c("NI", "NE", "IMS_BS", "PD", "GDPPC", "HDI" ,"WS", "WUE", "TPASDW", "TAPND")

MAR_NRI <- mar %>% 
  finalfit(dependent_NRI, explanatory_NRI, metrics = TRUE)

write.csv(MAR_NRI, file = "MAR check for National Rainfall Index.csv", row.names = TRUE, col.names = TRUE)

# Checking the missingness of Total Population with access to Safe Drinking Water

dependent_TPASDW = "TPASDW_missing"
explanatory_TPASDW = c("NI", "NE", "IMS_BS", "PD", "GDPPC", "HDI" ,"WS", "WUE", "NRI", "TAPND")

MAR_TPASDW <- mar %>% 
  finalfit(dependent_TPASDW, explanatory_TPASDW, metrics = TRUE)

write.csv(MAR_TPASDW, file = "MAR check for Total Population with access to safe Drinking Water.csv", row.names = TRUE, col.names = TRUE)

# Checking the missingness of Average Number of Total Affected People by Natural Disaster

dependent_TAPND = "TAPND_missing"
explanatory_TAPND = c("NI", "NE", "IMS_BS", "PD", "GDPPC", "HDI" ,"WS", "WUE", "NRI", "TPASDW")

MAR_TAPND <- mar %>% 
  finalfit(dependent_TAPND, explanatory_TAPND, metrics = TRUE)

write.csv(MAR_TAPND, file = "MAR check for Average Number of Total Affected People by Natural Disaster.csv", row.names = TRUE, col.names = TRUE)


## Checking whether the missingness is MAR in case of the dataset with additional variables

# loading the dataset

mar1 <- read_excel("D:\\HSE\\R Projects\\Missing Data Imputation of Panel Data\\Final Dataset_Water-Migration.xlsx",
                   sheet = "MAR1", col_types = c("numeric", "numeric", "numeric", "numeric",
                                                 "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                 "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                 "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                 "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                 "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                 "numeric", "numeric", "numeric", "numeric"))
View(mar1)


# Checking the missingness of Human Development Index
dependent_HDI1 = "HDI_missing"
explanatory_HDI1 = c("NI",	"NE",	"IMS_BS",	"PD", "GDPPC", "WS",	"WUE",	"PCAEI",	"NRI",	"TPASDW",	"TAPND",	"RPASDW",	"UPASDW",	"TAC",	"CA",	"TP",	"GDP",	"TRWR",	"EFR",	"TFW",	"AEI",	"AWWTWW",	"IWWTWW",	"MWWTWW",	"IAWUE",	"IWUE",	"SWUE",	"LR",	"PSE",	"SSE",	"TSE",	"LEB")

MAR_HDI1 <- mar1 %>% 
  finalfit(dependent_HDI1, explanatory_HDI1, metrics = TRUE)

write.csv(MAR_HDI1, file = "MAR check for Human Development Index_additional variables.csv", row.names = TRUE, col.names = TRUE)

# Checking the missingness of Total Population with Access to Safe Drinking Water
dependent_TPASDW1 = "TPASDW_missing"
explanatory_TPASDW1 = c("NI",	"NE",	"IMS_BS",	"PD", "GDPPC", "HDI", "WS",	"WUE",	"PCAEI",	"NRI", "TAPND",	"RPASDW",	"UPASDW",	"TAC",	"CA",	"TP",	"GDP",	"TRWR",	"EFR",	"TFW",	"AEI",	"AWWTWW",	"IWWTWW",	"MWWTWW",	"IAWUE",	"IWUE",	"SWUE",	"LR",	"PSE",	"SSE",	"TSE",	"LEB")

MAR_TPASDW1 <- mar1 %>% 
  finalfit(dependent_TPASDW1, explanatory_TPASDW1, metrics = TRUE)

write.csv(MAR_TPASDW1, file = "MAR check for Total Population with access to Safe Drinking water_additional variables.csv", row.names = TRUE, col.names = TRUE)

# Checking the missingness of Water USE Efficiency
dependent_WUE1 = "WUE_missing"
explanatory_WUE1 = c("NI",	"NE",	"IMS_BS",	"PD", "GDPPC", "HDI", "WS",	"PCAEI",	"NRI", "TPASDW" ,"TAPND",	"RPASDW",	"UPASDW",	"TAC",	"CA",	"TP",	"GDP",	"TRWR",	"EFR",	"TFW",	"AEI",	"AWWTWW",	"IWWTWW",	"MWWTWW",	"IAWUE",	"IWUE",	"SWUE",	"LR",	"PSE",	"SSE",	"TSE",	"LEB")

MAR_WUE1 <- mar1 %>% 
  finalfit(dependent_WUE1, explanatory_WUE1, metrics = TRUE)

write.csv(MAR_WUE1, file = "MAR check for Water Use Efficiency_additional variables.csv", row.names = TRUE, col.names = TRUE)

# Checking the missingness of Water Stress
dependent_WS1 = "WS_missing"
explanatory_WS1 = c("NI",	"NE",	"IMS_BS",	"PD", "GDPPC", "HDI", "WUE",	"PCAEI",	"NRI", "TPASDW" ,"TAPND",	"RPASDW",	"UPASDW",	"TAC",	"CA",	"TP",	"GDP",	"TRWR",	"EFR",	"TFW",	"AEI",	"AWWTWW",	"IWWTWW",	"MWWTWW",	"IAWUE",	"IWUE",	"SWUE",	"LR",	"PSE",	"SSE",	"TSE",	"LEB")

MAR_WS1 <- mar1 %>% 
  finalfit(dependent_WS1, explanatory_WS1, metrics = TRUE)

write.csv(MAR_WS1, file = "MAR check for Water Stress_additional variables.csv", row.names = TRUE, col.names = TRUE)

# Checking the missingness of National Rainfall Index
dependent_NRI1 = "NRI_missing"
explanatory_NRI1 = c("NI",	"NE",	"IMS_BS",	"PD", "GDPPC", "HDI", "WS", "WUE",	"PCAEI", "TPASDW" ,"TAPND",	"RPASDW",	"UPASDW",	"TAC",	"CA",	"TP",	"GDP",	"TRWR",	"EFR",	"TFW",	"AEI",	"AWWTWW",	"IWWTWW",	"MWWTWW",	"IAWUE",	"IWUE",	"SWUE",	"LR",	"PSE",	"SSE",	"TSE",	"LEB")

MAR_NRI1 <- mar1 %>% 
  finalfit(dependent_NRI1, explanatory_NRI1, metrics = TRUE)

write.csv(MAR_NRI1, file = "MAR check for National Rainfall Index_additional variables.csv", row.names = TRUE, col.names = TRUE)

# Checking the missingness of Average Number of Total Affected People by Natural Disaster
dependent_TAPND1 = "TAPND_missing"
explanatory_TAPND1 = c("NI",	"NE",	"IMS_BS",	"PD", "GDPPC", "HDI", "WS", "WUE",	"PCAEI", "NRI", "TPASDW" , "RPASDW",	"UPASDW",	"TAC",	"CA",	"TP",	"GDP",	"TRWR",	"EFR",	"TFW",	"AEI",	"AWWTWW",	"IWWTWW",	"MWWTWW",	"IAWUE",	"IWUE",	"SWUE",	"LR",	"PSE",	"SSE",	"TSE",	"LEB")

MAR_TAPND1 <- mar1 %>% 
  finalfit(dependent_TAPND1, explanatory_TAPND1, metrics = TRUE)

write.csv(MAR_TAPND1, file = "MAR check for Average Number of Total Affected People by Natural Disaster_additional variables.csv", row.names = TRUE, col.names = TRUE)

# Checking the missingness of Total Population with Access to Safe Drinking Water
dependent_TPASDW1 = "TPASDW_missing"
explanatory_TPASDW1 = c("NI",	"NE",	"IMS_BS",	"PD", "GDPPC", "HDI", "WS",	"WUE",	"PCAEI",	"NRI", "TAPND",	"RPASDW",	"UPASDW",	"TAC",	"CA",	"TP",	"GDP",	"TRWR",	"EFR",	"TFW",	"AEI",	"AWWTWW",	"IWWTWW",	"MWWTWW",	"IAWUE",	"IWUE",	"SWUE",	"LR",	"PSE",	"SSE",	"TSE",	"LEB")

MAR_TPASDW2 <- mar1 %>% 
  finalfit(dependent_TPASDW1, explanatory_TPASDW1, metrics = TRUE) 
  

write.csv(MAR_TPASDW2, file = "MAR check for Total Population with access to Safe Drinking water 2_additional variables.csv", row.names = TRUE, col.names = TRUE)

## Missing Data Imputation by using Different imputation methods of MICE

# Creating a dataset by excluding the non-numeric columns and TAPND for imputation by using Predictive Mean Matching (pmm) method


# WMNCA_pmm

# 1st Trial
WMNCA_pmmImp1 <- mice(WMNCA1[6:16], m=5, maxit=50, meth="pmm", seed=500)
print(WMNCA_pmmImp1)

# Pooling

model_WMNCA_pmmImp1 <- with(WMNCA_pmmImp1,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool_model_WMNCA_pmmImp1 <- pool(model_WMNCA_pmmImp1)
summary_model_WMNCA_pmmImp1 <- summary(pool(model_WMNCA_pmmImp1))

write.csv(summary_model_WMNCA_pmmImp1, file = "summary_pmm1_WMNCA.csv")


# 2nd Trial

WMNCA_pmmImp2 <- mice(WMNCA1[6:16], m=5, maxit=200, meth="pmm", seed=145435)
print(WMNCA_pmmImp2)

# Pooling

model_WMNCA_pmmImp2 <- with(WMNCA_pmmImp2,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool_model_WMNCA_pmmImp2 <- pool(model_WMNCA_pmmImp2)
summary_model_WMNCA_pmmImp2 <- summary(pool(model_WMNCA_pmmImp2))

write.csv(summary_model_WMNCA_pmmImp2, file = "summary_pmm2_WMNCA.csv")

# 3rd Trial

WMNCA_pmmImp3 <- mice(WMNCA1[6:16], m=5, maxit=400, meth="pmm", seed=245435)
print(WMNCA_pmmImp3)

# Pooling

model_WMNCA_pmmImp3 <- with(WMNCA_pmmImp3,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool_model_WMNCA_pmmImp3 <- pool(model_WMNCA_pmmImp3)
summary_model_WMNCA_pmmImp3 <- summary(pool(model_WMNCA_pmmImp3))

write.csv(summary_model_WMNCA_pmmImp3, file = "summary_pmm3_WMNCA.csv")

pool(with(WMNCA_pmmImp3,lm(IMS_BS~ PD + GDPPC + HDI + WS + WUE + NRI + TPASDW)))

# Imputted Dataset

WMNCA_pmm <- complete(WMNCA_pmmImp1)
write.csv(WMNCA_pmm, file = "Imputted Dataset_pmm.csv")

summary(WMNCA_pmm)

WMNCA_pmm2 <- complete(WMNCA_pmmImp1, 2)
write.csv(WMNCA_pmm2, file = "Imputted Dataset_pmm2.csv")

summary(WMNCA_pmm2)

# Inspecting the distribution of Original and Imputted Dataset for the method pmm

xyplot(WMNCA_pmmImp1, WUE ~ WS + PD + TPASDW + NRI + GDPPC + HDI + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution of Imputted values of WUE with respect to other variables by using PMM Method")
xyplot(WMNCA_pmmImp1, WS ~ WUE + PD + TPASDW + NRI + GDPPC + HDI + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution of Imputted values of WS with respect to other variables by using PMM Method")
xyplot(WMNCA_pmmImp1, HDI ~ WS + WUE + PD + TPASDW + NRI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution Imputted values of HDI with respect to other variables by using PMM Method")
xyplot(WMNCA_pmmImp1, NRI ~ WS + WUE + PD + TPASDW + HDI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution Imputted values of NRI with respect to other variables by using PMM Method")
xyplot(WMNCA_pmmImp1, TPASDW ~ WS + WUE + PD + HDI + NRI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution Imputted values of TPASDW with respect to other variables by using PMM Method")

densityplot(WMNCA_pmmImp1, ~ WS + HDI, main = "Density plot of Imputted and Observed Data by using PMM Method")
stripplot(WMNCA_pmmImp1, HDI + WS ~ .imp, pch = 20, cex = 1.2, main = "Strip plot of Imputted and Observed Data by using PMM Method")

# Missing data Imputation by using Classification and Regression Trees (Cart) method

# WMNCA_cart

# 1st Trial
WMNCA_cartImp1 <- mice(WMNCA[6:16], m=5, maxit=50, meth="cart", seed=500)
print(WMNCA_cartImp1)

# Pooling

model_WMNCA_cartImp1 <- with(WMNCA_cartImp1,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool(model_WMNCA_cartImp1)
summary_model_WMNCA_cartImp1 <- summary(pool(model_WMNCA_cartImp1))

write.csv(summary_model_WMNCA_cartImp1, file = "summary_cart1_WMNCA.csv")


# 2nd Trial

WMNCA_cartImp2 <- mice(WMNCA[6:16], m=5, maxit=200, meth="cart", seed=145435)
print(WMNCA_cartImp2)

# Pooling

model_WMNCA_cartImp2 <- with(WMNCA_cartImp2,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool(model_WMNCA_cartImp2)
summary_model_WMNCA_cartImp2 <- summary(pool(model_WMNCA_cartImp2))

write.csv(summary_model_WMNCA_cartImp2, file = "summary_cart2_WMNCA.csv")

# 3rd Trial

WMNCA_cartImp3 <- mice(WMNCA[6:16], m=5, maxit=400, meth="cart", seed=245435)
print(WMNCA_cartImp3)

# Pooling

model_WMNCA_cartImp3 <- with(WMNCA_cartImp3,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool(model_WMNCA_cartImp3)
summary_model_WMNCA_cartImp3 <- summary(pool(model_WMNCA_cartImp3))

write.csv(summary_model_WMNCA_cartImp3, file = "summary_cart3_WMNCA.csv")

# Imputted Dataset

WMNCA_cart1 <- complete(WMNCA_cartImp1)
write.csv(WMNCA_pmm, file = "Imputted Dataset_cart1.csv")

summary(WMNCA_cart1)

WMNCA_cart2 <- complete(WMNCA_cartImp2, 1)
write.csv(WMNCA_cart2, file = "Imputted Dataset_cart2.csv")

summary(WMNCA_cart2)

WMNCA_cart3 <- complete(WMNCA_cartImp3, 1)
write.csv(WMNCA_cart3, file = "Imputted Dataset_cart3.csv")

summary(WMNCA_cart3)

# Inspecting the distribution of Original and Imputted Dataset for the method cart

xyplot(WMNCA_cartImp2, WUE ~ WS + PD + TPASDW + NRI + GDPPC + HDI + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution of Imputted values of WUE with respect to other variables by using cart Method")
xyplot(WMNCA_cartImp2, WS ~ WUE + PD + TPASDW + NRI + GDPPC + HDI + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution of Imputted values of WS with respect to other variables by using cart Method")
xyplot(WMNCA_cartImp2, HDI ~ WS + WUE + PD + TPASDW + NRI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution Imputted values of HDI with respect to other variables by using cart Method")
xyplot(WMNCA_cartImp2, NRI ~ WS + WUE + PD + TPASDW + HDI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution Imputted values of NRI with respect to other variables by using cart Method")
xyplot(WMNCA_cartImp2, TPASDW ~ WS + WUE + PD + HDI + NRI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution Imputted values of TPASDW with respect to other variables by using cart Method")

densityplot(WMNCA_cartImp2, main = "Density plot of Imputted and Observed Data by using cart Method")
stripplot(WMNCA_cartImp2, pch = 20, cex = 1.2, main = "Strip plot of Imputted and Observed Data by using cart Method")


# Missing data Imputation by using Random Forest method

# WMNCA_rf

# 1st Trial
WMNCA_rfImp1 <- mice(WMNCA[6:16], m=5, maxit=50, meth="rf", seed=500)
print(WMNCA_rfImp1)

# Pooling

model_WMNCA_rfImp1 <- with(WMNCA_rfImp1,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool(model_WMNCA_rfImp1)
summary_model_WMNCA_rfImp1 <- summary(pool(model_WMNCA_rfImp1))

write.csv(summary_model_WMNCA_rfImp1, file = "summary_rf1_WMNCA.csv")


# 2nd Trial

WMNCA_rfImp2 <- mice(WMNCA[6:16], m=5, maxit=200, meth="rf", seed=145435)
print(WMNCA_rfImp2)

# Pooling

model_WMNCA_rfImp2 <- with(WMNCA_rfImp2,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool(model_WMNCA_rfImp2)
summary_model_WMNCA_rfImp2 <- summary(pool(model_WMNCA_rfImp2))

write.csv(summary_model_WMNCA_rfImp2, file = "summary_rf2_WMNCA.csv")


# 3rd Trial

WMNCA_rfImp3 <- mice(WMNCA[6:16], m=5, maxit=400, meth="cart", seed=245435)
print(WMNCA_rfImp3)

# Pooling

model_WMNCA_rfImp3 <- with(WMNCA_rfImp3,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool(model_WMNCA_rfImp3)
summary_model_WMNCA_rfImp3 <- summary(pool(model_WMNCA_rfImp3))

write.csv(summary_model_WMNCA_rfImp3, file = "summary_rf3_WMNCA.csv")

# Imputted Dataset

WMNCA_rf1 <- complete(WMNCA_rfImp1)
write.csv(WMNCA_rf1, file = "Imputted Dataset_rf1.csv")

summary(WMNCA_rf1)

WMNCA_rf2 <- complete(WMNCA_rfImp2)
write.csv(WMNCA_rf2, file = "Imputted Dataset_rf2.csv")

summary(WMNCA_rf2)

WMNCA_rf3 <- complete(WMNCA_rfImp3, 1)
write.csv(WMNCA_rf3, file = "Imputted Dataset_rf3.csv")

summary(WMNCA_rf3)

# Inspecting the distribution of Original and Imputted Dataset for the method cart

xyplot(WMNCA_rfImp1, WUE ~ WS + PD + TPASDW + NRI + GDPPC + HDI + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution of Imputted values of WUE with respect to other variables by using rf Method")
xyplot(WMNCA_rfImp1, WS ~ WUE + PD + TPASDW + NRI + GDPPC + HDI + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution of Imputted values of WS with respect to other variables by using rf Method")
xyplot(WMNCA_rfImp1, HDI ~ WS + WUE + PD + TPASDW + NRI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution Imputted values of HDI with respect to other variables by using rf Method")
xyplot(WMNCA_rfImp1, NRI ~ WS + WUE + PD + TPASDW + HDI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution Imputted values of NRI with respect to other variables by using rf Method")
xyplot(WMNCA_rfImp1, TPASDW ~ WS + WUE + PD + HDI + NRI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution Imputted values of TPASDW with respect to other variables by using rf Method")

densityplot(WMNCA_rfImp1, main = "Density plot of Imputted and Observed Data by using rf Method")
stripplot(WMNCA_rfImp1, pch = 20, cex = 1.2, main = "Strip plot of Imputted and Observed Data by using rf Method")


# Missing data Imputation by using Bayesian Regression method

# WMNCA_norm

# 1st Trial
WMNCA_normImp1 <- mice(WMNCA1[6:16], m=5, maxit=50, meth="norm", seed=500)
print(WMNCA_normImp1)

# Pooling

model_WMNCA_normImp1 <- with(WMNCA_normImp1,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool(model_WMNCA_normImp1)
summary_model_WMNCA_normImp1 <- summary(pool(model_WMNCA_normImp1))

write.csv(summary_model_WMNCA_normImp1, file = "summary_norm1_WMNCA.csv")


# 2nd Trial

WMNCA_normImp2 <- mice(WMNCA1[6:16], m=5, maxit=200, meth="norm", seed=145435)
print(WMNCA_normImp2)

# Pooling

model_WMNCA_normImp2 <- with(WMNCA_normImp2,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool(model_WMNCA_normImp2)
summary_model_WMNCA_normImp2 <- summary(pool(model_WMNCA_normImp2))

write.csv(summary_model_WMNCA_normImp2, file = "summary_norm2_WMNCA.csv")


# 3rd Trial

WMNCA_normImp3 <- mice(WMNCA1[6:16], m=5, maxit=400, meth="norm", seed=245435)
print(WMNCA_normImp3)

# Pooling

model_WMNCA_normImp3 <- with(WMNCA_normImp3,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool(model_WMNCA_normImp3)
summary_model_WMNCA_normImp3 <- summary(pool(model_WMNCA_normImp3))

write.csv(summary_model_WMNCA_normImp3, file = "summary_norm3_WMNCA.csv")

# Imputted Dataset

WMNCA_norm1 <- complete(WMNCA_normImp1)
write.csv(WMNCA_norm1, file = "Imputted Dataset_norm1.csv")

summary(WMNCA_norm1)

WMNCA_norm2 <- complete(WMNCA_normImp2)
write.csv(WMNCA_norm2, file = "Imputted Dataset_norm2.csv")

summary(WMNCA_norm2)

WMNCA_norm3 <- complete(WMNCA_normImp3)
write.csv(WMNCA_norm3, file = "Imputted Dataset_norm3.csv")

summary(WMNCA_norm3)

# Inspecting the distribution of Original and Imputted Dataset for the method norm

xyplot(WMNCA_normImp3, WUE ~ WS + PD + TPASDW + NRI + GDPPC + HDI + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution of Imputted values of WUE with respect to other variables by using norm Method")
xyplot(WMNCA_normImp3, WS ~ WUE + PD + TPASDW + NRI + GDPPC + HDI + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution of Imputted values of WS with respect to other variables by using norm Method")
xyplot(WMNCA_normImp3, HDI ~ WS + WUE + PD + TPASDW + NRI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution Imputted values of HDI with respect to other variables by using norm Method")
xyplot(WMNCA_normImp3, NRI ~ WS + WUE + PD + TPASDW + HDI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution Imputted values of NRI with respect to other variables by using norm Method")
xyplot(WMNCA_normImp3, TPASDW ~ WS + WUE + PD + HDI + NRI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1, main = "Distribution Imputted values of TPASDW with respect to other variables by using norm Method")

densityplot(WMNCA_normImp3, main = "Density plot of Imputted and Observed Data by using norm Method")
stripplot(WMNCA_normImp3, pch = 20, cex = 1.2, main = "Strip plot of Imputted and Observed Data by using norm Method")


## Missing Data Imputation in the dataset with auxiliary variables

# Creating the dataframe excluding the non-numeric columns

WMNMDA_num1 <- WMNMDA1[-c(1:5, 17)]
View(WMNMDA_num1)


# WMNMDA_Predictive Mean Matching

# 1st Trial
WMNMDA_pmmImp1 <- mice(WMNMDA_num1, m=5, maxit=50, meth="pmm", seed=500)
print(WMNMDA_pmmImp1)

# Pooling

model_WMNMDA_pmmImp1 <- with(WMNMDA_pmmImp1,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool(model_WMNMDA_pmmImp1)
summary_model_WMNMDA_pmmImp1 <- summary(pool(model_WMNMDA_pmmImp1))

write.csv(summary_model_WMNMDA_pmmImp1, file = "summary_pmm1_WMNMDA.csv")


# 2nd Trial

WMNDA_pmmImp2 <- mice(WMNMDA_num1, m=5, maxit=200, meth="pmm", seed=145435)
print(WMNDA_pmmImp2)

# Pooling

model_WMNDA_pmmImp2 <- with(WMNDA_pmmImp2,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool_model_WMNDA_pmmImp2 <- pool(model_WMNDA_pmmImp2)
summary_model_WMNDA_pmmImp2 <- summary(pool(model_WMNDA_pmmImp2))

write.csv(summary_model_WMNDA_pmmImp2, file = "summary_pmm2_WMNDA.csv")

# 3rd Trial

WMNDA_pmmImp3 <- mice(WMNMDA_num1, m=5, maxit=400, meth="pmm", seed=245435)
print(WMNDA_pmmImp3)

# Pooling

model_WMNDA_pmmImp3 <- with(WMNDA_pmmImp3,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool_model_WMNDA_pmmImp3 <- pool(model_WMNDA_pmmImp3)
summary_model_WMNDA_pmmImp3 <- summary(pool(model_WMNDA_pmmImp3))

write.csv(summary_model_WMNDA_pmmImp3, file = "summary_pmm3_WMNDA.csv")



# Imputted Dataset

WMNDA_pmm <- complete(WMNDA_pmmImp3)
write.csv(WMNDA_pmm, file = "Imputted Dataset with additional variables_pmm.csv")

summary(WMNDA_pmm)

WMNDA_pmm2 <- complete(WMNDA_pmmImp3, 2)
write.csv(WMNDA_pmm2, file = "Imputted Dataset with additional variables_pmm2.csv")

summary(WMNDA_pmm2)

WMNDA_pmm3 <- complete(WMNDA_pmmImp3, 3)
write.csv(WMNDA_pmm3, file = "Imputted Dataset with additional variables_pmm3.csv")

summary(WMNDA_pmm3)

WMNDA_pmm4 <- complete(WMNDA_pmmImp3, 4)
write.csv(WMNDA_pmm4, file = "Imputted Dataset with additional variables_pmm4.csv")

summary(WMNDA_pmm4)

WMNDA_pmm5 <- complete(WMNDA_pmmImp3, 5)
write.csv(WMNDA_pmm5, file = "Imputted Dataset with additional variables_pmm5.csv")

summary(WMNDA_pmm5)

WMNDA_pmm6 <- complete(WMNDA_pmmImp2)
write.csv(WMNDA_pmm6, file = "Imputted Dataset with additional variables_pmm6.csv")

summary(WMNDA_pmm6)

WMNDA_pmm7 <- complete(WMNMDA_pmmImp1)
write.csv(WMNDA_pmm7, file = "Imputted Dataset with additional variables_pmm7.csv")

summary(WMNDA_pmm7)

# Inspecting the distribution of Original and Imputted Dataset for the method pmm

xyplot(WMNDA_pmmImp3, WUE ~ WS + PD + TPASDW + NRI + GDPPC + HDI + IMS_BS + NI + NE, pch=18, cex=1)
xyplot(WMNDA_pmmImp3, WS ~ WUE + PD + TPASDW + NRI + GDPPC + HDI + IMS_BS + NI + NE, pch=18, cex=1)
xyplot(WMNDA_pmmImp3, HDI ~ WS + WUE + PD + TPASDW + NRI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1)
xyplot(WMNDA_pmmImp3, NRI ~ WS + WUE + PD + TPASDW + HDI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1)
xyplot(WMNDA_pmmImp3, TPASDW ~ WS + WUE + PD + HDI + NRI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1)

densityplot(WMNDA_pmmImp3, ~ WS + NRI + HDI + WUE, pch = 20, cex = 1.2)
stripplot(WMNDA_pmmImp3, WS + TPASDW + NRI + HDI + WUE ~ .imp, pch = 20, cex = 1.2)


# WMNMDA_cart

# 1st Trial
WMNMDA_cartImp1 <- mice(WMNMDA_num, m=5, maxit=50, meth="cart", seed=500)
print(WMNMDA_cartImp1)

# Pooling

model_WMNMDA_cartImp1 <- with(WMNMDA_cartImp1,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool(model_WMNMDA_cartImp1)
summary_model_WMNMDA_cartImp1 <- summary(pool(model_WMNMDA_cartImp1))

write.csv(summary_model_WMNMDA_cartImp1, file = "summary_cart1_WMNMDA.csv")


# 2nd Trial

WMNDA_cartImp2 <- mice(WMNMDA_num, m=5, maxit=200, meth="cart", seed=145435)
print(WMNDA_cartImp2)

# Pooling

model_WMNDA_cartImp2 <- with(WMNDA_cartImp2,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool_model_WMNDA_cartImp2 <- pool(model_WMNDA_cartImp2)
summary_model_WMNDA_cartImp2 <- summary(pool(model_WMNDA_cartImp2))

write.csv(summary_model_WMNDA_cartImp2, file = "summary_cart2_WMNDA.csv")

# 3rd Trial

WMNDA_cartImp3 <- mice(WMNMDA_num, m=5, maxit=400, meth="cart", seed=245435)
print(WMNDA_cartImp3)

# Pooling

model_WMNDA_cartImp3 <- with(WMNDA_cartImp3,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool_model_WMNDA_cartImp3 <- pool(model_WMNDA_cartImp3)
summary_model_WMNDA_cartImp3 <- summary(pool(model_WMNDA_cartImp3))

write.csv(summary_model_WMNDA_cartImp3, file = "summary_cart3_WMNDA.csv")



# Imputted Dataset

WMNDA_cart <- complete(WMNMDA_cartImp1)
write.csv(WMNDA_cart, file = "Imputted Dataset with additional variables_cart.csv")

summary(WMNDA_cart)

WMNDA_cart2 <- complete(WMNMDA_cartImp1, 2)
write.csv(WMNDA_cart2, file = "Imputted Dataset with additional variables_cart2.csv")

summary(WMNDA_cart2)

WMNDA_cart3 <- complete(WMNMDA_cartImp1, 3)
write.csv(WMNDA_cart3, file = "Imputted Dataset with additional variables_cart3.csv")

summary(WMNDA_cart3)

WMNDA_cart4 <- complete(WMNMDA_cartImp1, 4)
write.csv(WMNDA_cart4, file = "Imputted Dataset with additional variables_cart4.csv")

summary(WMNDA_cart4)

WMNDA_cart5 <- complete(WMNMDA_cartImp1, 5)
write.csv(WMNDA_cart5, file = "Imputted Dataset with additional variables_cart5.csv")

summary(WMNDA_cart5)

WMNDA_cart6 <- complete(WMNDA_cartImp2)
write.csv(WMNDA_cart6, file = "Imputted Dataset with additional variables_cart6.csv")

summary(WMNDA_cart6)

WMNDA_cart7 <- complete(WMNDA_cartImp3)
write.csv(WMNDA_cart7, file = "Imputted Dataset with additional variables_cart7.csv")

summary(WMNDA_cart7)

# Inspecting the distribution of Original and Imputted Dataset for the method pmm

xyplot(WMNMDA_cartImp1, WUE ~ WS + PD + TPASDW + NRI + GDPPC + HDI + IMS_BS + NI + NE, pch=18, cex=1)
xyplot(WMNMDA_cartImp1, WS ~ WUE + PD + TPASDW + NRI + GDPPC + HDI + IMS_BS + NI + NE, pch=18, cex=1)
xyplot(WMNMDA_cartImp1, HDI ~ WS + WUE + PD + TPASDW + NRI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1)
xyplot(WMNMDA_cartImp1, NRI ~ WS + WUE + PD + TPASDW + HDI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1)
xyplot(WMNMDA_cartImp1, TPASDW ~ WS + WUE + PD + HDI + NRI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1)

densityplot(WMNMDA_cartImp1, ~ WS + NRI + HDI + WUE, pch = 20, cex = 1.2)
stripplot(WMNMDA_cartImp1, WS + TPASDW + NRI + HDI + WUE ~ .imp, pch = 20, cex = 1.2)


# WMNMDA_rf

# 1st Trial
WMNMDA_rfImp1 <- mice(WMNMDA_num, m=5, maxit=50, meth="rf", seed=500)
print(WMNMDA_rfImp1)

# Pooling

model_WMNMDA_rfImp1 <- with(WMNMDA_rfImp1,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool(model_WMNMDA_rfImp1)
summary_model_WMNMDA_rfImp1 <- summary(pool(model_WMNMDA_rfImp1))

write.csv(summary_model_WMNMDA_rfImp1, file = "summary_rf1_WMNMDA.csv")


# 2nd Trial

WMNDA_rfImp2 <- mice(WMNMDA_num, m=5, maxit=200, meth="rf", seed=145435)
print(WMNDA_rfImp2)

# Pooling

model_WMNDA_rfImp2 <- with(WMNDA_rfImp2,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool_model_WMNDA_rfImp2 <- pool(model_WMNDA_rfImp2)
summary_model_WMNDA_rfImp2 <- summary(pool(model_WMNDA_rfImp2))

write.csv(summary_model_WMNDA_rfImp2, file = "summary_rf2_WMNDA.csv")

# 3rd Trial

WMNDA_rfImp3 <- mice(WMNMDA_num, m=5, maxit=400, meth="rf", seed=245435)
print(WMNDA_rfImp3)

# Pooling

model_WMNDA_rfImp3 <- with(WMNDA_rfImp3,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool_model_WMNDA_rfImp3 <- pool(model_WMNDA_rfImp3)
summary_model_WMNDA_rfImp3 <- summary(pool(model_WMNDA_rfImp3))

write.csv(summary_model_WMNDA_rfImp3, file = "summary_rf3_WMNDA.csv")



# Imputted Dataset

WMNDA_rf <- complete(WMNDA_rfImp2)
write.csv(WMNDA_rf, file = "Imputted Dataset with additional variables_rf.csv")

summary(WMNDA_rf)

WMNDA_rf2 <- complete(WMNDA_rfImp2, 2)
write.csv(WMNDA_rf2, file = "Imputted Dataset with additional variables_rf2.csv")

summary(WMNDA_rf2)

WMNDA_rf3 <- complete(WMNDA_rfImp2, 3)
write.csv(WMNDA_rf3, file = "Imputted Dataset with additional variables_rf3.csv")

summary(WMNDA_rf3)

WMNDA_rf4 <- complete(WMNDA_rfImp2, 1)
write.csv(WMNDA_rf4, file = "Imputted Dataset with additional variables_rf4.csv")

summary(WMNDA_rf4)


# Inspecting the distribution of Original and Imputted Dataset for the method RF

xyplot(WMNDA_rfImp2, WUE ~ WS + PD + TPASDW + NRI + GDPPC + HDI + IMS_BS + NI + NE, pch=18, cex=1)
xyplot(WMNDA_rfImp2, WS ~ WUE + PD + TPASDW + NRI + GDPPC + HDI + IMS_BS + NI + NE, pch=18, cex=1)
xyplot(WMNDA_rfImp2, HDI ~ WS + WUE + PD + TPASDW + NRI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1)
xyplot(WMNDA_rfImp2, NRI ~ WS + WUE + PD + TPASDW + HDI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1)
xyplot(WMNDA_rfImp2, TPASDW ~ WS + WUE + PD + HDI + NRI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1)

densityplot(WMNDA_rfImp2, ~ WS + NRI + HDI + WUE, pch = 20, cex = 1.2)
stripplot(WMNDA_rfImp2, WS + TPASDW + NRI + HDI + WUE ~ .imp, pch = 20, cex = 1.2)

# WMNMDA_norm

# 1st Trial
WMNMDA_normImp1 <- mice(WMNMDA_num1, m=5, maxit=50, meth="norm", seed=500)
print(WMNMDA_normImp1)

# Pooling

model_WMNMDA_normImp1 <- with(WMNMDA_normImp1,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool(model_WMNMDA_normImp1)
summary_model_WMNMDA_normImp1 <- summary(pool(model_WMNMDA_normImp1))

write.csv(summary_model_WMNMDA_normImp1, file = "summary_norm1_WMNMDA.csv")


# 2nd Trial

WMNDA_normImp2 <- mice(WMNMDA_num1, m=5, maxit=200, meth="norm", seed=145435)
print(WMNDA_normImp2)

# Pooling

model_WMNDA_normImp2 <- with(WMNDA_normImp2,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool(model_WMNDA_normImp2)
summary_model_WMNDA_normImp2 <- summary(pool(model_WMNDA_normImp2))

write.csv(summary_model_WMNDA_normImp2, file = "summary_norm2_WMNDA.csv")

# 3rd Trial

WMNDA_normImp3 <- mice(WMNMDA_num1, m=5, maxit=400, meth="norm", seed=245435)
print(WMNDA_normImp3)

# Pooling

model_WMNDA_normImp3 <- with(WMNDA_normImp3,lm(IMS_BS~ HDI + WS + WUE + NRI + TPASDW))
pool(model_WMNDA_normImp3)
summary_model_WMNDA_normImp3 <- summary(pool(model_WMNDA_normImp3))

write.csv(summary_model_WMNDA_normImp3, file = "summary_norm3_WMNDA.csv")


# Imputted Dataset

WMNDA_norm <- complete(WMNMDA_normImp1)
write.csv(WMNDA_norm, file = "Imputted Dataset with additional variables_norm.csv")

summary(WMNDA_norm)



# Inspecting the distribution of Original and Imputted Dataset for the method RF

xyplot(WMNMDA_normImp1, WUE ~ WS + PD + TPASDW + NRI + GDPPC + HDI + IMS_BS + NI + NE, pch=18, cex=1)
xyplot(WMNMDA_normImp1, WS ~ WUE + PD + TPASDW + NRI + GDPPC + HDI + IMS_BS + NI + NE, pch=18, cex=1)
xyplot(WMNMDA_normImp1, HDI ~ WS + WUE + PD + TPASDW + NRI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1)
xyplot(WMNMDA_normImp1, NRI ~ WS + WUE + PD + TPASDW + HDI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1)
xyplot(WMNMDA_normImp1, TPASDW ~ WS + WUE + PD + HDI + NRI + GDPPC + IMS_BS + NI + NE, pch=18, cex=1)

densityplot(WMNMDA_normImp1, ~ WS + NRI + HDI + WUE, pch = 20, cex = 1.2)
stripplot(WMNMDA_normImp1, WS + TPASDW + NRI + HDI + WUE ~ .imp, pch = 20, cex = 1.2)


