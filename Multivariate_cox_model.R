#Multivariate Cox regression model
#To identify the confounding factors of survival analysis

library("data.table")
library("dplyr")
library('survminer')
library('survival')


#Import the file containing the clinical & survival information of the patients
clinicalInfo <- as.data.frame(fread("clinicalInfo.tsv",
                                    sep="\t", header = TRUE))

#Import the file containing the transcriptomics subtypes of the patients
subtype <- read.csv("subtype.csv", header= TRUE)

#Merge the above two data frame (by patient ID)
df <- merge(clinicalInfo, subtype, by="patient_ID")

#Set the factor level (for example: TNM Stage I as baseline)
df_$TNM <- factor(df_merge$TNM, levels=c("I","II", "III"))

#Perform multivariate cox regression
res.cox <-coxph(Surv(OS_days, OS_status) ~ variable1 + variable2 + variable3 , data = df)
summary(res.cox)

#Forest plot
ggforest(res.cox)

