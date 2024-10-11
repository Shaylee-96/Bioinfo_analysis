library(survminer)
library(survival)
library(dplyr)
library(data.table)
library(xlsx)

#Survival analysis based on gene expression

#Input CSV file with normalized gene expression
normData <- read.csv("normalizedData.csv", row.names = 1)

#Excel file with survival info
surData <- readxl::read_xlsx("survivalData.xlsx")

#Merge the two data frame by patient ID
mergeTbl <- merge(normData,surData, by="patient_ID")

#Stratify the patients into high (1) and low (0) subgroups based on 
#median of the gene expression
mergeTbl$MedianStatus[mergeTbl$gene > median(mergeTbl$gene)] <-'1'
mergeTbl$MedianStatus[mergeTbl$gene <= median(mergeTbl$gene)] <-'0'

#Remove the non-recurred patients with death status 
mergeTbl <-mergeTbl %>%filter(!(Recurrence_status==0 & Vital_status==1))

#Overall survival
fit <-surv_fit(Surv(OS_month, OS_status) ~ MedianStatus, data = mergeTbl)
p1<-ggsurvplot(fit, pval = TRUE, conf.int = TRUE,
               risk.table = TRUE, risk.table.col = "strata", xlab='Month', ylab = "Overall Survival (%)",
               linetype = "strata", legend.labs = c("Low expression", "High expression"))


#Time-to-recurrence
fit <-surv_fit(Surv(TTR_month, TTR_status) ~ MedianStatus, data = mergeTbl)
p2<-ggsurvplot(fit, pval = TRUE, conf.int = TRUE,
               risk.table = TRUE, risk.table.col = "strata", xlab='Month', ylab = "Time-to-recurrence survival (%)",
               linetype = "strata", legend.labs = c("Low expression", "High expression"))
