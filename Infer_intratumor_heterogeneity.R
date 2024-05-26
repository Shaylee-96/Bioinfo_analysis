#Infer intra-tumor heterogeneity using Spearman distance (1- Spearman correlation)
#using multi-sectors

library("data.table")
library("dplyr")

#Import the file with normalized expression value
normCnt <- read.csv("Normalised_data.csv")
#Set the gene name as row names
rownames(normCnt) <- normCnt[,1]

#Import the file contains clinical and samples information
metadata <- read.csv("metadata.csv")
#Exclude the adj. normal samples (only keep the tumor samples)
metadata <- metadata[metadata$Sample_type !="Normal",]

#Create a data frame to keep the Spearman distance
pairwise_df <-data.frame(Patient_ID=character(),
                         Pair_lib=character(), #Keep the paired sample ID
                         ITH_score = numeric(),
                         etiology = character()
)
#Create a data frame to keep the patient with only 1 tumor sample
#at least 2 samples are needed to conduct the pairwise calculation
nonPaired_df <-data.frame(Patient_ID=character(),
                          etiology = character()
)

#Get the list of patient IDs
pts <- unique(pts$Patient_ID)

for (p in pts){
  #Retrieve the normalized data of the patient's multi-sector tumor
  lib <- metadata$Sample[metadata$Patient_ID==p]
  tmp <- normCnt[,colnames(normCnt) %in% lib]
  #Compute the pairwise comparison
  if(length(as.data.frame(tmp))>1){
    for (i in 1:length(tmp)){
      for (j in 2:length(tmp)){
        if(j >i){
          #Perform Spearman correlation 
          res <- cor(tmp[i],tmp[j], method="spearman")
          #Append the result to the 'pairwise_df' data frame
          pairwise_df[nrow(pairwise_df) +1,] <- c(p,
                                                  paste(colnames(tmp)[i], colnames(tmp)[j], sep="_"),
                                                  1-res[1],
                                                  "HBV" #The etiology of the patients, 
                                                  #for example, HBV
          )
        }
      }
    }
  }
  #Keep the patient without multi-sector tumor sample
  else if (length(as.data.frame(tmp))==1){
    nonPaired_df[nrow(nonPaired_df) +1,] <- c(p,
                                              "HBV"
    )
  }
}

#Change the data type to numeric
pairwise_df$ITH_score = as.numeric(as.character(pairwise_df$ITH_score))
#Get the mean of the ITH score of all tumors of a patient
perPat <- aggregate(ITH_score ~ Patient_ID, data=pairwise_df, mean)
a <- unique(pairwise_df[,c("Patient_ID", "etiology")])
mergedf <- merge(perPat,a, by="Patient_ID")




