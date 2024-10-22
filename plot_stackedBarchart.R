library('ggplot2')
library('ggpp')

df <-read.csv('celltypeProportions.csv')

#To remove the percentage sign from the proportion column
df$Proportion<-gsub('%','',as.character(df$Proportion))

#To change the class of Proportion column (char to numeric)
df[,"Proportion"]<-sapply(df[,"Proportion"], as.numeric)

#Calculate the midpoint of the bars

ggplot(df, aes(fill=df$Cell_type , y=df$Proportion, x=df$Tumour_sample, label =df$Proportion)) +
  geom_bar(position = 'stack', stat = 'identity') +
  geom_text(aes(label = df$Proportion), position = position_stack(vjust=0.5),size =3) +
  xlab('Sample') + ylab ('Proportion (%)') + labs(fill = 'Cell type')

#dev.off()
ggsave(filename = 'stackedBar.pdf', device = 'pdf', dpi =300)