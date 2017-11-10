setwd('/Users/Linchun/dsc3214/pre-req-clu/')


df<-read.csv('pre_clu_df.csv',stringsAsFactors = F)
df.1<-df
rownames(df)<-df[,1]
#View(df)
df<-df[,2:ncol(df)]
head(df)
#Checking
rownames(df) %in% colnames(df)

numrow<-length(rownames(df))
for (i in 1:numrow){
  df[i,i]<-0
  cat(df[i,i],"\n")
}

df["MKT1003","MKT1003"]

write.csv(df,"pre_clu_df.csv")
