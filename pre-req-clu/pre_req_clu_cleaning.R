library(compare)
df<-read.csv('/Users/LinChun/dsc3214/prereq_preclu.csv',stringsAsFactors = F)
df2<-read.csv('/Users/LinChun/dsc3214/prereq_preclu_2.csv',stringsAsFactors = F)
df$X<-NULL
df$Title<-NULL

df2$X<-NULL
df2$Title<-NULL

df<-df[order(df$Module),]
df2<-df2[order(df2$Module),]
totaldf<- merge(df,df2,by="Module",all = T)

totaldf<-totaldf[,c('Module','Prereq.x','Prereq.y','Preclu.x','Preclu.y')]
totaldf$Prereq.x[which(totaldf$Prereq.x=="")]<-totaldf$Prereq.y[which(totaldf$Prereq.x=="")]
totaldf$Prereq.x[which(is.na(totaldf$Prereq.x))]<-totaldf$Prereq.y[which(is.na(totaldf$Prereq.x))]
totaldf$Prereq.y<-NULL
totaldf$Preclu.x[which(totaldf$Preclu.x=="")]<-totaldf$Preclu.y[which(totaldf$Preclu.x=="")]
totaldf$Preclu.x[which(is.na(totaldf$Preclu.x))]<-totaldf$Preclu.y[which(is.na(totaldf$Preclu.x))]
totaldf$Preclu.y<-NULL

df<-totaldf
df$Module<-gsub("[A-Za-z]$","",df$Module,ignore.case=T)

list0<-as.list(df) 
list1<-list0
words<-c("^and","^or","[[:digit:]]")
words.2<-c("[[:digit:]]")

word.cleaning<-function(col,mod){
  col<-unlist(col)
  col<-gsub("/"," or ",col,ignore.case=T)
  b<-unlist(strsplit(col," "))
  b<-unlist(strsplit(b,"\\\\"))
  b<-unlist(strsplit(b,"\n"))
  b<-gsub("[A-Za-z]$","",b,ignore.case=T)
  a<-grepl(paste(words,collapse = "|"),b)
  #how to consider case where there is no numeric value
  c<-b[a]
  if(!is.na(pmatch(mod,c))){
    c<-c[-pmatch(mod,c)]
  }
  if(identical(c, character(0))){
    return(NA)
  }else{
    return(list(c))
  }
}

word.cleaning2<-function(col){
  col<-unlist(col)
  col<-gsub("[A-Za-z]$","",col,ignore.case=T)
  a<-grepl(paste(words.2,collapse = "|"),col)
  #how to consider case where there is no numeric value
  c<-col[a]
  if(identical(c, character(0))){
    return(NA)
  }else{
    return(list(c))
  }
}

##################
####PREREQUISITE##
##################
#For Sem 1
for(i in 1:length(list0[[2]])){
  cat('loop',i,'\n')
  if(is.na(list0[[2]][i])){
    next
  }
  if(list0[[2]][i] == ""){
    next
  }
  list0[[2]][i]<-word.cleaning(list0[[2]][i],list0[[1]][i])
}


#########################
###REMOVAL OF NON BIZ####
#########################

list.compare<-list0
biz.mods<-list0[[1]]
biz.mods<-unique(gsub("[A-Za-z]$","",biz.mods,ignore.case=T))

for ( i in 1:length(list0[[2]])){
  cat('loop',i,'\n')
  #a<-(unlist(list0[[2]][i]) %in% biz.mods)
  a<-charmatch(unlist(list0[[2]][i]),biz.mods)
  a<-a[!is.na(a)]
  list0[[2]][i]<-list(biz.mods[a])
}

list0[[2]]
list0[[2]][91]
list.compare[[2]]
list0[[1]]
#List of 'ands'
list0[[2]][172]<-list(c('MNO1001andMNO2007'))
list0[[2]][164]<-list(c("MNO1001andMNO2007"))
list0[[2]][104]<-list(c("FIN3101andFIN3102andFIN3103"))
list0[[2]][97]<-list(c("FIN3101"))
list0[[2]][96]<-list(c("FIN3101"))
list0[[2]][95]<-list(c("FIN3101"))
list0[[2]][92]<-list(c("FIN3101andFIN3102andFIN3103"))
list0[[2]][87]<-list(c("ACC1002andFIN2004andFIN3102"))
list0[[2]][85]<-list(c("FIN2004andFIN3103"))
list0[[2]][84]<-list(c("FIN2004andFIN3103"))
list0[[2]][80]<-list(c("FIN2004andFIN3102"))
list0[[2]][55]<-list(c("DSC1007andDSC2008"))
list0[[2]][35]<-list(c("BSP1005andBSP2001"))
list0[[2]][18]<-list(c("ACC3603andACC3616","ACC3603andACC3611andACC3612"))
list0[[2]][15]<-list(c("ACC1002andBSP1004andFIN2004"))
list0[[2]][12]<-list(c("FIN2004andBSP1004"))
list0[[2]][11]<-list(c("ACC1002andBSP1004"))
list0[[2]][9]<-list(c("ACC1002andBSP1004"))
list0[[2]][98]<-list(c("ACC1002andFIN3101andFIN3102"))
list0[[2]]

###Next Step: Extract all of the unique modules into a single vector

################
####PRECLUSION##
################
words.pc<-c("^or","[[:digit:]]")

list0[[3]]

word.cleaning.pc<-function(col,mod){
  col<-unlist(col)
  col<-gsub("/"," or ",col,ignore.case=T)
  col<-gsub("[[:punct:]]","",col,ignore.case=T)
  b<-unlist(strsplit(col," "))
  b<-unlist(strsplit(b,"\\\\"))
  b<-unlist(strsplit(b,"\n"))
  b<-gsub("[A-Za-z]$","",b,ignore.case=T)
  a<-grepl(paste(words.pc,collapse = "|"),b)
  #how to consider case where there is no numeric value
  c<-b[a]
  if(!is.na(pmatch(mod,c))){
    c<-c[-pmatch(mod,c)]
  }
  if(identical(c, character(0))){
    return(NA)
  }else{
    return(list(c))
  }
}

for(i in 1:length(list0[[3]])){
  cat('loop',i,'\n')
  if(is.na(list0[[3]][i])){
    next
  }
  if(list0[[3]][i] == ""){
    next
  }
  list0[[3]][i]<-word.cleaning.pc(list0[[3]][i],list0[[1]][i])
}

for ( i in 1:length(list0[[3]])){
  cat('loop',i,'\n')
  #a<-(unlist(list0[[2]][i]) %in% biz.mods)
  a<-charmatch(unlist(list0[[3]][i]),biz.mods)
  a<-a[!is.na(a)]
  list0[[3]][i]<-list(biz.mods[a])
}

list0[[2]]

prereq.mod.list<-unique(unlist(list0[[2]]))
preclusion.mod.list<-unique(unlist(list0[[3]]))
prerequ.mod.list
preclusion.mod.list
biz.mods

#Let row be biz mods and col be req/preclu
pre.req.df<-setNames(data.frame(matrix(ncol = length(prerequ.mod.list), nrow = length(biz.mods))), prerequ.mod.list)
row.names(pre.req.df) <- biz.mods

pre.clu.df<-setNames(data.frame(matrix(ncol = length(preclusion.mod.list), nrow = length(biz.mods))), preclusion.mod.list)
row.names(pre.clu.df) <- biz.mods

for(i in 1:length(list0[[2]])){
  if(identical(unlist(list0[[2]][i]), character(0))){
    list0[[2]][i]<-list(c(""))
  }else{
    next
  }
  
}

for(i in 1:length(list0[[1]])){
  for( j in 1:length(prereq.mod.list)){
    pre.req.df[list0[[1]][i],j]<-ifelse(prereq.mod.list[j] %in% unlist(list0[[2]][i]),1,0) 
  }
}

for(i in 1:length(list0[[1]])){
  for( j in 1:length(preclusion.mod.list)){
    pre.clu.df[list0[[1]][i],j]<-ifelse(preclusion.mod.list[j] %in% unlist(list0[[3]][i]),1,0) 
  }
}


View(pre.req.df)
View(pre.clu.df)

  remove.pre.clu<-which(colSums(pre.clu.df)==0)
  remove.pre.req<-which(colSums(pre.req.df)==0)
  #FIN3102 is removed from the DF
  pre.req.df$FIN3101<-NULL
  
write.csv(pre.req.df,"pre_req_df.csv")  
write.csv(pre.clu.df,"pre_clu_df.csv")
