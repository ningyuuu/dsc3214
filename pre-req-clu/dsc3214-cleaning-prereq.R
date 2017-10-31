library(compare)
setwd('/Users/Linchun/dsc3214/')
df<-read.csv('/Users/LinChun/dsc3214/pre-req-clu/raw data/prereq_preclu.csv',stringsAsFactors = F)
#df2<-read.csv('/Users/LinChun/dsc3214/pre-req-clu/raw data/prereq_preclu_2.csv',stringsAsFactors = F)
df3<-read.csv('/Users/LinChun/dsc3214/1718_s1_modsect_timeslots.csv')
df$X<-NULL
df$Title<-NULL

#df2$X<-NULL
#df2$Title<-NULL
df<-df[order(df$Module),]
#df2<-df2[order(df2$Module),]
#totaldf<- merge(df,df2,by="Module",all = T)

#totaldf<-totaldf[,c('Module','Prereq.x','Prereq.y','Preclu.x','Preclu.y')]
#totaldf$Prereq.x[which(totaldf$Prereq.x=="")]<-totaldf$Prereq.y[which(totaldf$Prereq.x=="")]
#totaldf$Prereq.x[which(is.na(totaldf$Prereq.x))]<-totaldf$Prereq.y[which(is.na(totaldf$Prereq.x))]
#totaldf$Prereq.y<-NULL
#totaldf$Preclu.x[which(totaldf$Preclu.x=="")]<-totaldf$Preclu.y[which(totaldf$Preclu.x=="")]
#totaldf$Preclu.x[which(is.na(totaldf$Preclu.x))]<-totaldf$Preclu.y[which(is.na(totaldf$Preclu.x))]
#totaldf$Preclu.y<-NULL

#df<-totaldf
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
  a<-grepl(paste(words,collapse = "|"),b)
  #how to consider case where there is no numeric value
  c<-b[a]
  for( i in 1:length(c)){
    if("or" %in% c[i]){
      next
    }else{
      b<-gsub("[A-Za-z]$","",c,ignore.case=T)
    }
  }
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
  a<-grepl(paste(words.2,collapse = "|"),col)
  #how to consider case where there is no numeric value
  c<-col[a]
  for( i in 1:length(c)){
    if("or" %in% c[i]){
      next
    }else{
      b<-gsub("[A-Za-z]$","",c,ignore.case=T)
    }
  }
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

list0[[2]]
#########################
###REMOVAL OF NON BIZ####
#########################

list.compare<-list0
biz.mods<-list0[[1]]
biz.mods<-unique(gsub("[A-Za-z]$","",biz.mods,ignore.case=T))

to.keep<-append(biz.mods,"or")
 
for ( i in 1:length(list0[[2]])){
  cat('loop',i,'\n')
  list0[[2]][i]<-list(unlist(list0[[2]][i])[unlist(list0[[2]][i]) %in% to.keep])
}

list0[[2]]

for ( i in 1:length(list0[[2]])){
  cat('loop',i,'\n')
  list0[[2]][i]<-list(unlist(list0[[2]][i])[unlist(list0[[2]][i]) %in% biz.mods])
}


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
  a<-grepl(paste(words.pc,collapse = "|"),b)
  #how to consider case where there is no numeric value
  c<-b[a]
  for( i in 1:length(c)){
    if("or" %in% c[i]){
      next
    }else{
      b<-gsub("[A-Za-z]$","",c,ignore.case=T)
    }
  }
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

to.keep<-append(biz.mods,"or")

for ( i in 1:length(list0[[3]])){
  cat('loop',i,'\n')
  list0[[3]][i]<-list(unlist(list0[[3]][i])[unlist(list0[[3]][i]) %in% to.keep])
}

####AFTER INSPECTION########
#### REMOVE ALL THE "OR"####
############################

for ( i in 1:length(list0[[3]])){
  cat('loop',i,'\n')
  list0[[3]][i]<-list(unlist(list0[[3]][i])[unlist(list0[[3]][i]) %in% biz.mods])
}
list0[[3]]

prereq.mod.list<-unique(unlist(list0[[2]]))
preclusion.mod.list<-unique(unlist(list0[[3]]))
prereq.mod.list
preclusion.mod.list

############
#Code to Add module_sectionals -> Not required now
module_sectionals<-unique(df3$module_sectional)

list.of.add.mods<-c("")
for( i in 1:length(biz.mods)){
  if(identical(which(grepl(biz.mods[i],module_sectionals)),integer(0))){
    list.of.add.mods<-append(list.of.add.mods,i)
  }else{
    next
  }
}
###########
#Let row be biz mods and col be req/preclu
pre.req.df<-setNames(data.frame(matrix(ncol = length(biz.mods), nrow = length(biz.mods))), biz.mods)
row.names(pre.req.df) <- biz.mods
pre.clu.df<-pre.req.df

pre.clu.df<-pre.clu.df[order(row.names(pre.clu.df)),]
pre.req.df<-pre.req.df[order(row.names(pre.req.df)),]
####################

for(i in 1:length(list0[[2]])){
  if(identical(unlist(list0[[2]][i]), character(0))){
    list0[[2]][i]<-list(c(""))
  }else{
    next
  }
  
}

list0[[2]]

a<-""
for( i in 1:length(list0[[1]])){
  a<-grep(list0[[1]][i],rownames(pre.req.df))
  for( j in 1:length(colnames(pre.req.df))){
    pre.req.df[a,j]<-ifelse((colnames(pre.req.df)[j] %in% unlist(list0[[2]][i])),1,0)
  }
}

a<-""
for( i in 1:length(list0[[1]])){
  a<-grep(list0[[1]][i],rownames(pre.clu.df))
  for( j in 1:length(colnames(pre.clu.df))){
    pre.clu.df[a,j]<-ifelse((colnames(pre.clu.df)[j] %in% unlist(list0[[3]][i])),1,0)
  }
}

write.csv(pre.req.df,"pre_req_df.csv")  
write.csv(pre.clu.df,"pre_clu_df.csv")
