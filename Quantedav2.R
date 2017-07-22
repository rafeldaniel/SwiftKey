
library(parallel)
library(quanteda)
library(plyr)
library(dplyr)

# bad words list from google
# exceptions removed adult, amateur, hell, homo

badwords<-read.csv2(file = "C:/Users/rafael.daniel/Documents/R/project/bad words/full-list-of-bad-words-banned-by-google-csv-file_2013_11_26_04_52_30_695.csv"
                    ,stringsAsFactors = F)
badwords<-badwords[,1]

datapath<-"C:/Users/rafael.daniel/Documents/R/project/en_US"


###################################
# LOAD BLOGGER

myfile<- file(paste0(datapath,"/en_US.blogs.txt"), open="r")
dat<-readLines(con = myfile,encoding = "UTF-8")
close(myfile)

# REPLACE ANY BADWORD BY 'BADWORD'
# This operation takes more than 90 min for en_US_blogs file

# Prepare to work in parallel
# calculate de number of cores
#ncores<-detectCores()-1

#init clusters
#cl <- makeCluster(ncores)
#clusterExport(cl,"dat")
print(paste("Start:",Sys.time()))

j<-0
for (i in badwords){
  dat<-gsub(pattern=paste("\\<\\w*",i,"\\w*\\>"),replacement="BADWORD",ignore.case = T,x = dat)
  j<-j+1
  print (paste(j,Sys.time()))
}


#tarda de 22:20 a 03:50

#dat<-parSapply(cl = cl,X = badwords,
#               FUN = function (x) gsub(pattern=x,replacement="BADWORD",ignore.case = T,x = dat))

#stopCluster(cl)
print(paste("End:",Sys.time()))
#dat<-as.character(dat)

save(dat,file = paste(datapath,"en_US.blogs_BDW_rm"))

tokenize.corpus<-function(mycorpus){
  #tokenze sentences
  dat <- tokenize(x = mycorpus,what="sentence",simplify=T)
  
  #substitute BADWORDS
  
  #substitute any  '_' by white space
  dat<-gsub(pattern = "_",replacement = " ",x = dat)
  
  #unify ' and ´ to '
  dat<-gsub(pattern = "´|'",replacement = "'",x = dat)
  
  #convert to Quanteda corpus to tokenize
  dat<-corpus(dat)


  mytokens<-tokenize(x=dat,what="word",remove_numbers=TRUE,
                   remove_symbols=T,simplify = F, remove_separators=TRUE,remove_twitter = T,remove_url = T)
}

myfilter<-function(words){
  
  #Remove anyword that does not contain A to z or ',-,_
  
  f1<-grep(pattern = "[^A-z_'-]",x = row.names(words),value = F)
  print(paste("Removed",length(f1),"rows"))
  words[-f1,]
}

print(paste("Start:",Sys.time()))
mytokens<-tokenize.corpus(dat)
print(paste("End:",Sys.time()))

dfm1<-dfm(x = mytokens,tolower = T,remove=c("BADWORD"))

#tokens_ngrams

W1<-data.frame(W1=featnames(dfm1),n1=colSums(dfm1))
rm(dfm1)
W1<-myfilter(W1)


dfm2<-dfm(tokens_ngrams(x = mytokens,n = 2),tolower=T,remove=c("BADWORD"))

W2<-strsplit(x = featnames(dfm2),split = "_",fixed = T)
W2<-ldply(W2,rbind) #muy lento
W2<-cbind.data.frame(W2,n2=colSums(dfm2))
rm(dfm2)

MAT<-merge(x = W1,y=W2,by.x="W1",by.y="1",all=T)


dfm3<-dfm(tokens_ngrams(x = mytokens,n = 3),tolower=T,remove=c("BADWORD"))

W3<-strsplit(x = featnames(dfm3),split = "_",fixed = T)
W3<-ldply(W3,rbind)
W3<-cbind.data.frame(W3,n3=colSums(dfm3))
rm(dfm3)


MAT<-merge(x = MAT,y=W3,by.x=c("W1","2"),by.y=c("1","2"),all=T)
names(MAT)<-c("W1","W2","n1","n2","W3","n3")
MAT<-MAT%>%select(W1,n1,W2,n2,W3,n3)%>%arrange(desc(n1),desc(n2),desc(n3))



