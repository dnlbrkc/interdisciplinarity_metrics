# Step 1. -----------------------------------------------------------------
#For each publication create all pairwise combinations of referred work and calculate the frequency of each journal pairing

setwd("Data/PapersJournals/")

#combine data files
files <- list.files(pattern="csv")
mat <- matrix(0,ncol=7,nrow=1)
for(f in files){
  a <- read.csv(f,sep=',',head=F)
  mat <- rbind(mat,a)
}
mat <- mat[-1,]
allPapers <- mat[,-c(4,6)] #remove unnecessary columns
colnames(allPapers) <- c("PaperID","Reference","PaperJournalID","Title","Year")

#create all reference pairs
papers <- unique(allPapers[,1])
pairs <- matrix(0,ncol=4,nrow=1)

#create all reference pairs
papers <- unique(allPapers[,1])
pairs <- matrix(0,ncol=4,nrow=1)

for(p in papers){
  refs <- allPapers[which(allPapers[,1]==p),2]
  if(length(refs) > 1){
    prs <- t(combn(refs,2))
    prs <- cbind(prs,rep(allPapers[which(allPapers[,1]==p),5][1],nrow(prs)),rep(allPapers[which(allPapers[,1]==p),1][1],nrow(prs)))
    pairs<- rbind(pairs, prs)
  }
}
pairs <- pairs[-1,]
pairs <- pairs[,c(1,2,4,3)]



#get Journal info cited papers
setwd("Data/ReferencesJournals/")
files <- list.files(pattern="csv")
mat <- matrix(0,ncol=4,nrow=1)
colnames(mat) <- c("Paper","Ref","Journal","Year")
for(f in files){
  a <- read.csv(f,sep=',',head=F)
  colnames(a) <- c("Paper","Ref","Journal","Year")
  a[which(is.na(a[,3])),3] <- 0
  unname(a)
  unname(mat)
  mat <- rbind(mat,a)
}
mat <- mat[-1,]


pairyears <- pairs
pairs <- pairs[,-c(3,4)]

pairs2 <- as.matrix(cbind(pairs,pairs,pairs))
for(i in 1:nrow(pairs)){
  
  print(i)
  paperPairs <- pairs[i,]
  a <- mat[which(mat$Ref == paperPairs[1]),c(3,4)]
  b <- mat[which(mat$Ref == paperPairs[2]),c(3,4)]
  
  pairs2[i,c(3,4)] <- as.numeric(a[1,])
  pairs2[i,c(5,6)] <- as.numeric(b[1,])
  
}
journalPairs <- pairs2[,c(1,3,5)]
journalPairs <- cbind(journalPairs,pairyears[,3])
journalPairs <- journalPairs[,-1]
journalPairs <- cbind(journalPairs,pairyears[,4])
#remove non-journal entries
journalPairs <- journalPairs[-which(journalPairs[,1]==0),]
journalPairs <- journalPairs[-which(journalPairs[,2]==0),]



#count pair frequencies
comb <- matrix(0,ncol=1,nrow=nrow(journalPairs))
for(i in 1:nrow(journalPairs)){
  comb[i,] <- paste0(journalPairs[i,1],'_',journalPairs[i,2])
}


comb <- cbind(comb,journalPairs[,3],journalPairs[,4])
comb <- as.data.frame(comb)

empirical_frequencies <- comb

save(empirical_frequencies,file="Data/empirical_frequencies.Rdata")


