
# Step 1. -----------------------------------------------------------------
# Create 10 null models by rewiring the empirical dataset

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
allPapers <- allPapers[,c(1,2,5)]

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

#add year to references
referencesYear <- as.matrix(cbind(allPapers,rep(0,nrow(allPapers))))
for(i in 1:nrow(allPapers)){
  
  a <- mat[which(mat$Ref == allPapers$Reference[i]),4]
  #b <- mat[which(mat$Ref == paperPairs[2]),c(4)]
  referencesYear[i,4] <- a[1]
  #pairs2[i,c(3,4)] <- as.numeric(a[1,])
  #pairs2[i,c(5,6)] <- as.numeric(b[1,])
  
}

colnames(referencesYear) <- c("PaperId","References","PaperYear","ReferenceYear")

years <- sort(unique(referencesYear[,3]))

#Q*E rewirings 157980 * 100
instances <- list()
for(i in 1:10){
  print(i)
  for(i in 1:15798000){ 
    
    y = sample(years,1)
    
    #select papers from particular year
    a <- referencesYear[ which(referencesYear[,3] == y),]
    
    others <- 0
    while(length(others) <= 1){
      choose <- sample(1:nrow(a),1) #choose a random paper
      pap1 <- a[choose,]
      
      others <- which(a[,4] == pap1[4])
      others <- others[others!=choose]
      
    }
    pap2 <- a[sample(others,1),]
    
    referencesYear[intersect(which(referencesYear[,1] == pap1[1]),  which(referencesYear[,2] == pap1[2])),2] <- pap2[2] 
    referencesYear[intersect(which(referencesYear[,1] == pap2[1]),  which(referencesYear[,2] == pap2[2])),2] <- pap1[2] 
    
    
  }
  
  instances[[i]] <- referencesYear
}

save(instances,file="NullModels.Rdata")



# Step 2 ------------------------------------------------------------------
# For each of the 10 null-models calculate the journal-pair frequencies

q <- as.integer( commandArgs(TRUE)[1])


files <- list.files()

#for(fi in files[-1]){
#  print(fi)
load(files[q])
a <- instances[[1]]
a <- a[,1:3]

papers <- unique(a[,1])
pairs <- matrix(0,ncol=4,nrow=1)

for(p in papers){
  refs <- a[which(a[,1]==p),2]
  if(length(refs) > 1){
    prs <- t(combn(refs,2))
    prs <- cbind(prs,rep(a[which(a[,1]==p),3][1],nrow(prs)),rep(a[which(a[,1]==p),1][1],nrow(prs)))
    pairs<- rbind(pairs, prs)
  }
}
pairs <- pairs[-1,]


#get year and journal ID for each pair
setwd("ReferencesJournals/")
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
  
  #print(i)
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


name <- paste0("null",q,".Rdata")
save(comb,file=name)
