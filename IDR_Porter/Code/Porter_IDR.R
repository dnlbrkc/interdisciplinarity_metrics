devtools::install_github("ikashnitsky/sjrdata")
library(sjrdata)
library(tidyverse)
library(lsa)
library(qlcMatrix)
# STEP 1. -----------------------------------------------------------------
#get journal Subject Categories and match with MAG dataset by ISSN number

#format ISSNs to match MAG dataset
issns <- sjr_journals$issn
new_issn <- vector()
for(i in 1:length(issns)){
  print(i)
  a <- issns[i]
  b <- strsplit(a,split=',')
  c <- b[[1]][1]
  new_issn[i] <- c
}
sjr_journals$issn <- new_issn


#load journal ISSNs from MAG
setwd("Porter_Data/journalISSN/")
files <- list.files()
mat <- matrix(0,ncol=3,nrow=1)
for(f in files){
  a <- read.csv(f,sep=',',head=F)
  mat <- rbind(mat,a)
}
mat <- mat[-1,]

#format ISSN numbers
issn2 <- vector()
for(i in 1:length(mat$V3)){
  a <- mat$V3[i]
  b <- strsplit(a,split='-')
  c <- paste0(b[[1]][1],b[[1]][2])
  issn2[[i]] <- c
}
mat$V3 <- issn2

#remove NAs
mat  <- mat[-which(mat$V3 == "NANA"),]
mat <- as.data.frame(mat)
colnames(mat) <- c("id","name","issn")

#join "SJR journals"  and MAG dataset
joined <- sjr_journals %>% inner_join(mat,by="issn")
journalsDB <- joined %>% group_by(issn) %>% arrange(desc(issn)) %>% slice(1)


# Step 2. -----------------------------------------------------------------
#Create SCxSC matrix based on categories

#create list of unique categories
cats <- journalsDB$categories
newcat <- list()
for(i in 1:length(cats)){
  newcat[[i]] <- strsplit(cats[i],split=';')
}
newcat2 <- unlist(newcat)

#create SCxSC matrix counting the number of co-occurences between categories
SC <- matrix(0,ncol=length(unique(newcat2)),nrow=length(unique(newcat2)))
colnames(SC) <- unique(newcat2)
rownames(SC) <- unique(newcat2)
for(i in 1:length(newcat)){
  count <- newcat[[i]]
  for(y in count[[1]] ){
    pos1 <- which(colnames(SC) == y)
    pos2 <- which(rownames(SC) == y)
    SC[pos1,pos2] <- SC[pos1,pos2] + 1
  }
  
  if(length(count[[1]])>1){
    combinations <- combn(count[[1]],2)
    for(y in 1:ncol(combinations)){
      pos1 <- which(colnames(SC) == combinations[1,y])
      pos2 <- which(rownames(SC) == combinations[2,y])
      SC[pos1,pos2] <- SC[pos1,pos2] + 1
      SC[pos2,pos1] <- SC[pos2,pos1] + 1
      
    }
  }
}

#calculate cosine similarity between journal categories co-occurrences
similarity <- cosSparse(SC)
#similarity <- cosine(lower.tri(SC))
#save(similarity, file="similarity.Rdata")


# Step 3. -----------------------------------------------------------------
#Create Reference-Journal Matrix

setwd("Porter_Data/PapersReferencesJournals")
files <- list.files(pattern="csv")
mat <- matrix(0,ncol=3,nrow=1)
for(f in files){
  a <- read.csv(f,sep=',',head=F)
  mat <- rbind(mat,a)
}
mat <- mat[-1,]
M1 <- mat
#M1 <- M1[order(M1[,1],decreasing=FALSE),]
M1 <- as.data.frame(M1)
colnames(M1) <- c("papers","references","id")

#save(M1, file="JournalsOfReferences.Rdata")

#merge Subject categories dataframe with Reference-Journal Matrix
journalCategories <- journalsDB %>% group_by(id) %>% inner_join(M1)
#save("journalCategories.Rdata")
#categories=joined %>% select(categories,id)
#jcat <- left_join(M1,e)




# Step 4. -----------------------------------------------------------------
# Calculate Porter's measure

#calculate Porter's score
#load("journalCategories.Rdata")
#load("similarity.Rdata")

#parallel computing 
#library(foreach) 
#library(doParallel)
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #not to overload your computer
# registerDoParallel(cl)
# #D <- foreach(p = unique(jcat$papers)) %dopar% {

jcat <- journalCategories %>%group_by(papers,references)%>% slice(1)

paper_info <-vector()
c=0
for(p in unique(journalCategories$papers)){
  c=c+1
  print(c)
  SCs <- jcat[which(jcat$papers == p),]$categories
  if(!all(is.na(SCs))){
    SCs2 <- list()
    for(e in 1:length(unlist(SCs))){
      SCs2[[e]] <- trimws(strsplit(unlist(SCs)[e],split=';')[[1]])
    }
    
    #remove NA
    count_na <- vector()
    SCs2 <- unlist(SCs2)
    for(n in 1:length(SCs2)){
      if(all(is.na(SCs2[[n]] ))){
        count_na[n] <- n
      } else {
        
      }
    }
    na_s <- which(count_na != "NA")
    #SCs2 <- unlist(SCs2)
    SCs <- unlist(SCs)
    if(any(na_s)>=1){
      SCs2 <- SCs2[-na_s]
      #SCs <- SCs[-na_s]
    }
    total <- length(unique(SCs2))
    calc <- table(SCs2) / total
    
    Pi <- calc
    
    if(length(Pi) > 1){
      #Pi * Pj
      PiPj <- matrix(0,length(unique(names(Pi))),length(unique(names(Pi))))
      ids <- expand.grid(1:length(unique(names(Pi))),1:length(unique(names(Pi))))
      t=0
      for(i in 1:(length(unique(names(Pi)))*length(unique(names(Pi))))){
        t=t+1
        PiPj[ids[t,1],ids[t,2]] <- Pi[ids[t,1]] * Pi[ids[t,2]]
      }
      PiPj[is.na(PiPj)] <- 0
      
      colnames(PiPj) <- names(Pi)
      rownames(PiPj) <- names(Pi)
     
      #Sij
      
      colnames(similarity) <- trimws(colnames(similarity))
      rownames(similarity) <- trimws(rownames(similarity))
      Sij_id <- which(colnames(similarity) %in% unique(names(Pi)))
      
      Sij <- similarity[Sij_id,Sij_id]
      Sij <- Sij[, !duplicated(colnames(Sij))]
      Sij <- Sij[!duplicated(rownames(Sij)),]
      
      paper_info[c] <- 1-sum(PiPj[lower.tri(PiPj)] * Sij[lower.tri(Sij)])
      #print(paper_info[c])
    } else{
      paper_info[c] <- NA  
    }
  }
}

save(paper_info,file="Results.Rdata")
#stopCluster(cl)
# setwd("..")
# name<-paste0("Run_",q,".Rdata")
#save(D,file="Results.Rdata")

