library(foreach)
library(doParallel)
setwd("allJournals/allJournals")

cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

#LOAD DATA OF PUBLICATIONS, REFERENCES, YEAR, ETC
#combine data files
files <- list.files(pattern="csv")
mat <- matrix(0,ncol=7,nrow=1)
for(f in files){
  a <- read.csv(f,sep=',',head=F)
  mat <- rbind(mat,a)
}
mat <- mat[-1,]

allPapers <- mat[,-4]
allPapers_1 <- allPapers   
colnames(allPapers_1) <- c("PaperID","Reference","JournalID","Title","CitationCount","Year")
allPapers <- allPapers_1


#GO THROUGH EACH PAPER AND IDENTIFY "SUBSEQUENT WORK"
papers <- unique(allPapers$PaperID)
allYears <- sort(unique(allPapers$Year))

D <- foreach(p = papers) %dopar% {
  
  #check year of paper and identify subsequent work
  year <- allPapers[allPapers$PaperID == p,]$Year[1]
  if(year != allYears[length(allYears)]){
    subsequent <- allPapers[allPapers$Year %in% allYears[(which(allYears == year)+1):length(allYears)],]
  } else {
    subsequent <- allPapers[allPapers$Year %in% allYears[length(allYears) ],]  
  }
  
  #select "subsequent work" and check whether they cite the "focal work" "focal work & its references" or "only it's references"
  subPapers <- unique(subsequent$PaperID) 
  type_i <- vector() #cites focal work
  type_j <- vector() #cites focal work and references
  type_k <- vector() #cites only references
  type_x <- vector() #none of the above
  
  
  focal_refs <- allPapers[which(allPapers$PaperID %in% p),]$Reference #references of focal paper
  a <- allPapers$PaperID
  b <- subPapers
  
  ids <- unname(by(seq_along(a), a, list)[as.character(b)])
  ids2 <- sapply(1:length(p), function(x) a[p[[x]]])
  
  c=0
  for(s in 1:length(subPapers)){
    
    c=c+1
    
    #sub_refs <- allPapers[which(allPapers$PaperID %in% s),]$Reference #references of subsequent work
    sub_refs <- allPapers[ids[[s]],]$Reference
    if(any(sub_refs == p) && any(sub_refs %in% focal_refs)){
      type_j[c] <- 1
    } else if (!any(sub_refs == p) && any(sub_refs %in% focal_refs)){
      type_k[c] <- 1
    } else if (any(sub_refs == p) && !any(sub_refs %in% focal_refs)){
      type_i[c] <- 1
    } else {
      type_x[c] <- 1
    }
  }
  
  (length(type_i[!is.na(type_i)]) - length(type_j[!is.na(type_j)])) / (length(type_i[!is.na(type_i)]) + length(type_j[!is.na(type_j)]) + length(type_k[!is.na(type_k)]))
  
  
}

stopCluster(cl)

save(D,file="Results.Rdata")