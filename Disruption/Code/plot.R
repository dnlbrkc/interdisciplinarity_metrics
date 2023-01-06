load("Results/Results.Rdata")

D2 <- unlist(D)

D2 <- D2[which(D2!=0)]
D2 <- D2[which(!is.nan(D2))]
hist(sort(D2),breaks=200,main="",xlab="Disruption")

# abline(v=D2[which(papers$PaperID == "2140448817")],col="blue",lwd=2) #Jacob
# abline(v=D2[which(papers$PaperID == "2163657674")],col="blue",lwd=2) #Billinger
# abline(v=D2[which(papers$PaperID == "2107017285")],col="blue",lwd=2) #Dobra
# abline(v=D2[which(papers$PaperID == "2223634589")],col="blue",lwd=2) #Sang
# abline(v=D2[which(papers$PaperID == "3210169015")],col="blue",lwd=2) #Chris
# abline(v=D2[which(papers$PaperID == "2100593292")],col="blue",lwd=2) #Baum





exclude <- which(allPapers$CitationCount<5)

papers <- allPapers[-c(exclude),]


all <- cbind(unique(allPapers$PaperID),D2)
D2 <- all[(all[,1] %in% unique(papers$PaperID)),]     
D2 <- D2[,2]
D2 <- D2[which(D2!=0)]
D2 <- D2[which(!is.nan(D2))]
hist(sort(D2),breaks=200,main="Citations < 5 excluded ",xlab="Disruption")
