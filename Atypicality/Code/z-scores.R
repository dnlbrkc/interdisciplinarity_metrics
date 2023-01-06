library(tidyverse)

load("Data/empirical_frequencies")

setwd("Data/nullModels")
files <- list.files()
nulls <- list()
i=0
for(f in files){
  i=i+1
  load(f)
  nulls[[i]] <- null
}


years <- unique(empfreq$V3)
i=0
e <- list()
for(y in years){
  i=i+1

empYear = empfreq %>% group_by(V1,V2,V3) %>% filter(V3 == y) %>% summarise(n=n())
null1 = nulls[[1]][[1]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
null2 = nulls[[2]][[1]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
null3 = nulls[[3]][[1]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
null4 = nulls[[4]][[1]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
null5 = nulls[[5]][[1]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
null6 = nulls[[6]][[1]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
null7 = nulls[[7]][[1]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
null8 = nulls[[8]][[1]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
null9 = nulls[[9]][[1]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
null10 = nulls[[10]][[1]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())


df2 = empYear %>% full_join(null1,by="V1") %>%
  full_join(null2,by="V1") %>%
  full_join(null3,by="V1") %>%
  full_join(null4,by="V1") %>%
  full_join(null5,by="V1") %>%
  full_join(null6,by="V1") %>%
  full_join(null7,by="V1") %>%
  full_join(null8,by="V1") %>%
  full_join(null9,by="V1") %>%
  full_join(null10,by="V1") 

df2[which(is.na(df2[,4])),4] <- 0
df2[which(is.na(df2[,5])),5] <- 0
df2[which(is.na(df2[,6])),6] <- 0
df2[which(is.na(df2[,7])),7] <- 0
df2[which(is.na(df2[,8])),8] <- 0
df2[which(is.na(df2[,9])),9] <- 0
df2[which(is.na(df2[,10])),10] <- 0
df2[which(is.na(df2[,11])),11] <- 0
df2[which(is.na(df2[,12])),12] <- 0
df2[which(is.na(df2[,13])),13] <- 0
df2[which(is.na(df2[,14])),14] <- 0
colnames(df2) <- c("V1","P1","Y1","N1","N2","N3","N4","N5","N6","N7","N8","N9","N10","N11")
df2 <- df2 %>% group_by(V1)

df3 <- df2 %>% select(V1,P1,Y1, N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11) %>%
  mutate(z = (N1 - mean(c(N2, N3, N4, N5, N6, N7, N8, N9, N10, N11)) / sd(c(N2, N3, N4, N5, N6, N7, N8, N9, N10, N11))) )

e[[i]]=empYear %>%  left_join(df3,by="V1") %>% select(V1,V2,V3,z)
} 

combine <- purrr::reduce(e, dplyr::full_join)

save(combine,file="~/Desktop/final_SMJ_zcores.Rdata")
#remove NaN
combine2 <- combine %>% na.omit()

#calculate median z-score
z_median <- combine2 %>% group_by(V2) %>% mutate(zmed = median(z))

#calculate 10th percentile z-score
z_10th <- z_median %>% group_by(V2) %>% mutate(z10 = quantile(z,0.1))

z_scores <- z_median %>% group_by(V2) %>% slice(1)
plot(ecdf((z_scores$zmed)),main="median z-score",xlab="median z-score",ylab="cumulative distribution",xlim=c(-5,70))
abline(v=0)




allPapers <- as.data.frame(allPapers)
allPapers <- allPapers %>%  
  mutate(V2 = as.character(V2))

titles <- z_scores %>% full_join(allPapers,by="V2")


# 
# 
# # Christensen et al, 2020
# 
# thor <- allPapers[which(allPapers[,6] == "2020"),]
# words <- list()
# for(i in 1:nrow(thor)){
# words[[i]] <- word(thor$Title[i],1)
# }
# which(words %in% "industry")
# which(z_scores[,2] == "3034164684") 
# 
# 
# 
# 
# 
# # Stieglitz et al, 2015
# 
# thor <- allPapers#[which(allPapers[,6] == "2015"),]
# words <- list()
# for(i in 1:nrow(thor)){
#   words[[i]] <- word(thor$Title[i],1)
# }
# thor[which(words %in% "adaptation"),4]
# 
# thor[16485,]
# which(z_scores[,2] == "1962401551") 
# 
# 
# 
# 
# # Billinger et al, 2020
# 
# thor <- allPapers   #[which(allPapers[,6] == "2015"),]
# words <- list()
# for(i in 1:nrow(thor)){
#   words[[i]] <- word(thor$V5[i],1)
# }
# thor[which(words %in% "exploration"),4]
# which(words %in% "exploration")[303]
# 
# thor[138742,]
# which(z_scores[,2] == "3046382452") 
# 
# 
# 
# 
# # Bauman and Stieglitz 2013
# 
# thor <- allPapers   #[which(allPapers[,6] == "2015"),]
# words <- list()
# for(i in 1:nrow(thor)){
#   words[[i]] <- word(thor$Title[i],1)
# }
# thor[which(words %in% "rewarding"),4]
# which(words %in% "rewarding")[52]
# 
# thor[107562,]
# which(z_scores[,2] == "2334007494") 
# 
# # Knusden at eal 2013
# 
# thor <- allPapers   #[which(allPapers[,6] == "2015"),]
# words <- list()
# for(i in 1:nrow(thor)){
#   words[[i]] <- word(thor$Title[i],1)
# }
# thor[which(words %in% "hidden"),4]
# which(words %in% "hidden")[15]
# 
# thor[13725,]
# which(z_scores[,2] == "1942971405") 
# 
# 
# 
# # Barney at eal 2018
# 
# thor <- allPapers[which(allPapers[,6] == "2018"),]
# words <- list()
# for(i in 1:nrow(thor)){
#   words[[i]] <- word(thor$Title[i],1)
# }
# thor[which(words %in% "the"),4]
# which(words %in% "the")[579]
# 
# thor[3974,]
# which(z_scores[,2] == "2783149505") 
# 
# 
# 
# 
# #z scores
# z_scores <- z_median %>% group_by(V2) %>% slice(1)
# plot(ecdf((z_scores$zmed)),main="median z-score",xlab="median z-score",ylab="cumulative distribution",xlim=c(-10,200))
# abline(v=0)
# 
# 
# z_scores <- as.data.frame(z_scores)
# 
# 
# 
# e = ecdf(z_scores$zmed)
# 
# ggplot(z_scores, aes(zmed)) + stat_ecdf(geom = "step")+
#   labs(title="",
#        y = "Cumulative distribution", x="Median z-score")+
#   theme_classic()+
#   geom_label(aes(x = z_scores[which(z_scores[,2] == "3034164684"),]$zmed, y = e(z_scores[which(z_scores[,2] == "3034164684"),]$zmed)), 
#              label = "Chris 2020")+
#   geom_label(aes(x = z_scores[which(z_scores[,2] == "1962401551"),]$zmed, y = e(z_scores[which(z_scores[,2] == "1962401551"),]$zmed)), 
#              label = "Stig 2015")+
#   geom_label(aes(x = z_scores[which(z_scores[,2] == "3046382452"),]$zmed, y = e(z_scores[which(z_scores[,2] == "3046382452"),]$zmed)), 
#              label = "Bill 2020")+
#   geom_label(aes(x = z_scores[which(z_scores[,2] == "2334007494"),]$zmed, y = e(z_scores[which(z_scores[,2] == "2334007494"),]$zmed)), 
#              label = "Bau 2013")+
#   geom_label(aes(x = z_scores[which(z_scores[,2] == "1942971405"),]$zmed, y = e(z_scores[which(z_scores[,2] == "1942971405"),]$zmed)), 
#              label = "Knu 2013")+
#   geom_label(aes(x = z_scores[which(z_scores[,2] == "2783149505"),]$zmed, y = e(z_scores[which(z_scores[,2] == "2783149505"),]$zmed)), 
#              label = "Barn 2018")
# 
# 
# 
#   #geom_point(data = z_scores[which(z_scores[,2] == "3034164684"),]$z, aes(x=x, y=y))
# 
# 
# z_scores[which(z_scores[,2] == "3034164684"),]$z
# z_scores[which(z_scores[,2] == "1962401551"),]$z
# z_scores[which(z_scores[,2] == "3046382452"),]$z
# z_scores[which(z_scores[,2] == "2334007494"),]$z
# z_scores[which(z_scores[,2] == "1942971405"),]$z
# z_scores[which(z_scores[,2] == "2783149505"),]$z
# 
