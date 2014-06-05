rm(list=ls())
wd<-"C:/Users/gabel/Documents/football/wcmig/"

year <- 2014
df0 <- read.csv(paste0(wd,paste0("wc",year,"sq.csv")))
df1 <- read.csv(paste0(wd,"iso.csv"), stringsAsFactors=FALSE)

m <- table(df0$league, df0$country)
#write.csv(m, paste0(wd,"m.csv"))
df1 <- subset(df1, country %in% unlist(dimnames(m)))
df1$country
df1$order <- 1:nrow(df1)

m <- table(df0$league, df0$country)
m<-rbind(m, matrix(0, sum(is.na(match(df1$country[df1[,paste0("wc",year)]==1], rownames(m)))), ncol(m)))
m<-cbind(m, matrix(0, nrow(m), nrow(m)-ncol(m)))
rownames(m)[rownames(m)==""]<-colnames(m)[!(colnames(m) %in% rownames(m))]
colnames(m)[colnames(m)==""]<-rownames(m)[!(rownames(m) %in% colnames(m))]
m<-as.matrix(m)
m<-m[df1$country,df1$country]
dimnames(m)<-list(orig=rownames(m),dest=colnames(m))

df1$country <- factor(df1$country, levels=df1$country)
#levels(df1$country) <- gsub(" ", "\n", levels(df1$country))
dimnames(m)<-list(orig=levels(df1$country),dest=levels(df1$country))


##
##define ranges of circos sectors and their colors (both of the sectors and the links)
##
df1$xmin <- 0
df1$xmax <- 0
df1$xmax[df1[,paste0("wc",year)]==1] <- 23 
df1$xmax <- df1$xmax + rowSums(m)
n<-nrow(df1)

df1$rcol1<-"grey90"
df1$lcol1<-"grey90"
df1$rcol1[df1[,paste0("wc",year)]==1]<-rgb(df1$r1[df1[,paste0("wc",year)]==1], df1$g1[df1[,paste0("wc",year)]==1], df1$b1[df1[,paste0("wc",year)]==1], max = 255)
df1$lcol1[df1[,paste0("wc",year)]==1]<-rgb(df1$r1[df1[,paste0("wc",year)]==1], df1$g1[df1[,paste0("wc",year)]==1], df1$b1[df1[,paste0("wc",year)]==1], alpha=200, max = 255)

df1$rcol2<-"grey90"
df1$lcol2<-"grey90"
df1$rcol2[df1[,paste0("wc",year)]==1]<-rgb(df1$r2[df1[,paste0("wc",year)]==1], df1$g2[df1[,paste0("wc",year)]==1], df1$b2[df1[,paste0("wc",year)]==1], max = 255)
df1$lcol2[df1[,paste0("wc",year)]==1]<-rgb(df1$r2[df1[,paste0("wc",year)]==1], df1$g2[df1[,paste0("wc",year)]==1], df1$b2[df1[,paste0("wc",year)]==1], alpha=200, max = 255)

df1$rcol<-df1$rcol1
df1$lcol<-df1$lcol1

#replace whites
# df1$rcol[df1$rcol=="#FFFFFF"]<-df1$rcol2[df1$rcol=="#FFFFFF"]
# df1$lcol[df1$lcol=="#FFFFFFC8"]<-df1$lcol2[df1$lcol=="#FFFFFFC8"]

df1$adj<-0
# df1$adj[df1$country %in% c("Austria")]<- 8
# df1$adj[df1$country %in% c("Ukraine")]<- -5
# df1$adj[df1$country %in% c("U.A.E")]<- -20


#link info
#add sum values to df1, marking the x-position of the first links out (sum1) and in (sum2). Updated for further links in loop below.
# df1$sum1 <- colSums(m)
# df1$sum2 <- numeric(n)
df1$sum1 <- numeric(n)
df1$sum2 <- rowSums(m)

# library("plyr")
# df1$sum3 <- rowSums(m)+colSums(m)
# df1<-ddply(df1, .(region), mutate, csum=cumsum(sum3))
# df1<-ddply(df1, .(region), mutate, regsum=sum(sum3))
# df1<-ddply(df1, .(region), mutate, reglab=ifelse(regsum/2>csum, 0, 1))
# df1<-ddply(df1, .(region), mutate, reglab=cumsum(reglab)==1)
# df1<-arrange(df1, order)

source(paste0(wd,"do_ccircos.R"))
