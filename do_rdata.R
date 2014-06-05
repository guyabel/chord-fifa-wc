rm(list=ls())
wd<-"C:/Users/gabel/Documents/football/wcmig/"

year <- 2014
df0 <- read.csv(paste0(wd,paste0("wc",year,"sq.csv")))
df1 <- read.csv(paste0(wd,"iso.csv"), stringsAsFactors=FALSE)

m <- table(df0$league, df0$country)
#write.csv(m, paste0(wd,"m.csv"))
df1 <- subset(df1, country %in% unlist(dimnames(m)))

df0<-merge(df0, df1[,c("country","region")])
names(df0)[ncol(df0)]<-"cregion"
df1$league<-df1$country
df0<-merge(df0, df1[,c("region","league")])
names(df0)[ncol(df0)]<-"lregion"
df1$league<-NULL

##
##make regions
##
m <- table(df0$cregion, df0$lregion)
m<-as.matrix(m)

df1<-data.frame(region=unique(df1$region), rcol=c("red", "forestgreen", "yellow", "white","royalblue"),  stringsAsFactors=FALSE)
df1$area<-c("Asia", "North & Central America", "South America", "Africa", "Europe")
df1$lcol<-adjustcolor(df1$rcol, alpha=0.8)
df1$region <- factor(df1$region, levels=df1$region)

dimnames(m)<-list(league=levels(df1$region),squad=levels(df1$region))
m<-m[df1$region,df1$region]

subset(df0, cregion=="UEFA" & lregion!="UEFA")

##
##define ranges of circos sectors and their colors (both of the sectors and the links)
##
df1$xmin <- 0
df1$xmax <- rowSums(m)+colSums(m)
n <-nrow(df1)

df1$sum1 <- numeric(n)
df1$sum2 <- rowSums(m)

m<- t(m)

source(paste0(wd,"do_rcircos.R"))
