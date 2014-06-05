wd<-"C:/Users/gabel/Documents/football/wcmig/"
library(XML)
library(stringr)

year<-2010
year<-2006
year<-2002
year<-2014
theurl <- paste0("http://en.wikipedia.org/wiki/",year,"_FIFA_World_Cup_squads")
doc <- htmlParse(theurl)
tableNodes = getNodeSet(doc, "//table")

df0<-NULL
for(i in 1:32){
  df0<-rbind(df0, readHTMLTable(tableNodes[[i*2]]))
}
names(df0)<-c("number","position","player","dob","caps","club")

info <-xpathSApply(doc,'//*[@class="mw-headline"]',xmlValue)[1:(32+8)]
df0$grp    <-rep(info[seq(1,32+8,5)],  each=23*4)
df0$country<-rep(info[-seq(1,32+8,5)], each=23)
df0$country[df0$country=="Korea Republic"]<-"South Korea"
df0$country[df0$country=="Korea DPR"]<-"North Korea"
df0$country[df0$country=="Côte d'Ivoire"]<-"Ivory Coast"
df0$country[df0$country=="Trinidad and Tobago"]<-"Trinidad & Tobago"
df0$country[df0$country=="Serbia and Montenegro"]<-"Serbia & Mont."
df0$country[df0$country=="China PR"]<-"China"
df0$country[df0$country=="Republic of Ireland"]<-"Ireland"
df0$country[df0$country=="Bosnia and Herzegovina"]<-"Bosnia & Herz."

df0$league<-NA
df0$league<- sapply( getNodeSet(doc, "//table[@class='sortable']/tr/td[6]//span[@class='flagicon']/a/@title"), xmlNode)
if(year==2010)
  df0$league[df0$club!="Unattached"]<- sapply( getNodeSet(doc, "//table[@class='sortable']/tr/td[6]//span[@class='flagicon']/a/@title"), xmlNode)
if(year==2006)
  df0$league<- sapply( getNodeSet(doc, "//table[@class='sortable']/tr/td[6]//span[@class='flagicon']/a/@title"), xmlNode)[-737]

df0$league <- as.character(df0$league)
df0$league <- substring(df0$league,14)
df0$league <- str_extract(df0$league, "^(.+?),")
df0$league <- substring(df0$league,1, nchar(df0$league)-2)
df0$league[is.na(df0$league)]<-"None"
df0$league[df0$league=="United Arab Emirates"]<-"U.A.E."
df0$league[df0$league=="Trinidad and Tobago"]<-"Trinidad & Tobago"
if(year<2010) df0$league[df0$league=="Serbia"]<-"Serbia & Mont."
df0$league[df0$league=="Bosnia and Herzegovina"]<-"Bosnia & Herz."

df0$position <- substring(df0$position,2,3)
df0$dob <- as.Date(substring(df0$dob,2,11))
head(df0)

write.csv(df0, paste0(wd,"wc",year,"sq.csv"), row.names=FALSE)