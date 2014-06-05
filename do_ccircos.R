##
##plot sectors
##
library("circlize")
par(mar=rep(0,4), bg = "black")
circos.clear()

#basic circos graphic parameters
circos.par(cell.padding=c(0,0,0,0), track.margin=c(0,0), start.degree = 0, gap.degree = 2)

#sector details
circos.initialize(factors = df1$country, xlim = cbind(df1$xmin, df1$xmax))

#plot white sectors
circos.trackPlotRegion(ylim = c(0, 1), factors = df1$country, track.height=0.2, bg.border = NA, bg.col = NA, bg.lty =0, bg.lwd=0.0001,
                       #panel.fun for each sector
                       panel.fun = function(x, y) {
                         #select details of current sector
                         name = get.cell.meta.data("sector.index")
                         i = get.cell.meta.data("sector.numeric.index")
                         xlim = get.cell.meta.data("xlim")
                         ylim = get.cell.meta.data("ylim")
                         
                         circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2], ytop=ylim[2], 
                                     col = adjustcolor("white", 0), border=NA)
#                          if(df1$reglab[df1$country==name]){
#                            region = df1$region[df1$country==name]
#                            if(region!="None")
#                              circos.text(x=max(xlim), y=1, labels=region, direction = "arc", cex=10)#, adj=0.99)
#                          }
                       })

#add association labels
for(i in unique(df1$region)){
  if(i!="None"){
    df2 <- subset(df1, region==i)
    nn <- nrow(df2)
    x1 = get.cell.meta.data("cell.start.degree", df2$country[1] , 1)
    x2 = get.cell.meta.data("cell.end.degree",   df2$country[nn], 1)
    y2 = get.cell.meta.data("cell.top.radius",   df2$country[1] , 1)
    y1 = get.cell.meta.data("cell.bottom.radius",df2$country[nn], 1)
    draw.sector(start.degree = x1+0.5, end.degree = x2-0.5, rou1 = y2,  rou2 = y1, 
                col = adjustcolor("grey50", 0.5), border = NA)
    #  circos.text(x=(x2-x1)/2, y=1, labels=i, direction = "arc", cex=1)
  }
}
#show.index()


#plot sectors
circos.trackPlotRegion(ylim = c(0, 1), factors = df1$country, track.height=0.05, bg.border = NA, bg.col = NA, bg.lty =0, bg.lwd=0.0001,
                       #panel.fun for each sector
                       panel.fun = function(x, y) {
                         #select details of current sector
                         name = get.cell.meta.data("sector.index")
                         i = get.cell.meta.data("sector.numeric.index")
                         xlim = get.cell.meta.data("xlim")
                         ylim = get.cell.meta.data("ylim")
                         
                         #text direction (dd) and adjusmtents (aa)
                         theta = circlize(mean(xlim), 1.3)[1, 1] %% 360
                         dd <- ifelse(theta < 90 || theta > 270, "vertical_right", "vertical_left")
                         aa = c(1, 0.5)
                         if(theta < 90 || theta > 270)  aa =c(0, 0.5)
                         xx = mean(xlim)
                         if(name %in% df1$country[df1$adj!=0]) xx <- df1$adj[name==df1$country]
                         
                         #pdf
                         #circos.text(x=xx, y=1.1, labels=name, direction = dd, cex=5, adj=aa, col="white")  
                         
                         #png
                         circos.text(x=xx, y=1.1, labels=name, direction = dd, cex=0.6, adj=aa, col="white")  
                         
                         #plot main sectors for origin
                         df1$sum1 <- numeric(n)
                         for(j in 1:n){
                           if(m[i,j]>0){
                             circos.rect(xleft=df1$sum1[i], ybottom=ylim[1], 
                                         xright=df1$sum1[i] + abs(m[i, j]), ytop=ylim[2], col = df1$lcol[j], border=NA)  
                             df1$sum1[i] = df1$sum1[i] + abs(m[i, j])
                           }
                         }
                         
                         #plot arrow heads for destination
                         df1$sum2 <- rowSums(m)
                         for(ii in 1:n){
                           if(colSums(m)[i]>0){
                             d1 = c(df1$sum1[i], df1$sum1[i] + (abs(m[ii, i]))/2, df1$sum1[i] + abs(m[ii, i]))
                             d2 = c(0, 0.25, 0)
                             circos.polygon(d1, d2, col = df1$lcol[i], border=NA) 
                             df1$sum1[i] = df1$sum1[i] + abs(m[ii, i])
                           }
                         }
                         
                         #blank in part of main sector
                         #circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2]-rowSums(m)[i], ytop=ylim[1]+0.45, col = "white", border = "white")
                         #circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2], ytop=ylim[1]+0.45, col = "white", border = "white")
                         #circos.rect(xleft=xlim[2], ybottom=ylim[1], xright=xlim[2]-rowSums(m)[i], ytop=0.3, col = "white", border = "white")
                         
                         #white line all the way around
                         #circos.rect(xleft=xlim[1], ybottom=0.85, xright=xlim[2], ytop=1, col = "white", border = "white")
                         circos.rect(xleft=xlim[1], ybottom=0.85, xright=xlim[2], ytop=1, col = "black", border = "black")
                         
#                          if(name=="Spain"){
#                            circos.rect(xleft=xlim[2]-23+8, ybottom=0.9, xright=xlim[2], ytop=1, col = "white", border = "black")
#                          }
                           
                         
                         #plot axis
                         #circos.axis(labels.cex=0.4, major.at=seq(from=0,to=floor(df1$xmax)[i],by=15), labels.away.percentage = 0.15)
                       })

text(-1,1,"2014 World Cup Squads", col="white", cex=1.4, pos=4)
text(-1,0.94,"Leagues to National Teams", col="white", cex=1, pos=4)
text(-1,0.89,"by Guy J. Abel", col="white", cex=0.7, pos=4)

text(1,1.02,"How to Read:", col="white", cex=0.7, pos=2)
x=0.99 
text(1,x,     "Outgoing players are denoted by the colour of their national", col="white", cex=0.5, pos=2)
text(1,x-0.03,          "team. At base of a players arrow is the country of", col="white", cex=0.5, pos=2)
text(1,x-0.03*2,                 "their club team. At the head of the arrow", col="white", cex=0.5, pos=2)
text(1,x-0.03*3,                        "is the players national squad. The", col="white", cex=0.5, pos=2)
text(1,x-0.03*4,                          "thicker lines represent stonger", col="white", cex=0.5, pos=2)
text(1,x-0.03*5,                               "connections between leagues", col="white", cex=0.5, pos=2)
text(1,x-0.03*6,                                       "and national squads", col="white", cex=0.5, pos=2)


text(-1,-1.01,c("Plot produced in R. Data from:"), pos=4, cex=0.5, col="white")
text(-1,-1.04,c("http://en.wikipedia.org/wiki/2014_FIFA_World_Cup_squads"), pos=4, cex=0.5, col="white")

text(1,-1.01,c("Further details see: http://gjabel.wordpress.com"), pos=2, cex=0.5, col="white")
text(1,-1.04,c("Twitter: @gjabel22"), pos=2, cex=0.5, col="white")

#reset culmative sums for links
df1$sum1 <- numeric(n)
df1$sum2 <- rowSums(m)

#create a data.frame of matrix sorted by element size, to allow largest plotted first
df2 <- cbind(as.data.frame(m), orig=rownames(m),  stringsAsFactors=FALSE)
df2 <- reshape(df2, idvar="orig", varying=list(1:n), direction="long", timevar="dest", time=rownames(m),  v.names = "m")
#df2 <- arrange(df2,desc(m))

#loose non zero links
df2 <- subset(df2, m>0)

#plot links
for(k in 1:nrow(df2)){
#for(k in 1:1){
  #i,j reference of flow matrix
  i<-match(df2$orig[k],df1$country)
  j<-match(df2$dest[k],df1$country)
  
  #plot link
  circos.link(sector.index1=df1$country[i], point1=c(df1$sum1[i], df1$sum1[i] + abs(m[i, j])),
              sector.index2=df1$country[j], point2=c(df1$sum2[j], df1$sum2[j] + abs(m[i, j])),
              col = df1$lcol[j], top.ratio=0.65, top.ratio.low=0.67)

  #update sum1 and sum2 for use when plotting the next link
  df1$sum1[i] = df1$sum1[i] + abs(m[i, j])
  df1$sum2[j] = df1$sum2[j] + abs(m[i, j])
}

#save current plot to pdf
dev.copy2pdf(file = paste0(wd,"wc",year,".pdf"), height=100, width=100)

dev.copy(png, file = paste0(wd,"wc",year,".png"), width=10, height=10, units="in", res=600)
#dev.copy(png, file = paste0(wd,"wc",year,".png"), width=10, height=10, units="in", res=150)
dev.off()

file.show(paste0(wd,"wc",year,".png"))


# #out first
# blank in part of main sector
# circos.rect(min(xlim)+rowSums(m)[i], min(ylim), max(xlim), 0.3, col = "white", border = "white")

# #out first
# df1$sum1 <- numeric(n)
# df1$sum2 <- colSums(m)