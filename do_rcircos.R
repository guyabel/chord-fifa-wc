library("circlize")
par(mar=rep(0,4), bg = "black")
circos.clear()
#m<-t(m)
#basic circos graphic parameters
circos.par(cell.padding=c(0,0,0,0), track.margin=c(0.0,0.0), start.degree = 0, gap.degree = 6)

#sector details
circos.initialize(factors = df1$region, xlim = cbind(df1$xmin, df1$xmax))

df1$sum1 <- numeric(n)
df1$sum2 <- rowSums(m)

#plot sectors
circos.trackPlotRegion(ylim = c(0, 1), factors = df1$region, track.height=0.15, bg.border = NA, bg.col = NA, bg.lty =0, bg.lwd=0.0001,
                       #panel.fun for each sector
                       panel.fun = function(x, y) {
                         #select details of current sector
                         name = get.cell.meta.data("sector.index")
                         i = get.cell.meta.data("sector.numeric.index")
                         xlim = get.cell.meta.data("xlim")
                         ylim = get.cell.meta.data("ylim")
                         
                         circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2], ytop=ylim[2], col = adjustcolor("grey50", 0.5), border=NA)  
                         
                         #plot labels
                         circos.text(x=mean(xlim), y=0.75, labels=name, direction = "arc", cex=1.2, col="white")
                         circos.text(x=mean(xlim), y=0.35, labels=df1$area[df1$region==name], direction = "arc", cex=0.7, col="white")
                         
                       })
text(-1,1,"2014 World Cup Squads", col="white", cex=1.4, pos=4)
text(-1,0.94,"Leagues to National Teams", col="white", cex=1, pos=4)
text(-1,0.89,"by Guy J. Abel", col="white", cex=0.7, pos=4)


text(-1,-1.01,c("Plot produced in R. Data from:"), pos=4, cex=0.5, col="white")
text(-1,-1.04,c("http://en.wikipedia.org/wiki/2014_FIFA_World_Cup_squads"), pos=4, cex=0.5, col="white")

text(1,-1.01,c("Further details see: http://gjabel.wordpress.com"), pos=2, cex=0.5, col="white")
text(1,-1.04,c("Twitter: @gjabel22"), pos=2, cex=0.5, col="white")

#plot sectors
circos.trackPlotRegion(ylim = c(0, 1), factors = df1$region, track.height=0.15, bg.border = NA, bg.col = NA, bg.lty =0, bg.lwd=0.0001,
                       #panel.fun for each sector
                       panel.fun = function(x, y) {
                         #select details of current sector
                         name = get.cell.meta.data("sector.index")
                         i = get.cell.meta.data("sector.numeric.index")
                         xlim = get.cell.meta.data("xlim")
                         ylim = get.cell.meta.data("ylim")
                         
                         #plot labels
                         #circos.text(x=mean(xlim), y=1.4, labels=name, direction = "arc", cex=1.8, col="white")
                         
                         #plot main sectors for origin
                         df1$sum1 <- numeric(n)
                         for(j in 1:n){
                           if(m[i,j]>0){
                             circos.rect(xleft=df1$sum1[i], ybottom=ylim[1], 
                                         xright=df1$sum1[i] + abs(m[i, j]), ytop=ylim[2], col = df1$rcol[j], border=NA)  
                             df1$sum1[i] = df1$sum1[i] + abs(m[i, j])
                           }
                         }
                         
                         #plot arrow heads for destination
                         df1$sum2 <- rowSums(m)
                         for(ii in 1:n){
                           if(colSums(m)[i]>0){
                             d1 = c(df1$sum1[i], df1$sum1[i] + (abs(m[ii, i]))/2, df1$sum1[i] + abs(m[ii, i]))
                             d2 = c(0, 0.35, 0)
                             circos.polygon(d1, d2, col = df1$rcol[i], border=NA)
                             
                             #circos.rect(xleft=d1[1], ybottom=ylim[2], xright=d1[3], ytop=ylim[2]-0.1, col = df1$rcol[i], border=NA)  
                             
                             df1$sum1[i] = df1$sum1[i] + abs(m[ii, i])
                           }
                         }
                       
                       })

##
##plot links
##
#add sum values to df1, marking the x-position of the first links out (sum1) and in (sum2). Updated for further links in loop below.
#reset culmative sums for links
df1$sum1 <- numeric(n)
df1$sum2 <- rowSums(m)

#create a data.frame of matrix sorted by element size, to allow largest plotted first
df2 <- cbind(as.data.frame(m),orig=rownames(m),  stringsAsFactors=FALSE)
df2 <- reshape(df2, idvar="orig", varying=list(1:n), direction="long", timevar="dest", time=rownames(m),  v.names = "m")
#df2 <- arrange(df2,desc(m))

#loose non zero links
df2 <- subset(df2, m>0)

#plot links
for(k in 1:nrow(df2)){
  #i,j reference of flow matrix
  i<-match(df2$orig[k],df1$region)
  j<-match(df2$dest[k],df1$region)
  
  #plot link
  circos.link(sector.index1=df1$region[i], point1=c(df1$sum1[i], df1$sum1[i] + abs(m[i, j])),
              sector.index2=df1$region[j], point2=c(df1$sum2[j], df1$sum2[j] + abs(m[i, j])),
              col = df1$rcol[j], top.ratio=0.66, top.ratio.low=0.67)
  
  #update sum1 and sum2 for use when plotting the next link
  df1$sum1[i] = df1$sum1[i] + abs(m[i, j])
  df1$sum2[j] = df1$sum2[j] + abs(m[i, j])
}



#save current plot to pdf
dev.copy2pdf(file = paste0(wd,"wcreg",year,".pdf"), height=10, width=10)
dev.copy(png, file = paste0(wd,"wcreg",year,".png"), width=10, height=10, units="in", res=600)
dev.off()

file.show(paste0(wd,"wcreg",year,".png"))
