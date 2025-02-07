library(clevRvis)
library(png)

dir<-"C:/Manuscript/Figures/"

##UPN211
fracTable<-data.frame(TP1=c(96,75,0,0,0,0,0,0,0,0,0,0),
                      TP2=c(98,75,73,71,62,0,0,0,0,0,0,0),
                      TP3=c(98,76,74,71,52,0,0,0,0,0,0,0),
                      TP4=c(95,75,62,0,0,62,5,20,15,9,2,11))

parents <- c(0,1,2,3,4,3,6,6,6,6,6,6)
cloneLabels <- c(paste0("Clone 0",c(1:9)),paste0("Clone ",c(10:12)))
timepoints <-c(0,304,348,468)

seaObject <- createSeaObject(as.matrix(fracTable), parents, timepoints = timepoints, 
                             cloneLabels=cloneLabels, timepointInterpolation = F,originTimepoint = -304)

png(paste0(dir,"Figure5A_shark.png"),height = 350,width=1500)
esplot<-extSharkPlot(seaObject, main = "", showLegend = T,timepoints,
                     width = 16,interactivePlot = F)
print(esplot)
dev.off()



seaObject <- createSeaObject(as.matrix(fracTable), parents, timepoints = timepoints, 
                             cloneLabels=cloneLabels, timepointInterpolation = T,originTimepoint = -304)

png(paste0(dir,"Figure5A_dolphin.png"),height = 700,width=2500)
annotTbl <- data.frame(x = c(-197,-70,70,
                             170,255,
                             375,
                             446,431,431,433,433,438), 
                       y=c(50,50,50,
                           50,50,
                           82,
                           22,37,55,65,72,79), 
                       col = c('black',"black",'black',
                               "white","white",
                               "white",
                               "white","white","white","white","white","white"),
                       lab = c("del in 9p\ndel in 13p\ndel in 16p","dup20","dup16",
                               "PIK3CA","NT5C2",
                               "TP53 p.R248Q\nPIK3R1","FBXW7","PTEN p.T232fs","PTEN p.R234fs","TP53 p.G245R",
                               "TP53 p.G245S","TP53 p.G51*"))

dplot<-dolphinPlot(seaObject, shape = 'spline', borderCol = 'black', vlines = timepoints,
                   vlab =timepoints, vlabSize = 13, main = "UPN211: T-LBL pediatric relapsed", xlab = '',
                   ylab = '', pad.left = 0.001, showLegend = F,mainSize = 17,separateIndependentClones = T,
                   annotations = annotTbl,annotSize = c(14,14,13,13,13,10,10,9,9,9,9,9))
print(dplot)
dev.off()


plot1<-readPNG(paste0(dir,"Figure5A_shark.png"))

png(paste0(dir,"Figure5A_shark2.png"),height = 250,width=500)
layout(matrix(data=1,nrow=1))
par(mar=c(0,0,0,0))
plot(NULL,xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,1),bty="n")
rasterImage(plot1,-0.2,-0.2,2.7,1.2)
dev.off()



plot1<-readPNG(paste0(dir,"Figure5A_shark2.png"))
plot2<-readPNG(paste0(dir,"Figure5A_dolphin.png"))

png(paste0(dir,"Figure 5A.png"),height = 800,width=2200)
layout(matrix(data=1,nrow=1))
par(mar=c(0,0,0,0))
plot(NULL,xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,1),bty="n")
rasterImage(plot2,-0.25,0,1.05,1)
rasterImage(plot1,-0.02,-0.01,0.18,0.25)
dev.off()







##UPN190
fracTable<-data.frame(TP0=c(100,20,0,0,0,0,0,0,0),
                      TP1=c(100,96,81,60,30,5,20,0,0),
                      TP2=c(100,100,92,0,0,0,62,0,0),
                      TP3=c(100,100,100,0,0,0,32,68,40))

parents <- c(0,1,2,3,4,5,3,3,8)
cloneLabels <- paste0("Clone ",c(1:9))
timepoints <-c(-230,0,230,307)

seaObject <- createSeaObject(as.matrix(fracTable), parents, timepoints = timepoints, 
                             cloneLabels=cloneLabels, timepointInterpolation = F,originTimepoint = -330)
col(seaObject)[8]<-"#4d979b"
col(seaObject)[9]<-"#305e61"

png(paste0(dir,"Figure5B_shark.png"),height = 350,width=1500)
esplot<-extSharkPlot(seaObject, main = "", showLegend = T,timepoints,
                     width = 16,interactivePlot = F)
print(esplot)
dev.off()



seaObject <- createSeaObject(as.matrix(fracTable), parents, timepoints = timepoints, 
                             cloneLabels=cloneLabels, timepointInterpolation = T,originTimepoint = -330)
col(seaObject)[8]<-"#4d979b"
col(seaObject)[9]<-"#305e61"

png(paste0(dir,"Figure5B_dolphin.png"),height = 700,width=2500)
annotTbl <- data.frame(x = c(-320,-250,-195,-115,-38,50,100,260,266), 
                       y=c(50,50,50,45,49,38,71,92, 63), 
                       col = c("black",'black',"black",'white',"white","white","white","white","white"),
                       lab = c("LOH in 4q\n LOH in 6q\n LOH in 8q\n LOH in Xp\n LOH in Xq",
                               "LOH in 9p\n del in 1p","dup17","NOTCH1 p.Q2503*\n NOTCH1 p.V1578del","PIK3CD",
                               "USH2A p.T3635N\n MYB, EZH2\n KMT2C, PHF6","NOTCH1 p.V244fs\n SETD1B p.D458fs",
                               "dup in Xp, dup in Xp\ndel in 10q","        normal in 17p\n            normal 17p\n USH2A p.P3795Q\n  SETD1B p.S284*"))

dplot<-dolphinPlot(seaObject, shape = 'spline', borderCol = 'black', vlines = timepoints,
                   vlab =c("Germline",0,230,307), vlabSize = 13, main = "UPN190: T-LBL pediatric relapsed", xlab = '',
                   ylab = '', pad.left = 0.001, showLegend = F,mainSize = 17,separateIndependentClones = T,
                   annotations = annotTbl,annotSize = c(13,12.5,12,11,12,10,12,10,10))
print(dplot)
dev.off()



plot1<-readPNG(paste0(dir,"Figure5B_shark.png"))

png(paste0(dir,"Figure5B_shark2.png"),height = 250,width=500)
layout(matrix(data=1,nrow=1))
par(mar=c(0,0,0,0))
plot(NULL,xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,1),bty="n")
rasterImage(plot1,-0.2,-0.2,2.7,1.2)
dev.off()



plot1<-readPNG(paste0(dir,"Figure5B_shark2.png"))
plot2<-readPNG(paste0(dir,"Figure5B_dolphin.png"))

png(paste0(dir,"Figure 5B.png"),height = 800,width=2200)
layout(matrix(data=1,nrow=1))
par(mar=c(0,0,0,0))
plot(NULL,xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,1),bty="n")
rasterImage(plot2,-0.15,0.1,1.05,1.05)
rasterImage(plot1,-0.04,-0.02,0.15,0.238)
rect(-0.1,0.17,0.045,1,col="white",border = NA)
dev.off()





##UPN193
fracTable<-data.frame(TP1=c(98,90,86,47,0,0,0,0,0),
                      TP2=c(99,95,95,95,95,89,61.5,16,7))

parents <- c(0,1,2,3,4,5,6,7,8)
cloneLabels <- paste0("Clone ",c(1:9))
timepoints <-c(0,288)

seaObject <- createSeaObject(as.matrix(fracTable), parents, timepoints = timepoints, 
                             cloneLabels=cloneLabels, timepointInterpolation = F,originTimepoint = -288)

png(paste0(dir,"Figure5C_shark.png"),height = 350,width=1500)
esplot<-extSharkPlot(seaObject, main = "", showLegend = T,timepoints,
                     width = 16,interactivePlot = F)
print(esplot)
dev.off()



seaObject <- createSeaObject(as.matrix(fracTable), parents, timepoints = timepoints, 
                             cloneLabels=cloneLabels, timepointInterpolation = T,originTimepoint = -288)

png(paste0(dir,"Figure5C_dolphin.png"),height = 700,width=2500)
annotTbl <- data.frame(x = c(-240,-170,-96,-30,35, 90, 155,207,256), 
                       y=c(50,50,50,50,50,50, 50,50,50), 
                       col = c('black',"black",'black',"white","white","white","white","white","white"),
                       lab = c("LOH 9\ndel4","dup in 16p\ndup in 16p\ndup in 16q","dup in 16p\ndup20",
                               "dup in 7q","TP53\nSETD1B\nCREBBP\nUSH2A\nPIK3CA","ZBTB7A\nCCND3 p.L137*\nKMT2D",
                               "MED12","PTPRD","CCND3 p.L188I\nMYB"))

dplot<-dolphinPlot(seaObject, shape = 'spline', borderCol = 'black', vlines = timepoints,
                   vlab =timepoints, vlabSize = 13, main = "UPN193: T-LBL pediatric relapsed", xlab = '',
                   ylab = '', pad.left = 0.001, showLegend = F,mainSize = 17,separateIndependentClones = T,
                   annotations = annotTbl,annotSize = c(14,13,13,13,10,10,11,10.5,10))
print(dplot)
dev.off()


plot1<-readPNG(paste0(dir,"Figure5C_shark.png"))

png(paste0(dir,"Figure5C_shark2.png"),height = 250,width=500)
layout(matrix(data=1,nrow=1))
par(mar=c(0,0,0,0))
plot(NULL,xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,1),bty="n")
rasterImage(plot1,-0.2,-0.2,2.7,1.2)
dev.off()



plot1<-readPNG(paste0(dir,"Figure5C_shark2.png"))
plot2<-readPNG(paste0(dir,"Figure5C_dolphin.png"))

png(paste0(dir,"Figure 5C.png"),height = 800,width=2200)
layout(matrix(data=1,nrow=1))
par(mar=c(0,0,0,0))
plot(NULL,xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,1),bty="n")
rasterImage(plot2,-0.25,0,1.05,1)
rasterImage(plot1,-0.02,-0.01,0.18,0.25)
dev.off()






##UPN142
fracTable<-data.frame(TP1=c(80,72,56,38,30,17,11),
                      TP2=c(0,0,0,0,0,0,0))

parents <- c(0,1,2,3,4,5,3)
cloneLabels <- paste0("Clone ",c(1:7))
timepoints <-c(0,1)

seaObject <- createSeaObject(as.matrix(fracTable), parents, timepoints = timepoints, 
                             cloneLabels=cloneLabels, timepointInterpolation = F,originTimepoint = -1)


png(paste0(dir,"Figure5D_shark.png"),height = 350,width=1500)
esplot<-extSharkPlot(seaObject, main = "", showLegend = T,timepoints,
                     width = 16,interactivePlot = F)
print(esplot)
dev.off()



seaObject <- createSeaObject(as.matrix(fracTable), parents, timepoints = timepoints, 
                             cloneLabels=cloneLabels, timepointInterpolation = T,originTimepoint = -1)

png(paste0(dir,"Figure5D_dolphin.png"),height = 700,width=2500)
annotTbl <- data.frame(x = c(-0.9,-0.73,-0.55,-0.38,-0.16,0.1,-0.03), 
                       y=c(50,50,54,45,40,44,69), 
                       col = c('black',"black","black","white","white","white","white"),
                       lab = c("dup6\ndel in 6q\nNOTCH1","dup20\nBCL11B","DDX3X p.S28fs",
                               "PTEN p.E235fs","PTEN p.R233fs","FBXW7\nSMARCA4","DDX3X p.S654fs\nPTEN p.N228fs"))

dplot<-dolphinPlot(seaObject, shape = 'spline', borderCol = 'black', vlines = timepoints[1],
                   vlab = timepoints[1], vlabSize = 13, main = "UPN142: T-LBL pediatric not relapsed", xlab = '',
                   ylab = '', pad.left = 0.001, showLegend = F,mainSize = 17,separateIndependentClones = T,
                   annotations = annotTbl,annotSize = c(13,12,11.5,11,11,11,11))
print(dplot)
dev.off()




plot1<-readPNG(paste0(dir,"Figure5D_shark.png"))

png(paste0(dir,"Figure5D_shark2.png"),height = 250,width=500)
layout(matrix(data=1,nrow=1))
par(mar=c(0,0,0,0))
plot(NULL,xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,1),bty="n")
rasterImage(plot1,-0.2,-0.2,2.7,1.2)
dev.off()



plot1<-readPNG(paste0(dir,"Figure5D_shark2.png"))
plot2<-readPNG(paste0(dir,"Figure5D_dolphin.png"))

png(paste0(dir,"Figure 5D.png"),height = 800,width=2200)
layout(matrix(data=1,nrow=1))
par(mar=c(0,0,0,0))
plot(NULL,xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,1),bty="n")
rasterImage(plot2,-0.25,0,1.05,1)
rasterImage(plot1,-0.02,-0.01,0.18,0.25)
for(i in seq(0.65,1.1,0.001)){
  alf<-1/length(seq(0.65,1.1,0.001))
  rect(i,0,i+0.01,0.8,col = rgb(1,1,1,alpha = min(alf*which(i==seq(0.65,1.1,0.001)),1)),border = NA)
}
dev.off()











