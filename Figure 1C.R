library(openxlsx)
library(stringr)
library(png)

dir<-"C:/Manuscript/Figures/"
threshold<-0.1

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 1,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-ALL"
temp$Category<-"adult"
temp$Relapse<-FALSE
temp$NewID<-paste0("A_B_A_a_",temp$ID)
temp$IMI.ID<-str_split_fixed(temp$ID,"_",n=Inf)[,1]
input<-temp

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 2,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-ALL"
temp$Category<-"adult"
temp$Relapse<-TRUE
helper<-str_split_fixed(temp$ID,"_",n=Inf)[,2]
temp$NewID<-paste0("A_B_B_",helper,"_",temp$ID)
temp$IMI.ID<-str_split_fixed(temp$ID,"_",n=Inf)[,1]
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 3,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-ALL"
temp$Category<-"pediatric"
temp$Relapse<-FALSE
temp$NewID<-paste0("A_A_A_a_",temp$ID)
temp$IMI.ID<-str_split_fixed(temp$ID,"_",n=Inf)[,1]
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 4,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-ALL"
temp$Category<-"pediatric"
temp$Relapse<-TRUE
helper<-str_split_fixed(temp$ID,"_",n=Inf)[,2]
temp$NewID<-paste0("A_A_B_",helper,"_",temp$ID)
temp$IMI.ID<-str_split_fixed(temp$ID,"_",n=Inf)[,1]
input<-rbind(input,temp)


temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 5,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-LBL"
temp$Category<-"adult"
temp$Relapse<-FALSE
temp$NewID<-paste0("B_B_A_a_",temp$ID)
temp$IMI.ID<-str_split_fixed(temp$ID,"_",n=Inf)[,1]
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 6,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-LBL"
temp$Category<-"adult"
temp$Relapse<-TRUE
helper<-str_split_fixed(temp$ID,"_",n=Inf)[,2]
temp$NewID<-paste0("B_B_B_",helper,"_",temp$ID)
temp$IMI.ID<-str_split_fixed(temp$ID,"_",n=Inf)[,1]
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 7,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-LBL"
temp$Category<-"pediatric"
temp$Relapse<-FALSE
temp$NewID<-paste0("B_A_A_a_",temp$ID)
temp$IMI.ID<-str_split_fixed(temp$ID,"_",n=Inf)[,1]
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 8,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-LBL"
temp$Category<-"pediatric"
temp$Relapse<-TRUE
helper<-str_split_fixed(temp$ID,"_",n=Inf)[,2]
temp$NewID<-paste0("B_A_B_",helper,"_",temp$ID)
temp$IMI.ID<-str_split_fixed(temp$ID,"_",n=Inf)[,1]
input<-rbind(input,temp)

input<-input[grep("_r",input$ID,invert = T),]
input<-input[input$Category!="adult",]


overview<-read.xlsx(paste0(dir,"/Supplemental Data 0.xlsx"),sheet = 1,startRow = 2)
input$Time<-NA

for(i in 1:length(input[,1])){
  input$Time[i]<-max(overview[overview$ID==input$IMI.ID[i],c(13,16,19,20)],na.rm=T)
}



input[!is.na(input[,13])&input[,13]<threshold,c(2:17)]<-NA
input<-unique(input)



output<-input[,c("Gene","Disease","Category","Relapse","ID","NewID","Time")]
output<-unique(output)
output$Status<-0
output$Status[output$Relapse==T]<-1






gene<-"NOTCH1"

message(gene)
test<-output[grep(gene,output$Gene),]
found<-unique(test$NewID)
output2<-data.frame(Group=NA,ID=output$NewID,Status=output$Status,
                    Time=output$Time,Disease=output$Disease)
output2<-unique(output2)
output2$Group_helper[output2$ID%in%found]<-paste0(gene," (mut)")
output2$Group_helper[!output2$ID%in%found]<-paste0(gene," (wt)")

output2$Group<-paste0(output2$Disease," pediatric ",output2$Group_helper)

output2b<-output2[output2$Disease=="T-LBL",]
output2b$Time2<-output2b$Time/365
cir_neu<-as.data.frame.matrix(table(output2b$Status,output2b$Group_helper))
cir_neu<-cir_neu/rowSums(cir_neu)

png(paste0(dir,"NOTCH1_TLBL.png"),width=1200,height=400)
par(mar=c(6,11,5,11),mgp=c(7,3.7,0))
barplot(t(as.matrix(cir_neu)),beside = F,names.arg = c("primary\n not relapsed","primary\n relapsed"),
        yaxt="n",ylab="Frequency",xlim=c(0.2,5.8),cex.lab = 3,col=c("lightblue2","deepskyblue3"),cex.names = 2.4)
par(mgp=c(2.7,0.9,0))
axis(2,at=seq(0,1,0.2),lab=paste0(seq(0,100,20),"%"),las=2,cex.axis=2.3,font=2)
points(x=c(0.1,2.6),y=c(0,0),type="l")
dens_helper1<-density(output2b$Time2[output2b$Status==1&output2b$Group_helper=="NOTCH1 (mut)"],bw = "SJ")
dens_helper2<-density(output2b$Time2[output2b$Status==1&output2b$Group_helper=="NOTCH1 (wt)"],bw = "SJ")
points(x=3.5+2*dens_helper1$x/4,
       y=dens_helper1$y/max(dens_helper1$y,dens_helper2$y),
       type="l",lwd=8,col="lightblue2")
points(x=3.5+2*dens_helper2$x/4,
       y=dens_helper2$y/max(dens_helper1$y,dens_helper2$y),
       type="l",lwd=8,col="deepskyblue3")
par(mgp=c(2.7,1.3,-0.2))
axis(1,at=seq(3.5,5.5,length.out=3),labels = c(0,2,4),cex.axis=2.6)
points(x=c(3.1,5.85),y=c(0,0),type="l")
text(x=4.5,y=-0.31,"Time to relapse [years]",cex=2.6,xpd=NA)
axis(4,at=seq(0,1,0.2),las=2,cex.axis=2.3,font=2)
text(x=6.8,y=0.5,"Normalized density",cex=3,srt=90,xpd=NA)
dev.off()
  
