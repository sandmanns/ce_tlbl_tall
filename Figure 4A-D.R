library(stringr)
library(openxlsx)
library(stringr)

dir<-"C:/Manuscript/Figures/"

###4 TP
temp<-read.xlsx(paste0(dir,"Supplemental Data 3.xlsx"),sheet = 1,startRow = 3)

temp<-temp[,c(1:17)]
temp<-temp[temp$CCF.P!=0,]

temp$VariantNew<-NA
i<-1
while(i<=length(temp[,1])){
  if(length(grep("(",temp$Variant[i],fixed = T))==0){
    ##CNV
    if(length(grep(" in ",temp$Variant[i],fixed = T))>0){
      ## just insert _ no further variants
      temp$VariantNew[i]<-gsub(" ","_",temp$Variant[i],fixed=T)
    }
    if(length(grep(" in ",temp$Variant[i],fixed = T))==0&&length(grep("_in_",temp$VariantNew[i],fixed = T))==0&&
       (substr(temp$Variant[i],nchar(temp$Variant[i]),nchar(temp$Variant[i]))=="p"||substr(temp$Variant[i],nchar(temp$Variant[i]),nchar(temp$Variant[i]))=="q")){
      ##whole p or q arm affected -> add variant with "in" to the bottom
      temp$VariantNew[i]<-paste0(substr(temp$Variant[i],1,3),"_in_",substr(temp$Variant[i],4,nchar(temp$Variant[i])))
    }
    if(length(grep(" in ",temp$Variant[i],fixed = T))==0&&length(grep("_in_",temp$VariantNew[i],fixed = T))==0&&
       substr(temp$Variant[i],nchar(temp$Variant[i]),nchar(temp$Variant[i]))!="p"&&substr(temp$Variant[i],nchar(temp$Variant[i]),nchar(temp$Variant[i]))!="q"){
      ##whole chr affected -> add variant with only p and q to the bottom
      temp$VariantNew[i]<-paste0(substr(temp$Variant[i],1,3),"_in_",substr(temp$Variant[i],4,4),"p")
      helper<-as.data.frame(temp[i,])
      helper$VariantNew[1]<-paste0(substr(temp$Variant[i],1,3),"_in_",substr(temp$Variant[i],4,4),"q")
      temp<-rbind(temp,helper)
    }
  }else{
    temp$VariantNew[i]<-strsplit(temp$Variant[i],split = " ")[[1]][1]
  }
  
  i<-i+1
}
temp<-temp[order(temp$Clone,temp$CCF.G,temp$CCF.P,temp$CCF.R1,temp$CCF.R2,temp$CCF.R3,temp$VariantNew),]
temp2<-temp[,c(1,2,3,18,5,15,16,17)]
temp2$Relapse<-T


####generate Input
temp2<-temp2[order(temp2$Clone,temp2$VariantNew),]

helper_out<-data.frame(Disease=temp2$Disease[1],Category=temp2$Category[1],Relapse=temp2$Relapse[1],ID=temp2$ID[1],Relations="")
for(i in 1:(length(temp2[,1])-1)){
  zweig<-temp2$Clone[i]
  for(j in (i+1):(length(temp2[,1]))){
    if(temp2$Clone[i]==temp2$Clone[j]){
      ##same clone -?-
      helper_out$Relations[1]<-paste0(helper_out$Relations[1]," ",temp2$VariantNew[i],"-/-",temp2$VariantNew[j])
    }
    if(temp2$Clone[i]!=temp2$Clone[j]&&temp2$Nested[i]!=temp2$Nested[j]){
      ##different clone, different nested level, so direct descendant
      ##check parents to check whether really in the same branch
      if(temp2$Parent[j]%in%zweig){
        zweig<-c(zweig,temp2$Clone[j])
        helper_out$Relations[1]<-paste0(helper_out$Relations[1]," ",temp2$VariantNew[i],"->-",temp2$VariantNew[j])
      }#else{
        ##successively, but on different branches
      #}
    }
    #if(temp2$Clone[i]!=temp2$Clone[j]&&temp2$Nested[i]==temp2$Nested[j]){
      ##different clone, same nested level, so developed in parallel
    #}
  }
}
helper_split<-as.vector(str_split_fixed(helper_out$Relations[1],pattern = " ",n=Inf))
helper_split<-unique(helper_split)
helper_split2<-str_split_fixed(helper_split,"-",Inf)
helper_split2<-helper_split2[-1,-2]

helper_all<-unique(expand.grid(temp2$VariantNew,temp2$VariantNew))
helper_all$Var1<-as.character(helper_all$Var1)
helper_all$Var2<-as.character(helper_all$Var2)
helper_all2<-data.frame(helper_all,Disease=helper_out[1,1],Category=helper_out[1,2],
                        Relapse=helper_out[1,3],ID=helper_out[1,4])
for(i in 1:length(helper_all2[,1])){
  helper_all2$SameClone[i]<-sum((helper_split2[,1]==helper_all2$Var1[i]&helper_split2[,2]==helper_all2$Var2[i])|
                                  (helper_split2[,2]==helper_all2$Var1[i]&helper_split2[,1]==helper_all2$Var2[i]))>0
}
helper_all3<-helper_all2[!(helper_all2$Var1==helper_all2$Var2&helper_all2$SameClone==F),]

helper_split2<-str_split_fixed(helper_split,"-",Inf)
helper_split2<-helper_split2[helper_split2[,1]!=""&helper_split2[,2]!="/",]
for(i in 1:length(helper_all3[,1])){
  helper_all3$Nachfahre[i]<-sum((helper_split2[,1]==helper_all3$Var1[i]&helper_split2[,3]==helper_all3$Var2[i]))>0
}

out_4<-helper_all3





###3 TP
input<-read.xlsx(paste0(dir,"Supplemental Data 3.xlsx"),sheet = 2,startRow = 3)
input<-input[input$Category=="pediatric",]
ids<-unique(input[,c(1,2,3)])

for(k in 1:length(ids[,1])){
  temp<-input[input$Disease==ids$Disease[k]&input$Category==ids$Category[k]&input$ID==ids$ID[k],c(1:15)]
  temp$CCF.P[is.na(temp$CCF.P)]<-27
  temp<-temp[temp$CCF.P!=0,]
  temp$VariantNew<-NA
  i<-1
  while(i<=length(temp[,1])){
    if(length(grep("(",temp$Variant[i],fixed = T))==0){
      ##CNV
      if(length(grep(" in ",temp$Variant[i],fixed = T))>0){
        ## just insert _ no further variants
        temp$VariantNew[i]<-gsub(" ","_",temp$Variant[i],fixed=T)
      }
      if(length(grep(" in ",temp$Variant[i],fixed = T))==0&&length(grep("_in_",temp$VariantNew[i],fixed = T))==0&&
         (substr(temp$Variant[i],nchar(temp$Variant[i]),nchar(temp$Variant[i]))=="p"||substr(temp$Variant[i],nchar(temp$Variant[i]),nchar(temp$Variant[i]))=="q")){
        ##whole p or q arm affected -> add variant with "in" to the bottom
        temp$VariantNew[i]<-paste0(substr(temp$Variant[i],1,3),"_in_",substr(temp$Variant[i],4,nchar(temp$Variant[i])))
      }
      if(length(grep(" in ",temp$Variant[i],fixed = T))==0&&length(grep("_in_",temp$VariantNew[i],fixed = T))==0&&
         substr(temp$Variant[i],nchar(temp$Variant[i]),nchar(temp$Variant[i]))!="p"&&substr(temp$Variant[i],nchar(temp$Variant[i]),nchar(temp$Variant[i]))!="q"){
        ##whole chr affected -> add variant with only p and q to the bottom
        temp$VariantNew[i]<-paste0(substr(temp$Variant[i],1,3),"_in_",substr(temp$Variant[i],4,4),"p")
        helper<-as.data.frame(temp[i,])
        helper$VariantNew[1]<-paste0(substr(temp$Variant[i],1,3),"_in_",substr(temp$Variant[i],4,4),"q")
        temp<-rbind(temp,helper)
      }
    }else{
      temp$VariantNew[i]<-strsplit(temp$Variant[i],split = " ")[[1]][1]
    }
    
    i<-i+1
  }
  temp<-temp[order(temp$Clone,temp$CCF.G,temp$CCF.P,temp$CCF.R1,temp$CCF.R2,temp$VariantNew),]
  temp2<-temp[,c(1,2,3,16,5,13,14,15)]
  temp2$Relapse<-T

  
  ####generate Input
  temp2<-temp2[order(temp2$Clone,temp2$VariantNew),]
  
  helper_out<-data.frame(Disease=temp2$Disease[1],Category=temp2$Category[1],Relapse=temp2$Relapse[1],ID=temp2$ID[1],Relations="")
  for(i in 1:(length(temp2[,1])-1)){
    zweig<-temp2$Clone[i]
    for(j in (i+1):(length(temp2[,1]))){
      if(temp2$Clone[i]==temp2$Clone[j]){
        ##same clone -?-
        helper_out$Relations[1]<-paste0(helper_out$Relations[1]," ",temp2$VariantNew[i],"-/-",temp2$VariantNew[j])
      }
      if(temp2$Clone[i]!=temp2$Clone[j]&&temp2$Nested[i]!=temp2$Nested[j]){
        ##different clone, different nested level, so direct descendant
        ##check parents to check whether really in the same branch
        if(temp2$Parent[j]%in%zweig){
          zweig<-c(zweig,temp2$Clone[j])
          helper_out$Relations[1]<-paste0(helper_out$Relations[1]," ",temp2$VariantNew[i],"->-",temp2$VariantNew[j])
        }#else{
          ##successively, but on different branches
        #}
      }
      #if(temp2$Clone[i]!=temp2$Clone[j]&&temp2$Nested[i]==temp2$Nested[j]){
        ##different clone, same nested level, so developed in parallel
      #}
    }
  }
  helper_split<-as.vector(str_split_fixed(helper_out$Relations[1],pattern = " ",n=Inf))
  helper_split<-unique(helper_split)
  helper_split2<-str_split_fixed(helper_split,"-",Inf)
  helper_split2<-helper_split2[-1,-2]
  
  helper_all<-unique(expand.grid(temp2$VariantNew,temp2$VariantNew))
  helper_all$Var1<-as.character(helper_all$Var1)
  helper_all$Var2<-as.character(helper_all$Var2)
  helper_all2<-data.frame(helper_all,Disease=helper_out[1,1],Category=helper_out[1,2],
                          Relapse=helper_out[1,3],ID=helper_out[1,4])
  for(i in 1:length(helper_all2[,1])){
    helper_all2$SameClone[i]<-sum((helper_split2[,1]==helper_all2$Var1[i]&helper_split2[,2]==helper_all2$Var2[i])|
                                    (helper_split2[,2]==helper_all2$Var1[i]&helper_split2[,1]==helper_all2$Var2[i]))>0
  }
  helper_all3<-helper_all2[!(helper_all2$Var1==helper_all2$Var2&helper_all2$SameClone==F),]
  
  helper_split2<-str_split_fixed(helper_split,"-",Inf)
  helper_split2<-helper_split2[helper_split2[,1]!=""&helper_split2[,2]!="/",]
  for(i in 1:length(helper_all3[,1])){
    helper_all3$Nachfahre[i]<-sum((helper_split2[,1]==helper_all3$Var1[i]&helper_split2[,3]==helper_all3$Var2[i]))>0
  }
  
  if(k==1){
    out_3<-helper_all3
  }else{
    out_3<-rbind(out_3,helper_all3)
  }
}





###2 TP
input<-read.xlsx(paste0(dir,"Supplemental Data 3.xlsx"),sheet = 3,startRow = 3)
input<-input[input$Category=="pediatric",]
ids<-unique(input[,c(1,2,3)])

for(k in 1:length(ids[,1])){
  temp<-input[input$Disease==ids$Disease[k]&input$Category==ids$Category[k]&input$ID==ids$ID[k],c(1:13)]
  temp<-temp[temp$CCF.P!=0,]
  temp$VariantNew<-NA
  i<-1
  while(i<=length(temp[,1])){
    if(length(grep("(",temp$Variant[i],fixed = T))==0){
      ##CNV
      if(length(grep(" in ",temp$Variant[i],fixed = T))>0){
        ## just insert _ no further variants
        temp$VariantNew[i]<-gsub(" ","_",temp$Variant[i],fixed=T)
      }
      if(length(grep(" in ",temp$Variant[i],fixed = T))==0&&length(grep("_in_",temp$VariantNew[i],fixed = T))==0&&
         (substr(temp$Variant[i],nchar(temp$Variant[i]),nchar(temp$Variant[i]))=="p"||substr(temp$Variant[i],nchar(temp$Variant[i]),nchar(temp$Variant[i]))=="q")){
        ##whole p or q arm affected -> add variant with "in" to the bottom
        temp$VariantNew[i]<-paste0(substr(temp$Variant[i],1,3),"_in_",substr(temp$Variant[i],4,nchar(temp$Variant[i])))
      }
      if(length(grep(" in ",temp$Variant[i],fixed = T))==0&&length(grep("_in_",temp$VariantNew[i],fixed = T))==0&&
         substr(temp$Variant[i],nchar(temp$Variant[i]),nchar(temp$Variant[i]))!="p"&&substr(temp$Variant[i],nchar(temp$Variant[i]),nchar(temp$Variant[i]))!="q"){
        ##whole chr affected -> add variant with only p and q to the bottom
        temp$VariantNew[i]<-paste0(substr(temp$Variant[i],1,3),"_in_",substr(temp$Variant[i],4,4),"p")
        helper<-as.data.frame(temp[i,])
        helper$VariantNew[1]<-paste0(substr(temp$Variant[i],1,3),"_in_",substr(temp$Variant[i],4,4),"q")
        temp<-rbind(temp,helper)
      }
    }else{
      temp$VariantNew[i]<-strsplit(temp$Variant[i],split = " ")[[1]][1]
    }
    
    i<-i+1
  }
  temp<-temp[order(temp$Clone,temp$CCF.G,temp$CCF.P,temp$CCF.R1,temp$VariantNew),]
  temp2<-temp[,c(1,2,3,14,5,11,12,13)]
  temp2$Relapse<-T
  
  ####generate Input
  temp2<-temp2[order(temp2$Clone,temp2$VariantNew),]
  
  helper_out<-data.frame(Disease=temp2$Disease[1],Category=temp2$Category[1],Relapse=temp2$Relapse[1],ID=temp2$ID[1],Relations="")
  for(i in 1:(length(temp2[,1])-1)){
    zweig<-temp2$Clone[i]
    for(j in (i+1):(length(temp2[,1]))){
      if(temp2$Clone[i]==temp2$Clone[j]){
        ##same clone -?-
        helper_out$Relations[1]<-paste0(helper_out$Relations[1]," ",temp2$VariantNew[i],"-/-",temp2$VariantNew[j])
      }
      if(temp2$Clone[i]!=temp2$Clone[j]&&temp2$Nested[i]!=temp2$Nested[j]){
        ##different clone, different nested level, so direct descendant
        ##check parents to check whether really in the same branch
        if(temp2$Parent[j]%in%zweig){
          zweig<-c(zweig,temp2$Clone[j])
          helper_out$Relations[1]<-paste0(helper_out$Relations[1]," ",temp2$VariantNew[i],"->-",temp2$VariantNew[j])
        }#else{
          ##successively, but on different branches
        #}
      }
      #if(temp2$Clone[i]!=temp2$Clone[j]&&temp2$Nested[i]==temp2$Nested[j]){
        ##different clone, same nested level, so developed in parallel
      #}
    }
  }
  helper_split<-as.vector(str_split_fixed(helper_out$Relations[1],pattern = " ",n=Inf))
  helper_split<-unique(helper_split)
  helper_split2<-str_split_fixed(helper_split,"-",Inf)
  helper_split2<-as.data.frame(matrix(helper_split2[-1,-2],ncol=2))
  names(helper_split2)<-c("X1","X2")
  
  helper_all<-unique(expand.grid(temp2$VariantNew,temp2$VariantNew))
  helper_all$Var1<-as.character(helper_all$Var1)
  helper_all$Var2<-as.character(helper_all$Var2)
  helper_all2<-data.frame(helper_all,Disease=helper_out[1,1],Category=helper_out[1,2],
                          Relapse=helper_out[1,3],ID=helper_out[1,4])
  
  if(length(helper_split2[,1])>1){
    for(i in 1:length(helper_all2[,1])){
      helper_all2$SameClone[i]<-sum((helper_split2[,1]==helper_all2$Var1[i]&helper_split2[,2]==helper_all2$Var2[i])|
                                      (helper_split2[,2]==helper_all2$Var1[i]&helper_split2[,1]==helper_all2$Var2[i]))>0
    }
    helper_all3<-helper_all2[!(helper_all2$Var1==helper_all2$Var2&helper_all2$SameClone==F),]
    helper_split2<-str_split_fixed(helper_split,"-",Inf)
    helper_split2<-helper_split2[helper_split2[,1]!=""&helper_split2[,2]!="/",]
    for(i in 1:length(helper_all3[,1])){
      helper_all3$Nachfahre[i]<-sum((helper_split2[,1]==helper_all3$Var1[i]&helper_split2[,3]==helper_all3$Var2[i]))>0
    }
  }else{
    helper_all3<-helper_all2
    helper_all3$SameClone<-F
    helper_all3$Nachfahre<-F
  }
  
  if(k==1){
    out_2<-helper_all3
  }else{
    out_2<-rbind(out_2,helper_all3)
  }
}





###1 TP
input<-read.xlsx(paste0(dir,"Supplemental Data 3.xlsx"),sheet = 4,startRow = 3)
input<-input[input$Category=="pediatric",]
ids<-unique(input[,c(1,2,3)])

for(k in 1:length(ids[,1])){
  temp<-input[input$Disease==ids$Disease[k]&input$Category==ids$Category[k]&input$ID==ids$ID[k],c(1:11)]
  temp<-temp[temp$CCF.P!=0,]
  temp$VariantNew<-NA
  i<-1
  while(i<=length(temp[,1])){
    if(length(grep("(",temp$Variant[i],fixed = T))==0){
      ##CNV
      if(length(grep(" in ",temp$Variant[i],fixed = T))>0){
        ## just insert _ no further variants
        temp$VariantNew[i]<-gsub(" ","_",temp$Variant[i],fixed=T)
      }
      if(length(grep(" in ",temp$Variant[i],fixed = T))==0&&length(grep("_in_",temp$VariantNew[i],fixed = T))==0&&
         (substr(temp$Variant[i],nchar(temp$Variant[i]),nchar(temp$Variant[i]))=="p"||substr(temp$Variant[i],nchar(temp$Variant[i]),nchar(temp$Variant[i]))=="q")){
        ##whole p or q arm affected -> add variant with "in" to the bottom
        temp$VariantNew[i]<-paste0(substr(temp$Variant[i],1,3),"_in_",substr(temp$Variant[i],4,nchar(temp$Variant[i])))
      }
      if(length(grep(" in ",temp$Variant[i],fixed = T))==0&&length(grep("_in_",temp$VariantNew[i],fixed = T))==0&&
         substr(temp$Variant[i],nchar(temp$Variant[i]),nchar(temp$Variant[i]))!="p"&&substr(temp$Variant[i],nchar(temp$Variant[i]),nchar(temp$Variant[i]))!="q"){
        ##whole chr affected -> add variant with only p and q to the bottom
        temp$VariantNew[i]<-paste0(substr(temp$Variant[i],1,3),"_in_",substr(temp$Variant[i],4,4),"p")
        helper<-as.data.frame(temp[i,])
        helper$VariantNew[1]<-paste0(substr(temp$Variant[i],1,3),"_in_",substr(temp$Variant[i],4,4),"q")
        temp<-rbind(temp,helper)
      }
    }else{
      temp$VariantNew[i]<-strsplit(temp$Variant[i],split = " ")[[1]][1]
    }
    
    i<-i+1
  }
  temp<-temp[order(temp$Clone,temp$CCF.G,temp$CCF.P,temp$VariantNew),]
  temp2<-temp[,c(1,2,3,12,5,9,10,11)]
  temp2$Relapse<-F
  
  ####generate Input
  temp2<-temp2[order(temp2$Clone,temp2$VariantNew),]
  
  helper_out<-data.frame(Disease=temp2$Disease[1],Category=temp2$Category[1],Relapse=temp2$Relapse[1],ID=temp2$ID[1],Relations="")
  for(i in 1:(length(temp2[,1])-1)){
    zweig<-temp2$Clone[i]
    for(j in (i+1):(length(temp2[,1]))){
      if(temp2$Clone[i]==temp2$Clone[j]){
        ##same clone -?-
        helper_out$Relations[1]<-paste0(helper_out$Relations[1]," ",temp2$VariantNew[i],"-/-",temp2$VariantNew[j])
      }
      if(temp2$Clone[i]!=temp2$Clone[j]&&temp2$Nested[i]!=temp2$Nested[j]){
        ##different clone, different nested level, so direct descendant
        ##check parents to check whether really in the same branch
        if(temp2$Parent[j]%in%zweig){
          zweig<-c(zweig,temp2$Clone[j])
          helper_out$Relations[1]<-paste0(helper_out$Relations[1]," ",temp2$VariantNew[i],"->-",temp2$VariantNew[j])
        }#else{
          ##successively, but on different branches
        #}
      }
      #if(temp2$Clone[i]!=temp2$Clone[j]&&temp2$Nested[i]==temp2$Nested[j]){
        ##different clone, same nested level, so developed in parallel
      #}
    }
  }
  helper_split<-as.vector(str_split_fixed(helper_out$Relations[1],pattern = " ",n=Inf))
  helper_split<-unique(helper_split)
  helper_split2<-str_split_fixed(helper_split,"-",Inf)
  helper_split2<-as.data.frame(matrix(helper_split2[-1,-2],ncol=2))
  names(helper_split2)<-c("X1","X2")
  
  helper_all<-unique(expand.grid(temp2$VariantNew,temp2$VariantNew))
  helper_all$Var1<-as.character(helper_all$Var1)
  helper_all$Var2<-as.character(helper_all$Var2)
  helper_all2<-data.frame(helper_all,Disease=helper_out[1,1],Category=helper_out[1,2],
                          Relapse=helper_out[1,3],ID=helper_out[1,4])
  
  if(length(helper_split2[,1])>1){
    for(i in 1:length(helper_all2[,1])){
      helper_all2$SameClone[i]<-sum((helper_split2[,1]==helper_all2$Var1[i]&helper_split2[,2]==helper_all2$Var2[i])|
                                      (helper_split2[,2]==helper_all2$Var1[i]&helper_split2[,1]==helper_all2$Var2[i]))>0
    }
    helper_all3<-helper_all2[!(helper_all2$Var1==helper_all2$Var2&helper_all2$SameClone==F),]
    helper_split2<-str_split_fixed(helper_split,"-",Inf)
    helper_split2<-helper_split2[helper_split2[,1]!=""&helper_split2[,2]!="?"&helper_split2[,2]!="/",]
    for(i in 1:length(helper_all3[,1])){
      helper_all3$Nachfahre[i]<-sum((helper_split2[,1]==helper_all3$Var1[i]&helper_split2[,3]==helper_all3$Var2[i]))>0
    }
  }else{
    helper_all3<-helper_all2
    helper_all3$SameClone<-F
    helper_all3$Nachfahre<-F
  }
  
  if(k==1){
    out_1<-helper_all3
  }else{
    out_1<-rbind(out_1,helper_all3)
  }
}






output<-rbind(out_4,out_3,out_2,out_1)

output2b<-output


namen1<-c("A","B")
namen2<-c("T-ALL pediatric (n=","T-LBL pediatric (n=")

subgroups<-list()
subgroups[[1]]<-output2b[output2b$Disease=="T-ALL"&output2b$Category=="pediatric",]
subgroups[[2]]<-output2b[output2b$Disease=="T-LBL"&output2b$Category=="pediatric",]

cut_off<-c(3,5)


test<-unique(output2b[,c(1,3:6)])
test$HelpID<-paste0(test$Disease,test$Category,test$Relapse,test$ID)
test2<-as.data.frame.matrix(table(test[,c(1,2)]))
test2<-test2[test2$`T-ALL`>=cut_off[1]|test2$`T-LBL`>=cut_off[2],]
genes<-row.names(test2)


######################################
#Co-occurrence of mutations in samples

for(n in 1:2){
  subgroup1<-subgroups[[n]]
  samples<-length(unique(subgroup1[,c("Relapse","ID")])[,1])

  subgroup2<-subgroup1
  subgroup2$keep<-F
  for(i in 1:length(subgroup2[,1])){
    if((subgroup2$Var1[i]%in%genes)&(subgroup2$Var2[i]%in%genes)){
      subgroup2$keep[i]<-T
    }
  }
  subgroup2<-subgroup2[subgroup2$keep==T,]
  temp_size<-as.data.frame.matrix(table(subgroup2$Var1,subgroup2$Var2))
  
  varianten<-genes
  
  varianten_helper<-data.frame(Variante=genes,Type="SNV",Chr=NA,Name=NA)
  varianten_helper$Type[grep("LOH|del|dup",varianten_helper$Variante)]<-"CNV"
  varianten_helper$Chr<-as.numeric(gsub("p|q","",str_split_fixed(varianten_helper$Variante,"_",Inf)[,3]))
  varianten_helper$Name[varianten_helper$Type=="SNV"]<-varianten_helper$Variante[varianten_helper$Type=="SNV"]
  varianten_helper<-varianten_helper[order(varianten_helper$Type,varianten_helper$Chr,varianten_helper$Name),]
  
  varianten<-rev(varianten_helper$Variante)
  
  farben<-colorRampPalette(c("blue","skyblue1","salmon1","firebrick"))(101)
  png(paste0(dir,"Figure4",namen1[n],"_middle.png"),
      width = 1500,height=1500)
  par(mar=c(19,20,0.5,0.5),mgp=c(16,1,0))
  plot(NULL,xlim=c(0.5,length(varianten)+0.5),ylim=c(0.5,length(varianten)+0.5),
       xlab="Variant 2",ylab="Variant 1",xaxt="n",yaxt="n",xaxs="i",yaxs="i",cex.lab=5)
  axis(1,at=c(1:length(varianten)),labels = varianten,las=2,tick=F,cex.axis=3)
  axis(2,at=c(1:length(varianten)),labels = varianten,las=2,tick=F,cex.axis=3)
  points(x=c(0.5,length(varianten)+0.5),y=c(0.5,length(varianten)+0.5),type="l",col="grey70",lwd=3)
  for(i in 1:length(varianten)){
    for(j in 1:length(varianten)){
      groesse<-sum(varianten[i]==subgroup2$Var1&varianten[j]==subgroup2$Var2)
      #if(groesse>cut_off[n]){
      faerben<-sum(varianten[i]==subgroup2$Var1&varianten[j]==subgroup2$Var2&subgroup2$SameClone==T)
      points(x=i,y=j,
             cex=ifelse(groesse==1,0.5,ifelse(groesse==2,1,1+15*groesse/samples)),
             col=farben[round(100*faerben/groesse,0)+1],pch=16)
      #}
    }
  }
  for(i in 1:(length(varianten)+1)){
    points(x=c(0.5,length(varianten)+0.5),y=c(i-0.5,i-0.5),type="l",col="grey85")
    points(y=c(0.5,length(varianten)+0.5),x=c(i-0.5,i-0.5),type="l",col="grey85")
  }
  points(x=c(0.5,length(varianten)+0.5),y=c(sum(varianten_helper$Type=="SNV")+0.5,sum(varianten_helper$Type=="SNV")+0.5),
         col="black",type="l")
  points(x=c(0.5,length(varianten)+0.5),y=c(0.5,0.5),type="l",col="black")
  points(x=c(0.5,length(varianten)+0.5),y=c(length(varianten)+0.5,length(varianten)+0.5),type="l",col="black")
  points(y=c(0.5,length(varianten)+0.5),x=c(sum(varianten_helper$Type=="SNV")+0.5,sum(varianten_helper$Type=="SNV")+0.5),
         col="black",type="l")
  points(y=c(0.5,length(varianten)+0.5),x=c(0.5,0.5),type="l",col="black")
  points(y=c(0.5,length(varianten)+0.5),x=c(length(varianten)+0.5,length(varianten)+0.5),type="l",col="black")
  
  dev.off()
  
  png(paste0(dir,"Figure4",namen1[n],"_right.png"),width = 200,height=1500)
  par(mar=c(19,0.5,0.5,0.5),mgp=c(3,1,0))
  barplot(test2[match(varianten,rownames(test2)),n],horiz=T,col="black",
          yaxt="n",yaxs="i",cex.axis=2)
  dev.off()
  
  png(paste0(dir,"Figure4",namen1[n],"_top.png"),width = 1500,height=300)
  par(mar=c(0.5,20,0,0.5),mgp=c(3,1,0))
  barplot(test2[match(varianten,rownames(test2)),n],horiz=F,col="black",
          xaxt="n",xaxs="i",cex.main=5,las=1,cex.axis=2,ylim=c(0,40),yaxt="n")
  text(x=13,y=36,paste0(namen2[n],samples,")"),cex=5.2,font=2)
  axis(2,at=seq(0,30,10),las=1,cex.axis=2)
  
  legend("topright",legend = c("10%","20%","30%"),
         pch=16,pt.cex=15*c(0.1,0.2,0.3),bty="n",border=NA,cex=3,title="Same sample",title.font = 2,title.cex = 3)
  dev.off()
  
  
  png(paste0(dir,"Figure4",namen1[n],"_legend.png"),width =200,height=300)
  par(mar=c(0,0,0,0))
  plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xaxs="i",yaxs="i")
  legend("topright",legend = c("100%","75%","50%","25%","0%"),
         fill=farben[c(101,75,50,25,1)],bty="n",border=NA,cex=3,title="Same clone",title.font = 2,title.cex = 2.8)
  dev.off()
  
  
  plot1<-readPNG(paste0(dir,"Figure4",namen1[n],"_middle.png"))
  plot2<-readPNG(paste0(dir,"Figure4",namen1[n],"_right.png"))
  plot3<-readPNG(paste0(dir,"Figure4",namen1[n],"_top.png"))
  plot4<-readPNG(paste0(dir,"Figure4",namen1[n],"_legend.png"))
  
  png(paste0(dir,"Figure 4",namen1[n],".png"),width = 1700,height=1800)
  layout(matrix(data=c(rep(3,15),4,4,
                       rep(3,15),4,4,
                       rep(3,15),4,4,
                       rep(c(rep(1,15),2,2),15)),nrow=18,byrow = T))
  par(mar=c(0,0,0,0))
  plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xaxs="i",yaxs="i")
  rasterImage(plot1,0,0,1,1)
  plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xaxs="i",yaxs="i")
  rasterImage(plot2,0,0,1,1)
  plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xaxs="i",yaxs="i")
  rasterImage(plot3,0,0,1,1)
  plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xaxs="i",yaxs="i")
  rasterImage(plot4,0,0,1,1)
  dev.off()
}






#####################################
#Mutational order in clonal evolution

namen1<-c("C","D")

for(n in 1:2){
  subgroup1<-subgroups[[n]]
  
  subgroup1<-subgroups[[n]]
  samples<-length(unique(subgroup1[,c("Relapse","ID")])[,1])

  subgroup2<-subgroup1
  subgroup2$keep<-F
  for(i in 1:length(subgroup2[,1])){
    if((subgroup2$Var1[i]%in%genes)&(subgroup2$Var2[i]%in%genes)){
      subgroup2$keep[i]<-T
    }
  }
  subgroup2<-subgroup2[subgroup2$keep==T,]
  temp_size<-as.data.frame.matrix(table(subgroup2$Var1,subgroup2$Var2))
  
  varianten<-genes
  
  varianten_helper<-data.frame(Variante=varianten,Type="SNV",Chr=NA,Name=NA)
  varianten_helper$Type[grep("LOH|del|dup",varianten_helper$Variante)]<-"CNV"
  varianten_helper$Chr<-as.numeric(gsub("p|q","",str_split_fixed(varianten_helper$Variante,"_",Inf)[,3]))
  varianten_helper$Name[varianten_helper$Type=="SNV"]<-varianten_helper$Variante[varianten_helper$Type=="SNV"]
  varianten_helper<-varianten_helper[order(varianten_helper$Type,varianten_helper$Chr,varianten_helper$Name),]
  varianten<-rev(varianten_helper$Variante)
  
  farben<-colorRampPalette(c("blue","skyblue1","salmon1","firebrick"))(101)
  png(paste0(dir,"Figure4",namen1[n],"_middle.png"),
      width = 1500,height=1500)
  par(mar=c(19,20,0.5,0.5),mgp=c(16,1,0))
  plot(NULL,xlim=c(0.5,length(varianten)+0.5),ylim=c(0.5,length(varianten)+0.5),
       xlab="Descendant",ylab="Ancestor",xaxt="n",yaxt="n",xaxs="i",yaxs="i",cex.lab=5)
  axis(1,at=c(1:length(varianten)),labels = varianten,las=2,tick=F,cex.axis=3)
  axis(2,at=c(1:length(varianten)),labels = varianten,las=2,tick=F,cex.axis=3)
  points(x=c(0.5,length(varianten)+0.5),y=c(0.5,length(varianten)+0.5),type="l",col="grey70",lwd=3)
  for(i in 1:length(varianten)){
    for(j in 1:length(varianten)){
      groesse<-sum(varianten[i]==subgroup2$Var1[subgroup2$SameClone==T]&varianten[j]==subgroup2$Var2[subgroup2$SameClone==T])
      faerben<-sum(varianten[i]==subgroup2$Var1&varianten[j]==subgroup2$Var2&subgroup2$Nachfahre==T)
      points(x=j,y=i,
             cex=ifelse(groesse==1,0.5,ifelse(groesse==2,1,1+15*groesse/samples)),
             col=farben[round(100*faerben/groesse,0)+1],pch=16)
    }
  }
  for(i in 1:(length(varianten)+1)){
    points(x=c(0.5,length(varianten)+0.5),y=c(i-0.5,i-0.5),type="l",col="grey85")
    points(y=c(0.5,length(varianten)+0.5),x=c(i-0.5,i-0.5),type="l",col="grey85")
  }
  points(x=c(0.5,length(varianten)+0.5),y=c(sum(varianten_helper$Type=="SNV")+0.5,sum(varianten_helper$Type=="SNV")+0.5),
         col="black",type="l")
  points(x=c(0.5,length(varianten)+0.5),y=c(0.5,0.5),type="l",col="black")
  points(x=c(0.5,length(varianten)+0.5),y=c(length(varianten)+0.5,length(varianten)+0.5),type="l",col="black")
  points(y=c(0.5,length(varianten)+0.5),x=c(sum(varianten_helper$Type=="SNV")+0.5,sum(varianten_helper$Type=="SNV")+0.5),
         col="black",type="l")
  points(y=c(0.5,length(varianten)+0.5),x=c(0.5,0.5),type="l",col="black")
  points(y=c(0.5,length(varianten)+0.5),x=c(length(varianten)+0.5,length(varianten)+0.5),type="l",col="black")
  
  dev.off()
  
  
  png(paste0(dir,"Figure4",namen1[n],"_right.png"),width = 200,height=1500)
  par(mar=c(19,0.5,0.5,0.5),mgp=c(3,1,0))
  barplot(test2[match(varianten,rownames(test2)),n],horiz=T,col="black",
          yaxt="n",yaxs="i",cex.axis=2)
  dev.off()
  
  png(paste0(dir,"Figure4",namen1[n],"_top.png"),width = 1500,height=300)
  par(mar=c(0.5,20,0,0.5),mgp=c(3,1,0))
  barplot(test2[match(varianten,rownames(test2)),n],horiz=F,col="black",
          xaxt="n",xaxs="i",cex.main=4,las=1,cex.axis=2,ylim=c(0,40),yaxt="n")
  text(x=13,y=36,paste0(namen2[n],samples,")"),cex=5.2,font=2)
  axis(2,at=seq(0,30,10),las=1,cex.axis=2)
  
  legend("topright",legend = c("10%","20%","30%"),
         pch=16,pt.cex=15*c(0.1,0.2,0.3),bty="n",border=NA,cex=3,title="Same clone",title.font = 2,title.cex = 3)
  dev.off()
  
  png(paste0(dir,"Figure4",namen1[n],"_legend.png"),width =200,height=300)
  par(mar=c(0,0,0,0))
  plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xaxs="i",yaxs="i")
  legend("topright",legend = c("100%","75%","50%","25%","0%"),
         fill=farben[c(101,75,50,25,1)],bty="n",border=NA,cex=3,title="Ancestor",title.font = 2,title.cex = 2.8)
  dev.off()
  
  
  
  plot1<-readPNG(paste0(dir,"Figure4",namen1[n],"_middle.png"))
  plot2<-readPNG(paste0(dir,"Figure4",namen1[n],"_right.png"))
  plot3<-readPNG(paste0(dir,"Figure4",namen1[n],"_top.png"))
  plot4<-readPNG(paste0(dir,"Figure4",namen1[n],"_legend.png"))
  
  png(paste0(dir,"Figure 5",namen1[n],".png"),width = 1700,height=1800)
  layout(matrix(data=c(rep(3,15),4,4,
                       rep(3,15),4,4,
                       rep(3,15),4,4,
                       rep(c(rep(1,15),2,2),15)),nrow=18,byrow = T))
  par(mar=c(0,0,0,0))
  plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xaxs="i",yaxs="i")
  rasterImage(plot1,0,0,1,1)
  plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xaxs="i",yaxs="i")
  rasterImage(plot2,0,0,1,1)
  plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xaxs="i",yaxs="i")
  rasterImage(plot3,0,0,1,1)
  plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xaxs="i",yaxs="i")
  rasterImage(plot4,0,0,1,1)
  dev.off()
}






