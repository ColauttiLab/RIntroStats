library(ggplot2)
Time<-18
Ind<-135

VacDat<-data.frame(NULL)


for(sex in c("M","F")){
  for(i in 1:Ind){
    tmp<-data.frame(
      ID=rep(Ind,Time),
      Time=c(0:(Time-1)),
      Sex=rep(sex,Time)
    )
    tmp$IgG<-rnorm(1,sd=3)+rnorm(1,mean=-2)*tmp$Time+rnorm(tmp$Time)
      
    if(sex=="M"){
      tmp$IgG<-tmp$IgG+2
    }
    VacDat<-rbind(VacDat,tmp)
    tmp<-NA

  }
}

VacDat$IgG<-VacDat$IgG - min(VacDat$IgG)

qplot(Time,IgG,colour=Sex,data=VacDat)

write.csv(VacDat,"./Data/ImmuneData.csv",row.names=F)
