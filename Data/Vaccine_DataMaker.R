library(ggplot2)
Time<-18
Ind<-135
Family<-c(1:8)
FamMean<-rnorm(length(Family),sd=3) # Family means
FamSlope<-rnorm(length(Family),mean=-2) # Family slopes

VacDat<-data.frame(NULL)


for(sex in c("M","F")){
  for(i in 1:Ind){
      tmpFam<-sample(c(1:length(Family)),1) # Randomly sample family
      
      tmp<-data.frame( # Make data frame
        FamID=rep(Family[tmpFam],Time),
        IndID=rep(i,Time),
        Time=c(0:(Time-1)),
        Sex=rep(sex,Time)
    )
    tmp$IgG<-rnorm(1,sd=3) + # Individual Intercept
      FamMean[tmpFam] + # Family intercept
      rnorm(1,mean=-2)*tmp$Time + # Individual slope
      FamSlope[tmpFam]*tmp$Time + # Family slope
      rnorm(tmp$Time) # Measurement Error
      
    if(sex=="M"){
      tmp$IgG<-2 + 1.2*tmp$IgG # Higher IgG and steeper slope For Males
    }
    VacDat<-rbind(VacDat,tmp)
    tmp<-NA
    tmpFam<-NA
  }
}

VacDat$IgG<-VacDat$IgG - min(VacDat$IgG)
VacDat<-VacDat %>%
  arrange(FamID,IndID,Time)

qplot(Time,IgG,colour=Sex,data=VacDat)

write.csv(VacDat,"./Data/ImmuneData.csv",row.names=F)
