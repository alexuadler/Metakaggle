setwd("~/GitHub/NYCDSA/Personal Projects/KaggleMeta")

###### Animation ########
require(dplyr)
require(ggplot2)
require(lubridate)
require(animation) # to make the animated GIF
require(grid) # to keep the time stamp static

# Info about the competition
plFile<-"axa-PL.csv" # download and unzip the PL file and place it in the working directory
PLHist<-read.csv(plFile)
PLHist$SubmissionDate<-ymd_hms(PLHist$SubmissionDate)

# is a higher score better? (i.e. ROC)
higherBetter<-T 

# teamName<-"Vivi's Angels"
# teamID<-PLHist[which(PLHist$TeamName==teamName),1][1]

# Start of the competition (midnight UTC)
start<-min(PLHist$SubmissionDate)+days(1)
hour(start)<-0
minute(start)<-0
second(start)<-0

# Latest day of the competition (midnight UTC)
latest<-max(PLHist$SubmissionDate)+days(1)
hour(latest)<-0
minute(latest)<-0
second(latest)<-0

# totalTeams<-length(unique(PLHist$TeamId))

# daysIn is the number of days into the competition
# yMin and yMax describe the score window you'd like to see
# higherBetter decides whether higher scores are better or lower scores are better
# PLHist is the dataframe with the PL history
# startDate is the start date of the competition

genPlot=function(daysIn,yMin=0,yMax=1,higherBetter=T,PLHist,startDate){
  thisTime<-startDate+days(daysIn)
  thisPL<-PLHist %>%
    filter(SubmissionDate<thisTime) %>%
    group_by(TeamId) %>%
    summarise(bestScore=ifelse(higherBetter,max(Score),min(Score)))
  
  if(higherBetter){
    thisPL<-arrange(thisPL,desc(bestScore))
  } else{
    thisPL<-arrange(thisPL,desc(bestScore))
  }
  
  # Add the day's ranks
  thisPL<-thisPL %>%
    group_by(1:n())
  names(thisPL)[3]<-"rank"
  
  # teamScore<-NA
  # teamRank<-NA
  # if(teamID %in% thisPL$TeamId){
  #   teamScore<-as.numeric(thisPL[which(thisPL$TeamId==teamID),2])
  #   teamRank<-as.numeric(thisPL[which(thisPL$TeamId==teamID),3])
  # }
  
  timeStamp <- grobTree(textGrob(as.character(thisTime), x=0.1,  y=0.95, hjust=0,
                                 gp=gpar(col="red", fontsize=18, fontface="bold")))
  
  p<-ggplot(data=thisPL)+
    geom_point(aes(y=bestScore,x=rank),size=1)+
    ylim(c(yMin,yMax))+
    scale_x_reverse()+
    #   xlim(c(totalTeams,0))+
    xlab("Rank")+
    ylab("Score")+
    annotation_custom(timeStamp)+
    theme_bw()
  
  print(p)
}

pl.animate <- function() {
  lapply(seq(0,as.numeric(latest-start),1), function(i) {
    genPlot(i,PLHist=PLHist,startDate=start,yMax=1,yMin=0.4)
  })
}

saveGIF(pl.animate(), interval = .3, movie.name="axa.gif")

