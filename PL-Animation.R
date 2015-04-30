###### Animation ########
require(lubridate)
require(animation)
require(grid)
# Info about the competition
### download the zip file and place it in the working directory
### go to the public leaderboard page and download the file at the end
plFile<-"axa-PL.csv"
animFile<-"axa-vivi.gif"

PLHist<-read.csv(plFile)
PLHist$SubmissionDate<-ymd_hms(PLHist$SubmissionDate)
higherBetter<-T # is a higher score better? (i.e. ROC)

#### Which team are you competing on ###
teamName<-"Vivi's Angels"
teamID<-PLHist[which(PLHist$TeamName==teamName),1][1]



# Start of the competition (last second of that day)
start<-min(PLHist$SubmissionDate)+days(1)
hour(start)<-0
minute(start)<-0
second(start)<-0

# Start of the competition (last second of that day)
latest<-max(PLHist$SubmissionDate)+days(1)
hour(latest)<-0
minute(latest)<-0
second(latest)<-0

# totalTeams<-length(unique(PLHist$TeamId))

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
  
  teamScore<-ifelse(teamID %in% thisPL$TeamId,as.numeric(thisPL[which(thisPL$TeamId==teamID),2]),NA)
  teamRank<-ifelse(teamID %in% thisPL$TeamId,as.numeric(thisPL[which(thisPL$TeamId==teamID),3]),NA)
  
  timeStamp <- grobTree(textGrob(as.character(thisTime), x=0.1,  y=0.95, hjust=0,
                                 gp=gpar(col="red", fontsize=18, fontface="bold")))
  
  p<-ggplot(data=thisPL)+
    geom_point(aes(y=bestScore,x=rank),size=2)+
    ylim(c(yMin,yMax))+
    scale_x_reverse()+
    #   xlim(c(totalTeams,0))+
    xlab("Rank")+
    ylab("Score")+
    annotation_custom(timeStamp)+
    theme_bw()
  
  if(is.na(teamRank)) print(p)
  if(!is.na(teamRank)) print(p+geom_hline(yintercept=teamScore,color="red",alpha=0.7)+geom_vline(xintercept=teamRank,color="red",alpha=0.7))

}

animInt=1

pl.animate <- function() {
  lapply(seq(0,as.numeric(latest-start),animInt), function(i) {
    genPlot(i,PLHist=PLHist,startDate=start,yMax=1,yMin=0.4)
  })
}

saveGIF(pl.animate(), interval = .25, movie.name=animFile)

