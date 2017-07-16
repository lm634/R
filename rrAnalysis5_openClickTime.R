#-  PLOT OPENS AND CLICKS AGAINST TIME SINCE CAMPAIGN INITIATION
#-
#-  this script generates an analysis of the open- and click-time
#-  for campaigns, showing when the best time to close a mailshot
#-  due to diminishing returns is.
#-
#-  it generates plots of opens by group against time, and a plot
#-  showing the rates for all campaigns

#=============================================>
# work space and working directory setup 
#=============================================>
#set up directory

rm(list=ls())
library(ggplot2)
library(zoo)

wd1 = "/Users/.../campaigns/csvExports/"
wd2 = "/Users/.../plots/chronological/"
wd3 = "/Users/.../granular_activity/opens/"
wd4 = "/Users/.../granular_activity/clicks/"

setwd(wd1)
directory = read.csv("fileDirectory.csv", sep = ",")

rR1 <<- NA
rR2 <<- NA
rR3 <<- NA
rR4 <<- NA
rR5 <<- NA

#=============================================>
# define functions
#=============================================>
#---------------------------------------------
#- function: generate and save plot for
#- campaign open rates

# make tables for each segment within campaign
allPlots = function(segmentationPath, workingDirectory, savePath){
  for (i in 1:length(directory$X)) {
    if (directory[i,2]==segmentationPath) {
      listname = directory$listName[i]
      setwd(wd3)
      #set up instances
      dat1 = read.csv(as.character(directory$filePathKey[i]), sep = ",")
      dat1$asDate = NA
      dat1$instance = 1
      dat1$Timestamp = as.POSIXlt(dat1$Timestamp)
      dat1$asDate = hour=cut(as.POSIXct(dat1$Timestamp), "hour")
      dat1 = data.frame(date = as.POSIXct(dat1$asDate),instance = dat1$instance)
      #fill in time sequence gaps
      timeSeq = seq(from = dat1$date[1], to = dat1$date[length(dat1$date)], by = "hour")
      timeSeq = data.frame(date = timeSeq, instance = 0)
      fullSet = rbind(dat1,timeSeq)
      #aggregate
      finalDat = aggregate(fullSet$instance, by = list(time=fullSet$date),FUN = sum)
      finalDat$time = as.POSIXlt(finalDat$time)
      #save to appropriate name-space
      if (listname == "Charity 1") {rR1 <<- data.frame(date=finalDat$time, instance=finalDat$x,GROUP="RR1")}
      if (listname == "Charity 2") {rR2 <<- data.frame(date=finalDat$time, instance=finalDat$x,GROUP="RR2")}
      if (listname == "Charity 3") {rR3 <<- data.frame(date=finalDat$time, instance=finalDat$x,GROUP="RR3")}
      if (listname == "Charity 4") {rR4 <<- data.frame(date=finalDat$time, instance=finalDat$x,GROUP="RR4")}
      if (listname == "Charity 5") {rR5 <<- data.frame(date=finalDat$time, instance=finalDat$x,GROUP="RR5")}
    }
  }
  #wd
  fileName = paste(gsub('^.{0}|.{4}$','',segmentationPath),'.pdf',sep = "")
  dir.create(file.path(wd2, "aggregatedData"), showWarnings = FALSE)
  setwd(file.path(wd2, "aggregatedData"))
  #aggregate
  collectedDat <<- rbind(rR1,rR2,rR3,rR4,rR5)
  collectedDat <<- aggregate(collectedDat$instance, by = list(time=collectedDat$date),FUN=sum)
  collectedPlot = ggplot(data=collectedDat, aes(collectedDat$time,collectedDat$x)) + geom_line()
    labs(title = segmentationPath, x="date/time", y="opens")
  ggsave(fileName, plot = last_plot(), device = NULL, width = 15, height = 8)
  #plot
  subDir = gsub('^.{0}|.{4}$','',segmentationPath)
  dir.create(file.path(wd2, subDir), showWarnings = FALSE)
  setwd(file.path(wd2, subDir))
  fileName = paste(gsub('^.{0}|.{4}$', "" ,segmentationPath),'_RR1.pdf',sep = "")
  groupPlot = ggplot(data=rR1, aes(rR1$date, rR1$instance)) + geom_line() + labs(title=fileName, x="date/time", y="opens")
  ggsave(fileName, plot = last_plot(), device = NULL, width = 15, height = 8)
  fileName = paste(gsub('^.{0}|.{4}$', "" ,segmentationPath),'_RR2.pdf',sep = "")
  groupPlot = ggplot(data=rR2, aes(rR2$date, rR2$instance)) + geom_line() + labs(title=fileName, x="date/time", y="opens")
  ggsave(fileName, plot = last_plot(), device = NULL, width = 15, height = 8)
  fileName = paste(gsub('^.{0}|.{4}$', "" ,segmentationPath),'_RR3.pdf',sep = "")
  groupPlot = ggplot(data=rR3, aes(rR3$date, rR3$instance)) + geom_line() + labs(title=fileName, x="date/time", y="opens")
  ggsave(fileName, plot = last_plot(), device = NULL, width = 15, height = 8)
  fileName = paste(gsub('^.{0}|.{4}$', "" ,segmentationPath),'_RR4.pdf',sep = "")
  groupPlot = ggplot(data=rR4, aes(rR4$date, rR4$instance)) + geom_line() + labs(title=fileName, x="date/time", y="opens")
  ggsave(fileName, plot = last_plot(), device = NULL, width = 15, height = 8)
  fileName = paste(gsub('^.{0}|.{4}$', "" ,segmentationPath),'_RR5.pdf',sep = "")
  groupPlot = ggplot(data=rR5, aes(rR5$date, rR5$instance)) + geom_line() + labs(title=fileName, x="date/time", y="opens")
  ggsave(fileName, plot = last_plot(), device = NULL, width = 15, height = 8)
}


main = function(segmentationPath){
  allPlots(segmentationPath)
}

#=============================================>
# run the script and clear working space
#=============================================>

for (i in seq(1,length(directory$segmentationPath),5)) {
  main(as.character(directory$segmentationPath[i]))
}

rm(list=ls())

