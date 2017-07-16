#-  GENERATE OTHER CAMPAIGN ANALYSIS
#-
#-  this script generates cumulative descriptive analysis
#-  for RR mail-out campaigns, and saves them to a formatted .xlsx file.
#-
#-  the script is set up to run using raw mailChimp data, however requires
#-  segmentation by campaign, so only the relevant data should be included
#-  in the campaign .csv file (see runAnalysis function).

#---------------------------------------------
# work space and working directory setup 
#---------------------------------------------

rm(list=ls())

library(reshape2)
library(XLConnect)
library(ggplot2)

wd = "/Users/.../campaigns/csvExports/segmentationReports/"
setwd(wd)
segsList = list.files(wd)

wd = "/Users/.../campaigns/csvExports/"
setwd(wd)

masterDat1 = read.csv("cumulNonSegmented.csv", sep = ",")
masterDat1 = masterDat1[,-c(1)]
masterDat1$Campaign.Name = as.character(masterDat1$Campaign.Name)
#renames three identical entries
masterDat1$Campaign.Name[9] = 'changeIsPossible01'
masterDat1$Campaign.Name[13] = 'changeIsPossible02'
masterDat1$Campaign.Name[18] = 'changeIsPossible03'
j = 11
for (i in seq(1,length(masterDat1$Campaign.Name),1)) {
  masterDat1$Campaign.Name[i] = (paste(as.character(j),"_",masterDat1$Campaign.Name[i],sep=""))
  j = j + 1
}
#masterDat1[,11] = gsub('|.{9}$', '', masterDat1[,11])
#for (i in 1:length(masterDat1)) { masterDat1[i,11] = strptime(masterDat1[i,11], "%b %d, %Y") }

masterDat2 = read.csv("cumulSegmentation.csv", sep = ",")
MDcolNames = c("metric","1P","2B","3R","4M","5D")
colnames(masterDat2) = MDcolNames
iRange = c(2,3,4,5,6)
for (i in iRange) {
  
  masterDat2[3,i] = round((as.numeric(masterDat2[2,i])/as.numeric(masterDat2[1,i]))*100, digits = 2)
  masterDat2[5,i] = round((as.numeric(masterDat2[4,i])/as.numeric(masterDat2[1,i]))*100, digits = 2)
  masterDat2[7,i] = round((as.numeric(masterDat2[6,i])/as.numeric(masterDat2[2,i]))*100, digits = 2)
  masterDat2[10,i] = round((as.numeric(masterDat2[9,i])/as.numeric(masterDat2[6,i]))*100, digits = 2)
  
}

rm(iRange, i, j, MDcolNames)

#---------------------------------------------
# functions
#---------------------------------------------

#-----------------------------------------
#- PLOT KEY STATS ACROSS CAMPAIGNS - REAL NUMBERS

cumulCampaigns = function() {

  plot1 = ggplot(masterDat1, aes(masterDat1$Campaign.Name)) + 
    geom_line(aes(y = masterDat1$Successful.Deliveries, stat = "identity", group = 1, show.legend = FALSE), color = "darkgreen", size = 0.8) +
    geom_line(aes(y = masterDat1$Recipients.Who.Opened, stat = "identity", group = 1, show.legend = FALSE), color = "firebrick1", size = 0.8) +
    geom_line(aes(y = masterDat1$Recipients.Who.Clicked, stat = "identity", group = 1, show.legend = FALSE), color = "royalblue3", size = 0.8) +
    theme(plot.title = element_text(size=10, face="bold", margin = margin(10, 0, 10, 0)), axis.text.x = element_text(angle = 70, hjust = 1),
          legend.title = element_blank(), legend.background = element_rect(fill="grey97", size=0.5, linetype="solid"),
          legend.key=element_rect(fill= "white")) +
    geom_hline(yintercept = mean(masterDat1$Recipients.Who.Opened), linetype = "dashed", color = "black") +
    geom_hline(yintercept = mean(masterDat1$Recipients.Who.Clicked), linetype = "dashed", color = "black") +
    ylim(0, 3200) +
    labs(x = "", y = "", title = "campaign measures across campaigns")

  setwd("/Users/.../plots/cumulative")
  graphG1 <<- "summaryCampaignPlot.png"
  if (file.exists(graphG1)) file.remove(graphG1)
  ggsave(graphG1, width = 15, height = 8)

}

#-----------------------------------------
#- PLOT KEY STATS ACROSS CAMPAIGNS - PERCENTAGE

cumulCampaignsPerc = function() {
  
  dataSubset = data.frame(masterDat1$Campaign.Name,
                          (masterDat1$Successful.Deliveries/masterDat1$Total.Recipients)*100,
                          (masterDat1$Recipients.Who.Opened/masterDat1$Total.Recipients)*100,
                          (masterDat1$Recipients.Who.Clicked/masterDat1$Recipients.Who.Opened)*100)
  
  plot1 = ggplot(dataSubset, aes(dataSubset$masterDat1.Campaign.Name)) + 
    geom_line(aes(y = dataSubset$X.masterDat1.Successful.Deliveries.masterDat1.Total.Recipients...., stat = "identity", group = 1, show.legend = FALSE), color = "darkgreen", size = 0.8) +
    geom_line(aes(y = dataSubset$X.masterDat1.Recipients.Who.Opened.masterDat1.Total.Recipients...., stat = "identity", group = 1, show.legend = FALSE), color = "firebrick1", size = 0.8) +
    geom_line(aes(y = dataSubset$X.masterDat1.Recipients.Who.Clicked.masterDat1.Recipients.Who.Opened...., stat = "identity", group = 1, show.legend = FALSE), color = "royalblue3", size = 0.8) +
    geom_hline(yintercept = mean(dataSubset$X.masterDat1.Recipients.Who.Opened.masterDat1.Total.Recipients....), linetype = "dashed", color = "black") +
    geom_hline(yintercept = mean(dataSubset$X.masterDat1.Recipients.Who.Clicked.masterDat1.Recipients.Who.Opened....), linetype = "dashed", color = "black") +
    theme(plot.title = element_text(size=10, face="bold", margin = margin(10, 0, 10, 0)), axis.text.x = element_text(angle = 70, hjust = 1),
          legend.title = element_blank(), legend.background = element_rect(fill="grey97", size=0.5, linetype="solid"),
          legend.key=element_rect(fill= "white")) +
    ylim(0, 100) +
    labs(x = "", y = "", title = "campaign measures across campaigns - as percentage")
  
  setwd("/Users/.../plots/cumulative")
  graphG1 <<- "summaryCampaignPlotPercentages.png"
  if (file.exists(graphG1)) file.remove(graphG1)
  ggsave(graphG1, width = 15, height = 8)
  
}

#-----------------------------------------
#- PLOT CAMPAIGN PERFORMANCE REPORTS

segPerfReport = function(fileName, plotTitle) {
  
  wd = "/Users/.../campaigns/csvExports/segmentationReports/"
  setwd(wd)
  
  masterDat3 = read.csv(fileName, sep = ",")
  cumulCampaignStat <<- ggplot(masterDat3, aes(masterDat3$Subject), ylim=c(0, 100)) + 
    geom_bar(width = 0.55, aes(y = masterDat3$X..delivered, colour = "% (of sent) delivered"), stat = "identity", fill = "thistle3", show.legend = FALSE) +
    geom_line(aes(y = masterDat3$X..opened..delivered., group = 1, colour = "% (of delivered) opened"), size = 0.8) +
    geom_line(aes(y = masterDat3$X..clicked..opened., group = 1, colour = "% (of opened) clicked"), size = 0.8) +
    labs(x = "", y = "", title = plotTitle) +
    geom_hline(yintercept = mean(masterDat3$X..opened..delivered.), linetype = "dashed", color = "black") +
    geom_hline(yintercept = mean(masterDat3$X..clicked..opened.), linetype = "dashed", color = "black") +
    theme(plot.title = element_text(size=7, face="bold", margin = margin(20, 0, 20, 0)), axis.text.x = element_text(angle = 70, hjust = 1),
          legend.title = element_blank(), legend.background = element_rect(fill="grey97", size=0.5, linetype="solid"),
          legend.key=element_rect(fill= "white")) + ylim(0, 100) +
    scale_color_manual(values = c("firebrick1","royalblue3","thistle3"))

  topCampaigns(masterDat3, plotTitle)
  
  setwd("/Users/.../plots/bySegment")
  cName = gsub('|.{4}$', '', fileName)
  graphG2 = paste(cName, "analysis.png", sep = "")
  if (file.exists(graphG2)) file.remove(graphG2)
  ggsave(graphG2, width = 15, height = 7)
  setwd("/Users/.../")

}

#-----------------------------------------
#- create lists of high-performing campaigns

topCampaigns = function(dataSet, nameOfGroup) {

  avgDelivs = mean(dataSet$X..delivered)
  avgOpens = mean(dataSet$X..opened..delivered.)
  avgClicks = mean(dataSet$X..clicked..opened.)

  hiAvDelivs = NA
  hiAvOpens = NA
  hiAvClicks = NA
  for (i in seq(1,22,1)) { if (dataSet[i,5] > avgDelivs) { hiAvDelivs = c(hiAvDelivs, dataSet[i,2]) } }
  for (i in seq(1,22,1)) { if (dataSet[i,9] > avgOpens) { hiAvOpens = c(hiAvOpens, dataSet[i,2]) } }
  for (i in seq(1,22,1)) { if (dataSet[i,12] > avgClicks) { hiAvClicks = c(hiAvClicks, dataSet[i,2]) } }
  hiAvDelivs = hiAvDelivs[-c(1)]
  hiAvOpens = hiAvOpens[-c(1)]
  hiAvClicks = hiAvClicks[-c(1)]

  # #- returns campaigns that had both high average opens and clicks
  # for (i in hiAvClicks) {
  #   for (j in hiAvOpens) {
  #     if (i == j) { print(as.character(dataSet[i,2])) }
  #   }
  # }

}

#-----------------------------------------
#- create grophs and write

main = function(segmentations) {
 
  for (i in segmentations) { segPerfReport(i, gsub('|.{4}$', '', i)) }
  cumulCampaigns()
  cumulCampaignsPerc()
  print(">>> analysis complete")
  
}

#---------------------------------------------
# big red button - CREATE SPREADSHEET 1 and close
#---------------------------------------------

main(segsList)

rm(list=ls())

