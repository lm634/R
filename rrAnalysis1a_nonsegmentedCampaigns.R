#-  GENERATE FIRST STAGE CAMPAIGN ANALYSIS - GENERATES AUTOMATIC SEGMENTATIONS
#-
#-  this script generates cumulative and across-group descriptive analysis
#-  for RR mail-out campaigns, and saves them to a formatted .xlsx file.
#-
#-  the script is set up to run using raw mailChimp data, however requires
#-  segmentation by campaign, so only the relevant data should be included
#-  in the campaign .csv file (see runAnalysis function).

#=============================================>
# work space and working directory setup 
#=============================================>

rm(list = ls())
library(XLConnect)
library(ggplot2)
wd = "/Users/.../campaigns/partialSegmented"
setwd(wd)

#=============================================>
# defining functions
#=============================================>

#set segments
setSegment = function(fileName, campaign) {
  tags <<- NA
  for (i in seq(1,length(campaign$List),1)) {
    tags[i] <<- gsub('^.{13}|', '', campaign$List[i])
  }
}

#import data
import = function(segmentation) {
  globalT1 <<- matrix(data = NA, ncol = 2, nrow = 2, byrow = TRUE)
  globalT2 <<- matrix(data = NA, ncol = 2, nrow = 2, byrow = TRUE)
  globalG1 <<- NA
  globalG2 <<- NA
  globalT3 <<- matrix(data = 0, ncol = length(segmentation), nrow = 13, byrow = TRUE)
  gTRowNames = c('Total Recipients','Successful Deliveries','% delivered','Bounces','% bounced',
                 'Recipients Who Opened','% opened (delivered)','Total Opens','Recipients Who Clicked',
                 '% clicked (opened)','Total Clicks','Total Unsubs','Total Abuse Complaints')
  gTColNames = c(segmentation)
  rownames(globalT3) = c(gTRowNames)
  colnames(globalT3) = c(gTColNames)
  globalT4 <<- matrix(data = NA, ncol = 11, nrow = 0, byrow = TRUE)
  globalT5 <<- matrix(data = NA, ncol = 14, nrow = 0, byrow = TRUE)
  gTColNames = c("Total Recipients","Successful Deliveries","Bounces","Recipients Who Opened",
                 "Total Opens","Recipients Who Clicked","Total Clicks","Total Unsubs","Total Abuse Complaints","Campaign Name", "Campaign Date")
  colnames(globalT4) = c(gTColNames)
  gTRowNames = c('Campaign Name','Total Recipients','Successful Deliveries','% delivered','Bounces','% bounced',
                 'Recipients Who Opened','% opened (delivered)','Total Opens','Recipients Who Clicked',
                 '% clicked (opened)','Total Clicks','Total Unsubs','Total Abuse Complaints')
  colnames(globalT5) = c(gTRowNames)
}

#generate tables
tables = function(fileName, campaign, segmentation) {
  table1 = matrix(data = NA,ncol = 2, nrow = 0, byrow = TRUE)
  colnames(table1) = c('number','%')
  table1 = rbind(table1, 'Total Recipients' = c(sum(campaign$Total.Recipients),NA))
  table1 = rbind(table1, 'Successful Deliveries' = c(sum(campaign$Successful.Deliveries),
                                                     round(sum(campaign$Successful.Deliveries)/sum(campaign$Total.Recipients)*100,
                                                           digits = 2)))
  table1 = rbind(table1, 'Bounces' = c(sum(campaign$Total.Bounces),
                                       round(sum(campaign$Total.Bounces)/sum(campaign$Total.Recipients)*100, digits = 2)))
  table1 = rbind(table1, 'Recipients Who Opened' = c(sum(campaign$Unique.Opens),
                                                     round(sum(campaign$Unique.Opens)/sum(campaign$Successful.Deliveries)*100, digits = 2)))
  table1 = rbind(table1, 'Total Opens' = c(sum(campaign$Total.Opens),NA))
  table1 = rbind(table1, 'Recipients Who Clicked' = c(sum(campaign$Unique.Clicks),
                                                      round(sum(campaign$Unique.Clicks)/sum(campaign$Unique.Opens)*100, digits = 2)))
  table1 = rbind(table1, 'Total Clicks' = c(sum(campaign$Total.Clicks),NA))
  table1 = rbind(table1, 'Total Unsubs' = c(sum(campaign$Unsubscribes),NA))
  table1 = rbind(table1, 'Total Abuse Complaints' = c(sum(campaign$Abuse.Complaints),NA))
  globalT1 <<- table1

  if (nrow(campaign)>1) {
    table2 = matrix(data = NA, ncol = length(segmentation), nrow = 13, byrow = TRUE)
    t2RowNames = c('Total Recipients','Successful Deliveries','% delivered','Bounces','% bounced',
                   'Recipients Who Opened','% opened (delivered)','Total Opens','Recipients Who Clicked',
                   '% clicked (opened)','Total Clicks','Total Unsubs','Total Abuse Complaints')
    t2ColNames = c(segmentation)
    rownames(table2) = c(t2RowNames)
    colnames(table2) = c(t2ColNames)
    columnOrder = seq(1,length(segmentation),1)
    for (i in 1:length(segmentation)) {
      j = columnOrder[i]
      table2[1,j] = campaign[i,6]
      globalT3[1,j] <<- globalT3[1,j] + campaign[i,6]
      table2[2,j] = campaign[i,7]
      globalT3[2,j] <<- globalT3[2,j] + campaign[i,7]
      table2[3,j] = round((campaign[i,7]/campaign[i,6])*100, digits = 2)
      table2[4,j] = campaign[i,10]
      globalT3[4,j] <<- globalT3[4,j] + campaign[i,10]
      table2[5,j] = round((campaign[i,10]/campaign[i,7])*100, digits = 2)
      table2[6,j] = campaign[i,13]
      globalT3[6,j] <<- globalT3[6,j] + campaign[i,13]
      table2[7,j] = round((campaign[i,13]/campaign[i,7])*100, digits = 2)
      table2[8,j] = campaign[i,15]
      globalT3[8,j] <<- globalT3[8,j] + campaign[i,15]
      table2[9,j] = campaign[i,16]
      globalT3[9,j] <<- globalT3[9,j] + campaign[i,16]
      table2[10,j] = round((campaign[i,16]/campaign[i,13])*100, digits = 2)
      table2[11,j] = campaign[i,18]
      globalT3[11,j] <<- globalT3[11,j] + campaign[i,18]
      table2[12,j] = campaign[i,19]
      globalT3[12,j] <<- globalT3[12,j] + campaign[i,19]
      table2[13,j] = campaign[i,20]
      globalT3[13,j] <<- globalT3[13,j] + campaign[i,20]
      globalT2 <<- table2
    }
  } else { print('>>> error: across-group analysis not undertaken') }
}

#generate plot
plotCampaign = function(fileName, campaign, segmentation) {
  if (is.na(globalT2)) {
    print('>>> error: across-group graphing not undertaken')
  } else {
    table2 = matrix(data = NA, ncol = 13, nrow = length(segmentation), byrow = TRUE)
    t1ColNames = c('Total_Recipients','Successful_Deliveries','pc_delivered','Bounces','pc_bounced',
                   'Recipients_Who_Opened','pc_opened_(delivered)','Total_Opens','Recipients_Who_Clicked',
                   'pc_clicked_(opened)','Total_Clicks','Total_Unsubs','Total_Abuse_Complaints')
    t1RowNames = c(segmentation)
    rownames(table2) = c(t1RowNames)
    colnames(table2) = c(t1ColNames)
    columnOrder = seq(1,length(segmentation),1)
    for (i in 1:length(segmentation)) {
      j = columnOrder[i]
      table2[j,1] = campaign[i,6]
      table2[j,2] = campaign[i,7]
      table2[j,3] = round((campaign[i,7]/campaign[i,6])*100, digits = 2)
      table2[j,4] = campaign[i,10]
      table2[j,5] = round((campaign[i,10]/campaign[i,7])*100, digits = 2)
      table2[j,6] = campaign[i,13]
      table2[j,7] = round((campaign[i,13]/campaign[i,7])*100, digits = 2)
      table2[j,8] = campaign[i,15]
      table2[j,9] = campaign[i,16]
      table2[j,10] = round((campaign[i,16]/campaign[i,6])*100, digits = 2)
      table2[j,11] = campaign[i,18]
      table2[j,12] = campaign[i,19]
      table2[j,13] = campaign[i,20]
    }
    tableData = as.data.frame(table2)
    remove(table2)

    #- globalG1 plots the proportions of deliveries, open rate and click rate across the segmentations
    globalG1 <<- ggplot(tableData, aes(t1RowNames)) + 
      geom_bar(width = 0.75, aes(y = tableData$pc_delivered, colour = "% (of sent) delivered"), stat = "identity", fill = "thistle3", show.legend = FALSE) +
      geom_line(aes(y = tableData$`pc_opened_(delivered)`, group = 1, colour = "% (of delivered) opened"), size = 0.8) +
      geom_line(aes(y = tableData$`pc_clicked_(opened)`, group = 1, colour = "% (of opened) clicked"), size = 0.8) + 
      ggtitle("campaign report") +
      labs(x = "", y = "", title = "segmentation report (proportional)") +
      theme(plot.title = element_text(size=11, face="bold", margin = margin(20, 0, 20, 0)),
            legend.title = element_blank(), legend.background = element_rect(fill="grey97", size=0.5, linetype="solid"),
            legend.key=element_rect(fill= "white")) +
      scale_color_manual(values = c("firebrick1","royalblue3","thistle3"))
    setwd("/Users/.../plots/nonRegular/segmented")
    cName = gsub('^.{1}|.{4}$', '', fileName)
    globalG1 <<- paste(cName, "SegReport.png", sep = "")
    if (file.exists(globalG1)) file.remove(globalG1)
    ggsave(globalG1, width = 6, height = 5)
    setwd("/Users/.../")
  }
}

#generate spreadsheet
writeSheet = function(workbookName, fileName, campaign, segmentation) {
  wd = "/Users/.../analysis/"
  setwd(wd)
  if (file.exists('nonMailoutCampaignAnalysis.xlsx') == TRUE) {file.remove('nonMailoutCampaignAnalysis.xlsx')}
  wb = loadWorkbook(workbookName, create = TRUE)
  sheetName = gsub('.{4}$','',fileName)
  createSheet(wb, name = sheetName)
  cs = createCellStyle(wb)
  setFillForegroundColor(cs, color = XLC$"COLOR.WHITE")
  setFillPattern(cs, fill = XLC$"FILL.SOLID_FOREGROUND")
  for (i in 1:8) { setCellStyle(wb, sheet = sheetName, row = i, col = 1:14, cellstyle = cs) }
  for (i in 9:18) { setCellStyle(wb, sheet = sheetName, row = i, col = 4:5, cellstyle = cs) }
  for (i in 19:22) { setCellStyle(wb, sheet = sheetName, row = i, col = 1:5, cellstyle = cs) }
  for (i in 23:60) { setCellStyle(wb, sheet = sheetName, row = i, col = 1:14, cellstyle = cs) }
  for (i in 9:22) {setCellStyle(wb, sheet = sheetName, row = i, col = 12:14, cellstyle = cs)}
  createName(wb, name = "image", formula = paste(sheetName, "!$A$1:$G$2", sep = "")) 
  createName(wb, name = "title", formula = paste(sheetName, "!$A$4", sep = ''))
  createName(wb, name = "date", formula = paste(sheetName, "!$A$5", sep = ''))
  createName(wb, name = 'data1', formula = paste(sheetName, "!$A$9", sep = ''))
  createName(wb, name = 'data2', formula = paste(sheetName, "!$F$9", sep = ''))
  createName(wb, name = 'graph1', formula = paste(sheetName, "!$B$28:$I$55", sep = ''))
  addImage(wb, filename = "rrLogo.png", name = "image", originalSize = FALSE)
  setRowHeight(wb, sheet = sheetName, row = 2, height = 50)
  cDate = gsub('.{9}$', '', campaign$Send.Date[1])
  cName = gsub('^.{10}|.{4}$', '', fileName)
  writeNamedRegion(wb, cName, header = FALSE, rownames = NULL, name = "title")
  setRowHeight(wb, sheet = sheetName, row = 4, height = 25)
  writeNamedRegion(wb, cDate, header = FALSE, rownames = NULL, name = "date")
  setRowHeight(wb, sheet = sheetName, row = length(segmentation), height = 25)
  writeNamedRegion(wb, globalT1, rownames = '', name = "data1")
  setColumnWidth(wb, sheet = sheetName, column = 1, width = 5500)
  if (is.na(globalT2)) {
    for (i in 9:22) { setCellStyle(wb, sheet = sheetName, row = i, col = 6:11, cellstyle = cs) }
    print(">>> error: unable to write across-group comparison data")
  } else {
    writeNamedRegion(wb, globalT2, rownames = '', name = "data2")
    setColumnWidth(wb, sheet = sheetName, column = 6, width = 5500)
    setwd("/Users/.../plots/nonRegular/segmented/")
    addImage(wb, filename = globalG1, name = "graph1", originalSize = FALSE)
    setwd("/Users/.../analysis/")
  }
  removeName(wb, name = "image")
  removeName(wb, name = "header")
  removeName(wb, name = "title")
  removeName(wb, name = "date")
  removeName(wb, name = "data1")
  removeName(wb, name = "data2")
  removeName(wb, name = "graph1")
  saveWorkbook(wb)
}

#globalT4 values
cumulAnalysis = function(fileName, campaign) {
  valueToBeAdded = globalT1[1,1]
  for (i in 2:9) { valueToBeAdded = append(valueToBeAdded, globalT1[i,1]) }
  valueToBeAdded = append(valueToBeAdded, gsub('^.{10}|.{4}$', '', fileName))
  valueToBeAdded = append(valueToBeAdded, campaign$Send.Date[1])
  globalT4 <<- rbind(globalT4, valueToBeAdded)
}

#reset globals to NA for next analysis
resetGlobalsToNA = function(fileName) {
  globalT1 <<- matrix(data = NA, ncol = 2, nrow = 2, byrow = TRUE)
  globalT2 <<- matrix(data = NA, ncol = 2, nrow = 2, byrow = TRUE)
  globalG1 <<- NA
  wd = "/Users/.../"
  setwd(wd)
  print(paste(">>> ", gsub('.{4}$','',fileName), " analysis complete and spreadsheet added to .xlsx file", sep = ""))
}

#run all functions
main = function(fileName) {
  campaign = read.csv(fileName, sep = ",")
  setSegment(fileName, campaign)
  import(tags)
  tables(fileName, campaign, tags)
  plotCampaign(fileName, campaign, tags)
  writeSheet('nonMailoutCampaignAnalysis.xlsx', fileName, campaign, tags)
  cumulAnalysis(fileName, campaign)
  resetGlobalsToNA(fileName)
}

#=============================================>
# run the script
#=============================================>

main("testing.csv")
#for (i in 1:length(listOfFiles)) { rownames(globalT4)[i] = i }

rm(list = ls())
