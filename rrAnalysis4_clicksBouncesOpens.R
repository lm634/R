#-  GENERATE CAMPAIGN DATA FOR CLICKS, OPENS AND BOUNCES
#-
#-  this script aggregates bounces across campaigns, and aggregates clicks
#-  and opens within RR mailout campaigns.
#-
#-  it then generates a list of recent bounces, lists of opens and
#-  a list of clicks per campaign

#=============================================>
# work space and working directory setup 
#=============================================>
rm(list = ls())

setwd("/Users/.../campaigns/csvExports/")
directory = read.csv("fileDirectory.csv", sep = ",")

setwd("/Users/.../aggregate_activity/bounces/")
bounces = data.frame("emailAddress" = character(), "bounceType" = character(), "filePath" = character(),
                     "segmentationPath" = character(), stringsAsFactors = FALSE)

opens = data.frame("emailAddress" = character(), "timeStamp" = character(), "filePath" = character(),
                   "segmentationPath" = character(), "listName" = character(), stringsAsFactors = FALSE)

clicks= data.frame("emailAddress" = character(), "timeStamp" = character(), "url" = character(), "filePath" = character(),
                     "segmentationPath" = character(), stringsAsFactors = FALSE)

#=============================================>
# bounces
#=============================================>
#-----------------------------------------
#- populates bounces table
counter = 1
for (i in as.numeric(directory$X)) {
  if (!is.na(directory$filePathKey[i])) {
    temp1 = read.csv(as.character(directory$filePathKey[i]), sep = ",")
    for (j in as.numeric(row.names(temp1))) {
      bounces[counter,1] = as.character(temp1$Email.Address[j])
      bounces[counter,2] = as.character(temp1$Bounce.Type[j])
      bounces[counter,3] = as.character(directory$filePathKey[i])
      bounces[counter,4] = as.character(directory$segmentationPath[i])
      counter = counter + 1
    }
  }
}
#-----------------------------------------
#- separate to hard and soft bounces
hardBounces = subset(bounces, bounceType == "hard")
softBounces = subset(bounces, bounceType == "soft")
hardBounce = as.data.frame(table(hardBounces$emailAddress))
softBounce = as.data.frame(table(softBounces$emailAddress))
rm(hardBounces, softBounces, temp1, counter)

#-----------------------------------------
#- find the recency of latest bounce
hardBounce$recency = NA
hardBounce$lastBounce = NA
for (i in as.numeric(row.names(hardBounce))) {
  x = hardBounce$Var1[i]
  for (j in as.numeric(row.names(bounces))) {
    if (x == bounces$emailAddress[j]) {
      scoreTable = as.data.frame(table(directory$segmentationPath))
      scoreTable$score = seq(length(scoreTable$Var1),1,-1)
      for (k in as.numeric(row.names(scoreTable))) {
        if (scoreTable$Var1[k] == bounces$segmentationPath[j]) {
          hardBounce$recency[x] = scoreTable$score[k]
          hardBounce$lastBounce[x] = bounces$segmentationPath[j]
        }
      }
    }
  }
  percent = paste(round(i * (100 / length(hardBounce$Var1)), digits = 1)," % complete", sep = "")
  print(percent)
}

#-----------------------------------------
#- remove soft bounces on hard bounce list
toDelete = c()
for (i in as.numeric(row.names(softBounce))) {
  x = as.character(softBounce$Var1[i])
  for (j in as.numeric(row.names(hardBounce))) {
    y = as.character(hardBounce$Var1[j])
    if (x == y) {
      toDelete = c(toDelete, i)
    }
  }
}
softBounce = softBounce[-c(toDelete),]
rm(i, j, k, percent, x, y, toDelete, scoreTable)

#=============================================>
# opens
#=============================================>
#-----------------------------------------
#- populates opens table - takes some time
setwd("/Users/.../granular_activity/opens")
counter = 1
for (i in as.numeric(directory$X)) {
  if (!is.na(directory$filePathKey[i])) {
    temp1 = read.csv(as.character(directory$filePathKey[i]), sep = ",")
    for (j in as.numeric(row.names(temp1))) {
      opens[counter,1] = as.character(temp1$Email[j])
      opens[counter,2] = as.character(temp1$Timestamp[j])
      opens[counter,3] = as.character(directory$filePathKey[i])
      opens[counter,4] = as.character(directory$segmentationPath[i])
      opens[counter,5] = as.character(directory$listName[i])
      counter = counter + 1
    }
    percent = paste(round(i * (100 / length(directory$filePathKey)),digits = 1), " % complete", sep = "")
    print(percent)
  }
}
rm(temp1, i, j, counter, percent)

#-----------------------------------------
#- opens by campaign and total opens
countCumulOpens = as.data.frame(table(opens$emailAddress))

countOpensByCampaign = as.data.frame(table(opens$emailAddress, opens$segmentationPath))
toDelete = c()
for (i in as.numeric(row.names(countOpensByCampaign))) {
  if (as.numeric(countOpensByCampaign$Freq[i]) == 0) {
    toDelete = c(toDelete, i)
  }
}
countOpensByCampaign = countOpensByCampaign[-c(toDelete),]
rm(i, toDelete)

#=============================================>
# clicks
#=============================================>
#-----------------------------------------
#- clicks subset table
setwd("/Users/.../granular_activity/clicks")
counter = 1
for (i in as.numeric(directory$X)) {
  if (!is.na(directory$filePathKey[i])) {
    temp1 = read.csv(as.character(directory$filePathKey[i], sep = ","))
    for (j in as.numeric(row.names(temp1))) {
      clicks[counter,1] = as.character(temp1$Email[j])
      clicks[counter,2] = as.character(temp1$Timestamp[j])
      clicks[counter,3] = as.character(temp1$Url[j])
      clicks[counter,4] = as.character(directory$filePathKey[i])
      clicks[counter,5] = as.character(directory$segmentationPath[i])
      counter = counter + 1
    }
  }
}

#-----------------------------------------
#- clicks subset by campaign --> email
test = as.data.frame(table(temp1$Email))

clicksTest1 = subset(clicks, segmentationPath == "r20170622_juneNewsletter.csv")
clicksTest2 = subset(clicks, segmentationPath == 'r20170509_yourNewChallenge.csv')

url = c('https://www.url1.co.uk',
        'https://www.url2.co.uk',
        'https://www.url3.co.uk',
        'https://www.url4.co.uk',
        'https://www.url5.co.uk')

tempList = data.frame("url" = character(), "emailAddress" = character(), "filePath" = character(), stringsAsFactors = FALSE)
for (i in url) {
  for (j in seq(1,length(clicksTest1$url),1)) {
    k = clicksTest1$url[j]
    if (startsWith(k,i) == TRUE) {
      toAppend = c(as.character(i),as.character(clicksTest1$emailAddress[j]),as.character(clicksTest1$filePath[j]))
      print(toAppend)
      tempList <<- rbind(tempList,toAppend)
    }
  }
}



for (i in temp1) {
  if (i = )
  
temp1= as.data.frame(table(directory$segmentationPath))
}

rm(counter, i, j, temp1)




#-find anyone that has clicked multiple times - output a list per campaign
#name, email, group, times clicked, ranked by click-rate

