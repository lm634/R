#-  GENERATE A/B TEST FOR MAILOUT
#-
#-  this script creates two random groups
#-  for an A/B test
#-
#-  it outputs the exported lists to the 
#-  csvExports directory

#=============================================>
# work space and working directory setup 
#=============================================>
rm(list = ls())
wd1 = "/Users/.../campaigns/csvExports/a_bLists/20170730/"
wd2 = "/Users/.../campaigns/csvExports/a_bLists/20170730/abSegments"
setwd(wd1)
if (file.exists("abSegments")) { 
  setwd(wd2)
  toRemove = list.files(wd2)
  file.remove(toRemove)
  setwd(wd1)
  file.remove("abSegments")
  rm(toRemove)
}
fileList = list.files(wd1)
dir.create(file.path(wd1, "abSegments"), showWarnings = FALSE)

#=============================================>
# divide lists for A/B and export csv
#=============================================>
for (i in 1:length(fileList)) {
  setwd(wd1)
  inList = FALSE
  listA = data.frame("ContactId" = integer(), "FirstName" = character(), "LastName" = character(),
                              "Email" = character(), stringsAsFactors = FALSE)
  listB = data.frame("ContactId" = integer(), "FirstName" = character(), "LastName" = character(),
                     "Email" = character(), stringsAsFactors = FALSE)
  abseg = read.csv(fileList[i], sep = ",")
  abseg$ContactId = as.character(abseg$ContactId)
  randomSample = sample(length(abseg$ContactId),round(length(abseg$ContactId)/2, digits = 0))
  for (j in 1:length(abseg$ContactId)) {
    for (k in randomSample) {
      if (j == k) { inList = TRUE }
    }
    if (inList == TRUE) {
      rowNumber = length(listA$ContactId) + 1
      listA[rowNumber,1] = as.character(abseg$ContactId[j])
      listA[rowNumber,2] = as.character(abseg$FirstName[j])
      listA[rowNumber,3] = as.character(abseg$LastName[j])
      listA[rowNumber,4] = as.character(abseg$Email[j])
    } else {
      rowNumber = length(listB$ContactId) + 1
      listB[rowNumber,1] = as.character(abseg$ContactId[j])
      listB[rowNumber,2] = as.character(abseg$FirstName[j])
      listB[rowNumber,3] = as.character(abseg$LastName[j])
      listB[rowNumber,4] = as.character(abseg$Email[j])
    }
    inList = FALSE
  }
  setwd(wd2)
  write.csv(listA,paste(fileList[i],"listA.csv", sep = "_"))
  write.csv(listB,paste(fileList[i],"listB.csv", sep = "_"))
  setwd(wd1)
}

rm(list = ls())
