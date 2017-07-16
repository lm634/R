#-  DIRECTORY GENERATOR
#-  this script generates a table of values which pair up
#-  segmented files with their appropriate MC pathname and
#-  unique ID
#-  
#-  the output is a .csv file sent to the csvExports
#-  pathway

#=============================================>
# work space and working directory setup 
#=============================================>
rm(list = ls())
wdRoot = "/Users/.../"
wd1 = "/Users/.../campaigns/segmented/"
wd2 = "/Users/.../aggregate_activity/bounces/"
wd3 = "/Users/.../Analysis/"


#=============================================>
# determine number of files
#=============================================>
setwd(wd1)
instance = 0
for (i in list.files(wd1)) { 
  file1 = read.csv(i)
  for (j in file1$Unique.Id) {
    instance = instance + 1
  }
}
rm(i, j)

#=============================================>
# create directory and convert title to path
#=============================================>
directory = matrix(data = NA, ncol = 6, nrow = instance, byrow = TRUE)
colnames(directory) = c("segmentationPath","listName", "uniqueID","dateKey", "stringToConvert", "filePathKey")

counter = 0
setwd(wd1)
for (i in list.files(wd1)) {
  file1 = read.csv(i, sep = ",")
  for (j in 1:length(file1$Unique.Id)) {
    counter = counter + 1
    directory[counter,1] = i
    directory[counter,2] = as.character(file1$List[j])
    directory[counter,3] = as.character(file1$Unique.Id[j])
    directory[counter,4] = as.character(file1$Title[j])
  }
}
rm(i, j)

for (i in 1:instance) {
  stringToConvert = directory[i,4]
  stringToConvert = gsub(" ", "-", stringToConvert, fixed = TRUE)
  stringToConvert = gsub("(", "", stringToConvert,fixed = TRUE)
  stringToConvert = gsub(")", "-", stringToConvert, fixed = TRUE)
  stringToConvert = gsub("_", "-", stringToConvert, fixed = TRUE)
  stringToConvert = gsub(",", "-", stringToConvert, fixed = TRUE)
  stringToConvert = gsub("'", "-", stringToConvert, fixed = TRUE)
  stringToConvert = gsub("--", "-", stringToConvert, fixed = TRUE)
  stringToConvert = tolower(stringToConvert)
  stringToConvert = paste("_",stringToConvert, ".csv", sep = "")
  directory[i,5] = stringToConvert
}
rm(i)

#=============================================>
# compare path names and populate directory
#=============================================>
setwd(wd2)
listOfFiles = list.files(wd2)

for (i in 1:counter) {
  test1 = as.character(directory[i,5])
  for (j in 1:length(listOfFiles)) {
    test2 = as.character(gsub("^.*?_", "_", listOfFiles[j]))
    if (test1 == test2) {
      directory[i,6] = listOfFiles[j]
    }
  }
}

#=============================================>
# save file to csvExports
#=============================================>
setwd("/Users/.../campaigns/csvExports/")
write.csv(directory, file = "fileDirectory.csv", na = "NA")

rm(list = ls())

