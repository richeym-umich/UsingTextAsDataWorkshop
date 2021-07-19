# Authors:     Jule Krüger
# Maintainers: Jule Krüger, Meghan Dailey
# Copyright:   2021
#
# Purpose: get a better understanding of the UN Corpus, 
#          the file structure, xml data, and read part or all 
#          of the data in
# ============================================

# let's keep the UN-data out of GitHub on a local machine
# here is the path to my local, unzipped copy
# (i used the following command to unzip them: cat UNv1.0-TEI.en.tar.gz.* | tar -xzf -)
my_path_to_unzipped_un_data <- "~/Documents/teaching/UN-Corpus/import/input/UNv1.0-TEI/en/"

#set working directory
setwd(my_path_to_unzipped_un_data)

#get all the filenames to each xml file
xml_filepaths <- list.files(pattern = "*.xml$", recursive = TRUE)
head(xml_filepaths)
print(paste("We are dealing with a total of", length(xml_filepaths), "xml data files."))

#the data are organized by year
#how many data files are there for each year?
# each path starts with the relevant year, 
# e.g., "^1990/" using regular expressions
years <- data.frame(year=c(1990:2014), number_files=NA)
for (year in years$year) {
  years[years$year==year, "number_files"] <- length(grep(paste("^", year, sep=""), xml_filepaths))
}
#let's test whether we got our document management right so far
stopifnot(sum(years$number_files)==length(xml_filepaths))

#we can visualize the number of documents over time
barplot(years$number_files, names=years$year,
        las=1, main="UN Parallel Corpus: yearly xml files over time")

# to read xml files, R has the "XML" package
# you can install it with install.packages("XML)
library("XML")
library("methods")

tsample <- xmlParse(xml_filepaths[1])
tsample

xmlSize(xmlRoot(tsample))

tdf <- xmlToDataFrame(xml_filepaths[1])
tdf

# there is a structure to this document 
# but we will have to work it a bit more for our purposes


