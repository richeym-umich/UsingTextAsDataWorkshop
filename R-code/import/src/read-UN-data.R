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

documentDF <- data.frame(filename = as.character(), 
                         publisher = as.character(),
                         pubPlace = as.character(),
                         date = as.character(),
                         symbol = as.character(),
                         jobno = as.character(),
                         text = as.character(), 
                         stringsAsFactors = FALSE)

#we create a function that parses each xml file into one data row,
# we collect some metadata for each document, and the document's text
parseUNDocumentFromXML <- function(file_path) {
  
  filename <- setNames(data.frame(file_path, stringsAsFactors = FALSE), c("filename"))

  this_xml_file <- xmlParse(file_path)

  publisher <- setNames(xmlToDataFrame(getNodeSet(this_xml_file, 
                                                "//TEI.2/teiHeader/fileDesc/publicationStmt/publisher"), 
                                       stringsAsFactors = FALSE), 
                      c("publisher"))
  
  pubPlace <- setNames(xmlToDataFrame(getNodeSet(this_xml_file, 
                                               "//TEI.2/teiHeader/fileDesc/publicationStmt/pubPlace"), 
                                      stringsAsFactors = FALSE),
                     c("pubPlace"))
  
  date <- setNames(xmlToDataFrame(getNodeSet(this_xml_file, "//TEI.2/teiHeader/fileDesc/publicationStmt/date"), 
                                  stringsAsFactors = FALSE),
                 c("date"))
  
  idno <- xmlToDataFrame(getNodeSet(this_xml_file, "//TEI.2/teiHeader/fileDesc/publicationStmt/idno"), 
                         stringsAsFactors = FALSE)
  idno <- setNames(data.frame(t(idno), row.names = NULL, 
                              stringsAsFactors = FALSE), c("symbol", "jobno"))
  
  body <- xmlToDataFrame(getNodeSet(this_xml_file, "//TEI.2/text/body"), 
                         stringsAsFactors = FALSE)
  body$text <- apply(body, 1, function(x) paste(x, collapse = " "))
  
  new_df_row <- cbind(filename, publisher, pubPlace, date, idno, body["text"])
  
  documentDF <- rbind(documentDF, new_df_row)
  return(documentDF)
}

documentDF <- parseUNDocumentFromXML(xml_filepaths[1])
print(documentDF)

#FIXME: iterating over multiple rows doesn't yet work
# breaks in second document
for (i in 1:10) {
  print(i)
  documentDF <- parseUNDocumentFromXML(xml_filepaths[i])
}

print(documentDF)

