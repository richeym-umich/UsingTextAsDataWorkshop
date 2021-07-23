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
pdf(file="~/git/UsingTextAsDataWorkshop/R-code/import/output/bg-UNCorpus-file-distribution-over-time.pdf", height = 7, width = 12)
barplot(years$number_files, names=years$year,
        las=1, main="UN Parallel Corpus: yearly xml files over time")
dev.off()
# to read xml files, R has the "xml2" package
# (the "XML" package is no longer being maintained)
# you can install it with install.packages("xml2")
library("xml2")
library("methods")

#we create a function that parses each xml file into one data row,
# we collect some metadata for each document, and the document's text
parseUNDocumentFromXML <- function(iter, file_path, row_container) {
  
  filename <- setNames(data.frame(file_path, stringsAsFactors = FALSE), c("filename"))

  this_xml_file <- read_xml(file_path)
  
  #we need one cleaning step here by adding white spaces before "</s>"
  # otherwise, the text in the body is collapsed such that words get 
  # baked together
  this_xml_file <- read_xml(gsub("</s>", " </s>", this_xml_file))

  pubPlace <- setNames(data.frame(xml_text(xml_find_all(this_xml_file, ".//pubPlace")),
                                  stringsAsFactors = FALSE),
                     c("pubPlace"))
  
  date <- setNames(data.frame(xml_text(xml_find_all(this_xml_file, ".//date")),
                              stringsAsFactors = FALSE),
                 c("date"))
  
  symbol <- setNames(data.frame(xml_text(xml_find_all(this_xml_file, ".//idno"))[1],
                                stringsAsFactors = FALSE),
                     c("symbol"))
  jobno <- setNames(data.frame(xml_text(xml_find_all(this_xml_file, ".//idno"))[2],
                               stringsAsFactors = FALSE),
                    c("jobno"))
  
  body <- setNames(data.frame(xml_text(xml_find_all(this_xml_file, ".//body")),
                              stringsAsFactors = FALSE), 
                   c("body"))

  new_df_row <- cbind(filename, pubPlace, date, symbol, jobno, body)
  
  row_container[[iter]] <- new_df_row
  return(row_container)
}

processSaveYearData <- function(year) {
  #we create an empty list that we will fill with new rows, 
  # one for each parsed UN document
  row_container <- list()
  
  year_selection <- paste("^", year, sep = "")
  
  # warning, this step takes a while 
  # as most years contain thousands of xml files
  for (i in grep(year_selection, xml_filepaths)) {
    row_container <- parseUNDocumentFromXML(i, xml_filepaths[i], row_container)
  }
  documentDF <- do.call(rbind, row_container)
  stopifnot(nrow(documentDF)==length(grep(year_selection, xml_filepaths)))
  row_container <- NULL
  
  #let's save a csv file for our year selection
  outputfilename <- paste("~/git/UsingTextAsDataWorkshop/R-code/import/output/UNdocuments-",
                          year, ".csv", sep = "")
  write.table(documentDF, 
              file=outputfilename,
              row.names = FALSE, sep = '|')
  print(paste("Saved UN", year, "data to", outputfilename))
  documentDF <- NULL
}

processSaveYearData("2011")
processSaveYearData("2014")

#end of Rscript.