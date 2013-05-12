setwd("/Users/Paul/Thesis/RProjects/")
library(XML)

#Define function - receive speeches and convert each para of the speech into a row of a dataframe
#function receives speeches, which is a node set. subset with [[number]]
as.vector(xpathApply(speeches[[1]], "descendant::NAME", xmlAttrs)) #for SGML
as.vector(xpathApply(speeches[[14]], "descendant::name.id", xmlValue)) #for XML
xpathApply(speeches[[14]], "//para", xmlValue)
xpathSApply(speeches[[14]], "descendant::para | descendant::p", xmlValue)
getNodeSet(speeches[[14]], "ancestor::debate[1]")[[1]] #this returns the debate that parents the speech

getNodeSet(questions[[1]], "ancestor::debate[1]")[[1]] #this returns the debate that parents the speech

getNodeSet(speeches[[12]], "ancestor::question[1]")
ids <- lapply(speeches, FUN=xpathSApply, path="descendant::name.id", fun="xmlValue")
lapply()


#read XML document into memory, grab headline details of document
doc <- xmlInternalTreeParse("testxml.xml") #read xml into memory #speeches[[14]] contains "continue" tags
doc <- xmlInternalTreeParse("testsgml.xml") #read xml into memory 
doc <- xmlInternalTreeParse("734736.xml") #read xml into memory
root <- xmlRoot(doc) #read root of xml

xpathApply(root, "//interjection", removeNodes) #remove interjections
speeches <- getNodeSet(root, "//speech") #get all speeches
questions <- xpathApply(root, "//question") #get all questions
answers <- xpathApply(root, "//answer") #get all answers
ids <-xpathApply(root, "//speech//name.id", xmlValue)
xmlSize(speeches) #gives number of speeches in the node set
for(i in 1:xmlSize(speeches)) print(speeches[[i]])

#speechindex <- 1:xmlSize(speeches)



#get date functions
xpathApply(root, "//HANSARD", xmlAttrs)[[1]]["DATE"] #for SGML data
xpathApply(root, "//date", xmlValue)[[1]] #for XML data

