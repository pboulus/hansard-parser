setwd("/Users/Paul/Thesis/RProjects/")
library(XML)

#read XML document into memory, grab headline details of document
doc <- xmlInternalTreeParse("testsgml-1981.xml") #read xml into memory #speeches[[14]] contains "continue" tags
root <- xmlRoot(doc) #read root of xml
speeches <- getNodeSet(root, "//SPEECH") #get all speeches
questions <- xpathApply(root,  "//QUESTION") #get all questions
answers <- xpathApply(root, "//ANSWER") #get all answers
ids <-xpathApply(root, "//SPEECH//NAMEID", xmlValue) #doesn't work for SGML
xmlSize(speeches) #gives number of speeches in the node set
for(i in 1:xmlSize(speeches)) print(speeches[[i]])

xpathApply(root, "//INTERJECTION", removeNodes) #remove interjections

#get date functions
xpathApply(root, "//HANSARD", xmlAttrs)[[1]]["DATE"] #for SGML data



#Define function - receive speeches and convert each para of the speech into a row of a dataframe
#function receives speeches, which is a node set. subset with [[number]]
as.vector(xpathApply(speeches[[1]], "descendant::NAME", xmlAttrs)) #for SGML
xpathSApply(speeches[[14]], "descendant::PARA | descendant::P", xmlValue)
getNodeSet(speeches[[14]], "ancestor::DEBATE[1]")[[1]] #this returns the debate that parents the speech
getNodeSet(questions[[1]], "ancestor::DEBATE[1]")[[1]] #this returns the debate that parents the question
getNodeSet(speeches[[12]], "ancestor::QUESTION[1]")
ids <- lapply(speeches, FUN=xpathSApply, path="descendant::NAME", fun="xmlValue")