setwd("/Users/Paul/Dropbox/ANU/Thesis/RProjects/")
library(XML)


#read XML document into memory, grab headline details of document
doc <- xmlInternalTreeParse("testxml-2012.xml") #read xml into memory #speeches[[14]] contains "continue" tags
root <- xmlRoot(doc) #read root of xml
xpathApply(root, "//interjection", removeNodes) #remove interjections
speeches <- getNodeSet(root, "//speech") #get all speeches
questions <- xpathApply(root,  "//question") #get all questions
answers <- xpathApply(root, "//answer") #get all answers
continues <- xpathApply(root, "//continue") #get all answers
ids <-xpathApply(root, "//speech//name.id", xmlValue)
xmlSize(speeches) #gives number of speeches in the node set
for(i in 1:xmlSize(speeches)) print(speeches[[i]])
XMLdate <- xpathApply(root, "//date", xmlValue)[[1]] #for XML data



#Define function - receive speeches and convert each para of the speech into a row of a dataframe
#function receives speeches, which is a node set. subset with [[number]]
as.vector(xpathApply(speeches[[51]], "descendant::name.id", xmlValue)) #for XML
xpathSApply(answers[[2]], "descendant::para | descendant::p", xmlValue)
xpathSApply(answers[[2]], "descendant::span[@class='HPS-MemberInterjecting']", xmlValue) #this identifies annoying interjection tags, but not the interjection text
getNodeSet(speeches[[4]], "descendant::span") #this returns the spans, which allow us to filter out meaningless tags like the name of the electorate
#spans are HPS-Normal, HPSS-MemberSpeech, HPS-Electorate, HPS-Time, HPS-OfficeSpeech, HPS-GeneralBold, HPS-GeneralInterjecting
getNodeSet(speeches[[1]], "ancestor::debate[1]")[[1]] #this returns the debate that parents the speech
getNodeSet(questions[[1]], "ancestor::debate[1]")[[1]] #this returns the debate that parents the question
getNodeSet(speeches[[1]], "ancestor::question[1]")
ids <- lapply(speeches, FUN=xpathSApply, path="descendant::name.id", fun="xmlValue")