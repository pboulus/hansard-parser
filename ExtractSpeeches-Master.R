# ===== CALL PACKAGES, DEFINE GLOBAL OPTIONS =====

library("XML")
library("RWeka")
library("tm")
library("Snowball")

options(stringsAsFactors = FALSE)
filenames <- list.files(path="/Users/Paul/Thesis/Sources/House/4/", full.names=TRUE)

# ===== DEFINE FUNCTIONS =====

nodesettoframe <- function(nodeset, hansarddate, nodesettype){
  
  masterframe1 <- data.frame(Speaker = character(),
                             Electorate = character(),
                             Party = character(),
                             InGov = character(),
                             Role = character(),
                             Debate = character(),
                             Date = character(),
                             House=character(),
                             SetType=character(),
                             Paratext = character(),
                             stringsAsFactors=FALSE)
  nameid <- as.vector(xpathApply(nodeset, "descendant::name.id", xmlValue)) #for XML
  electorate <- as.vector(xpathApply(nodeset, "descendant::electorate", xmlValue)) #for XML
  party <- as.vector(xpathApply(nodeset, "descendant::party", xmlValue)) #for XML
  in.gov <- as.vector(xpathApply(nodeset, "descendant::in.gov", xmlValue)) #for XML
  role <- as.vector(xpathApply(nodeset, "descendant::role", xmlValue)) #for XML
  
  #Define function - receive speeches and convert each para of the speech into a row of a dataframe
  #function receives speeches, which is a node set. subset with [[number]
  
  paras <- xpathSApply(nodeset, "descendant::para | descendant::p", xmlValue)
  
  parentdebate <- try(getNodeSet(nodeset[[1]], "ancestor::debate[1]")[[1]], silent=TRUE) #this returns the debate that parents the node
  debatetitle <-  try(as.vector(xpathSApply(parentdebate, "descendant::debateinfo//title", xmlValue)), silent=TRUE) #this returns the debate title
  
  for(para in paras){
    rowvector <- as.character(c(nameid[[1]],
                                electorate[1],
                                party[1],
                                in.gov[1],
                                role[1],
                                debatetitle[1], 
                                hansarddate[1], 
                                "House",
                                nodesettype,
                                para[[1]]))
    masterframe1[nrow(masterframe1)+1,] <- rowvector
  }
  masterframe1
}

parsexml <- function(filenamepath){  
  doc <- xmlInternalTreeParse(filenamepath) #read xml into memory #speeches[[14]] contains "continue" tags
  root <- xmlRoot(doc) #read root of xml
  
  # get all the base element nodesets in the file
  
  XMLdate <- xpathApply(root, "//date", xmlValue)[[1]] #for XML data
  speeches <- getNodeSet(root, "//speech") #get all speeches
  questions <- xpathApply(root,  "//question") #get all questions (need a different technique here)
  answers <- xpathApply(root, "//answer") #get all answers
  interjections <- xpathApply(root, "//interjection")
  continues <- xpathApply(root, "//continue")
  
  
  ids <-xpathApply(root, "//speech//name.id", xmlValue)
  
  interjections.list <-  list()
  speeches.list <- list()
  continues.list <- list()
  
  if(xmlSize(interjections)>0){
    for(i in 1:xmlSize(interjections)){
      interjections.list[[i]] <- nodesettoframe(interjections[[i]], XMLdate, "interjection")
    }
  }
  
  xpathApply(root, "//interjection", removeNodes) #remove nodes we don't want after we've finished with them?
  
  
  if(xmlSize(continues)>0){
    for(i in 1:xmlSize(continues)){
      continues.list[[i]] <- nodesettoframe(continues[[i]], XMLdate, "continue")
    }
  }
  
  xpathApply(root, "//continue", removeNodes) #remove nodes we don't want after we've finished with them?
  
  if(xmlSize(speeches)>0){
    for(i in 1:xmlSize(speeches)){
      speeches.list[[i]] <- nodesettoframe(speeches[[i]], XMLdate, "speech")
    }
  }
  
  print(paste("No of interjections: ", length(interjections.list)))
  print(paste("No of continues: ", length(continues.list)))
  print(paste("No of speeches: ", length(speeches.list)))
  
  big.df <- do.call("rbind", c(interjections.list, speeches.list, continues.list))
  big.df
}

# ===== ITERATIVELY CALL FUNCTIONS, STORE AND MERGE OUTPUT =====
masterframe.list <- list()
#for(i in 1:length(filenames)){     #  <- this line will loop through all files 
for(i in 800:810){ # <- this line will loop through a selection of files
  print(i)
  masterframe.list[[i]] <- parsexml(filenames[[i]])
}

masterframe.df <- do.call("rbind", masterframe.list)

#write.table(masterframe.df, file="test.csv", sep=",") # TO WRITE TO CSV FILE

#for xmlfile in filenames {
#  parsexml(xmlfile)
#}


# ===== SPARE CODE =====
#  for(i in 1:25){print(xpathSApply(speeches[[i]], "ancestor::subdebate.1[1]"))} #finds all subdebate.1 nodes
#  getNodeSet(speeches[[1]], "ancestor::debate[1]")[[1]] #this returns the debate that parents the question
#  getNodeSet(speeches[[1]], "ancestor::question[1]")
#  ids <- lapply(speeches, FUN=xpathSApply, path="descendant::name.id", fun="xmlValue")


# ===== INVERTED INDEX FUNCTION! =====

conv <- function(x) {
  l <- function(z) {
    paste(x$x[grep(z, x$y)], collapse=' ')
  }
  lv <- Vectorize(l)
  
  alphabet <- unique(unlist(strsplit(as.character(x$y), ' '))) # hard-coding this might be preferred for some uses.
  y <- lv(alphabet)
  data.frame(y=names(y), x=y)
}

#x <- data.frame(x=1:4, y=c("hello what when", "hello", "when", "hello what")) # ACTIVATE!
#conv(x) # ACTIVATE!

# ===== CREATE CORPUS AND TERM-DOCUMENT MATRIX =====

tdmfunction <- function(x) {
  textcorpus <- Corpus(VectorSource(as.character(masterframe.df[,10])))
  inspect(textcorpus[1:2])
  
  textcorpus <- tm_map(textcorpus, stripWhitespace)
  textcorpus <- tm_map(textcorpus, tolower)
  textcorpus <- tm_map(textcorpus, removeWords, stopwords("english"))
  textcorpus <- tm_map(textcorpus, stemDocument)
  
  meta(textcorpus, "speaker") <- as.character(masterframe.df[,1])
  
  dtm <- DocumentTermMatrix(textcorpus)
  sparsedtm <- removeSparseTerms(dtm, 0.995)
  inspect(dtm[1:10, 100:110])
  inspect(sparsedtm[100:110, 100:1000])
}
# ===== BIGRAM TOKENIZER! =====

bigramfunction <- function (x) {

  
  testcorpus <- Corpus(VectorSource(masterframe.df[1:100, 10]))
  
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  tdm <- TermDocumentMatrix(testcorpus, control = list(tokenize = BigramTokenizer))
  
  inspect(tdm[340:345,1:100])
  
}