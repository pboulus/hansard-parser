# ===== CALL PACKAGES, DEFINE GLOBAL OPTIONS =====

Sys.setenv(NOAWT=1) # required for snowball/rjava package

library("XML")
library("RWeka")
library("rJava")
library("tm")
library("Snowball")
library("Matrix")
library("RWekajars")
library("Rhetpack")

options(stringsAsFactors = FALSE)
filenames <- list.files(path="/Users/Paul/Thesis/Sources/House/4/", full.names=TRUE)

# ===== DEFINE FUNCTIONS =====

nodesettoframe <- function(nodeset, hansarddate, nodesettype, breakintoparas=TRUE){
  #Define function - receive speeches and convert each para of the speech into a row of a dataframe
  #function receives speeches, which is a node set. subset with [[number]
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
  
  
  paras <- xpathSApply(nodeset, "descendant::para | descendant::p", xmlValue)
  
  parentdebate <- try(getNodeSet(nodeset[[1]], "ancestor::debate[1]")[[1]], silent=TRUE) #this returns the debate that parents the node
  debatetitle <-  try(as.vector(xpathSApply(parentdebate, "descendant::debateinfo//title", xmlValue)), silent=TRUE) #this returns the debate title
  if (breakintoparas == TRUE) {
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
  } else {
    rowvector <- as.character(c(nameid[[1]],
                                electorate[1],
                                party[1],
                                in.gov[1],
                                role[1],
                                debatetitle[1], 
                                hansarddate[1], 
                                "House",
                                nodesettype,
                                paste(as.character(paras), collapse="")))
    masterframe1[nrow(masterframe1)+1,] <- rowvector
    
  }
  masterframe1
}


### PARSE XML FUNCTION ###
parsexml <- function(filenamepath){  
  doc <- xmlInternalTreeParse(filenamepath) #read xml into memory #speeches[[14]] contains "continue" tags
  root <- xmlRoot(doc) #read root of xml
  
  # get all the base element nodesets in the file
  
  XMLdate <- xpathApply(root, "//date", xmlValue)[[1]] #for XML data
  speeches <- getNodeSet(root, "//speech") #get all speeches
  questions <- xpathApply(root,  "//question") #get all questions 
  answers <- xpathApply(root, "//answer") #get all answers
  interjections <- xpathApply(root, "//interjection")
  continues <- xpathApply(root, "//continue")
  
  
  ids <-xpathApply(root, "//speech//name.id", xmlValue)
  
  interjections.list <-  list()
  speeches.list <- list()
  continues.list <- list()
  
  if(xmlSize(interjections)>0){
    for(i in 1:xmlSize(interjections)){
      interjections.list[[i]] <- nodesettoframe(interjections[[i]], XMLdate, "interjection", breakintoparas = FALSE)
    }
  }
  
  xpathApply(root, "//interjection", removeNodes) #remove nodes we don't want after we've finished with them?
  
  
  if(xmlSize(continues)>0){
    for(i in 1:xmlSize(continues)){
      continues.list[[i]] <- nodesettoframe(continues[[i]], XMLdate, "continue", breakintoparas = FALSE)
    }
  }
  
  xpathApply(root, "//continue", removeNodes) #remove nodes we don't want after we've finished with them?
  
  if(xmlSize(speeches)>0){
    for(i in 1:xmlSize(speeches)){
      speeches.list[[i]] <- nodesettoframe(speeches[[i]], XMLdate, "speech", breakintoparas = FALSE)
    }
  }
  
  print(paste("No of interjections: ", length(interjections.list)))
  print(paste("No of continues: ", length(continues.list)))
  print(paste("No of speeches: ", length(speeches.list)))
  
  big.df <- do.call("rbind", c(interjections.list, speeches.list, continues.list))
  big.df
}

### PARSE XML FUNCTION - THIS TIME WITH WHOLE SPEECHES###

parsexmlwholespeeches <- function(filenamepath){  
  doc <- xmlInternalTreeParse(filenamepath) #read xml into memory #speeches[[14]] contains "continue" tags
  root <- xmlRoot(doc) #read root of xml
  
  # get all the base element nodesets in the file
  
  XMLdate <- xpathApply(root, "//date", xmlValue)[[1]] #for XML data
  speeches <- getNodeSet(root, "//speech") #get all speeches
  questions <- xpathApply(root,  "//question") #get all questions 
  answers <- xpathApply(root, "//answer") #get all answers
  interjections <- xpathApply(root, "//interjection")
  continues <- xpathApply(root, "//continue")
  
  
  ids <-xpathApply(root, "//speech//name.id", xmlValue)
  
  interjections.list <-  list()
  speeches.list <- list()
  continues.list <- list()
  questions.list <- list()
  answers.list <- list()
  
  
  if(xmlSize(speeches)>0){
    for(i in 1:xmlSize(speeches)){
      speeches.list[[i]] <- nodesettoframe(speeches[[i]], XMLdate, "speech", breakintoparas = FALSE)
    }
  }
  xpathApply(root, "//speech", removeNodes) #remove nodes we don't want after we've finished with them?
  
  if(xmlSize(questions)>0){
    for(i in 1:xmlSize(questions)){
      questions.list[[i]] <- nodesettoframe(questions[[i]], XMLdate, "question", breakintoparas = FALSE)
    }
  }
  xpathApply(root, "//question", removeNodes) #remove nodes we don't want after we've finished with them?
  
  if(xmlSize(answers)>0){
    for(i in 1:xmlSize(answers)){
      answers.list[[i]] <- nodesettoframe(answers[[i]], XMLdate, "answer", breakintoparas = FALSE)
    }
  }
  xpathApply(root, "//answer", removeNodes) #remove nodes we don't want after we've finished with them?
  
  
  print(paste("No of questions: ", length(questions.list)))
  print(paste("No of answers: ", length(answers.list)))
  print(paste("No of speeches: ", length(speeches.list)))
  
  big.df <- do.call("rbind", c(questions.list, speeches.list, answers.list))
  big.df
}


# ===== CALL THE FUNCTIONS TO CREATE A MASTER DATASET =====
filestodata <- function(x, y){
  masterframe.list <- list()
  #for(i in 1:length(filenames)){     #  <- this line will loop through all files 
  for(i in x:y){ # <- this line will loop through a selection of files
    print(i)
    masterframe.list[[i]] <- parsexmlwholespeeches(filenames[[i]])
  }
  
  masterframe.df <- do.call("rbind", masterframe.list) # this line binds the list created from all files into one big dataframe
  masterframe.df

#write.table(masterframe.df, file="test.csv", sep=",") # TO WRITE TO CSV FILE

#for xmlfile in filenames {
#  parsexml(xmlfile)
#}

}


# ===== SPARE CODE =====
#  for(i in 1:25){print(xpathSApply(speeches[[i]], "ancestor::subdebate.1[1]"))} #finds all subdebate.1 nodes
#  getNodeSet(speeches[[1]], "ancestor::debate[1]")[[1]] #this returns the debate that parents the question
#  getNodeSet(speeches[[1]], "ancestor::question[1]")
#  ids <- lapply(speeches, FUN=xpathSApply, path="descendant::name.id", fun="xmlValue")




# ===== CREATE CORPUS AND TERM-DOCUMENT MATRIX =====

tdmfunction <- function(x) {
  textcorpus <- Corpus(VectorSource(as.character(speechframe.df[,10])))
  inspect(textcorpus[120:130])
    
  textcorpus <- tm_map(textcorpus, stripWhitespace)
  textcorpus <- tm_map(textcorpus, tolower)
  textcorpus <- tm_map(textcorpus, removeWords, stopwords("english"))
  textcorpus <- tm_map(textcorpus, stemDocument)
  
  inspect(textcorpus[120:130])
  
  dtm <- TermDocumentMatrix(textcorpus)
  colnames(dtm) <- as.character(speechframe.df[,1])
  sparsedtm <- removeSparseTerms(dtm, 0.98)
  inspect(dtm[1:50, 1:50])
  inspect(sparsedtm[80:90, 1:10])
  
  Sys.setenv(NOAWT=0) # debugging?
  
  
  #create sparse matrix in triplet form
  
  ncol(sparsedtm)
  TmpX <- as(as.matrix(sparsedtm), "dgTMatrix")
  TmpX[1:12, 1:10]
  X3col <- matrix(c(TmpX@i+1, TmpX@j+1, TmpX@x), ncol = 3) # +1 because dgTMatrix starts indexing at 0
  sparsemat <- list(data=X3col, as.character(rownames(sparsedtm)), as.character(colnames(sparsedtm)))
  save(sparsemat, file="/Users/Paul/Desktop/test.robject")
  load(file="/Users/Paul/Desktop/test.robject")
  numberofdocs <- ncol(sparsedtm)
  out <- EMDynMultMix(sparsemat, as.integer(rep.int(1, times=numberofdocs)), as.character("Day 1"), kernwidth=20, nclust=30, EMmaxiter=500,
               EMtol=1e-5, theta.start=NA, pi.start=NA,
               clustprobs.start=NA, priorcount=1.0,
               lazythresh=0.99, fullEMiter=1, initfullEMiter=1,
               aaa=1.0, bbb=1.0, ccc=1.0, ddd=1.0, C0=100,
               V00=10)
}






# ===== BIGRAM TOKENIZER! =====

bigramfunction <- function (x) {
  
  
  testcorpus <- Corpus(VectorSource(masterframe.df[1:10, 10]))
  
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  tdm <- TermDocumentMatrix(testcorpus, control = list(tokenize = BigramTokenizer))
  
  inspect(tdm[340:345,1:100])
  
}