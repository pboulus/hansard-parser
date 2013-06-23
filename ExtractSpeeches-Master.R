# ===== PAUL BOULUS - THESIS 2013 =====

# ===== CODE TO CALL FUNCTIONS =====

# SpeechID <- c(1:nrow(speechdata))
# speechdata <- cbind(SpeechID, speechdata)


# ===== Create and save master data frame from folders
# masterframe.senate.1 <- filestodata(filestart=1, folderstart=5, folderstop=5)
# masterframe.senate.2 <- filestodata(filestart=1, folderstart=6, folderstop=6)
# masterframe.senate.3 <- filestodata(filestart=1, folderstart=7, folderstop=7)
# masterframe.senate.4 <- filestodata(filestart=1, folderstart=8, folderstop=8)

# ===== Save/Load masterframe object
# save(masterframe.senate, file="/Users/Paul/ThesisData/20130621-Senate-MasterFrame.robject")
# save(masterframe, file="/Users/Paul/Thesis/20130620-House-MasterFrame.robject")
# load(file="/Users/Paul/ThesisData/20130620-House-MasterFrame.robject")
# load(file="/Users/Paul/ThesisData/20130621-Senate-MasterFrame.robject")
# load(file="/Users/Paul/ThesisData/20130622-Combined-MasterFrame.robject")
# load(file="/Users/Paul/ThesisData/20130620 - It Works.RData")

# ===== Subset masterframe to speeches only
# speechdata <- subset(masterframe, SetType=="speech") #subset masterframe into speeches only (or questions, or answers)
#  howardspeeches <- subset(masterframe, SpeakerID=="ZD4") #subset masterframe into speeches only (or questions, or answers)


# ===== SPARE CODE =====
#  for(i in 1:25){print(xpathSApply(speeches[[i]], "ancestor::subdebate.1[1]"))} #finds all subdebate.1 nodes
#  getNodeSet(speeches[[1]], "ancestor::debate[1]")[[1]] #this returns the debate that parents the question
#  getNodeSet(speeches[[1]], "ancestor::question[1]")
#  ids <- lapply(speeches, FUN=xpathSApply, path="descendant::name.id", fun="xmlValue")

# ===== CALL PACKAGES, DEFINE GLOBAL OPTIONS =====

Sys.setenv(NOAWT=1) # required for snowball/rjava package

set.seed(50)

library("XML")
library("RWeka")
library("rJava")
library("tm")
library("Snowball")
library("SnowballC")
library("Matrix")
library("RWekajars")
library("Rhetpack")
library("zoo")
library("xts")

options(stringsAsFactors = FALSE)

#Create a list of vectors from each folder of source files. These have slightly different schema. House/Senate folder 2 contains SGML files.
filenames <- list(list.files(path="/Users/Paul/ThesisData/Sources/House/1/", full.names=TRUE),
                  list.files(path="/Users/Paul/ThesisData/Sources/House/2/", full.names=TRUE),
                  list.files(path="/Users/Paul/ThesisData/Sources/House/3/", full.names=TRUE),
                  list.files(path="/Users/Paul/ThesisData/Sources/House/4/", full.names=TRUE),
                  list.files(path="/Users/Paul/ThesisData/Sources/Senate/1/", full.names=TRUE),
                  list.files(path="/Users/Paul/ThesisData/Sources/Senate/2/", full.names=TRUE),
                  list.files(path="/Users/Paul/ThesisData/Sources/Senate/3/", full.names=TRUE),
                  list.files(path="/Users/Paul/ThesisData/Sources/Senate/4/", full.names=TRUE))

# ===== DEFINE FUNCTIONS =====

filestodata <- function(filestart=1, filestop, folderstart, folderstop){
  
  masterframe.list <- list()
  
  for (j in folderstart:folderstop){
    
    #     House/Senate folder 2 contain converted SGML files. Pass these filetypes as SGML to the parse functions.
    if (any(j==2, j==6)){
      filetype <- "SGML"
    }else{
      filetype <- "XML"
    }
    
    if (j>=5){
      house <- "senate"
    }else{
      house <- "house"
    }
    
    #     Loop through each file in the list of filenames  
    for(i in filestart:length(filenames[[j]])){ # <- this line will loop through a selection of files
      print(i)
      masterframe.list[[length(masterframe.list)+1]] <- parsexml(filenames[[j]][i], filetype=filetype, house=house)
    }
  }
  
  masterframe.df <- do.call("rbind", masterframe.list) # this line binds the list created from all files into one big dataframe
  SpeechIDs <- c(1:nrow(masterframe.df))
  masterframe.df <- cbind(SpeechIDs, masterframe.df)
  masterframe.df
}

nodesettoframe <- function(nodeset, hansarddate, nodesettype, breakintoparas=TRUE, filetype="XML", house="house"){
  #Define function - receive speeches and convert each para of the speech into a row of a dataframe
  #function receives speeches, which is a node set. subset with [[number]]
  #Create a dataframe 'masterframe1' with variables as follows:
  masterframe1 <- data.frame(SpeakerID = character(),
                             Speaker = character(),
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
  
  #   Check whether filetype is XML or SGML to apply appropriate XPath parseing.
  if (filetype=="XML"){
    name <- as.vector(xpathApply(nodeset, "descendant::name", xmlValue))
    nameid <- as.vector(xpathApply(nodeset, "descendant::name.id", xmlValue)) #for XML
    electorate <- as.vector(xpathApply(nodeset, "descendant::electorate", xmlValue)) #for XML
    party <- as.vector(xpathApply(nodeset, "descendant::party", xmlValue)) #for XML
    in.gov <- as.vector(xpathApply(nodeset, "descendant::in.gov", xmlValue)) #for XML
    role <- as.vector(xpathApply(nodeset, "descendant::role", xmlValue)) #for XML
    paras <- xpathSApply(nodeset, "descendant::para | descendant::p", xmlValue)
    parentdebate <- try(getNodeSet(nodeset[[1]], "ancestor::debate[1]")[[1]], silent=TRUE) #this returns the debate that parents the node
    #this returns the debate title
    debatetitle <- ""
    if (class(parentdebate) != "try-error"){debatetitle <-  try(as.vector(xpathSApply(parentdebate, "descendant::debateinfo//title", xmlValue)), silent=TRUE)} 
  }else{
    name <- as.vector(xmlAttrs(nodeset)["speaker"])
    nameid <- as.vector(xmlAttrs(nodeset)["nameid"]) 
    electorate <- as.vector(xmlAttrs(nodeset)["electorate"])
    party <- as.vector(xmlAttrs(nodeset)["party"]) 
    in.gov <- as.vector(xmlAttrs(nodeset)["gov"]) 
    role <- as.vector(xmlAttrs(nodeset)["ministerial"]) 
    paras <- xpathSApply(nodeset, "descendant::para | descendant::p", xmlValue)
    parentdebate <- try(getNodeSet(nodeset[[1]], "ancestor::debate[1]")[[1]], silent=TRUE) #this returns the debate that parents the node
    #this returns the debate title
    debatetitle <- ""
    if (class(parentdebate) != "try-error"){debatetitle <-  try(xpathSApply(parentdebate, "descendant::title", xmlValue), silent=TRUE)} 
  }
  
  #Add data into masterframe1 as rows, either as whole chunks or broken down into paragraphs depending on parameter
  if (breakintoparas == TRUE) {
    for(para in paras){
      rowvector <- as.character(c(nameid[1],
                                  name[[1]],
                                  electorate[1],
                                  party[1],
                                  in.gov[1],
                                  role[1],
                                  debatetitle[1], 
                                  hansarddate[1], 
                                  house,
                                  nodesettype,
                                  para[[1]]))
      masterframe1[nrow(masterframe1)+1,] <- rowvector
    }
  } else {
    rowvector <- as.character(c(nameid[1],
                                name[[1]],
                                electorate[1],
                                party[1],
                                in.gov[1],
                                role[1],
                                debatetitle[1], 
                                hansarddate[1], 
                                house,
                                nodesettype,
                                paste(as.character(paras), collapse="")))
    masterframe1[nrow(masterframe1)+1,] <- rowvector
    
  }
  #return masterframe1
  masterframe1
}

parsexml <- function(filenamepath, filetype="XML", house="house", breakintoparas = FALSE){  
  doc <- xmlInternalTreeParse(filenamepath) #read xml into memory #speeches[[14]] contains "continue" tags
  root <- xmlRoot(doc) #read root of xml
  
  # get all the base element nodesets in the file
  
  if(filetype=="XML"){
    XMLdate <- xpathApply(root, "//date", xmlValue)[[1]] #for XML data
  }else{
    XMLdate <- xpathApply(root, "//hansard/@date")
  }
  
  
  speeches <- getNodeSet(root, "//speech") #get all speeches
  questions <- xpathApply(root,  "//question") #get all questions 
  answers <- xpathApply(root, "//answer") #get all answers
  interjections <- xpathApply(root, "//interjection") #get all interjections
  continues <- xpathApply(root, "//continue") #get all continues
  
  
  ids <-xpathApply(root, "//speech//name.id", xmlValue) #what's the point of this?
  
  interjections.list <-  list()
  speeches.list <- list()
  continues.list <- list()
  questions.list <- list()
  answers.list <- list()
  
  
  if(xmlSize(speeches)>0){
    for(i in 1:xmlSize(speeches)){
      speeches.list[[i]] <- nodesettoframe(speeches[[i]], XMLdate, "speech", breakintoparas = breakintoparas, filetype=filetype, house=house)
    }
  }
  
  xpathApply(root, "//speech", removeNodes) #remove nodes we don't want after we've finished with them?
  
  if(xmlSize(questions)>0){
    for(i in 1:xmlSize(questions)){
      questions.list[[i]] <- nodesettoframe(questions[[i]], XMLdate, "question", breakintoparas = breakintoparas, filetype=filetype, house=house)
    }
  }
  xpathApply(root, "//question", removeNodes) #remove nodes we don't want after we've finished with them?
  
  if(xmlSize(answers)>0){
    for(i in 1:xmlSize(answers)){
      answers.list[[i]] <- nodesettoframe(answers[[i]], XMLdate, "answer", breakintoparas = breakintoparas, filetype=filetype, house=house)
    }
  }
  xpathApply(root, "//answer", removeNodes) #remove nodes we don't want after we've finished with them?
  
  
  print(paste("No of questions: ", length(questions.list)))
  print(paste("No of answers: ", length(answers.list)))
  print(paste("No of speeches: ", length(speeches.list)))
  
  big.df <- do.call("rbind", c(questions.list, speeches.list, answers.list))
  big.df
}

tdmfunction <- function(x) {
  
  textcorpus <- Corpus(VectorSource(as.character(speechdata[,"Paratext"])))
  meta(textcorpus, "speakerid") <- as.character(speechdata[,"SpeakerID"])
  meta(textcorpus, "party") <- as.character(speechdata[,"Party"])
  meta(textcorpus, "date") <- as.character(speechdata[,"Date"])
  meta(textcorpus, "speechid") <- as.integer(speechdata[,"SpeechID"])
  inspect(textcorpus[120:130])
  
  
  
  
  textcorpus <- tm_map(textcorpus, stripWhitespace)
  textcorpus <- tm_map(textcorpus, tolower)
  textcorpus <- tm_map(textcorpus, removePunctuation)
  textcorpus <- tm_map(textcorpus, removeNumbers)
  textcorpus <- tm_map(textcorpus, removeWords, stopwords("english"))
  textcorpus <- tm_map(textcorpus, stemDocument)
  
  inspect(textcorpus[120:130])
  
  dtm <- TermDocumentMatrix(textcorpus) #create term document matrix
  colnames(dtm) <- as.character(speechdata[,"SpeakerID"]) #creates columnname vector with speaker ids
  sparsedtm <- removeSparseTerms(dtm, 0.995) #removes sparseness from the matrix
  inspect(dtm[1:50, 1:50])
  inspect(sparsedtm[1:10, 1:10])
  
  
  Sys.setenv(NOAWT=0) # debugging?
  
  
  #create sparse matrix in triplet form
  
  ncol(sparsedtm)
  nrow(sparsedtm)
  TmpX <- as(as.matrix(sparsedtm), "dgTMatrix")
  TmpX[1:12, 1:10]
  X3col <- matrix(c(TmpX@i+1, TmpX@j+1, TmpX@x), ncol = 3) # +1 because dgTMatrix starts indexing at 0
  X3col <- X3col[order(X3col[,1], X3col[,2], decreasing=FALSE),] #order dgTMatrix by 1st then 2nd columns, ascending
  
  speechdates <- as.Date(speechdata[,"Date"]) #gets speech dates
  speechdates <- as.numeric(speechdates) #converts dates to integers
  #as.Date(speechdates[1], origin="1970-01-01") # converts integers back to dates
  datenames <- as.numeric(speechdates[1]:speechdates[length(speechdates)]) #creates an ordered sequence from the first date in the corpus, to the last date in the corpus
  datenames <- as.character(as.Date(datenames, origin="1970-01-01")) #convert these integers to date names
  #we want the speechdates vector to start from 1, for correct name indexing so we need to subtract speechdates[1]-1 from all date values in speechdates
  speechdates <- speechdates-(speechdates[1]-1)
  
  
  
  sparsemat <- list(data=X3col, rownames(sparsedtm), as.character(c(1:20000))) #create list object required by EMDynMultiMix
  
  #===== Save/load objects for EMDynMultMix
  save(sparsemat, file="/Users/Paul/ThesisData/SparseTermDocumentMatrixObject.robject")
  save(speechdates, file="/Users/Paul/ThesisData/SpeechDates.robject")
  save(datenames, file="/Users/Paul/ThesisData/DateNames.robject")
  
  load(file="/Users/Paul/ThesisData/SparseTermDocumentMatrixObject.robject")
  load(file="/Users/Paul/ThesisData/SpeechDates.robject")
  load(file="/Users/Paul/ThesisData/DateNames.robject")
  
  
  #this one crashes
    out <- EMDynMultMix(sparsemat, timevec=speechdates, time.names=datenames, kernwidth=20, nclust=40, EMmaxiter=500,
                      EMtol=1e-5,
                      priorcount=1.01,
                      lazythresh=1.0, fullEMiter=1, initfullEMiter=1,
                      aaa=1.0, bbb=1.0, ccc=1.0, ddd=1.0, C0=100,
                      V00=10)
  
  #this one works
    out <- EMDynMultMix(sparsemat, timevec=speechdates, time.names=datenames, kernwidth=250, nclust=30, EMmaxiter=100,
                      EMtol=1e-5, theta.start=NA, pi.start=NA,
                      clustprobs.start=NA, priorcount=1.0,
                      lazythresh=0.99, fullEMiter=1, initfullEMiter=1,
                      aaa=1.0, bbb=1.0, ccc=1.0, ddd=1.0, C0=100,
                      V00=10)
  #20130623 - current model
    out <- EMDynMultMix(sparsemat, timevec=speechdates, time.names=datenames, kernwidth=20, nclust=40, EMmaxiter=100,
                      EMtol=1e-5, theta.start=NA, pi.start=NA,
                      clustprobs.start=NA, priorcount=1.01,
                      lazythresh=1.0, fullEMiter=1, initfullEMiter=1,
                      aaa=1.0, bbb=1.0, ccc=1.0, ddd=1.0, C0=100,
                      V00=10)
  
  
  topicindex.speeches <- numeric(length=0)
  topicindex.words <- numeric(length=0)
  
  for(i in 1:nrow(out$z)){
    topicindex.speeches[i] <- which.max(out$z[i,]) #generate vector of categories for each 
  }
  
  speechdata <- cbind(speechdata, topicindex.speeches)
  
  for(i in 1:nrow(out$beta)){
    topicindex.words[i] <- which.max(out$beta[i,]) #generate vector of categories for each 
  }
  
  
  
  print(table(topicindex)) # print frequencies for each category
  
  #=====Generate top 20 words for each category and export to topwords dataframe
  topwords <- list(0)
  for(j in 1:30){
    topwords[[j]] <- names((out$beta[order(out$beta[,j], decreasing=TRUE),])[1:20,j])
  }
  topwords <- do.call("cbind", topwords)
  categorynamevector <- character(length=30)
  for(i in 1:30){
    categorynamevector[i]<-paste("Category ", i)
  }
  colnames(topwords) <- categorynamevector
  #=====
  
  
  #=====Add n words per document to the speechdata matrix
  
  speechdata <- cbind(speechdata, out$nwords.perdoc)
  
  
  #=====Create timeseries object (xts) of a particular category, and then aggregate number of words by day. Create a list of these.
  
  speechdata.topiclist <- list()
  
  for(i in 1:30){
  speechdata.topiclist[[i]] <- subset(speechdata, topicindex==i)
  speechdata.topiclist[[i]] <- speechdata.topiclist[[i]][,c(8,13)]
  timeseries <- xts(speechdata.topiclist[[i]][,2], as.Date(speechdata.topiclist[[i]][,1]))
  speechdata.topiclist[[i]] <- aggregate(timeseries, time(timeseries), sum)
  }

  
  
  #findAssocs(sparsedtm, "migrat", 0.1) #find associations between words
  
}


# ===== BIGRAM TOKENIZER! =====
bigramfunction <- function (x) {
  
  
  testcorpus <- Corpus(VectorSource(masterframe.df[1:10, 10]))
  
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  tdm <- TermDocumentMatrix(testcorpus, control = list(tokenize = BigramTokenizer))
  
  inspect(tdm[340:345,1:100])
  
}