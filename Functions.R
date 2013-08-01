# ===== PAUL BOULUS - THESIS 2013
# ===== FUNCTIONS FILE 
# This file defines the functions used throughout the thesis project. It does not execute anything.
# 20130801


# ===== DEFINE FUNCTIONS =====
process.frames <- function(){
  #===== Create and save master data frame from folders =====
  masterframe.house.1 <- filestodata(filestart=1, folderstart=1, folderstop=1)
  saveObject(masterframe.house.1, file=paste(options$data.dir, "House.1.RData", sep=""))
  rm(masterframe.house.1)
  gc()
  masterframe.house.2 <- filestodata(filestart=1, folderstart=2, folderstop=2)
  saveObject(masterframe.house.2, file=paste(options$data.dir, "House.2.RData", sep=""))
  rm(masterframe.house.2)
  gc()
  masterframe.house.3 <- filestodata(filestart=1, folderstart=3, folderstop=3)
  saveObject(masterframe.house.3, file=paste(options$data.dir, "House.3.RData", sep=""))
  rm(masterframe.house.3)
  gc()
  masterframe.house.4 <- filestodata(filestart=1, folderstart=4, folderstop=4)
  saveObject(masterframe.house.4, file=paste(options$data.dir, "House.4.RData", sep=""))
  rm(masterframe.house.4)
  gc()
  masterframe.senate.1 <- filestodata(filestart=1, folderstart=5, folderstop=5)
  saveObject(masterframe.senate.1, file=paste(options$data.dir, "Senate.1.RData", sep=""))
  rm(masterframe.senate.1)
  gc()
  masterframe.senate.2 <- filestodata(filestart=1, folderstart=6, folderstop=6)
  saveObject(masterframe.senate.2, file=paste(options$data.dir, "Senate.2.RData", sep=""))
  rm(masterframe.senate.2)
  gc()
  masterframe.senate.3 <- filestodata(filestart=1, folderstart=7, folderstop=7)
  saveObject(masterframe.senate.3, file=paste(options$data.dir, "Senate.3.RData", sep=""))
  rm(masterframe.senate.3)
  gc()
  masterframe.senate.4 <- filestodata(filestart=1, folderstart=8, folderstop=8)
  saveObject(masterframe.senate.4, file=paste(options$data.dir, "Senate.4.RData", sep=""))
  rm(masterframe.senate.4)
  gc()
}

load.frames <- function(){
  # ===== Load all frames into a masterframe object =====
  frame.list <- list()
  frame.list[[1]] <- loadObject(paste(options$data.dir, "House.1.RData", sep=""))
  frame.list[[2]] <- loadObject(paste(options$data.dir, "House.2.RData", sep=""))
  frame.list[[3]] <- loadObject(paste(options$data.dir, "House.3.RData", sep=""))
  frame.list[[4]] <- loadObject(paste(options$data.dir, "House.4.RData", sep=""))
  frame.list[[5]] <- loadObject(paste(options$data.dir, "Senate.1.RData", sep=""))
  frame.list[[6]] <- loadObject(paste(options$data.dir, "Senate.2.RData", sep=""))
  frame.list[[7]] <- loadObject(paste(options$data.dir, "Senate.3.RData", sep=""))
  frame.list[[8]] <- loadObject(paste(options$data.dir, "Senate.4.RData", sep=""))
  
  masterframe <- do.call("rbind", frame.list) # combine all frames into a master data frame
  rm(frame.list)
  gc()
  return(masterframe)
}

filestodata <- function(filestart=1, filestop=0, folderstart=1, folderstop=8, includeinterjections=FALSE){
  
  filenames <- list(list.files(path=paste(options$source.dir, "House/1/", sep=""), full.names=TRUE),
                    list.files(path=paste(options$source.dir, "House/2/", sep=""), full.names=TRUE),
                    list.files(path=paste(options$source.dir, "House/3/", sep=""), full.names=TRUE),
                    list.files(path=paste(options$source.dir, "House/4/", sep=""), full.names=TRUE),
                    list.files(path=paste(options$source.dir, "Senate/1/", sep=""), full.names=TRUE),
                    list.files(path=paste(options$source.dir, "Senate/2/", sep=""), full.names=TRUE),
                    list.files(path=paste(options$source.dir, "Senate/3/", sep=""), full.names=TRUE),
                    list.files(path=paste(options$source.dir, "Senate/4/", sep=""), full.names=TRUE))
  
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
    
    
    if (filestop==0){
      filestop.loop <- length(filenames[[j]]) #if filestop is not specified as function input, loop through all filenames in the folder
    }else{
      filestop.loop <- filestop #if filestop is specified as function input, stop loop at this point
    }
    
    #     Loop through each file in the list of filenames  
    for(i in filestart:filestop.loop){ # <- this line will loop through a selection of files
      print(i)
      masterframe.list[[length(masterframe.list)+1]] <- parsexml(filenames[[j]][i], filetype=filetype, house=house, includeinterjections=includeinterjections)
    }
  }
  
  masterframe.df <- do.call("rbind", masterframe.list) # this line binds the list created from all files into one big dataframe
  SpeechID <- c(1:nrow(masterframe.df))
  masterframe.df <- cbind(SpeechID, masterframe.df)
  masterframe.df
}

nodesettoframe <- function(nodeset, hansarddate, nodesettype, breakintoparas=FALSE, filetype="XML", house="unspecified"){
  #Function nodsesettoframe converts a nodeset (e.g. <speech>, <question>, <answer>, <interjection>) and returns a data frame.
  #Will combine paragraphs if breakintoparas=FALSE
  #Will collate paragraphs attributed to different speakers using the name.id
  
  #Create an empty 'node.frame' with variables as follows. Function will return node.frame.
  node.frame <- data.frame(SpeakerID = character(),
                             Speaker = character(),
                             Electorate = character(),
                             Party = character(),
                             InGov = character(),
                             FirstSpeech = character(),
                             Role = character(),
                             Debate = character(),
                             Date = character(),
                             House=character(),
                             SetType=character(),
                             Paratext = character(),
                             stringsAsFactors=FALSE)
  
  #Check whether filetype is XML or SGML to apply appropriate XPath parsing
  if (filetype=="XML"){ #XML parser
    nameids <- unique(unlist(xpathApply(nodeset, "descendant::name.id", xmlValue)))
    para.list <- list()
    
    #Cycle through nameids present in the nodeset, and process paragraphs based on whether they match the nameid
    for (nameid in nameids){
      
      #Create a nodeset of paragraphs within this speech. Schema changes after 2011-03-24, hence two different xpath formulations.
      if(as.Date(hansarddate) > "2011-03-24"){
        para.nodes <- xpathApply(nodeset, paste("descendant::para[ancestor::talk.text/preceding-sibling::talk.start//name.id='",
                                                nameid,
                                                "'] | descendant::p[ancestor::talk.text/preceding-sibling::talk.start//name.id='",
                                                nameid,
                                                "']",
                                                sep=""))    
      }else{
        para.nodes <- xpathApply(nodeset, paste("descendant::para[preceding-sibling::talker[1]//descendant::name.id='",
                                                nameid,
                                                "' or preceding-sibling::talk.start[1]//descendant::name.id='",
                                                nameid,
                                                "'] | descendant::p[preceding-sibling::talker[1]//descendant::name.id='",
                                                nameid,
                                                "' or preceding-sibling::talk.start[1]//descendant::name.id='",
                                                nameid,
                                                "']",
                                                sep=""))
      }
      
      
      
      if(length(para.nodes)==0){next} #Kill loop if no paras are identified for this nameid.
      
      #Loop through paragraphs in the para.nodes object and extract metadata and paragraph text.
      for (para in para.nodes){
        
        if(as.Date(hansarddate)>"2011-03-24"){
          name <- xpathSApply(para, paste("ancestor::talk.text/preceding-sibling::talk.start[descendant::name.id='", nameid, "']//descendant::name", sep=""), xmlValue)
          electorate <- xpathSApply(para, paste("ancestor::talk.text/preceding-sibling::talk.start[descendant::name.id='", nameid, "']//descendant::electorate", sep=""), xmlValue)
          party <- xpathSApply(para, paste("ancestor::talk.text/preceding-sibling::talk.start[descendant::name.id='", nameid, "']//descendant::party", sep=""), xmlValue) 
          in.gov <- xpathSApply(para, paste("ancestor::talk.text/preceding-sibling::talk.start[descendant::name.id='", nameid, "']//descendant::in.gov", sep=""), xmlValue) 
          role <- xpathSApply(para, paste("ancestor::talk.text/preceding-sibling::talk.start[descendant::name.id='", nameid, "']//descendant::role", sep=""), xmlValue) 
          first.speech <- xpathSApply(para, paste("ancestor::talk.text/preceding-sibling::talk.start[descendant::name.id='", nameid, "']//descendant::first.speech", sep=""), xmlValue)
        }else{
          name <- xpathSApply(para, "preceding-sibling::talker[1]//descendant::name | preceding-sibling::talk.start[1]//descendant::name", xmlValue)
          electorate <- xpathSApply(para, "preceding-sibling::talker[1]//descendant::electorate | preceding-sibling::talk.start[1]//descendant::electorate", xmlValue)
          party <- xpathSApply(para, "preceding-sibling::talker[1]//descendant::party | preceding-sibling::talk.start[1]//descendant::party", xmlValue) 
          in.gov <- xpathSApply(para, "preceding-sibling::talker[1]//descendant::in.gov | preceding-sibling::talk.start[1]//descendant::in.gov", xmlValue) 
          role <- xpathSApply(para, "preceding-sibling::talker[1]//descendant::role | preceding-sibling::talk.start[1]//descendant::role", xmlValue) 
          first.speech <- xpathSApply(para, "preceding-sibling::talker[1]//descendant::first.speech | preceding-sibling::talk.start[1]//descendant::first.speech", xmlValue)
        }
        
        para.text <- xmlValue(para)
        parentdebate <- try(getNodeSet(para[[1]], "ancestor::debate[1]")[[1]], silent=TRUE) #this returns the debate that parents the node
        debatetitle <- character()
        if (class(parentdebate) != "try-error"){debatetitle <-  try(as.vector(xpathSApply(parentdebate, "descendant::debateinfo//title", xmlValue)), silent=TRUE)}   #this returns the debate title
        
        sub.frame <- data.frame()
        
        if(length(name)==0){name<-NA}
        if(length(electorate)==0){electorate<-NA}
        if(length(party)==0){party<-NA}
        if(length(in.gov)==0){in.gov<-NA}
        if(length(role)==0){role<-NA}
        if(length(first.speech)==0){first.speech<-NA}
        if(length(para.text)==0){para.text<-NA}
        if(length(debatetitle)==0){debatetitle<-NA}
        
        #Insert paragraph text and meta data into sub.frame object
        sub.frame[1, "SpeakerID"] <- nameid[1]
        sub.frame[1, "Speaker"] <- name[1]
        sub.frame[1, "Electorate"] <- electorate[1]
        sub.frame[1, "Party"] <- party[1]
        sub.frame[1, "InGov"] <- in.gov[1]
        sub.frame[1, "FirstSpeech"] <- first.speech[1]
        sub.frame[1, "Role"] <- role[1]
        sub.frame[1, "Debate"] <- debatetitle[1]
        sub.frame[1, "Date"] <- hansarddate[1]
        sub.frame[1, "House"] <- house[1]
        sub.frame[1, "SetType"] <- nodesettype[1]
        sub.frame[1, "Paratext"] <- para.text[1]
        para.list[[length(para.list)+1]] <- sub.frame
        
      }
      
    }
    
    
  }else{ #SGML parser
    
    #Get metadata from nodeset
    name <- as.vector(xmlAttrs(nodeset)["speaker"])
    nameid <- as.vector(xmlAttrs(nodeset)["nameid"]) 
    electorate <- as.vector(xmlAttrs(nodeset)["electorate"])
    party <- as.vector(xmlAttrs(nodeset)["party"]) 
    in.gov <- as.vector(xmlAttrs(nodeset)["gov"]) 
    first.speech <- "unknown"
    role <- as.vector(xmlAttrs(nodeset)["ministerial"]) 
    paras <- xpathSApply(nodeset, "descendant::para | descendant::p", xmlValue) #get all paras from the nodeset
    parentdebate <- try(getNodeSet(nodeset[[1]], "ancestor::debate[1]")[[1]], silent=TRUE) #this returns the debate that parents the node
    debatetitle <- character()
    if (class(parentdebate) != "try-error"){debatetitle <-  try(xpathSApply(parentdebate, "descendant::title", xmlValue), silent=TRUE)} #this returns the debate title
    
    para.list <- list()
    
    for (para in paras){
      sub.frame <- data.frame()
      
      if(length(name)==0){name<-NA}
      if(length(electorate)==0){electorate<-NA}
      if(length(party)==0){party<-NA}
      if(length(in.gov)==0){in.gov<-NA}
      if(length(role)==0){role<-NA}
      if(length(first.speech)==0){first.speech<-NA}
      if(length(debatetitle)==0){debatetitle<-NA}
      
      sub.frame[1, "SpeakerID"] <- nameid[1]
      sub.frame[1, "Speaker"] <- name[1]
      sub.frame[1, "Electorate"] <- electorate[1]
      sub.frame[1, "Party"] <- party[1]
      sub.frame[1, "InGov"] <- in.gov[1]
      sub.frame[1, "FirstSpeech"] <- first.speech[1]
      sub.frame[1, "Role"] <- role[1]
      sub.frame[1, "Debate"] <- debatetitle[1]
      sub.frame[1, "Date"] <- hansarddate[1]
      sub.frame[1, "House"] <- house[1]
      sub.frame[1, "SetType"] <- nodesettype[1]
      sub.frame[1, "Paratext"] <- para[1]
      
      para.list[[length(para.list)+1]] <- sub.frame
    }
  }
  
  
  #COMBINE OR SPLIT===============================================
  if(length(para.list)==0){return(NULL)}
  if (breakintoparas == TRUE) {
    for(para.element in para.list){
      node.frame <- rbind.fill(node.frame, para.element) # we use rbind.fill to fill with NAs for missing values
    }
  } else {
    #do something to collapse the paratext
    para.text.combined <- character()
    for(para.element in para.list){
      para.text.combined <- c(para.text.combined, para.element$Paratext)
    }
    para.list[[1]]$Paratext <- paste(para.text.combined, collapse="") #put collapsed para.text vector in the first row of subframe
    node.frame <- rbind.fill(node.frame, para.list[[1]]) # we use rbind.fill to fill with NAs for missing values
  }
  
  
  #return node.frame
  return(node.frame)
}

parsexml <- function(filenamepath, filetype="XML", house="house", breakintoparas=FALSE, includeinterjections=FALSE){
  doc <- xmlInternalTreeParse(filenamepath) #read xml into memory #speeches[[14]] contains "continue" tags
  root <- xmlRoot(doc) #read root of xml
  
  # get all the base element nodesets in the file
  
  if(filetype=="XML"){
    XMLdate <- xpathApply(root, "//date", xmlValue)[[1]] #for XML data
  }else{
    XMLdate <- xpathApply(root, "//hansard/@date")
  }
  
  speeches <- getNodeSet(root, "//speech") #get all speeches
  questions <- getNodeSet(root,  "//question") #get all questions 
  answers <- getNodeSet(root, "//answer") #get all answers
  # continues <- xpathApply(root, "//continue") #continues sit within speeches/questions/answers, so there is no need to select them separately
  
  if(filetype=="XML"){
    interjections <- getNodeSet(root, "//interjection")
  }else{
    interjections <- getNodeSet(root, "//interject")
  }
  
  
  # ids <-xpathApply(root, "//speech//name.id", xmlValue) #what's the point of this?
  
  interjections.list <-  list()
  speeches.list <- list()
  continues.list <- list()
  questions.list <- list()
  answers.list <- list()
  
  if(xmlSize(interjections)>0 &&  includeinterjections==TRUE){
    for(i in 1:xmlSize(interjections)){
      interjections.list[[i]] <- nodesettoframe(interjections[[i]], XMLdate, "interjection", breakintoparas = breakintoparas, filetype=filetype, house=house)
    }
  }
  
  if(filetype=="XML"){
    xpathApply(root, "//interjection", removeNodes) #remove nodes we don't want after we've finished with them
  }else{
    xpathApply(root, "//interject", removeNodes) #remove nodes we don't want after we've finished with them
  }
  
  
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
  print(paste("Date: ", XMLdate))
  print(paste("No of interjections: ", length(interjections.list)))
  print(paste("No of questions: ", length(questions.list)))
  print(paste("No of answers: ", length(answers.list)))
  print(paste("No of speeches: ", length(speeches.list)))
  
  big.df <- do.call("rbind", c(interjections.list, questions.list, speeches.list, answers.list))
  big.df
}

create.tdm <- function(filename.master=NULL, masterframe=NULL, block.size=50000, save.tdm=FALSE, save.filename){
  
  #==== Convert mastlerframe into memory management requires us to break the documents down into blocks of 50,000
  #==== Loads from file if a filename is present, otherwise takes the masterframe argument as input
  
  if(!is.null(filename.master)){
    print("Loading master data")
    masterframe <- loadObject(filename.master)
  }
  
  #==== Calculate number of blocks
  n.blocks <- ceiling(nrow(masterframe)/block.size)
  print(paste("Number of blocks", n.blocks))
  
  
  for (i in 1:n.blocks){
    
    if(i!=1 && !is.null(filename.master)){print("Loading master data"); masterframe <- loadObject(filename.master)} #if i=1 the object has already been assigned to masterframe to calculate n.blocks
    
    print (paste("Processing corpus block:", i))
    start <- ((i-1)*block.size)+1
    if(i*block.size>nrow(masterframe)){
      end <- nrow(masterframe)
    }else{
      end <- i*block.size
    }
    
    print("Subsetting master data")
    masterframe.block <- masterframe[start:end,]
    
    if(!is.null(filename.master)){
      print("Dumping master data")
      rm(masterframe)
      gc()
    }
    
    print("Creating named vector of paratext")
    paratext.block <- masterframe.block$Paratext
    names(paratext.block) <- masterframe.block$SpeechID
    
    print("Dumping subsetted data frame")
    rm(masterframe.block)
    
    print("Creating corpus block")
    textcorpus.block <- Corpus(VectorSource(paratext.block))
    gc()
    
    print("Dumping named vector of paratext")
    rm(paratext.block)
        
    print("Processing corpus block into tdm block")
    tdm.block <- processcorpus.tdm(textcorpus=textcorpus.block)
    
    print("Removing corpus block")
    rm(textcorpus.block)
    gc()
    
    print(paste("Number of documents in tdm:", ncol(tdm.block)))
    print(paste("Number of terms in tdm:", nrow(tdm.block)))
    
    #Saving tdm block
    saveObject(tdm.block, file=paste(options$data.dir, "TDMBlock-", i, ".RData", sep=""))
    
  }
  
  #===== Load and reassemble TDM blocks ========
  
  for (i in 1:n.blocks){
    name <- paste(options$data.dir, "TDMBlock-", i, ".RData", sep="")
    print(paste("Loading:", name))
    tdm.block <- loadObject(name)
    
    if (i==1){
      tdm <- loadObject(name) #no concatenation required for first iteration
      next
    }else{
      tdm.block <- loadObject(name)
      tdm <- c(tdm, tdm.block) #concatenate block to 'sparsetdm'
    }
    rm(tdm.block) #remove block once it has been added to 'sparsetdm' 
    gc()
  }
  
  if(save.tdm==TRUE){
    saveObject(tdm, file=save.filename)
  }
  gc() #collect garbage
  return(tdm)
}

processcorpus.tdm <- function(textcorpus){
  print("Stripping whitespace")
  textcorpus <- tm_map(textcorpus, stripWhitespace)
  
  print("Converting to lowercase")
  textcorpus <- tm_map(textcorpus, tolower)
  
  print("Removing punctuation")
  textcorpus <- tm_map(textcorpus, removePunctuation)
  
  print("Removing numbers")
  textcorpus <- tm_map(textcorpus, removeNumbers)
  
  print("Removing stopwords")
  textcorpus <- tm_map(textcorpus, removeWords, stopwords("english"))
  
  print("Stemimng corpus")
  textcorpus <- tm_map(textcorpus, stemDocument)
  
  print("Creating term-document matrix")
  tdm <- TermDocumentMatrix(textcorpus) #create term document matrix
  return(tdm)
}

bigramfunction <- function (x) {
  
  
  testcorpus <- Corpus(VectorSource(masterframe.df[1:10, 10]))
  
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  tdm <- TermDocumentMatrix(testcorpus, control = list(tokenize = BigramTokenizer))
  
  inspect(tdm[340:345,1:100])
  
}

subset.sparsemat <- function(){
  #source("/Users/Paul/Dropbox/ANU/Thesis/RProjects/ExtractSpeeches/Subset.R", verbose=TRUE)
  #.libPaths("/home/rstudio/R/x86_64-pc-linux-gnu-library/3.0/") #for amazon linux
  #install.packages("/Users/Paul/RhetpackV/Rhetpack_Call.tar.gz", repos=NULL, type="source")
  
  library("Rhetpack")
  library("R.utils")
  library("Matrix")
  
  setwd("~/ThesisData/20130716/")
  
  sparsemat <- loadObject("sparsemat.RData")
  timevec <- loadObject("timevec.RData")
  time.names <- loadObject("time.names.RData")
  
  min.doc <- 740000
  max.doc <- 1000000
  #limit appears to be just over 130,000
  #hard max 1062034
  
  # ===== CONVERT TO dgTMAtrix AND SUBSET =====
  working.matrix <- sparseMatrix(i=sparsemat$data[,1], j=sparsemat$data[,2], x=sparsemat$data[,3], index1=TRUE)
  working.matrix <- as(working.matrix, "dgTMatrix")
  rownames(working.matrix) <- sparsemat[[2]]
  colnames(working.matrix) <- sparsemat[[3]]
  
  working.matrix <- working.matrix[,min.doc:max.doc]
  
  # ===== REMOVE EMPTY ROWS =====
  working.matrix <- working.matrix[rowSums(working.matrix)>0,]
  
  # ===== ADD DATA AND ROW/COLUMN NAMES TO SPARSEMAT =====
  sparsemat$data <-subset(matrix(c(working.matrix@i+1, working.matrix@j+1, working.matrix@x), ncol = 3))
  sparsemat[[2]] <- rownames(working.matrix)
  sparsemat[[3]] <- colnames(working.matrix)
  
  # ===== SUBSET TIMEVEC AND TIME.NAMES =====
  timevec <- timevec[min.doc:max.doc]
  timevec <- timevec-timevec[1]+1
  time.names <- time.names[min(timevec):max(timevec)]
  
  # ===== CHECK THAT DIMENSIONS MATCH =====
  min(sparsemat$data[,2]) #first document
  max(sparsemat$data[,2]) #last document
  
  #These must match. If we subset too much we will lose words but not know which ones!
  max(sparsemat$data[,1])
  length(sparsemat[[2]])
}

analyse.model <- function(model.output){
  #=====Generate indices of speech topics, and word topics
  topicindex.speeches <- numeric(length=0)
  topicindex.words <- numeric(length=0)
  
  #generate vector of categories for each speech by calculating the category with the highest probability(z)
  topicindex.speeches <- apply(model.output$z, 1, which.max)
  
  #create a data frame using this vector, and add nwords, and index by SpeechID
  topicindex.speeches.frame <- data.frame(SpeechID=rownames(model.output$z), Category=topicindex.speeches, NWords=model.output$nwords.perdoc)
  
  #merge this data frame with masterframe by SpeechID to account for documents we deleted with zero significant words
  masterframe <- merge(masterframe, topicindex.speeches.frame, by.y="SpeechID") 
  
  #generate vector of categories for each word (beta)
  for(i in 1:nrow(model.output$beta)){
    topicindex.words[i] <- which.max(model.output$beta[i,])
  }
  
  
  #=====Define category names
  category.names <- character()
  for (i in 1:ncol(model.output$beta)){
    category.names[i] <- readline(paste("Category", i, "?"))
  }
  
  
  #=====Generate top 40 words for each category and export to topwords dataframe
  topwords <- list(0)
  for(j in 1:ncol(model.output$beta)){
    topwords[[j]] <- names((model.output$beta[order(model.output$beta[,j], decreasing=TRUE),])[1:40,j])
  }
  topwords <- do.call("cbind", topwords)
  categorynamevector <- character(length=ncol(model.output$beta))
  
  for(i in 1:ncol(model.output$beta)){
    categorynamevector[i]<-paste("Category ", i, ": ", category.names[i], sep="")
  }
  colnames(topwords) <- categorynamevector
  
  #=====Calculate how many category allocations have greater than 95% validity
  max.z.probs <- rowMax(model.output$z)
  length(max.z.probs[max.z.probs>.95])/length(max.z.probs)
  
  #=====Create timeseries object (xts) of a particular category, and then aggregate number of words by day. Create a list of these. Plot.
  speechdata.topiclist <- list()
  for(i in 1:ncol(model.output$beta)){
    speechdata.topiclist[[i]] <- subset(masterframe, Category==i)
    timeseries <- xts(speechdata.topiclist[[i]][,"NWords"], as.Date(speechdata.topiclist[[i]][,"Date"]))
    speechdata.topiclist[[i]] <- aggregate(timeseries, time(timeseries), sum)
    
    #METHOD 1 to get weekly sums
    # this is derived from the nextfri function. Uses 0 instead of 5 to get Sunday.
    # http://stackoverflow.com/questions/13944838/forcing-full-weeks-with-apply-weekly
    # nextsun <- function(x) 7 * ceiling(as.numeric(x-0+4) / 7) + as.Date(0-4)
    # aggregate(timeseries, nextsun, sum)
    
    # METHOD 2 to get weekly sums
    # weekly <- apply.weekly(timeseries, sum)#to get to weekly sums
    
    #plot the xts object 
    pdf(paste("category-", i, ".pdf", sep=""))
    plot(speechdata.topiclist[[i]], main=paste("Category ", i, ": ", category.names[i], sep=""), xlab="Date", ylab="n Words")
    dev.off()
    #try a lattice barchart plot
    #speechdata.data.frame <- as.data.frame(speechdata.topiclist[[i]])
    #speechdata.data.frame <- cbind(speechdata.data.frame, rownames(speechdata.data.frame))
    #colnames(speechdata.data.frame) <- c("Date", "NWords")
    #ggplot(speechdata.data.frame, aes(x=Date, y=NWords)) + geom_bar(stat="identity") + xlab("Date") + ylab("N Words")
  }
  
  #=====Print frequencies for each category
  print(table(topicindex.speeches))
  
  #=====Use MAD method to calculate important word stems
  topwords.MAD <- beta.importance.latex(model.output, 40, method="MAD")
  topwords.MAD <- lapply(topwords.MAD, rownames)
  topwords.MAD <- do.call(cbind, topwords.MAD)
  colnames(topwords.MAD) <- category.names 
  for(i in 1:ncol(topwords.MAD)){
    cat(i, " & ", category.names[i], " & ", paste(topwords.MAD[1:10,i], collapse=", "), "\\\\ \n")
  }
    
  #=====plot pi?
  pi.timeplot(model.output, xlab.interval=365)
  
  #=====plot hierarchical cluster dendogram
  hc <- hclust(dist(t(model.output$beta)))
  plot(hc)
  
  #=====Create vector of postwar election dates
  election.dates <- as.Date(c("1946-09-28",
                              "1949-12-10",
                              "1951-04-28",
                              "1954-05-29",
                              "1955-12-10",
                              "1958-11-22",
                              "1961-12-09",
                              "1963-11-30",
                              "1966-11-26",
                              "1969-10-25",
                              "1972-12-02",
                              "1974-05-18",
                              "1975-12-13",
                              "1977-12-10",
                              "1980-10-18",
                              "1983-03-05",
                              "1984-12-01",
                              "1987-07-11",
                              "1990-03-24",
                              "1993-03-13",
                              "1996-03-02",
                              "1998-10-03",
                              "2001-11-10",
                              "2004-10-09",
                              "2007-11-24",
                              "2010-08-21"),
                            origin="1970-01-01")
}

rowMin <- function(x) { 
  # Construct a call pmin(x[,1],x[,2],...x[,NCOL(x)]) 
  code <- paste("x[,",1:(NCOL(x)),"]",sep="",collapse=",") 
  code <- paste("pmin(",code,")") 
  return(eval(parse(text=code))) 
} 

rowMax <- function(x) { 
  # Construct a call pmin(x[,1],x[,2],...x[,NCOL(x)]) 
  code <- paste("x[,",1:(NCOL(x)),"]",sep="",collapse=",") 
  code <- paste("pmax(",code,")") 
  return(eval(parse(text=code))) 
} 



beta.importance.latex <- function (out, n, topics = NA, topics.compare = NA, method = c("MADdifratio", 
                                                               "SDdifratio", "difference", "largesmall"), onlypos = TRUE, 
          xtable = FALSE, ...) 
{
  nclust <- ncol(out$beta)
  method = match.arg(method)
  if (is.na(topics[1])) {
    topics <- 1:nclust
  }
  if (is.na(topics.compare[1])) {
    topics.compare <- 1:nclust
  }
  if (method == "difference") {
    for (i in topics) {
      for (j in topics.compare) {
        if (j != i) {
          dif <- out$beta[, i] - out$beta[, j]
          ord.indL <- order(dif, decreasing = TRUE)
          ord.indS <- order(dif, decreasing = FALSE)
          col1L <- out$beta[ord.indL, i]
          col2L <- out$beta[ord.indL, j]
          col3L <- col1L - col2L
          outmatL <- cbind(col1L[1:n], col2L[1:n], col3L[1:n])
          colnames(outmatL) <- c(paste("beta", i, sep = ""), 
                                 paste("beta", j, sep = ""), "Difference")
          col1S <- out$beta[ord.indS, i]
          col2S <- out$beta[ord.indS, j]
          col3S <- col1S - col2S
          outmatS <- cbind(col1S[1:n], col2S[1:n], col3S[1:n])
          colnames(outmatS) <- c(paste("beta", i, sep = ""), 
                                 paste("beta", j, sep = ""), "Difference")
          cat(paste("\n\nTopic ", i, " vs. Topic ", j, 
                    "\n", sep = ""))
          cat("  Largest Positive Differences:\n")
          print(outmatL, digits = 3)
          if (!onlypos) {
            cat("  Largest Negative Differences:\n")
            print(outmatS, digits = 3)
          }
        }
      }
    }
  }
  if (method == "largesmall") {
    for (i in topics) {
      largest.ind <- order(out$beta[, i], decreasing = TRUE)
      smallest.ind <- order(out$beta[, i], decreasing = FALSE)
      cat(paste("\n\nbeta", i, "\n", sep = ""))
      cat("largest values:")
      col1 <- cbind(out$beta[largest.ind, i][1:n])
      colnames(col1) <- ""
      print(col1, digits = 3)
      cat("\nsmallest values:")
      col1 <- cbind(out$beta[smallest.ind, i][1:n])
      colnames(col1) <- ""
      print(col1, digits = 3)
    }
  }
  if (method == "SDdifratio") {
    for (i in topics) {
      SDnoti <- apply(out$beta[, -i], 1, sd)
      avgnoti <- apply(out$beta[, -i], 1, mean)
      dif <- out$beta[, i] - avgnoti
      distSDratio <- dif/SDnoti
      ord.indL <- order(distSDratio, decreasing = TRUE)
      ord.indS <- order(distSDratio, decreasing = FALSE)
      col1L <- out$beta[ord.indL, i]
      col2L <- avgnoti[ord.indL]
      col3L <- col1L - col2L
      col4L <- SDnoti[ord.indL]
      col5L <- distSDratio[ord.indL]
      outmatL <- cbind(col1L[1:n], col2L[1:n], col3L[1:n], 
                       col4L[1:n], col5L[1:n])
      colnames(outmatL) <- c(paste("beta", i, sep = ""), 
                             paste("avg. ~i", sep = ""), "Difference", "SD ~i", 
                             "ratio")
      col1S <- out$beta[ord.indS, i]
      col2S <- avgnoti[ord.indS]
      col3S <- col1S - col2S
      col4S <- SDnoti[ord.indS]
      col5S <- distSDratio[ord.indS]
      outmatS <- cbind(col1S[1:n], col2S[1:n], col3S[1:n], 
                       col4S[1:n], col5S[1:n])
      colnames(outmatS) <- c(paste("beta", i, sep = ""), 
                             paste("avg. ~i", sep = ""), "Difference", "SD ~i", 
                             "ratio")
      cat(paste("\n\nTopic ", i, " vs. All Others\n", sep = ""))
      cat("  Largest Positive Ratios:\n")
      print(outmatL, digits = 3)
      if (!onlypos) {
        cat("  Largest Negative Ratios:\n")
        print(outmatS, digits = 3)
      }
    }
  }
  if (method == "MADdifratio") {
  keywords.list <- list()  
    for (i in topics) {
      MADnoti <- apply(as.matrix(out$beta[, topics.compare[topics.compare != 
                                                             i]]), 1, mad)
      MADnoti[MADnoti == 0] <- 1
      mediannoti <- apply(as.matrix(out$beta[, topics.compare[topics.compare != 
                                                                i]]), 1, median)
      dif <- out$beta[, i] - mediannoti
      distMADratio <- dif/MADnoti
      criterion <- rank(distMADratio) + rank(out$beta[, 
                                                      i])
      ord.indL <- order(criterion, decreasing = TRUE)
      ord.indS <- order(criterion, decreasing = FALSE)
      col1L <- out$beta[ord.indL, i]
      col2L <- mediannoti[ord.indL]
      col3L <- col1L - col2L
      col4L <- MADnoti[ord.indL]
      col5L <- distMADratio[ord.indL]
      outmatL <- cbind(col1L[1:n], col2L[1:n], col3L[1:n], 
                       col4L[1:n], col5L[1:n])
      colnames(outmatL) <- c(paste("beta", i, sep = ""), 
                             paste("med. ~i", sep = ""), "Difference", "MAD ~i", 
                             "ratio")
      col1S <- out$beta[ord.indS, i]
      col2S <- mediannoti[ord.indS]
      col3S <- col1S - col2S
      col4S <- MADnoti[ord.indS]
      col5S <- distMADratio[ord.indS]
      outmatS <- cbind(col1S[1:n], col2S[1:n], col3S[1:n], 
                       col4S[1:n], col5S[1:n])
      colnames(outmatS) <- c(paste("beta", i, sep = ""), 
                             paste("med. ~i", sep = ""), "Difference", "MAD ~i", 
                             "ratio")
      if (!xtable) {
        cat(paste("\n\nTopic ", i, " vs. ", topics.compare[topics.compare != 
                                                             i], "\n", sep = ""))
        cat("  Largest Positive Ratios:\n")
        print(outmatL, digits = 3)
        keywords.list[[i]] <- outmatL
        if (!onlypos) {
          cat("  Largest Negative Ratios:\n")
          print(outmatS, digits = 3)
        }
      }
      else {
        print(xtable(outmatL, digits = c(2, 2, 2, 2, 
                                         2, 2), ...))
        if (!onlypos) {
          print(xtable(outmatS, digits = c(2, 2, 2, 2, 
                                           2, 2), ...))
        }
      }
    }
  keywords.list <- apply(keywords.list, rownames)
  return(keywords.list)
  }
}
