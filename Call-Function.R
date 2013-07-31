# ===== PAUL BOULUS - THESIS 2013

# ===== CALL FUNCTION FILE

# ===== SOURCE FUNCTIONS FILE =====
source("Functions.R")

# ===== CREATE MASTERFRAME FROM XML DATA / OR LOAD FROM FILE =====

# ===== CREATE:
if(options$create.masterframe == TRUE){
  process.frames() #creates data frames with each of the XML datasets in the 8 source folders, and saves to disk
  masterframe <- load.frames()
# ===== LOAD:
}else{masterframe <- loadObject(file=paste(options$data.dir, "/masterframe.RData", sep=""))}


# ===== FIX DATES =====
#We need to do some fixing of dates here, as there are two date formats. We take the masterframe, and subset it 
#First format is "%Y-%m-%d" 1901-1980, 1998-2013
#Second format is "%d/%m/%Y" 1981-1995
#Third format is "%d/%m/%y" 1996-1997

masterframe$Date[grepl("/96", masterframe$Date)] <- as.Date(masterframe$Date[grepl("/96", masterframe$Date)], format="%d/%m/%y") #cath those that start with '96
masterframe$Date[grepl("/97", masterframe$Date)] <- as.Date(masterframe$Date[grepl("/97", masterframe$Date)], format="%d/%m/%y") #catch those that start with '97
masterframe$Date[grepl("/", masterframe$Date)] <- as.Date(masterframe$Date[grepl("/", masterframe$Date)], format="%d/%m/%Y") #now catch all other 1981-1995 with "/"
masterframe$Date[grepl("-", masterframe$Date)] <- as.Date(masterframe$Date[grepl("-", masterframe$Date)], format="%Y-%m-%d") #now catch all others with "-"
masterframe$Date <- as.numeric(masterframe$Date) #convert these number strings to numeric values
masterframe$Date <- as.Date(masterframe$Date, origin="1970-01-01") #convert these numeric values to dates
masterframe <- masterframe[order(masterframe$Date),] #we can now order masterframe by date
gc()

# ===== ADD A SPEECHID INDEX BEGINNING WITH "D" =====
masterframe$SpeechID <- paste("D", c(1:nrow(masterframe)), sep="")

# ===== SUBSET MASTERFRAME IF NECESSARY =====
#masterframe <- masterframe[1:50000,] #there is a bad value between 20000 and 30000 (zero column)
#masterframe <- subset(masterframe, Date>"2012-06-01")

#create a vector of dates of the subsetted masterframe so we can dump masterframe object
masterframe.dates <- masterframe$Date

# ===== CREATE TDM =====
tdm.combined <- create.tdm(masterframe=masterframe, block.size=40000) #blocks of 40000 seem to work well
#tdm.combined <- loadObject("tdm.RData") #load if already saved
sparsetdm <- removeSparseTerms(tdm.combined, 0.995) #removes sparseness from the matrix

print(paste("Number of documents in sparse tdm:", ncol(sparsetdm)))
print(paste("Number of terms in sparse tdm:", nrow(sparsetdm)))

document.names <- colnames(sparsetdm)
word.names <- rownames(sparsetdm)

print("Creating compressed dgTMatrix")
sparsematrix.1 <- as(as.matrix(sparsetdm[,1:ceiling(ncol(sparsetdm)/2)]), "dgTMatrix")
sparsematrix.2 <- as(as.matrix(sparsetdm[,(ceiling(ncol(sparsetdm)/2)+1):ncol(sparsetdm)]), "dgTMatrix")
sparsematrix <- as(cbind2(sparsematrix.1, sparsematrix.2), "dgTMatrix")

rm(sparsematrix.1, sparsematrix.2)
gc() #collect garbage

rm(masterframe)

# ===== REMOVE ZERO VECTOR COLUMNS FROM MATRIX =====
nonemptydocs <- colSums(sparsematrix)>0 #check for nonempty columns in the matrix
sparsematrix <- sparsematrix[,nonemptydocs] #remove zero vector columns from the matrix

# ===== CREATE X3COL MATRIX REPRESENTATION =====

print("Creating X3col matrix")
X3col <- matrix(c(sparsematrix@i+1, sparsematrix@j+1, sparsematrix@x), ncol = 3) # +1 because dgTMatrix starts indexing at 0

print("Ordering X3col by column 1, then by column 2")
X3col <- X3col[order(X3col[,1], X3col[,2], decreasing=FALSE),] #order dgTMatrix by 1st then 2nd columns, ascending

# ===== CREATE ALL OTHER VARIABLES REQUIRED FOR MODEL =====
sparsemat <- list(data=X3col, word.names, document.names[nonemptydocs]) #sparsemat requires a data component (X3col), the row names of the terms in the tdm, and the document names of the tdm
timevec <- as.numeric(masterframe.dates[nonemptydocs])-(as.numeric(masterframe.dates[1])-1) #creates a vector of days, starting from day 1, of the dates in masterframe
time.names <- as.numeric(masterframe.dates[1]:masterframe.dates[length(masterframe.dates)]) #creates an ordered sequence from the first date in the corpus, to the last date in the corpus
time.names <- as.character(as.Date(time.names, origin="1970-01-01")) #convert these integers to date names

# ===== REMOVE JUNK =====
rm(sparsematrix, sparsetdm, tdm.combined, X3col)

# ===== CALL MODEL FUNCTION =====
model.output <- EMDynMultMix(sparsemat, timevec=timevec, time.names=time.names, kernwidth=length(time.names)/100, nclust=40, EMmaxiter=10,
                             EMtol=1e-5, theta.start=NA, pi.start=NA,
                             clustprobs.start=NA, priorcount=1.01,
                             lazythresh=1.0, fullEMiter=1, initfullEMiter=1,
                             aaa=1.0, bbb=1.0, ccc=1.0, ddd=1.0, C0=100,
                             V00=10)
