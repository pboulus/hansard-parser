#Subset massive dataset

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
min(sparsemat$data[,2])
max(sparsemat$data[,2])

#These must match. If we subset too much we will lose words but not know which ones!
max(sparsemat$data[,1])
length(sparsemat[[2]])

# ===== RUN MODEL =====
model.output <- EMDynMultMix(sparsemat, timevec=timevec, time.names=time.names, kernwidth=length(time.names)/100, nclust=40, EMmaxiter=15,
                             EMtol=1e-5, theta.start=NA, pi.start=NA,
                             clustprobs.start=NA, priorcount=1.01,
                             lazythresh=1.0, fullEMiter=1, initfullEMiter=1,
                             aaa=1.0, bbb=1.0, ccc=1.0, ddd=1.0, C0=100,
                             V00=10)
