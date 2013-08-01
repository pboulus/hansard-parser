# ===== PAUL BOULUS - THESIS 2013
# 20130801

# ********** AMAZON VERSION **********

#Set up Amazon EC2 instance to run thesis data

# ===== 0: THINGS TO DO OUTSIDE R =====

# Ensure masterframe file is loaded into the working directory

# Attach EBS volume:
# sudo mkdir -p /thesis #make thesis directory
# sudo chmod –R 777 /thesis  #chmod required for R to be able to write to it
# sudo mount /dev/xvdf /thesis #mount EBS volume (once it's attached)

#Unmounting the EBS volume
# umount /dev/xvdf

# ===== 1: SET OPTIONS =====
options <- list()

# Working directory where THIS and OTHER code file lies, generally on the mounted /thesis directory
setwd("/Users/Paul/Dropbox/ANU/Thesis/RProjects/ExtractSpeeches/") #for local
# setwd("/thesis") #for amazon

# Directory to store data relevant for this iteration
options$data.dir <- "/Users/Paul/ThesisData/20130731/"
options$source.dir <- "/Users/Paul/ThesisData/Sources/"

#Is install required?
options$install.required <- FALSE #If install is required, set this to TRUE

#Create masterframe? If set to TRUE, will create masterframe by cycling through filenames. Otherwise, it will load masterframe.RData from the data directory.
options$create.masterframe <- FALSE

Sys.setenv(NOAWT=1) #Required for snowball/rjava package stemmer algorithm

set.seed(20131031) #Seed is set as the due date for this thesis.

options(stringsAsFactors = FALSE)

# ===== 2: INSTALL/CALL PACKAGES =====

if(options$install.required == TRUE){
  install.packages("coda")
  #install.packages("XML")
  install.packages("tm")
  install.packages("SnowballC") #Snowball C is now used by tm in stemming
  install.packages("Matrix")
  install.packages("Rhetpack_0.1-1.tar.gz", repos = NULL, type="source")
  install.packages("zoo")
  install.packages("xts")
  install.packages("R.utils")
  install.packages("plyr")
  install.packages("ineq")
}

library("XML")
library("tm")
library("SnowballC") #Snowball C is now used by tm in stemming
library("Matrix")
library("Rhetpack")
library("zoo")
library("xts")
library("R.utils")
library("plyr")

# ===== 3: CALL "Call-Function.R" =====
source("Call-Function.R", echo=TRUE)

# ===== 4: SAVE OBJECTS TO DISK FOR SAFEKEEPING =====
# Make sure to include an appropraite path to the EBS volume here
#saveObject(model.output, file=paste(options$data.dir, "/model.output.RData", sep=""))
save.image(file=paste(options$data.dir, "/workspace.RData", sep=""))

# ===== 5: STOP INSTANCE? =====
# system("ec2-stop-instances i-10a64379") # use instance name