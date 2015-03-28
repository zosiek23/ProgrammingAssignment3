Assignment 3 code:
#(Othr example:https://github.com/pachamaltese/RProgramming/)
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
ncol(outcome)
nrow(outcome)
names(outcome)

#To make a simple histogram of the 30-day death rates from heart attack 
#(column 11 in the outcome dataset),run
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])


# TODO: Add comment
# 
# Author: 
###############################################################################

library(lattice)
#setwd("/Users/pacha/Dropbox/R/RProgramming/tarea3")
Outcome <- read.csv("Outcome-of-care-measures.csv", colClasses = "character")
head(Outcome)

#PARTE 2
best <- function(stateChr, outcomeChr) {
  outcomeDfr <- Init("Outcome-of-care-measures.csv")
  
  suppressWarnings(outcomeDfr[, 11] <- as.numeric(outcomeDfr[, 11]))
  suppressWarnings(outcomeDfr[, 17] <- as.numeric(outcomeDfr[, 17]))
  suppressWarnings(outcomeDfr[, 23] <- as.numeric(outcomeDfr[, 23]))
  
  tableDfr <- data.frame(State = names(tapply(outcomeDfr$State, outcomeDfr$State, 
                                              length)), Freq = tapply(outcomeDfr$State, outcomeDfr$State, length))
  rownames(tableDfr) <- NULL
  
  inputDfr <- data.frame(Outcome = c("heart attack", "heart failure", "pneumonia"), 
                         Col = c(11, 17, 23))
  
  if (nrow(tableDfr[tableDfr$State == stateChr, ]) == 0) 
    stop("invalid state")
  if (nrow(inputDfr[inputDfr$Outcome == outcomeChr, ]) == 0) 
    stop("invalid outcome")
  
  stateDfr <- outcomeDfr[outcomeDfr$State == stateChr, ]
  colNum <- inputDfr[inputDfr$Outcome == outcomeChr, 2]
  rowNum <- which.min(stateDfr[, colNum])
  return(stateDfr[rowNum, ]$Hospital.Name)
}


#to call best funtion (R file)
source("best.R")
best("TX", "heart attack")
#[1] "CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart failure")
#[1] "FORT DUNCAN MEDICAL CENTER"
best("MD", "heart attack")
#[1] "JOHNS HOPKINS HOSPITAL, THE"
best("MD", "pneumonia")
#[1] "GREATER BALTIMORE MEDICAL CENTER"
best("BB", "heart attack")
#Error in best("BB", "heart attack") : invalid state
best("NY", "hert attack")
#Error in best("NY", "hert attack") : invalid outcome

# Call ranhospital function
source("rankhospital.R")
rankhospital("TX", "heart failure", 4)
#[1] "DETAR HOSPITAL NAVARRO"
rankhospital("MD", "heart attack", "worst")
#[1] "HARFORD MEMORIAL HOSPITAL"
rankhospital("MN", "heart attack", 5000)
#[1] NA


#Submit script for checking code:
  source("submitscript3.R")
  submit()
