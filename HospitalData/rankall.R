rankall <- function(outcome, num = 1) {
        #Read outcome data
    outcomefile <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings=c("Not Available","NA"))
    ailments <- c("heart attack", "heart failure", "pneumonia")

    #Check that state and outcome are valid
    if (!any(ailments == outcome))  {
        stop("invalid outcome")
    }

    #Return hospital name in that state with teh given rank for 30-day death rate
    #Convert the outcome variable to a proper naming convention
    if (outcome == "heart attack") {
        outcome <- "Heart.Attack"
    }
    if (outcome == "heart failure") {
        outcome <- "Heart.Failure"
    }
    if (outcome == "pneumonia") {
        outcome <- "Pneumonia"
    }

    correctCol <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep="")

    #Remove NAs
    returnablefile <- outcomefile[!is.na(returnablefile[, correctCol]), ]

    #Convert column to numeric
    returnablefile[, correctCol] <- as.numeric(returnablefile[, correctCol])

    #order the data
    ordereddata <- returnablefile[order(returnablefile[, correctCol], returnablefile[, "Hospital.Name"]), ]

    rankhospital <- ordereddata[, "Hospital.Name"]

}