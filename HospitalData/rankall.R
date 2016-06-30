rankall <- function(outcome, num = "best") {
        #Read outcome data
    outcomefile <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings=c("Not Available","NA"))
    ailments <- c("heart attack", "heart failure", "pneumonia")

    #fix best num
    if (num == "best") num <- 1

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
    returnablefile <- outcomefile[!is.na(outcomefile[, correctCol]), ]

    #Convert column to numeric
    returnablefile[, correctCol] <- as.numeric(returnablefile[, correctCol])

    statesplit <- split(returnablefile, returnablefile$State)

    rankhospital <- NULL

    for (astate in statesplit) {

        #order the data
        ordereddata <- astate[order(astate[, correctCol], astate[, "Hospital.Name"]), ]

        num2 <- num

        #account for "worst" as a num
        if (num == "worst") num2 <- nrow(astate)

        thisrow <- c(ordereddata[1, "State"], ordereddata[num2, "Hospital.Name"], nrow(astate), num2)

        rankhospital <- rbind(rankhospital, thisrow)
    }

    rankhospital
}