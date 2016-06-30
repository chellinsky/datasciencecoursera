rankhospital <- function(state, outcome, num = 1) {
    #Read outcome data
    outcomefile <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings=c("Not Available","NA"))
    ailments <- c("heart attack", "heart failure", "pneumonia")

    #Check that state and outcome are valid
    if (!any(outcomefile[, 7] == state)) {
        stop("invalid state")
    }
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

    #Find vector with the state's outcomes
    stateoutcomes <- outcomefile[outcomefile$State == state, ]

    #Find the minimum value for the state given the ailment and lookup the hostpital name
    correctCol <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep="")

    #Remove NAs
    stateoutcomes <- stateoutcomes[!is.na(stateoutcomes[, correctCol]), ]

    #Convert column to numeric
    stateoutcomes[, correctCol] <- as.numeric(stateoutcomes[, correctCol])

    #order the data
    ordereddata <- stateoutcomes[order(stateoutcomes[, correctCol], stateoutcomes[, "Hospital.Name"]), ]

    rankhospital <- ordereddata[, "Hospital.Name"]

    if (num == "worst") {
        rankhospital[length(rankhospital)]
    } else {
        rankhospital[num]

    }

}