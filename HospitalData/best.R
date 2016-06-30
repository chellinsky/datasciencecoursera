best <- function(state, outcome) {
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

    #Return hospital name in that state with lowest 30-day death rate

    #Find vector with the state's outcomes
    stateoutcomes <- outcomefile[outcomefile$State == state, ]

    #Find the minimum value for the state given the ailment and lookup the hostpital name
    correctCol <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep="")

    #Remove NAs
    stateoutcomes <- stateoutcomes[!is.na(stateoutcomes[, correctCol]), ]

    #Convert column to numeric
    stateoutcomes[, correctCol] <- as.numeric(stateoutcomes[, correctCol])

    minvalue <- min(stateoutcomes[, correctCol])

    matchinghosps <- stateoutcomes[stateoutcomes[, correctCol] == minvalue, ]

    besthospital <- min(matchinghosps$Hospital.Name)

    besthospital
}