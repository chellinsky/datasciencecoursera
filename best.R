best <- function(state, outcome) {

    ## Read outcome data
    dataframe <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings=c("Not Available","NA"))
    ailments <- c("heart attack", "heart failure", "pneumonia")

    ## Check state and outcome are valid
    if (!any(dataframe[, 7] == state)) {
        stop("invalid state")
    }
    if (!any(ailments == outcome))  {
        stop("invalid outcome")
    }

    ## Return hospital with lowest 30-day mortality rate
    focusset <- subset(dataframe, State == state)
    if(outcome == "heart attack") {
        focusset[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] <- as.numeric(focusset[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])
        resultframe <- focusset[order(focusset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, focusset$Hospital.Name), ]
    }
    if(outcome == "heart failure") {
        focusset[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"] <- as.numeric(focusset[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"])
        resultframe <- focusset[order(focusset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, focusset$Hospital.Name), ]
    }
    if(outcome == "pneumonia") {
        focusset[, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"] <- as.numeric(focusset[, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"])
        resultframe <- focusset[order(focusset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, focusset$Hospital.Name), ]
    }
    return(resultframe[1, "Hospital.Name"])
#    return(head(resultframe))

}