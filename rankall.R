rankall <- function(outcome, num = "best") {

    ## Read outcome data
    dataframe <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings=c("Not Available","NA"))
    ailments <- c("heart attack", "heart failure", "pneumonia")
    rankofall <- data.frame("hospital" = character(), "state" = character())

    ## Check outcome is valid
    if (!any(ailments == outcome))  {
        stop("invalid outcome")
    }

    statelist <- c(state.abb, "DC", "VI")

    ## Find hospital with given rank in each state
    for(state in statelist) {
        focusset <- subset(dataframe, State == state)
        if(outcome == "heart attack") {
            focusset[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] <- as.numeric(focusset[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])
            resultframe <- focusset[order(focusset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, focusset$Hospital.Name), ]
            while(is.na(resultframe[nrow(resultframe), "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])) {
                resultframe <- resultframe[-nrow(resultframe),]
            }
        }
        if(outcome == "heart failure") {
            focusset[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"] <- as.numeric(focusset[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"])
            resultframe <- focusset[order(focusset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, focusset$Hospital.Name), ]
            while(is.na(resultframe[nrow(resultframe), "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"])) {
                resultframe <- resultframe[-nrow(resultframe),]
            }
        }
        if(outcome == "pneumonia") {
            focusset[, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"] <- as.numeric(focusset[, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"])
            resultframe <- focusset[order(focusset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, focusset$Hospital.Name), ]
            while(is.na(resultframe[nrow(resultframe), "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"])) {
                resultframe <- resultframe[-nrow(resultframe),]
            }
        }
        if(num == "best") {
            rankofall <- rbind(rankofall, data.frame("hospital" = resultframe[1, "Hospital.Name"], "state" = state))
        }
        else if(num == "worst") {
            rankofall <- rbind(rankofall, data.frame("hospital" = resultframe[nrow(resultframe), "Hospital.Name"], "state" = state))
        }
        else {
            rankofall <- rbind(rankofall, data.frame("hospital" = resultframe[num, "Hospital.Name"], "state" = state))
        }
    }
    row.names(rankofall) <- rankofall$state
    rankofall$state <- as.character(rankofall$state)
    rankofall <- rankofall[order(rankofall$state), ]
    return(rankofall)
}