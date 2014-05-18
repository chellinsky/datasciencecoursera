complete <- function(directory, id = 1:332) {

    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    returnframe <- data.frame(id = numeric(), nobs = numeric())

    for(singleID in id) {

        ## make the singleID a three-character string
        if (singleID < 10) {
            singleIDstr <- paste("00", toString(singleID), sep="")
        }
        else if (singleID < 100) {
            singleIDstr <- paste("0", toString(singleID), sep="")
        }
        else {
            singleIDstr <- toString(singleID)
        }

        ## make the file path string
        fileloc <- paste(directory, "/", singleIDstr, ".csv", sep="")

        ## read in to data frame
        fulldataframe <- read.csv(fileloc)

        cleanframe <- na.omit(fulldataframe)

        numrows <- nrow(cleanframe)
        
        returnframe <- rbind(returnframe, data.frame(id = singleID, nobs = numrows))
    }

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases

    returnframe
}