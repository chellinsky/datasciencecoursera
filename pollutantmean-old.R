pollutantmean <- function(directory, pollutant, id = 1:332) {

	## load all ids in a dataframe

	firsttime <- TRUE

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

        fileloc <- paste(directory, "/", singleIDstr, ".csv", sep="")

        if(firsttime) {
	        fulldataframe <- read.csv(fileloc)
	        firsttime <- FALSE
        }
        else {
	        dataframe <- read.csv(fileloc)
	        fulldataframe <- rbind(fulldataframe, dataframe)
	    }
    }

    mean(fulldataframe[,pollutant],na.rm=TRUE)

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
}