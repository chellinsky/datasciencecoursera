corr <- function(directory, threshold = 0) {

    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    nitrate <- numeric()
    sulfate <- numeric()
    corrs <- numeric()

    i <- 1

    id <- 1:332

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

        if (nrow(cleanframe) > threshold) {
            corrs <- c(corrs, cor(cleanframe[,"nitrate"], cleanframe[,"sulfate"]))
        }

    #     j <- 1

    #     while (i <= threshold && j <= length(cleanframe[,"nitrate"])) {
    #         nitrate <- c(nitrate, cleanframe[j, "nitrate"])
    #         sulfate <- c(sulfate, cleanframe[j, "sulfate"])
    #         j <- j + 1
    #         i <- i + 1
    #     }

    #     if (i > threshold) {
    #         return(cor(nitrate, sulfate))
    #     }

    }

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations

    corrs
}