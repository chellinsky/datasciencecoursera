corr <- function(directory, threshold = 0) {
	#get the number of cases for each file int he directory
	cases <- complete(directory)

	#initialize the vector of correlations
	vectcorr <- vector()

	#loop through each case
	for(i in cases[,1]) {
		if(cases[i,2] > threshold) {
			#read the csv file--should be a more efficient way to do this because we already read it once
			file <- make3digit(i)
			file <- paste(directory, "/", file, ".csv", sep="")
			adf <- read.csv(file)

			#remove NAs for each pollutant
			badNAn <- is.na(adf[, "nitrate"])
			badNAs <- is.na(adf[, "sulfate"])
			ccadf <- complete.cases(adf)

			#add correlation to the vector
			vectcorr <- c(vectcorr, cor(adf[ccadf,"sulfate"], adf[ccadf, "nitrate"]))
		}
	}

	vectcorr

}