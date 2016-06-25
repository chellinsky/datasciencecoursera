pollutantmean <- function(directory, pollutant, id = 1:332) {
	#make a blank vector
	datvect <- vector()

	#use a for loop to load the requested data into a dataframe
	for(file in id) {
		#all files need to be in three digit format
		file <- make3digit(file)

		#finish making the filename
		file <- paste(directory, "/", file, ".csv", sep="")

		#read in that file
		adf <- read.csv(file)

		#remove NAs
		badNA <- is.na(adf[, pollutant])
#		print(badNA)

#		print(adf[!badNA, pollutant])

		#load that file's information into the vector
		datvect <- c(datvect, adf[!badNA, pollutant])
	}

	mean(datvect)
}

make3digit <- function(number) {
	number <- as.character(number)
	while(nchar(number) < 3) {
		number <- paste("0", number, sep="")
	}
	return(number)
}