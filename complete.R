complete <- function(directory, id = 1:332) {
	#make a blank vector
	datmatr <- matrix(ncol=2)
	colnames(datmatr) <- c("id", "nobs")

	#use a for loop to load the requested data into a dataframe
	for(file in id) {
		#save the raw number
		obsnum <- file

		#all files need to be in three digit format
		file <- make3digit(file)

		#finish making the filename
		file <- paste(directory, "/", file, ".csv", sep="")

		#read in that file
		adf <- read.csv(file)

		#count obs
		nobs <- sum(complete.cases(adf))
#		print(badNA)

#		print(adf[!badNA, pollutant])

		#load that file's information into the vector
		datmatr <- rbind(datmatr, matrix(c(obsnum, nobs), nrow=1, ncol=2))
	}

	datmatr <- datmatr[-1,]
	datmatr
}