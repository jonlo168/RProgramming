corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!

    corr <- numeric()
    
    for(monitorID in 1:332) {
        ## Read data from each ID file into currentData
        fileName <- paste0(formatC(monitorID, width = 3, flag = "0"), ".csv")
        filePath <- paste0(directory, "/", fileName)
        currentData <- read.csv(filePath)
        
        ## Get the completed cases in currentData
        completeData <- currentData[complete.cases(currentData), ]

        if(nrow(completeData) > threshold) {
            corr <- c(corr, cor(completeData$sulfate, completeData$nitrate))
        }
    }
    
    corr    
}