complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    completeData <- data.frame()
    
    for(monitorID in id) {
        ## Read data from each ID file into currentData
        fileName <- paste0(formatC(monitorID, width = 3, flag = "0"), ".csv")
        filePath <- paste0(directory, "/", fileName)
        currentData <- read.csv(filePath)
        
        ## Get the number of completed cases in currentData
        goodData <- data.frame(id = monitorID, 
                               nobs = sum(complete.cases(currentData)))
        
        ## Add the "nobs" into the data,frame completeData
        completeData <- rbind(completeData, goodData)
    }
        
    completeData
}