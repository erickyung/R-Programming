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
    
    completeCases <- numeric()
    for(i in id) {
        leading0 <- paste(rep("0", 3 - nchar(paste(i))), collapse = '')
        data <- read.csv(paste(directory, "/", leading0, i, ".csv", sep = ''))
        good <- complete.cases(data)
        completeCases <- c(completeCases, nrow(data[good, ]))
    }
    
    data.frame(id = id, nobs = completeCases)
}