corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    completeCases <- complete(directory)
    completeCasesOverThreshold <- completeCases[completeCases[["nobs"]] > threshold, ]
    if (nrow(completeCasesOverThreshold) == 0) {
        numeric()
    }
    else {
        correlations <- numeric()
        for(i in completeCasesOverThreshold[["id"]]) {
            leading0 <- paste(rep("0", 3 - nchar(paste(i))), collapse = '')
            data <- read.csv(paste(directory, "/", leading0, i, ".csv", sep = ''))
                        
            correlation <- cor(data[["sulfate"]], data[["nitrate"]], use = "complete.obs", method = "pearson")
            correlations <- c(correlations, correlation)
        }
        
        correlations
    }
}