espnStats <- function(url) {
        
        library("XML")
        
        period <- 1
        cache <- NULL
        stream <- character(0)
        while(!is.null(stream)) {
                stream <- tryCatch({
                        readLines(paste(url, "&matchupPeriodId=", 
                                        as.character(period), sep=""))
                }, warning=function(w) {
                        NULL
                }, error=function(e) {
                        NULL
                }, finally={ })
                
                if(!is.null(stream)) {
                        html <- htmlTreeParse(stream, useInternalNodes=TRUE)
                        row <- xpathSApply(html, "//table/tr[@class='linescoreTeamRow']/td", xmlValue)
                        row <- grep("[[:alnum:]]+", row, value=TRUE)
                        if(is.null(cache)) {
                                colNames <- xpathSApply(html, "//table/tr[@class='tableSubHead']/th", xmlValue)
                                colNames <- unique(colNames)
                                
                                teams <- xpathSApply(html, "//table/tr/td[@class='teamName']", xmlValue)
                                teams <- gsub("[[:blank:]]*\\([[:digit:]-]*\\)", "", teams)
                                
                                cache <- matrix("", nrow=23, ncol=length(row))
                        }
                        cache[period,] <- row
                        period <- period + 1
                } else {
                        period <- period - 1
                }
        }
        
        stats <- as.data.frame(matrix(0.0, nrow=length(teams) * period, 
                                      ncol=length(colNames) + 1))
        names(stats) <- c(colNames, "WEEK")
        dfIndex <- 1
        for(i in 1:period) {
                for(j in 1:ncol(cache)) {
                        if(i == 1 & j <= length(colNames)) {
                                if(grepl("[^[:digit:].]|\\.[[:digit:]]*\\.", cache[i, j])) {
                                        stats[[j]] <- as.character(stats[[j]])
                                }
                        }
                        colIndex <- ((j - 1) %% length(colNames)) + 1
                        if(class(stats[[colIndex]]) == "character") {
                                stats[dfIndex, colIndex] <- cache[i, j]
                        } else {
                                stats[dfIndex, colIndex] <- as.numeric(cache[i, j])
                        }
                        if(colIndex == length(colNames)) {
                                stats[dfIndex, colIndex + 1] <- i
                                dfIndex <- dfIndex + 1
                        }
                }
        }
        
        cache <- t(sapply(strsplit(stats$SCORE, "-", fixed=TRUE), FUN=as.integer))
        colnames(cache) <- c("WIN", "LOSS", "TIE")
        cbind(stats[!(names(stats) %in% c("SCORE"))], cache)
        stats$NAME <- gsub("[[:space:](]+[[:digit:]-]+[[:space:])]+", "", stats$NAME)
        
        stats
}