# Function: espnStats
# -------------------
# This function scrapes weekly scoreboard statistics from the ESPN 
# Fantasy Baseball website.
# 
# Argument: url should be of the form: 
# 
# http://games.espn.go.com/flb/scoreboard?leagueId=XXXXXX&seasonId=YYYY
# 
# where XXXXXX is replaced by the numerical league ID and YYYY is replaced
# by the four-digit year of the desired season.
# 
# Return value: data.frame containing statistics for each team in each 
# week of the season.

espnStats <- function(url) {
        
        # required for parsing HTML
        library("XML")
        
        # weekly scoring period index
        period <- 1
        
        # parsed HTML data storage
        cache <- NULL
        
        # raw HTML data storage 
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
                
                # cache all scoring periods before parsing
                if(!is.null(stream)) {
                        html <- htmlTreeParse(stream, useInternalNodes=TRUE)
                        row <- xpathSApply(html, "//table/tr[@class='linescoreTeamRow']/td", xmlValue)
                        row <- grep("[[:alnum:]]+", row, value=TRUE)
                        
                        # initialize cache
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
        
        # initialize output
        stats <- as.data.frame(matrix(0.0, nrow=length(teams) * period, 
                                      ncol=length(colNames) + 1))
        names(stats) <- c(colNames, "WEEK")
        
        # parse and format cache
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