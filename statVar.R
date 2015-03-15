# Function: statVar
# -----------------
# This function calculates the normalized standard deviation for each 
# stat category of each team.
# 
# Here, normalized standard deviation is calculated as follows: 
# 
# (1) Standard deviation of each stat category across all teams is 
#     calculated for each week (scoring period)
# 
# (2) Mean of weekly standard deviations is calculated for each stat 
#     category across all weeks
# 
# (3) Standard deviation of each stat category across all weeks is 
#     calculated for each team
# 
# Normalized standard deviation is defined as value calculated in (3) 
# divided by value calculated in (2)
# 
# Argument: stats should be data.frame in the form returned by espnStats.R
# 
# Return value: data.frame containing normalized standard deviations

statVar <- function(stats) {
        
        # stat categories
        cols <- c("R", "HR", "RBI", "SB", "OBP", "K", "QS", "SV", "ERA", "WHIP")
        meanSD <- aggregate(stats[cols], by=stats["WEEK"], FUN=sd)
        
        # selectively excluding some data (league-specific reasons)
        filter <- c("QS", "SV", "ERA", "WHIP")
        for(i in 1:length(filter)) {
                meanSD[[filter[i]]] <- 
                        tapply(stats[[filter[i]]][stats$NAME != "the Turnipheads"], 
                               stats$WEEK[stats$NAME != "the Turnipheads"], 
                               FUN=sd)
        }
        
        # calculate means of intra-week stat category standard deviations
        meanSD <- sapply(meanSD[cols], FUN=mean)
        
        # calculate inter-week stat category standard deviations for each team
        teamSD <- aggregate(stats[cols], by=stats["NAME"], FUN=sd)
        
        # divide inter-week sd by mean intra-week sd for each stat category
        for(i in 1:length(cols)) {
                teamSD[[cols[i]]] <- teamSD[[cols[i]]] / meanSD[cols[i]]
        }
        
        teamSD
}