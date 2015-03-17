# Function: normsd
# ----------------
# This function calculates the normalized standard deviation for each 
# stat category of each team.
# 
# Here, normalized standard deviation is calculated as follows: 
# 
# (1) Full-season stat category means are calculated for each team
#     (i.e. roto scoring)
# 
# (2) Standard deviation of (1) across all teams is calculated for
#     each week stat category
# 
# (3) Weekly stat category standard deviations are calculated across
#     all teams
# 
# (4) Means of intra-week standard deviations for each stat category (3) 
#     are calculated across all weeks
# 
# Normalized standard deviation is defined as the ratio of mean intra-week 
# standard deviation to standard deviation of season-averaged score in 
# each stat category. This value is calculated as (4) divided (2)
# 
# Argument: stats should be a data.frame with the same columns returned 
# by espnStats.R
# 
# Return value: vector containing normalized standard deviations

normsd <- function(stats) {
        
        # stat categories
        cols <- c("R", "HR", "RBI", "SB", "OBP", "K", "QS", "SV", "ERA", "WHIP")
        
        # calculate stat category standard deviations for means over all weeks
        teamAVG <- aggregate(stats[cols], by=stats["NAME"], FUN=mean)
        rotoSD <- sapply(teamAVG[-1], FUN=sd)
        
        # calculate means of intra-week stat category standard deviations
        weekSD <- aggregate(stats[cols], by=stats["WEEK"], FUN=sd)
        weekSD <- sapply(weekSD[cols], FUN=mean)
        
        # return normalized per week stat category standard deviations
        weekSD / rotoSD
}