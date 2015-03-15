statVar <- function(stats) {
        cols <- c("R", "HR", "RBI", "SB", "OBP", "K", "QS", "SV", "ERA", "WHIP")
        filter <- c("QS", "SV", "ERA", "WHIP")
        meanSD <- aggregate(stats[cols], by=stats["WEEK"], FUN=sd)
        
        for(i in 1:length(filter)) {
                meanSD[[filter[i]]] <- 
                        tapply(stats[[filter[i]]][stats$NAME != "the Turnipheads"], 
                               stats$WEEK[stats$NAME != "the Turnipheads"], 
                               FUN=sd)
        }
        
        meanSD <- sapply(meanSD[cols], FUN=mean)
        
        teamSD <- aggregate(stats[cols], by=stats["NAME"], FUN=sd)
        
        for(i in 1:length(cols)) {
                teamSD[[cols[i]]] <- teamSD[[cols[i]]] / meanSD[cols[i]]
        }
        
        teamSD
}