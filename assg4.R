assg4 <- function() {
    storm_data <- read.csv("StormData.csv", sep=",", stringsAsFactors=FALSE)
    # split the data based on event type, then add up the fatalities and 
    # injuries
    fatal_dmg <- with(storm_data, tapply(FATALITIES, EVTYPE, sum, na.rm=TRUE))
    inj_dmg <- with(storm_data, tapply(INJURIES, EVTYPE, sum, na.rm=TRUE))
    life_dmg <- fatal_dmg + inj_dmg
    harm_pop <- which(life_dmg==max(life_dmg, na.rm=TRUE))
    
    
    storm_data
}