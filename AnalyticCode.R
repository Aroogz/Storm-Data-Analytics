

## Preparing the workspace
    
rm(list = ls())
if("dplyr" %in% row.names(installed.packages()) == FALSE){install.packages("dplyr")}
if ("ggplot2" %in% row.names(installed.packages()) == FALSE){install.packages("ggplot2")}
library(dplyr)
library(ggplot2)

## Data Processing

if(!file.exists("Storm_Data.csv.bz2")){
  download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                mode = "wb", destfile = "Storm_Data.csv.bz2") 
}
  


temp_data <- read.csv("Storm_Data.csv.bz2", stringsAsFactors = F)

var <- c("COUNTYNAME", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG","PROPDMGEXP", "CROPDMG" , "CROPDMGEXP")

storm_data1 <- temp_data[, var]

rm(temp_data)

prop_dmg_exp <- factor(storm_data1$PROPDMGEXP, levels = unique(storm_data1$PROPDMGEXP), 
                     labels = c("1000", "1000000", "0", "1000000000", "1000000", "1", "10", "10", "10", "0", "10", "10", "10", "100", "10", "100", "0", "10", "10"))

storm_data1$prop_dmg_value <- as.numeric(as.character(storm_data1$PROPDMG)) * as.numeric(as.character(prop_dmg_exp))

crop_dmg_exp <- factor(storm_data1$CROPDMGEXP, levels = unique(storm_data1$CROPDMGEXP), 
                       labels = c("0", "1000000", "1000", "1000000", "1000000000", "0", "10", "1000", "10"))

storm_data1$crop_dmg_value <- as.numeric(as.character(storm_data1$CROPDMG)) * as.numeric(as.character(crop_dmg_exp))

storm_data1$total_cost <- storm_data1$prop_dmg_value + storm_data1$crop_dmg_value

## Results

cost <- summarise(group_by(storm_data1, EVTYPE), cost = sum(total_cost))

cost <- arrange(cost, desc(cost))

ggplot(cost[1:10, ], aes(factor(EVTYPE, levels = EVTYPE[1:10]), cost))+
                        geom_bar(stat= "identity", col= "red", fill = "blue", alpha= 0.2)+
                        labs(title = "Top 10 Economically Devastating Storm Event Type", x = "Event Type", y= "Cost of Property Loss")+
                      theme(axis.text.x = element_text(angle = 90, size = 8))

#I have weighted 1 death as equivalent to 3 injuries 
storm_data1$weighted_harm <- 3 * as.numeric(storm_data1$FATALITIES) + as.numeric(storm_data1$INJURIES)

Harm <- summarise(group_by(storm_data1, EVTYPE), harm_done = sum(weighted_harm))

Harm <- arrange(Harm, desc(harm_done))

ggplot(Harm[1:10,], aes(factor(EVTYPE, levels = EVTYPE[1:10]), harm_done))+
                      geom_bar(stat= "identity", col= "red", fill= "blue", alpha= 0.2)+
                      labs(title= "Top 10 Most Devastating Storm Event Type to Population Health",
                           x = "Event Type", y= "Fatality Index")+
                      theme(axis.text.x = element_text(angle = 90, size = 8))

rm(list = ls())


