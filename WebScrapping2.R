library('rvest')
library(ggplot2)
library(stringr)
library(xlsx)

# Take an HTML_Table from ESPN player stats, get the data, 
getHtmlTable <- function(columns, url)
{
  webpage <- read_html(url)
  values <- html_table(webpage)
  table <- data.frame(matrix(unlist(values), ncol=columns, byrow=F))
  colnames(table) <- lapply(table[1, ], as.character)
  table <- unique(table)
  table <- table[-1,]
  
  return(table)
}

# makes the 1st column display the year
addYear <- function(table, year)
{
  table[,1] <- year
  colnames(table)[1] <- "Year"
  return(table)
}

# Creates and combines tables from the last 10 years
multiYearTable <- function(type)
{
  for(i in 1:10)
  {
    year <- 2007+i
    tempTable <- makeTable(type, year)
    tempTable <- addYear(tempTable, year)
    if (i == 1)
      tableCombined <- tempTable
    else
      tableCombined <- rbind(tableCombined, tempTable)
  }
  return(tableCombined)
}

# remove commas from data
  # also makes the data type character
removeCommas <- function(data)
{
  data <- gsub(",", "", data)
  return(data)
}

# convert data type from factor to character
toCharacter <- function(data)
{
  data <- sapply(data, as.character)
  return(data)
}

# convert data type from character to numeric
toNumeric <- function(data)
{
  data <- sapply(data, as.numeric)
  return(data)
}

# gets the data to the correct format
cleanUp <- function(table, type)
{
  if (type == "passing")
  {
    table[,7] <- removeCommas(table[,7])
    table[,c(4:6, 8:14)] <- toCharacter(table[,c(4:6, 8:14)])
    table[,c(4:14)] <- toNumeric(table[,c(4:14)])
    return(table)
  }
  if (type == "rushing")
  {
    table[,5] <- removeCommas(table[,5])
    table[,c(4:12)] <- toCharacter(table[,c(4:12)])
    table[,c(4:12)] <- toNumeric(table[,c(4:12)])
    return(table)
  }
  if (type == "receiving")
  {
    table[,6] <- removeCommas(table[,6])
    table[,c(4:14)] <- toCharacter(table[,c(4:14)])
    table[,c(4:14)] <- toNumeric(table[,c(4:14)])
    return(table)
  }
  # Note: this will be difficult. Must split columns 8 - 12 into 2 columns each
  if (type == "kicking")
  {
    table[,c(2:23)] <- toCharacter(table[,c(2:23)])
    table <- splitAttemptsAndMakes(table)
    table <- table[,c(1:9, 11, 13, 15, 17, 19:33)]
    table[, c(5:18)] <- toNumeric(table[, c(5:18)])
    colnames(table)[c(7, 8, 10:14, 17, 18)] <- c("Pct_FG", "Blk_FG", "Pct_1_19", "Pct_20_29", "Pct_30_39", "Pct_40_49", "Pct_50_or_more", "Pct_XP", "Blk_XP")
    table <- table[,c(1:4, 6, 5, 7:9, 16, 15, 17:18, 19:20, 10, 21:22, 11, 23:24, 12, 25:26, 13, 27:28, 14)]
    return(table)
  }
}

# Create a table using ESPN.com or NFL.com
makeTable <- function(type, year)
{
  if(type == "kicking")
    return(makeTableNfl(type, year))
  else
    return(makeTableEspn(type, year))
}

# Create a table (cycling through all pages for the given year and type)
makeTableEspn <- function(type, year)
{
  year <- lapply(year, as.character)
  
  numberOfLinks <- getNumberOfLinks(type)
  numberOfColumns <- getNumberOfColumns(type)
  
  urlPart1 <- 'http://www.espn.com/nfl/statistics/player/_/stat/'
  urlPart2 <- '/year/'
  urlPart3 <- '/seasontype/2'
  urlPart4 <- '/qualified/false/count/'
  
  totalUrl <- paste(urlPart1, type, urlPart2, year, urlPart3, sep="")
  
  for(i in 1:(numberOfLinks))
  {
    tempTable <- getHtmlTable(numberOfColumns, paste(totalUrl, urlPart4, lapply((40*(i-1) + 1), as.character), sep=""))
    if (i==1)
      totalTable <- tempTable
    else
      totalTable <- rbind(totalTable, tempTable)
  }
  
  return(totalTable)
}


makeTableNfl <- function(type, year)
{
  year <- lapply(year, as.character)
  
  numberOfLinks <- getNumberOfLinks(type)
  numberOfColumns <- getNumberOfColumns(type)
  
  # http://www.nfl.com/stats/categorystats?archive=true&conference=null&statisticCategory=FIELD_GOALS&season=2015&seasonType=REG&experience=&tabSeq=0&qualified=true&Submit=Go
  urlPart1 <- "http://www.nfl.com/stats/categorystats?archive=true&conference=null&statisticCategory=FIELD_GOALS&season="
  urlPart2 <- "&seasonType=REG&experience=&tabSeq=0&qualified=true&Submit=Go"
  
  totalUrl <- paste(urlPart1, year, urlPart2, sep="")
  
  table <- getHtmlTable(numberOfColumns, totalUrl)
  return(table)
}

getNumberOfLinks <- function(type)
{
  if(type == "passing")
    return(3)
  if(type == "rushing")
    return(8)
  if(type == "receiving")
    return(11)
  if(type == "kicking")
    return(1)
}

getNumberOfColumns <- function(type)
{
  if(type == "passing")
    return(14)
  if(type == "rushing")
    return(12)
  if(type == "receiving")
    return(14)
  if(type == "kicking")
    return(23)
}

# Separate the PLAYER column into 2 columns (PLAYER and position)
createPositionColumn <- function(table, numberOfColumns)
{
  # Split the 2nd column wherever ", " is found
  nameAndPositionTable <- str_split_fixed(table[,2], ", ", 2)
  table$Position <- toCharacter(nameAndPositionTable[,2])
  table[,2] <- toCharacter(nameAndPositionTable[,1])
  table <- table[c(1:2, numberOfColumns+1, 3:numberOfColumns)]
  return(table)
}

# Separate the makes and attempts columns for kicking stats
splitAttemptsAndMakes <- function(table)
{
  attemptsAndMakes <- str_split_fixed(table[,10], "-", 2)
  table$Att_1_19 <- toNumeric(toCharacter(attemptsAndMakes[,1]))
  table$Made_1_19 <- toNumeric(toCharacter(attemptsAndMakes[,2]))
  
  attemptsAndMakes <- str_split_fixed(table[,12], "-", 2)
  table$Att_20_29 <- toNumeric(toCharacter(attemptsAndMakes[,1]))
  table$Made_20_29 <- toNumeric(toCharacter(attemptsAndMakes[,2]))
  
  attemptsAndMakes <- str_split_fixed(table[,14], "-", 2)
  table$Att_30_39 <- toNumeric(toCharacter(attemptsAndMakes[,1]))
  table$Made_30_39 <- toNumeric(toCharacter(attemptsAndMakes[,2]))
  
  attemptsAndMakes <- str_split_fixed(table[,16], "-", 2)
  table$Att_40_49 <- toNumeric(toCharacter(attemptsAndMakes[,1]))
  table$Made_40_49 <- toNumeric(toCharacter(attemptsAndMakes[,2]))
  
  attemptsAndMakes <- str_split_fixed(table[,18], "-", 2)
  table$Att_50_or_more <- toNumeric(toCharacter(attemptsAndMakes[,1]))
  table$Made_50_or_more <- toNumeric(toCharacter(attemptsAndMakes[,2]))
  
  return(table)
}

# Rename duplicate rows after creating position tables
renameDuplicates <- function(table, type)
{
  if(type == "QB")
    colnames(table)[c(6, 8:11, 15:19, 20, 21:22, 24)] <- c("P_Att", "P_Yds", "P_Yds_Avg", "P_Long", "P_TDs", "P_Yds_G", "R_Att", "R_Yds", "Yds_Avg", "R_Long", "20", "R_Tds", "R_Yds_G", "First_Dn")
  if(type == "RB")
    colnames(table)[c(6:13, 16:22, 24)] <- c("Ru_Yds", "Ru_Avg", "Ru_Long", "Ru_20", "Ru_TDs", "Ru_Yds_G", "Ru_Fum", "Ru_1DN", "Re_Yds", "Re_Avg", "Re_TDs", "Re_Long", "Re_20", "Re_Yds_G", "Re_Fum", "Re_1Dn")
  
  return(table)
}

# get only the top 32 players at a position
getTopPlayers <- function(table)
{
  for(i in (1:10))
  {
    year <- 2007+i
    temp <- table[table$Year %in% year,]
    temp <- temp[with(temp, order(-temp$`Fantasy_Points`)),]
    temp <- temp[1:32,]
    if(i == 1)
      topPlayers <- temp
    else
      topPlayers <- rbind(topPlayers, temp)
  }
  
  return(topPlayers)
}

# get a position for a certain year
oneYearTable <- function(table, year)
{
  oneYear <- table[table$Year %in% year,]
  return(oneYear)
}


###############################################
################ Create Tables ################
###############################################

# create a table with 10 years of passing stats
passing <- multiYearTable("passing")
passing <- cleanUp(passing, "passing")
View(passing)

# create a table with 10 years of rushing stats
rushing <- multiYearTable("rushing")
rushing <- cleanUp(rushing, "rushing")
View(rushing)

# create a table with 10 years of receiving stats
receiving <- multiYearTable("receiving")
receiving <- cleanUp(receiving, "receiving")
View(receiving)

# create a table with 10 years of kicking stats
kicking <- multiYearTable("kicking")
kicking <- cleanUp(kicking, "kicking")
View(kicking)

# change PLAYER column into 2 columns (name and position)
passing <- createPositionColumn(passing, 14)
rushing <- createPositionColumn(rushing, 12)
receiving <- createPositionColumn(receiving, 14)

# Create Position Tables
QB <- merge(x=passing, y=rushing, by.x = c("Year", "PLAYER", "Position", "TEAM"), by.y = c("Year", "PLAYER", "Position", "TEAM"))
QB <- QB[QB$Position %in% "QB",]

RB <- merge(x=rushing, y=receiving, by.x = c("Year", "PLAYER", "Position", "TEAM"), by.y = c("Year", "PLAYER", "Position", "TEAM"))
RB <- RB[RB$Position %in% "RB",]

WR <- receiving[receiving$Position %in% "WR",]

TE <- receiving[receiving$Position %in% "TE",]

# Rename duplicate rows
QB <- renameDuplicates(QB, "QB")
RB <- renameDuplicates(RB, "RB")
WR <- renameDuplicates(WR, "WR")
TE <- renameDuplicates(TE, "TE")

# add a fantasy point total column

QB$'Fantasy_Points' <- .04*QB$`P Yds` + 4*QB$`P TDs` - 2*QB$INT + .1*QB$`R Yds` + 6*QB$`R Tds`- 2*QB$FUM

RB$'Fantasy_Points' <- .1*RB$`Ru Yds` + 6*RB$`Ru TDs` - 2*RB$`Ru Fum` + .1*RB$`Re Yds` + 6*RB$`Re TDs` - 2*RB$`Re Fum`

WR$'Fantasy_Points' <- .1*WR$YDS + 6*WR$TD - 2*WR$FUM

TE$'Fantasy_Points' <- .1*TE$YDS + 6*TE$TD - 2*TE$FUM

kicking$'Fantasy_Points' <- 1*kicking$XPM - 1*(kicking$`FG Att`-kicking$FGM) +
  3*(kicking$Made_1_19+kicking$Made_20_29+kicking$Made_30_39) + 4*(kicking$Made_40_49) + 
  5*(kicking$Att_50_or_more)

###############################################
############### Top 32 Players ################
###############################################

QB32 <- getTopPlayers(QB)
RB32 <- getTopPlayers(RB)
WR32 <- getTopPlayers(WR)
TE32 <- getTopPlayers(TE)
K32 <- getTopPlayers(kicking)

########## means ##########

meanQB32 <- aggregate(QB32[, 5:25], list(Year = QB32$Year), mean)
meanQB32$Position <- "QB"

meanRB32 <- aggregate(RB32[, 5:25], list(Year = RB32$Year), mean)
meanRB32$Position <- "RB"

meanWR32 <- aggregate(WR32[, 5:16], list(Year = WR32$Year), mean)
meanWR32$Position <- "WR"

meanTE32 <- aggregate(TE32[, 5:16], list(Year = TE32$Year), mean)
meanTE32$Position <- "TE"

meanFP <- meanQB32[c("Year", "Fantasy_Points", "Position")]
meanFP <- rbind(meanFP, meanRB32[c("Year", "Fantasy_Points", "Position")])
meanFP <- rbind(meanFP, meanWR32[c("Year", "Fantasy_Points", "Position")])
meanFP <- rbind(meanFP, meanTE32[c("Year", "Fantasy_Points", "Position")])


###############################################
################ Plots I used #################
###############################################

# create colors
plotBackgroundColor <- rgb(200/255, 200/255, 200/255, 1)
stripColor <- rgb(210/255, 225/255, 245/255, 1)
panelBackgroundColor <- "black"

# plot mean change over time
plotMean <- ggplot(meanFP, aes(Year, `Fantasy_Points`, group=Position)) +
  geom_line(aes(col=meanFP$Position), size=1.5) +
  geom_point(aes(col=meanFP$Position), size=3) +
  scale_x_continuous("Year", labels = as.character(meanFP$Year), breaks = meanFP$Year) +
  guides(color=guide_legend(title="Position")) +
  ggtitle("Average Fantasy Points of the Top 32 Players") +
  theme(plot.title = element_text(size=30, hjust = 0.5)) +
  scale_color_discrete(breaks=c("QB","RB","WR", "TE")) +
  scale_y_continuous(name = "Fantasy Points")

# QB
# make a plot using the QB data table, x-axis = 'P Yds', y-axis = 'Fantasy_Points'
plotQB <- ggplot(QB, aes(`P_Yds`, `Fantasy_Points`)) +
  # color based on 'P TDS'
  geom_point(aes(col=QB$`P_TDs`)) +
  # change the color of points (blue = low, red = high)
  scale_color_gradientn(colors=c("blue", "light blue", "green", "gold", "orange", "red")) +
  # change legend title
  guides(color=guide_legend(title="Passing TDs")) +
  # split into separate graphs by Year
  facet_wrap(~Year) +
  ggtitle("QB Passing Yards vs Fantasy Points") +
  # center title, then change colors
  theme(plot.title = element_text(size=30, hjust = 0.5), 
        panel.background = element_rect(fill = panelBackgroundColor, colour = 'white'), 
        plot.background = element_rect(fill=plotBackgroundColor), 
        strip.background = element_rect(colour = "white", fill = stripColor)) +
  # change x-axis name
  scale_x_continuous(name = "Passing Yards") +
  # change y-axis name
  scale_y_continuous(name = "Fantasy Points")

# RB: Ru Yds vs FP (scale Ru TDs)
plotRB <- ggplot(RB, aes(`Ru_Yds`, `Fantasy_Points`)) +
  geom_point(aes(col=RB$`Ru_TDs`)) +
  scale_color_gradientn(colors=c("blue", "light blue", "green", "gold", "orange", "red")) +
  guides(color=guide_legend(title="Rushing TDs")) +
  facet_wrap(~Year) +
  ggtitle("RB Rushing Yards vs Fantasy Points") +
  theme(plot.title = element_text(size=30, hjust = 0.5), 
        panel.background = element_rect(fill = 'black', colour = 'white'), 
        plot.background = element_rect(fill=plotBackgroundColor), 
        strip.background = element_rect(colour = "white", fill = stripColor)) +
  scale_x_continuous(name = "Rushing Yards") +
  scale_y_continuous(name = "Fantasy Points")

# WR: 
plotWR <- ggplot(WR, aes(YDS, `Fantasy_Points`)) +
  geom_point(aes(col=WR$TD)) +
  scale_color_gradientn(colors=c("blue", "light blue", "green", "gold", "orange", "red")) +
  guides(color=guide_legend(title="Receiving TDs")) +
  facet_wrap(~Year) +
  ggtitle("WR Receiving Yards vs Fantasy Points") +
  theme(plot.title = element_text(size=30, hjust = 0.5), 
        panel.background = element_rect(fill = 'black', colour = 'white'), 
        plot.background = element_rect(fill=plotBackgroundColor), 
        strip.background = element_rect(colour = "white", fill = stripColor)) +
  scale_x_continuous(name = "Receiving Yards") +
  scale_y_continuous(name = "Fantasy Points")


# TE: 
plotTE <- ggplot(TE, aes(YDS, `Fantasy_Points`)) +
  geom_point(aes(col=TE$TD)) +
  scale_color_gradientn(colors=c("blue", "light blue", "green", "gold", "orange", "red")) +
  guides(color=guide_legend(title="Receiving TDs")) +
  facet_wrap(~Year) +
  ggtitle("TE Receiving Yards vs Fantasy Points") +
  theme(plot.title = element_text(size=30, hjust = 0.5), 
        panel.background = element_rect(fill = 'black', colour = 'white'), 
        plot.background = element_rect(fill=plotBackgroundColor), 
        strip.background = element_rect(colour = "white", fill = stripColor)) +
  scale_x_continuous(name = "Receiving Yards") +
  scale_y_continuous(name = "Fantasy Points")


# Actually Plot

plotMean
plotQB
plotRB
plotWR
plotTE


####################################################################################################################
###############################################                  ###################################################
###############################################  End of Project  ###################################################
###############################################                  ###################################################
####################################################################################################################

# Create condensed tables
  QB.condensed <- QB[, c(5, 6, 8, 11, 12, 16, 17, 21)]
  QB.condensed <- QB.condensed[c(3:5,1:2, 7:8, 6)]
  
  RB.condensed <- RB[, c(5:7, 10, 14:18)]
  RB.condensed <- RB.condensed[c(2, 4, 1, 3, 7, 9, 5, 6, 8)]
  
  WR.condensed <- WR[, c(5:9, 14)]
  WR.condensed <- WR.condensed[c(3, 5, 1, 2, 4, 6)]
  
  TE.condensed <- TE[, c(5:9, 14)]
  TE.condensed <- TE.condensed[c(3, 5, 1, 2, 4, 6)]

# Export data tables to csv files
  write.csv(QB, file="QB.csv")
  write.csv(RB, file="RB.csv")
  write.csv(WR, file="WR.csv")
  write.csv(TE, file="TE.csv")
  
  write.csv(QB.condensed, file="QB_condensed.csv")
  write.csv(RB.condensed, file="RB_condensed.csv")
  write.csv(WR.condensed, file="WR_condensed.csv")
  write.csv(TE.condensed, file="TE_condensed.csv")
  
##########################################################################################
########################################### My Stuff #####################################
##########################################################################################

# Create tables to export (add the previous year fantasy points)
  # QB
QB.2017 <- oneYearTable(QB, 2017)
QB.2016 <- oneYearTable(QB, 2016)
temp <- QB.2016[c(2, 25)]
colnames(temp)[c(2)] <- c("2016 Fantasy Points")
QB.export <- merge(x=QB.2017, y=temp, by.x = c("PLAYER"), by.y = c("PLAYER"), all.x=TRUE)
  # RB
RB.2017 <- oneYearTable(RB, 2017)
RB.2016 <- oneYearTable(RB, 2016)
temp <- RB.2016[c(2, 25)]
colnames(temp)[c(2)] <- c("2016 Fantasy Points")
RB.export <- merge(x=RB.2017, y=temp, by.x = c("PLAYER"), by.y = c("PLAYER"), all.x=TRUE)
  # WR
WR.2017 <- oneYearTable(WR, 2017)
WR.2016 <- oneYearTable(WR, 2016)
temp <- WR.2016[c(2, 16)]
colnames(temp)[c(2)] <- c("2016 Fantasy Points")
WR.export <- merge(x=WR.2017, y=temp, by.x = c("PLAYER"), by.y = c("PLAYER"), all.x=TRUE)
  # TE
TE.2017 <- oneYearTable(TE, 2017)
TE.2016 <- oneYearTable(TE, 2016)
temp <- TE.2016[c(2, 16)]
colnames(temp)[c(2)] <- c("2016 Fantasy Points")
TE.export <- merge(x=TE.2017, y=temp, by.x = c("PLAYER"), by.y = c("PLAYER"), all.x=TRUE)

# create an excel file with just this year's stats
write.xlsx(QB.export, file = "2017 Stats.xlsx", sheetName = "QB", row.names = FALSE)
write.xlsx(RB.export, file = "2017 Stats.xlsx", sheetName = "RB", append=TRUE, row.names = FALSE)
write.xlsx(WR.export, file = "2017 Stats.xlsx", sheetName = "WR", append=TRUE, row.names = FALSE)
write.xlsx(TE.export, file = "2017 Stats.xlsx", sheetName = "TE", append=TRUE, row.names = FALSE)


# Get the differences b/w starters and non-starters

# returns how many starters there are (for a 10 man standard league)
getNumberOfStarters <- function(position)
{
  numberOfStarters = 0;
  
  numberOfStarters = switch(position,
        "QB" = 10,
        "RB" = 20,
        "WR" = 20,
        "TE" = 10)
  
  return(numberOfStarters)
}

# get only the starting players at a position
getStartingPlayers <- function(table, position)
{
  for(i in (1:10))
  {
    year <- 2007+i
    temp <- table[table$Year %in% year,]
    temp <- temp[with(temp, order(-temp$`Fantasy_Points`)),]
    temp <- temp[1:getNumberOfStarters(position),]
    if(i == 1)
      startingPlayers <- temp
    else
      startingPlayers <- rbind(startingPlayers, temp)
  }
  
  return(startingPlayers)
}

# create a table with the starting players at each position
QB.startingPlayers <- getStartingPlayers(QB, "QB")
RB.startingPlayers <- getStartingPlayers(RB, "RB")
WR.startingPlayers <- getStartingPlayers(WR, "WR")
TE.startingPlayers <- getStartingPlayers(TE, "TE")

# create a table with the difference between the 1st and last starter each year
getDifferenceTable <- function(table, position)
{
  for(i in (1:10))
  {
    year <- 2007+i
    temp <- oneYearTable(table, year)
    difference <- max(temp$Fantasy_Points)-min(temp$Fantasy_Points)
    
    if(i == 1)
    {
      differenceTable <- c(position, year, difference)
    }
    else
    {
      differenceTable <- rbind(differenceTable, c(position, year, difference))
    }
  }
  colnames(differenceTable) <- c("Position", "Year", "Difference")
  return(differenceTable)
}

# Create tables with the point difference between the best and worst starters at each position
QB.differenceTable <- getDifferenceTable(QB.startingPlayers, "QB")
RB.differenceTable <- getDifferenceTable(RB.startingPlayers, "RB")
WR.differenceTable <- getDifferenceTable(WR.startingPlayers, "WR")
TE.differenceTable <- getDifferenceTable(TE.startingPlayers, "TE")

differenceTable <- rbind(QB.differenceTable, RB.differenceTable, WR.differenceTable, TE.differenceTable)


# Convert datatypes from factors to characters and numerics
differenceTable[, 1:3] <- toCharacter(differenceTable[, 1:3])
differenceTable[, 2:3] <- toNumeric(differenceTable[, 2:3])

# convert from a matrix to a data frame
differenceTable <- data.frame(differenceTable)

# plot starter difference change over time
ggplot(differenceTable, aes(Year, `Difference`, group=Position)) +
  geom_line(aes(col=differenceTable$Position), size=1.5) +
  geom_point(aes(col=differenceTable$Position), size=3) +
  scale_x_continuous("Year", labels = as.character(differenceTable$Year), breaks = differenceTable$Year) +
  guides(color=guide_legend(title="Position")) +
  ggtitle("Difference between best and worst starting players over time") +
  theme(plot.title = element_text(size=30, hjust = 0.5)) +
  scale_color_discrete(breaks=c("QB","RB","WR", "TE")) +
  scale_y_continuous(name = "Difference")




  
