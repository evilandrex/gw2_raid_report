#Load packages
library('jsonlite')
library('RMySQL')

# SQL DB Password
dbpass <- 'CHANGEME'

timeToSeconds <- function(time) {
  time <- as.character(time)
  
  if (time == '0') {
    return(0)
  }
  
  timeString <- strsplit(time, split = '\\w(?=[\\d])', perl = TRUE)[[1]]
  seconds <- 0
  for (string in timeString) {
    if (strsplit(string, ' ')[[1]][2] == 'm') {
      temp <- as.numeric(strsplit(string, ' ')[[1]][1]) * 60
      seconds <- seconds + temp
    } else {
      seconds <- seconds + as.numeric(strsplit(string, ' ')[[1]][1])
    }
  }
  return(seconds)
}

sendData <- function(parsedResults) {
  # Connect to database
  conn <- dbConnect(
    drv = MySQL(),
    dbname = "raid_report",
    host = '127.0.0.1',
    port = 3306,
    username = "root",
    password = dbpass)
  
  # Forces exit when database finishes
  on.exit(dbDisconnect(conn), add = TRUE)
  
  # Add encounter dataframe
  dbWriteTable(conn, 'encounter_data', parsedResults$encounterData, 
               row.names = FALSE, append = TRUE)
  
  # Add player dataframe
  dbWriteTable(conn, 'player_data', parsedResults$playerData, 
               row.names = FALSE, append = TRUE)
  
  # Check if any new columns need to be made
  buffList <- dbGetQuery(conn, 'SHOW COLUMNS FROM buff_data')$Field
  buffToAdd <- names(parsedResults$buffData)[!names(parsedResults$buffData) %in% 
                                               buffList]
  
  # Add columns as necessary
  for (newColumn in buffToAdd) {
    query <- "ALTER TABLE buff_data ADD ?newColumn FLOAT;"
    query <- sqlInterpolate(conn, query, newColumn = as.factor(newColumn))
    dbGetQuery(conn, query)
  }
  
  # Add buff data
  dbWriteTable(conn, 'buff_data', parsedResults$buffData, row.names = FALSE,
               append = TRUE)
  
  # Add boss attacks table to database
  dbWriteTable(conn, 'boss_attacks', parsedResults$bossAttacks, 
               row.names = FALSE, append = TRUE)
}

# Tester block
# setwd('~/gw2_raid_report/upload')
# parsed <- content(POST(url = 'https://dps.report/uploadContent',
#                        body = list(json = 1,
#                                    generator = 'ei',
#                                    userToken = 'kltu2he26nvdrk0451atc1s2p2',
#                                    file = upload_file('./20180713-222530.evtc.zip')
#                        )))
# results <- htmlParser(parsed, 'Potatos')
