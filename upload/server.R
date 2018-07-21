library(shiny)
library(jsonlite)
library(RMySQL)

options(shiny.maxRequestSize=50*1024^2) 

# SQL DB Password
dbpass <- 'CHANGME'

# Get team names and codes
conn <- dbConnect(
  drv = MySQL(),
  dbname = "raid_report",
  host = '127.0.0.1',
  port = 3306,
  username = "root",
  password = dbpass)

team_info <- dbGetQuery(conn, 'SELECT * FROM team_info')

# Disconnect from database
dbDisconnect(conn)

# Send data function
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

# Get HTML parser function
source('html_parser.R')

shinyServer(function(input, output, session) {
    output$fileName <- renderDataTable({
      # Check if nothing has been uploaded.
      if (is.null(input$evtc)) {
        return(data.frame('File' = 'File', 	
                          'Parsed' = 'Parsed', 	
                          'Report' = 'Report'))
      }
      
      # Check team code
      if (input$team_code %in% team_info$team_code) {
        team <- team_info$team_name
      }
      
      inFile <- input$evtc
      
      progress <- Progress$new(session, min = 0, max = length(inFile$datapath))
      progress$set(value = 0, message = 'Parsing EVTCs')
      
      outputText = data.frame()
      # Loop through all uploaded files
      for (file in 1:length(inFile$datapath)) {
        # If the uploaded file is a evtc, process it
        if (inFile$type[file] == 'application/zip' 
            | grepl('evtc', inFile$name[file])) {
          # Rename file to the original name
          toUpload <- file.path(dirname(inFile$datapath[file]), 
                                inFile$name[file])
          file.rename(inFile$datapath[file], toUpload)
          
          # Parse via dps.report API
          parsed <- content(POST(url = 'https://dps.report/uploadContent',
                         body = list(json = 1,
                                     generator = 'ei',
                                     userToken = 'kltu2he26nvdrk0451atc1s2p2',
                                     file = upload_file(toUpload)
                                     )))
          
          # Increment progress for finished dps.report parse
          progress$inc(amount = 0.5)
          
          # Convert parsed data into dataframes
          dataframes <- htmlParser(parsed)
          
          if (typeof(dataframes) == 'list'){
            # Write data to SQL database
            sendData(dataframes)
            
            # Increment progress for database saving
            progress$inc(amount = 0.5)
            
            # Update output text 
            outputText <- rbind(outputText, data.frame('File' = inFile$name[file],
                                                       'Link' = parsed$permalink,
                                                       'Report' = 'Success'))
          } else {
            outputText <- rbind(outputText, data.frame('File' = inFile$name[file],
                                                       'Link' = parsed$permalink,
                                                       'Report' = dataframes))
          }
        }
      }
      progress$close()
      return(outputText)
      })
  
  # Team name
  output$teamName <- renderText({
   if (input$team_code %in% team_info$team_code) {
     return(team_info$team_name[team_info$team_code == input$team_code])
   } else {
     return('Bad code')
   }
  })
  outputOptions(output, "teamName", suspendWhenHidden = FALSE)
})