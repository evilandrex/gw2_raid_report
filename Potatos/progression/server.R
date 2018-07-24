library(shiny)
library(plotly)
library(plyr)
library(RMySQL)

# Team name
teamName = 'Potatos'

# SQL DB Password
dbpass <- 'CHANGEME'

# Get team names and codes
conn <- dbConnect(
  drv = MySQL(),
  dbname = "raid_report",
  host = '127.0.0.1',
  port = 3306,
  username = "root",
  password = dbpass)

team_info <- dbGetQuery(conn,
                        paste('SELECT * FROM team_info WHERE team_name = "', 
                              teamName, '"', sep = ''))

encounter_data <- dbGetQuery(conn, 
                             paste('SELECT * FROM encounter_data ',
                                   'WHERE team = "', teamName, '"',
                                   sep = ''))
encounter_data <- encounter_data[order(encounter_data$date),]
encounter_data$date <- as.POSIXct(encounter_data$date, 
                                  origin = '1970-01-01')
buff_data <- dbGetQuery(conn, 
                        paste('SELECT buff_data.* FROM buff_data ',
                              'INNER JOIN encounter_data ON ',
                              'buff_data.fight_id = encounter_data.fight_id ',
                              'WHERE encounter_data.team = "', teamName, '"',
                              sep = ''))
player_data <- dbGetQuery(conn,
                          paste('SELECT player_data.* FROM player_data ',
                                'INNER JOIN encounter_data ON ',
                                'player_data.fight_id = encounter_data.fight_id ',
                                'WHERE encounter_data.team = "', teamName, '"',
                                sep = ''))
boss_attacks <- dbGetQuery(conn,
                           paste('SELECT boss_attacks.* FROM boss_attacks ',
                                 'INNER JOIN encounter_data ON ',
                                 'boss_attacks.fight_id = encounter_data.fight_id ',
                                 'WHERE encounter_data.team = "', teamName, '"',
                                 sep = ''))

# Disconnect from database
dbDisconnect(conn)

shinyServer(function(input, output, session) {
  # Subset data to the chosen boss
  selected <- reactiveValues()
  observe({
    selected$encounter <- encounter_data[encounter_data$boss == input$bossName,]
    selected$buffs <- buff_data[buff_data$fight_id %in%
                                  unique(selected$encounter$fight_id),]
    selected$players <- player_data[player_data$fight_id %in%
                                      unique(selected$encounter$fight_id),]
    selected$attacks <- boss_attacks[boss_attacks$fight_id %in%
                                       unique(selected$encounter$fight_id),]
    })
  
  output$progression <- renderPlotly({
    # Look for best run daily
    summaryData <- ddply(selected$encounter, 
                         .(format(date, format="%B %d, %Y")), summarize, 
                         killed = max(success), best_run = max(damage_done))
    # Fix first column name
    names(summaryData)[1] <- 'date'
    
    # Visualize
    plot_ly(data = summaryData,
            x = ~date,
            y = ~best_run,
            type = 'scatter',
            mode = 'line+markers')  %>% 
      add_trace(data = summaryData[summaryData$killed == 1,],
                x = ~date,
                y = ~best_run,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 10,
                              color = 'rgba(255, 182, 193, .9)',
                              line = list(color = 'rgba(152, 0, 0, .8)',
                                          width = 2)))
  })
  
})