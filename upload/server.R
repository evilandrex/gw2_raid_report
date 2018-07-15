library(shiny)
options(shiny.maxRequestSize=20*1024^2) 

shinyServer(function(input, output, session) {
    output$fileName <- renderTable({
      inFile <- input$evtc
      
      # Display nothing there if no file has been uploaded
      if (is.null(inFile))
        return(data.frame('File' = 'File', 
                          'Parsed' = 'Parsed', 
                          'Report' = 'Report'))
      
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
          
          # Increment progress 
          progress$inc(amount = 0.5)
          
          boss <- parsed$encounter$boss
          success <- parsed$encounter$success
          players <- parsed$players
          id <- parsed$id
          html <- read_html(parsed$permalink) %>% html_nodes('body')
          
          # Create blank data.frames, structures match sql database
          encounterData <- data.frame(fight_id = NA,
                                      team = NA,
                                      boss = NA,
                                      success = NA,
                                      date = NA, 
                                      duration = NA,
                                      team_dps = NA,
                                      damage_done = NA,
                                      player1 = NA,
                                      player2 = NA,
                                      player3 = NA,
                                      player4 = NA,
                                      player5 = NA,
                                      player6 = NA,
                                      player7 = NA,
                                      player8 = NA,
                                      player9 = NA,
                                      player10 = NA)
          playerData <- data.frame(fight_id = NA,
                                   player_name = NA,
                                   char_name = NA,
                                   specialization = NA,
                                   total_dps = NA,
                                   power_dps = NA,
                                   condi_dps = NA,
                                   crit_percent = NA,
                                   above90_percent = NA,
                                   downed_count = NA,
                                   dead_at = NA,
                                   subgroup = NA,
                                   wep1 = NA,
                                   wep2 = NA,
                                   wep3 = NA,
                                   wep4 = NA, 
                                   gear_stats = NA)
          buffData <- data.frame(fight_id = NA,
                                 char_name = NA,
                                 might = NA,
                                 fury = NA,
                                 quickness = NA,
                                 alacrity = NA,
                                 protection = NA,
                                 regeneration = NA,
                                 vigor = NA,
                                 aegis = NA,
                                 stability = NA,
                                 swiftness = NA,
                                 retaliation = NA,
                                 resistance = NA,
                                 banner_strength = NA,
                                 banner_discipline = NA,
                                 empowered_allies = NA,
                                 spirit_frost = NA,
                                 spirit_sun = NA,
                                 spirit_storm = NA,
                                 spirit_earth = NA,
                                 spirit_water = NA,
                                 glyph_empowerment = NA,
                                 spotter = NA,
                                 assassins_presence = NA,
                                 pinpoint_distribution = NA,
                                 vampiric_presence = NA)
          
          
          # Loop through each subgroup
          composition <- html_nodes(html, 'div table')[1] %>% html_children()
          for (subgroup in composition) {
            # Loop through each player
            players <- html_nodes(subgroup, 'td')
            text <- ''
            for (player in players) {
              images <- html_nodes(players, 'img') %>% html_attr('alt')
            }
          }
          # # Look for the parsed file in the directory
          # htmlFile <- list.files(getwd())
          # htmlFile <- htmlFile[which(grepl(".html", htmlFile))]
          # 
          # # Get boss name
          # match <- gregexpr("([[:alpha:]]{2,5})(?=\\.)", htmlFile, perl = TRUE)
          # boss <- regmatches(htmlFile, match)
          # 
          # # Get a formatted date string
          # dateInfo <- as.Date(substr(inFile$name[file], 1, 8), "%Y%m%d")
          # date <- format(dateInfo, format = "%b-%d-%Y")
          # 
          # # Rename file
          # file.rename(htmlFile, gsub("[[:digit:]+]", date, htmlFile))
          # 
          # # Look for the renamed file in the directory
          # htmlFile <- list.files(getwd())
          # htmlFile <- htmlFile[which(grepl(".html", htmlFile))]
          # 
          # # Parse the HTML file into a text file using parse function
          # data <- htmlParser(htmlFile)
          # 
          # # Increment progress 
          # progress$inc(amount = 0.5)
          # 
          # if (is.data.frame(data)) {
          #   # Export data file
          #   write.table(data, gsub('.html', '.txt', htmlFile), sep = "\t", row.names = FALSE)
          #   file.copy(gsub('.html', '.txt', htmlFile), paste('../Data/', boss, '/', gsub('.html', '.txt', htmlFile), sep = ''), overwrite = TRUE)
          #   file.remove(gsub('.html', '.txt', htmlFile))
          #   
          #   # Moved the finished file into parsed directory
          #   file.copy(htmlFile, paste('../RaidHeroesLogs/', boss, '/', htmlFile, sep = ''), overwrite = TRUE)
          #   file.remove(htmlFile)
          #   
          #   # Concatenate the completed file to report vector
          #   outputText <- rbind(outputText, data.frame('File' = inFile$name[file], 'Parsed' = htmlFile, 'Report' = 'Success'))
          # } else {
          #   outputText = rbind(outputText, data.frame('File' = inFile$name[file], 'Parsed' = htmlFile, 'Report' = data))
          #   file.remove(htmlFile)
          #   }
        }
      }
      progress$close()
      return(link = parsed$permalink)
      })
  }
)