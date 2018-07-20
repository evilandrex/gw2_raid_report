library(shiny)
options(shiny.maxRequestSize=50*1024^2) 

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
          dataframes <- htmlParser(parsed, team)
          
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