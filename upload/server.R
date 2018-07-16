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
          
          # Increment progress for finished dps.report parse
          progress$inc(amount = 0.5)
          
          # Convert parsed data into dataframes
          dataframes <- htmlParser(parsed)
          
          # WRITE DATA TO SQL DATABASE
          
          # Increment progress for database saving
          progress$inc(amount = 0.5)
          
          # Update output text 
          outputText <- rbind(outputText, data.frame('File' = inFile$name[file],
                                                     'Link' = parsed$permalink,
                                                     'Report' = 'Success')) # NOTE: NEED TO IMPLEMENT THIS
        }
      }
      progress$close()
      return(outputText)
      })
  }
)