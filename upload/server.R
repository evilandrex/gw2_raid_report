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