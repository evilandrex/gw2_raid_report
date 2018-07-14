library(shiny)
library(plotly)
library(plyr)

shinyServer(function(input, output, session) {
  
  #Makes single run drop down selecter reactive
  output$selectRun = renderUI({
    selectedBoss <- switch(input$bossName,
                           "Vale Guardian" = 'vg',
                           "Gorseval" = 'gorse',
                           "Sabetha" = "sab",
                           "Slothasor" = "sloth",
                           "Matthias Gabrel" = 'matt',
                           "Keep Construct" = 'kc',
                           "Xera" = 'xera',
                           "Cairn the Indomitable" = 'cairn',
                           "Mursaat Overseer" = 'mo',
                           "Samarog" = 'sam',
                           "Deimos" = 'dei') 
    
    subDataset <- dataset[which(dataset$Boss == selectedBoss), ]
    selectInput('selectedRun',
                label = NULL,
                levels(factor(subDataset$Date))[order(levels(factor(subDataset$Date)), decreasing = TRUE)], #Finds all dates and orders from highest to lowest
                width = '120px')
  })
  
  #Single run pie graph
  output$singleRun <- renderPlotly({
    selectedBoss <- switch(input$bossName,
                           "Vale Guardian" = 'vg',
                           "Gorseval" = 'gorse',
                           "Sabetha" = "sab",
                           "Slothasor" = "sloth",
                           "Matthias Gabrel" = 'matt',
                           "Keep Construct" = 'kc',
                           "Xera" = 'xera',
                           "Cairn the Indomitable" = 'cairn',
                           "Mursaat Overseer" = 'mo',
                           "Samarog" = 'sam',
                           "Deimos" = 'dei') 
    
    subDataset <- dataset[which(dataset$Boss == selectedBoss), ]
    subDataset <- subDataset[order(subDataset$TotalDPS), ]
    subDataset$plotColumn <- factor(subDataset$plotColumn, levels = unique(subDataset$plotColumn)[order(subDataset$TotalDPS, decreasing = FALSE)])
    
    #Subset day DPS Data
    DayData <- subDataset[order(-subDataset$Date), ]
    DayData <- DayData[which(DayData$Date == input$selectedRun),]
    DayData$plotColumn <- factor(DayData$plotColumn, levels = unique(DayData$plotColumn)[order(DayData$TotalDPS, decreasing = FALSE)])
    dayDPSSplit <- data.frame(cbind(rbind(sum(DayData$PowerDPS), sum(DayData$CondiDPS)), rbind('PowerDPS', 'CondiDPS')))
    
    #Subset day DPS data by group
    subgroupDPS <- ddply(DayData, .(Subgroup), summarize, TotalDPS = sum(TotalDPS), numPlayers  = sum(Subgroup)/Subgroup[1])
    #subgroupDPS <- subgroupDPS[seq(dim(subgroupDPS)[1],1),]
    
    #Plot day DPS data
    DayPlot <- plot_ly() %>% 
      add_pie(data = dayDPSSplit,
              values = dayDPSSplit[,1],
              labels = dayDPSSplit[,2],
              type = 'pie',
              marker = list(colors = c('rgba(25, 25, 255, 0.5)', 
                                       'rgba(255, 25, 25, 0.5)'),
                            line = list(color = 'white', width = 2)),
              hoverinfo = 'text+value+percent',
              hovertext = paste('<b>', dayDPSSplit[,2], '</b>', sep = ""),
              textinfo = 'none',
              pull = 0.025,
              sort = FALSE) %>%
      add_pie(data = subgroupDPS, 
              values = ~TotalDPS,
              labels = ~Subgroup,
              type = 'pie',
              marker = list(line = list(color = 'white', width = 1.5)),
              hoverinfo = 'text+value+percent',
              hovertext = paste('<b>Subgroup: </b>', subgroupDPS$Subgroup, ' <i>(', subgroupDPS$numPlayers, ' players)</i>', sep = ''),
              hole = 0.45,
              sort = FALSE) %>%
      add_pie(data = DayData,
              values = ~TotalDPS,
              labels = ~PlayerName,
              type = 'pie',
              marker = list(colors = ~SpecColour,
                            line = list(color = "white", width = 2)),
              hovertext = ~paste('<b>', PlayerName, '</b> - ', Specialization ,'<br><b>Power DPS: </b>', format(PowerDPS, big.mark = ','), " <b>Condi DPS: </b>", format(CondiDPS, big.mark = ','), '<br><b>Total DPS: </b>', format(TotalDPS, big.mark = ','), " <i>", sep = ""),
              hoverinfo = 'text',
              hole = 0.5
      ) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, fixedrange = TRUE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, fixedrange = TRUE),
        showlegend = F,
        plot_bgcolor = 'rgba(222,222,222,1)',
        paper_bgcolor = 'rgba(222,222,222,1)',
        margin = list(
          b = 20,
          t = 20,
          l = 0
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #Player table
  output$players <- renderDataTable({
    selectedBoss <- switch(input$bossName,
                           "Vale Guardian" = 'vg',
                           "Gorseval" = 'gorse',
                           "Sabetha" = "sab",
                           "Slothasor" = "sloth",
                           "Matthias Gabrel" = 'matt',
                           "Keep Construct" = 'kc',
                           "Xera" = 'xera',
                           "Cairn the Indomitable" = 'cairn',
                           "Mursaat Overseer" = 'mo',
                           "Samarog" = 'sam',
                           "Deimos" = 'dei') 
    
    subDataset <- dataset[which(dataset$Boss == selectedBoss), ]
    subDataset <- subDataset[order(subDataset$TotalDPS), ]
    subDataset$plotColumn <- factor(subDataset$plotColumn, levels = unique(subDataset$plotColumn)[order(subDataset$TotalDPS, decreasing = TRUE)])
    
    #Subset day DPS Data
    DayData <- subDataset[order(-subDataset$Date), ]
    DayData <- DayData[which(DayData$Date == input$selectedRun),]
    DayData$plotColumn <- factor(DayData$plotColumn, levels = unique(DayData$plotColumn)[-order(DayData$TotalDPS, decreasing = TRUE)])
    
    df <- data.frame(cbind(DayData$Subgroup, 
                           as.character(DayData$PlayerName), 
                           as.character(DayData$Specialization), 
                           as.character(DayData$BuildType), 
                           as.character(DayData$Weapons), 
                           DayData$TotalDPS, 
                           DayData$PowerDPS, 
                           DayData$CondiDPS))
    names(df) <- c('Subgroup', 'Player Name', 'Specialization', 'BuildType', 'Weapons', 'Total DPS', 'Power DPS', 'Condi DPS')
    df$`Total DPS` <- format(as.numeric(as.character(df$`Total DPS`)), big.mark = ',')
    df$`Power DPS` <- format(as.numeric(as.character(df$`Power DPS`)), big.mark = ',')
    df$`Condi DPS` <- format(as.numeric(as.character(df$`Condi DPS`)), big.mark = ',')
    df[seq(dim(df)[1],1),]
  }, options = list(
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    ordering = FALSE,
    scrollX = TRUE
  ))
  
  #Formatted date
  output$formattedDate <- reactive({
    unformattedDate <- as.Date(as.character(input$selectedRun), "%Y%m%d")
    
    paste(format(unformattedDate, format = "%B %d, %Y"))
  })
  
  #Killtime
  output$killTime <- reactive({
    selectedBoss <- switch(input$bossName,
                           "Vale Guardian" = 'vg',
                           "Gorseval" = 'gorse',
                           "Sabetha" = "sab",
                           "Slothasor" = "sloth",
                           "Matthias Gabrel" = 'matt',
                           "Keep Construct" = 'kc',
                           "Xera" = 'xera',
                           "Cairn the Indomitable" = 'cairn',
                           "Mursaat Overseer" = 'mo',
                           "Samarog" = 'sam',
                           "Deimos" = 'dei') 
    
    #Subset boss data
    subDataset <- dataset[which(dataset$Boss == selectedBoss), ]
    subDataset <- subDataset[which(subDataset$Date == input$selectedRun),]
    
    minutes <- subDataset$KillTime[1] %/% 60
    seconds <- round(subDataset$KillTime[1] %% 60, digits = 1)
    
    paste('<b>Kill time: </b>', minutes, ' minutes, ', seconds, ' seconds', sep = '')
  })
  
  #Time difference from best
  output$diffFromBest <- reactive({
    selectedBoss <- switch(input$bossName,
                           "Vale Guardian" = 'vg',
                           "Gorseval" = 'gorse',
                           "Sabetha" = "sab",
                           "Slothasor" = "sloth",
                           "Matthias Gabrel" = 'matt',
                           "Keep Construct" = 'kc',
                           "Xera" = 'xera',
                           "Cairn the Indomitable" = 'cairn',
                           "Mursaat Overseer" = 'mo',
                           "Samarog" = 'sam',
                           "Deimos" = 'dei') 
    
    #Subset boss data
    subDataset <- dataset[which(dataset$Boss == selectedBoss), ]
    
    #Best time
    bestTime <- min(subDataset$KillTime)
    
    #Subset selected run
    subDataset <- subDataset[which(subDataset$Date == input$selectedRun),]
    
    timeDiff <- subDataset$KillTime[1] - bestTime
    
    minutes <- timeDiff %/% 60
    seconds <- round(timeDiff %% 60, digits = 1)
    
    paste('<b>Time from best: </b>', minutes, ' minutes, ', seconds, ' seconds', sep = '')
  })
  
  #Kill time ranking
  output$killTimeRank <- reactive({
    selectedBoss <- switch(input$bossName,
                           "Vale Guardian" = 'vg',
                           "Gorseval" = 'gorse',
                           "Sabetha" = "sab",
                           "Slothasor" = "sloth",
                           "Matthias Gabrel" = 'matt',
                           "Keep Construct" = 'kc',
                           "Xera" = 'xera',
                           "Cairn the Indomitable" = 'cairn',
                           "Mursaat Overseer" = 'mo',
                           "Samarog" = 'sam',
                           "Deimos" = 'dei') 
    
    #Subset boss data
    subDataset <- dataset[which(dataset$Boss == selectedBoss), ]
    
    #Times list
    killTimeList <- unique(subDataset$KillTime)[order(unique(subDataset$KillTime), decreasing = FALSE)]
    
    #Subset selected run
    subDataset <- subDataset[which(subDataset$Date == input$selectedRun),]
    killTime <- subDataset$KillTime[1]
    
    paste('<b>Rank: </b>', match(killTime, killTimeList), sep = '')
  })
  
  #Core team count
  output$coreTeamCount <- reactive({
    selectedBoss <- switch(input$bossName,
                           "Vale Guardian" = 'vg',
                           "Gorseval" = 'gorse',
                           "Sabetha" = "sab",
                           "Slothasor" = "sloth",
                           "Matthias Gabrel" = 'matt',
                           "Keep Construct" = 'kc',
                           "Xera" = 'xera',
                           "Cairn the Indomitable" = 'cairn',
                           "Mursaat Overseer" = 'mo',
                           "Samarog" = 'sam',
                           "Deimos" = 'dei') 
    
    #Subset boss data
    subDataset <- dataset[which(dataset$Boss == selectedBoss), ]
    subDataset <- subDataset[which(subDataset$Date == input$selectedRun),]
    
    paste('<b>Core players: </b>', sum(subDataset$Core), sep = '')
  })
  
  #Core players missing
  output$playerMissing <- reactive({
    selectedBoss <- switch(input$bossName,
                           "Vale Guardian" = 'vg',
                           "Gorseval" = 'gorse',
                           "Sabetha" = "sab",
                           "Slothasor" = "sloth",
                           "Matthias Gabrel" = 'matt',
                           "Keep Construct" = 'kc',
                           "Xera" = 'xera',
                           "Cairn the Indomitable" = 'cairn',
                           "Mursaat Overseer" = 'mo',
                           "Samarog" = 'sam',
                           "Deimos" = 'dei') 
    
    #Subset boss data
    subDataset <- dataset[which(dataset$Boss == selectedBoss), ]
    subDataset <- subDataset[which(subDataset$Date == input$selectedRun),]
    
    if (sum(subDataset$Core) == length(coreTeam)) {
      paste('<b>Missing players: </b>None :)')
    } else {
      teamList <- data.frame(coreTeam)
      teamList$Missing <- TRUE
      for (row in 1:nrow(subDataset)) {
        if (!is.na(match(subDataset$PlayerName[row], coreTeam))) {
          teamList$Missing[which(teamList$coreTeam == as.character(subDataset$PlayerName[row]))] <- FALSE
        }
      }
      missingPlayers <- teamList[which(teamList$Missing),]$coreTeam
      
      paste('<b>Missing players: </b>', paste(missingPlayers, collapse = ', '), sep = '')
    }
  })
  
  #Dps totals
  output$DPSTotals <- reactive({
    selectedBoss <- switch(input$bossName,
                           "Vale Guardian" = 'vg',
                           "Gorseval" = 'gorse',
                           "Sabetha" = "sab",
                           "Slothasor" = "sloth",
                           "Matthias Gabrel" = 'matt',
                           "Keep Construct" = 'kc',
                           "Xera" = 'xera',
                           "Cairn the Indomitable" = 'cairn',
                           "Mursaat Overseer" = 'mo',
                           "Samarog" = 'sam',
                           "Deimos" = 'dei') 
    
    #Subset boss data
    subDataset <- dataset[which(dataset$Boss == selectedBoss), ]
    subDataset <- subDataset[which(subDataset$Date == input$selectedRun),]
    
    totalDPS <- sum(subDataset$TotalDPS)
    powerDPS <- sum(subDataset$PowerDPS)
    condiDPS <- sum(subDataset$CondiDPS)
    
    paste('<b>Total DPS: </b>', format(totalDPS, big.mark = ','), ' <b>Power DPS: </b>', format(powerDPS, big.mark = ','), ' <b>Condi DPS: </b>', format(condiDPS, big.mark = ','), sep = '')
  })
  
  #Buff table
  output$buffs <- renderDataTable({
    #Select boss
    selectedBoss <- switch(input$bossName,
                           "Vale Guardian" = 'vg',
                           "Gorseval" = 'gorse',
                           "Sabetha" = "sab",
                           "Slothasor" = "sloth",
                           "Matthias Gabrel" = 'matt',
                           "Keep Construct" = 'kc',
                           "Xera" = 'xera',
                           "Cairn the Indomitable" = 'cairn',
                           "Mursaat Overseer" = 'mo',
                           "Samarog" = 'sam',
                           "Deimos" = 'dei') 
    
    #Subset boss data
    subDataset <- dataset[which(dataset$Boss == selectedBoss), ]
    subDataset <- subDataset[order(subDataset$TotalDPS), ]
    subDataset$plotColumn <- factor(subDataset$plotColumn, levels = unique(subDataset$plotColumn)[order(subDataset$TotalDPS, decreasing = FALSE)])
    
    #Subset day data
    dayBuffs <- subDataset[order(-subDataset$Date), ]
    dayBuffs <- dayBuffs[which(dayBuffs$Date == input$selectedRun),]
    
    df <- data.frame(cbind(dayBuffs$Subgroup,
                           as.character(dayBuffs$PlayerName),
                           as.numeric(as.character(dayBuffs$Might)), 
                           as.numeric(as.character(dayBuffs$Fury)) / 100, 
                           as.numeric(as.character(dayBuffs$Quickness)) / 100, 
                           as.numeric(as.character(dayBuffs$Protection)) / 100, 
                           as.numeric(as.character(dayBuffs$Alacrity)) / 100, 
                           as.numeric(as.character(dayBuffs$Pinpoint.Distribution)) / 100, 
                           as.numeric(as.character(dayBuffs$Assassin.s.Presence)) / 100, 
                           as.numeric(as.character(dayBuffs$Vampiric.Presence)) / 100, 
                           as.numeric(as.character(dayBuffs$Banner.of.Strength)) / 100, 
                           as.numeric(as.character(dayBuffs$Banner.of.Discipline)) / 100, 
                           as.numeric(as.character(dayBuffs$Empower.Allies)) / 100, 
                           as.numeric(as.character(dayBuffs$Spirit.of.Frost)) / 100, 
                           as.numeric(as.character(dayBuffs$Sun.Spirit)) / 100, 
                           as.numeric(as.character(dayBuffs$Stone.Spirit)) / 100, 
                           as.numeric(as.character(dayBuffs$Spotter)) / 100, 
                           as.numeric(as.character(dayBuffs$Glyph.of.Empowerment)) / 100, 
                           as.numeric(as.character(dayBuffs$Grace.of.the.Land))
    ), stringsAsFactors = FALSE)
    names(df) <- c('Subgroup',
                   'Player Name',
                   'Might',
                   'Fury',
                   'Quickness',
                   'Protection',
                   'Alacrity',
                   'Pinpoint Distribution',
                   "Assassin's Presence",
                   'Vampiric Presence',
                   'Banner of Strength',
                   'Banner of Discipline',
                   'Empower Allies',
                   'Spirit of Frost',
                   'Sun Spirit',
                   'Stone Spirit',
                   'Spotter',
                   'Glyph of Empowerment',
                   'Grace of the Land')
    df <- df[order(as.numeric(df$Subgroup)),]
    totals <- ddply(df, .(Subgroup), summarize,
                    `Player Name` = 'Subgroup Avg',
                    Might = mean(as.numeric(as.character(Might))),
                    Fury = mean(as.numeric(as.character(Fury))),
                    Quickness = mean(as.numeric(as.character(Quickness))),
                    Protection = mean(as.numeric(as.character(Protection))),
                    Alacrity = mean(as.numeric(as.character(Alacrity))),
                    `Pinpoint Distribution` = mean(as.numeric(as.character(`Pinpoint Distribution`))),
                    `Assassin's Presence` = mean(as.numeric(as.character(`Assassin's Presence`))),
                    `Vampiric Presence` = mean(as.numeric(as.character(`Vampiric Presence`))),
                    `Banner of Strength` = mean(as.numeric(as.character(`Banner of Strength`))),
                    `Banner of Discipline` = mean(as.numeric(as.character(`Banner of Discipline`))),
                    `Empower Allies` = mean(as.numeric(as.character(`Empower Allies`))),
                    `Spirit of Frost` = mean(as.numeric(as.character(`Spirit of Frost`))),
                    `Sun Spirit` = mean(as.numeric(as.character(`Sun Spirit`))),
                    `Stone Spirit` = mean(as.numeric(as.character(`Stone Spirit`))),
                    Spotter = mean(as.numeric(as.character(Spotter))),
                    `Glyph of Empowerment` = mean(as.numeric(as.character(`Glyph of Empowerment`))),
                    `Grace of the Land` = mean(as.numeric(as.character(`Grace of the Land`)))
    )
    boonTable <- rbind(df, totals)
    names(boonTable) <- c('Group', 
                          'Player Name', 
                          'Might', 
                          'Fury',
                          'Quick',
                          'Prot',
                          'Alac',
                          'PinDis',
                          'AssPre',
                          'VamPre',
                          'BanStr',
                          'BanDis',
                          'EmpAll',
                          'FroSpir',
                          'SunSpir',
                          'StoSpir',
                          'Spotter',
                          'GlyEmp',
                          'GracLand')
    boonTable
  }, options = list(
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    scrollX = TRUE,
    columnDefs = list(list(className = 'dt-body-center', targets = "_all"))
  ))
  
  #DPS highscore bar graph
  output$highscores <- renderPlotly({
    selectedBoss <- switch(input$bossName,
                           "Vale Guardian" = 'vg',
                           "Gorseval" = 'gorse',
                           "Sabetha" = "sab",
                           "Slothasor" = "sloth",
                           "Matthias Gabrel" = 'matt',
                           "Keep Construct" = 'kc',
                           "Xera" = 'xera',
                           "Cairn the Indomitable" = 'cairn',
                           "Mursaat Overseer" = 'mo',
                           "Samarog" = 'sam',
                           "Deimos" = 'dei') 
    
    subDataset <- dataset[which(dataset$Boss == selectedBoss), ]
    subDataset <- subDataset[order(subDataset$TotalDPS), ]
    subDataset$plotColumn <- factor(subDataset$plotColumn, levels = unique(subDataset$plotColumn)[order(subDataset$TotalDPS, decreasing = FALSE)])
    
    plot_ly(tail(subDataset, 30),
            x = ~PowerDPS,
            y = ~plotColumn,
            type = 'bar',
            marker = list(color = ~SpecColour, line = list(color = "rgba(0,0,255,0.25)", width = 2)),
            orientation = 'h',
            hoverinfo = 'none'
    ) %>%
      add_trace(x = ~CondiDPS,
                name = 'Condi',
                showlegend = FALSE,
                marker = list(line = list(color = "rgba(255,0,0,0.25)", width = 2)),
                hoverinfo = 'none'
      ) %>%
      add_annotations(text = paste(tail(subDataset$plotText, 30), "-", tail(subDataset$Date, 30), sep = ""),
                      x = 25,
                      y = ~plotColumn,
                      font = list(family = 'helvetica',
                                  size = 12,
                                  color = 'black'),
                      showarrow = FALSE,
                      xanchor = 'left',
                      hovertext = ~paste("<b>Power DPS: </b>", format(PowerDPS, big.mark = ','), " <b>Condi DPS: </b>", format(CondiDPS, big.mark = ','), "<br><b>Total DPS: </b>", format(TotalDPS, big.mark = ','), " - <i>", round(TotalDPS/TotalDPS[30]*100), "%</i> <b>Kill time: </b>", KillTime, "s", sep = "")
      ) %>%
      layout(
        yaxis = list(
          visible = c(FALSE),
          fixedrange = TRUE
        ),
        xaxis = list(
          title = 'DPS',
          fixedrange = TRUE
        ),
        plot_bgcolor = 'rgba(222,222,222,1)',
        paper_bgcolor = 'rgba(222,222,222,1)',
        margin = list(
          t = 20,
          l = 20
        ),
        barmode = 'stack',
        hovermode = 'closest') %>%
      config(displayModeBar = FALSE)
  })
  
  #Average DPS bar graph
  output$averages <- renderPlotly({
    #Subset Average DPS data
    selectedBoss <- switch(input$bossName,
                           "Vale Guardian" = 'vg',
                           "Gorseval" = 'gorse',
                           "Sabetha" = "sab",
                           "Slothasor" = "sloth",
                           "Matthias Gabrel" = 'matt',
                           "Keep Construct" = 'kc',
                           "Xera" = 'xera',
                           "Cairn the Indomitable" = 'cairn',
                           "Mursaat Overseer" = 'mo',
                           "Samarog" = 'sam',
                           "Deimos" = 'dei') 
    
    subDataset <- dataset[which(dataset$Boss == selectedBoss), ]
    subDataset <- subDataset[order(subDataset$TotalDPS), ]
    subDataset$plotColumn <- factor(subDataset$plotColumn, levels = unique(subDataset$plotColumn)[order(subDataset$TotalDPS, decreasing = FALSE)])
    
    AvgData <- ddply(subDataset, .(PlayerName, Specialization, BuildType, Weapons), summarize, AvgTotalDPS = mean(TotalDPS), AvgKillTime = mean(KillTime), DataPoints = length(TotalDPS), SE = standardError(TotalDPS), AvgPowerDPS = mean(PowerDPS), AvgCondiDPS = mean(CondiDPS))
    AvgData$SE[which(is.na(AvgData$SE))] = 0
    plotColumn <- paste(AvgData$PlayerName, "-", AvgData$Specialization, "-", AvgData$BuildType, "-", AvgData$Weapons, " (", AvgData$DataPoints, ")", sep = "")
    AvgData <- cbind(AvgData, plotColumn)
    AvgData <- AvgData[order(AvgData$AvgTotalDPS), ]
    AvgData$plotColumn <- factor(AvgData$plotColumn, levels = unique(AvgData$plotColumn)[order(AvgData$AvgTotalDPS, decreasing = FALSE)])
    SpecColour <- ddply(AvgData, .(plotColumn), summarize, SpecColour = SpecColours(Specialization))
    SpecColour <- SpecColour$SpecColour
    AvgData <- cbind(AvgData, SpecColour)
    
    #Plot average DPS
    AvgPlot <- plot_ly(tail(AvgData, 30),
                       x = ~AvgPowerDPS,
                       y = ~plotColumn,
                       name = 'Power',
                       type = 'bar',
                       orientation = 'h',
                       marker = list(color = ~SpecColour, line = list(color = "rgba(0,0,255,0.25)", width = 2)),
                       hoverinfo = 'none'
    ) %>%
      add_trace(
        x = ~AvgCondiDPS,
        name = 'Condi',
        error_x = list(type = "data", array = (tail(AvgData$SE, 30)), color = 'white'),
        marker = list(line = list(color = "rgba(255,0,0,0.25)", width = 2)),
        showlegend = FALSE,
        hoverinfo = 'none'
      ) %>%
      add_annotations(text = paste(tail(AvgData$plotColumn, 30), sep = ""),
                      x = 25,
                      y = ~plotColumn,
                      font = list(family = 'helvetica',
                                  size = 12,
                                  color = 'black'),
                      showarrow = FALSE,
                      xanchor = 'left',
                      align = 'left',
                      hovertext = ~paste('<b>Power DPS: </b>', format(round(AvgPowerDPS), big.mark = ','), " <b>Condi DPS: </b>", format(round(AvgCondiDPS), big.mark = ','), '<br><b>Total Avg DPS: </b>', format(round(AvgTotalDPS), big.mark = ','), " - <i>", round(AvgTotalDPS/max(AvgData$AvgTotalDPS)*100), "%</i>", sep = "")
      ) %>%
      layout(
        yaxis = list(
          visible = c(FALSE),
          fixedrange = TRUE
        ),
        xaxis = list(
          title = 'DPS',
          fixedrange = TRUE
        ),
        plot_bgcolor = 'rgba(222,222,222,1)',
        paper_bgcolor = 'rgba(222,222,222,1)',
        margin = list(
          t = 20,
          l = 20
        ),
        barmode = 'stack'
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #Weekly composition bar graph
  output$weeklyComp <- renderPlotly({
    selectedBoss <- switch(input$bossName,
                           "Vale Guardian" = 'vg',
                           "Gorseval" = 'gorse',
                           "Sabetha" = "sab",
                           "Slothasor" = "sloth",
                           "Matthias Gabrel" = 'matt',
                           "Keep Construct" = 'kc',
                           "Xera" = 'xera',
                           "Cairn the Indomitable" = 'cairn',
                           "Mursaat Overseer" = 'mo',
                           "Samarog" = 'sam',
                           "Deimos" = 'dei') 
    
    subDataset <- dataset[which(dataset$Boss == selectedBoss), ]
    subDataset <- subDataset[order(subDataset$TotalDPS), ]
    subDataset$plotColumn <- factor(subDataset$plotColumn, levels = unique(subDataset$plotColumn)[order(subDataset$TotalDPS, decreasing = FALSE)])
    
    for (row in 1:nrow(subDataset)) {
      rowDate <- subDataset$Date[row]
      weekTotalDPS <- sum(subDataset$TotalDPS[which(subDataset$Date == rowDate)])
      subDataset$Proportion[row] <- subDataset$TotalDPS[row] / weekTotalDPS 
    }
    #Create new dataframe by date and use players as columns
    weeklyKillTimeData <- ddply(subDataset, .(Date), summarize, Player1 = Proportion[1]*KillTime[1], 
                                Player2 = Proportion[2]*KillTime[1],
                                Player3 = Proportion[3]*KillTime[1],
                                Player4 = Proportion[4]*KillTime[1],
                                Player5 = Proportion[5]*KillTime[1],
                                Player6 = Proportion[6]*KillTime[1],
                                Player7 = Proportion[7]*KillTime[1],
                                Player8 = Proportion[8]*KillTime[1],
                                Player9 = Proportion[9]*KillTime[1],
                                Player10 = Proportion[10]*KillTime[1],
                                P1PlotText = plotText[1],
                                P2PlotText = plotText[2],
                                P3PlotText = plotText[3],
                                P4PlotText = plotText[4],
                                P5PlotText = plotText[5],
                                P6PlotText = plotText[6],
                                P7PlotText = plotText[7],
                                P8PlotText = plotText[8],
                                P9PlotText = plotText[9],
                                P10PlotText = plotText[10],
                                P1SpecColor = SpecColour[1],
                                P2SpecColor = SpecColour[2],
                                P3SpecColor = SpecColour[3],
                                P4SpecColor = SpecColour[4],
                                P5SpecColor = SpecColour[5],
                                P6SpecColor = SpecColour[6],
                                P7SpecColor = SpecColour[7],
                                P8SpecColor = SpecColour[8],
                                P9SpecColor = SpecColour[9],
                                P10SpecColor = SpecColour[10],
                                KillTime = KillTime[1],
                                PowerDPS = sum(PowerDPS),
                                CondiDPS = sum(CondiDPS),
                                TotalDPS = sum(TotalDPS),
                                TeamCount = sum(Core))
    weeklyKillTimeData$Date <- factor(weeklyKillTimeData$Date, levels = unique(weeklyKillTimeData$Date)[order(weeklyKillTimeData$KillTime, decreasing = TRUE)])
    
    WeeksPlot <- plot_ly(weeklyKillTimeData,
                         x = ~Player1,
                         y = ~Date,
                         type = 'bar',
                         orientation = 'h',
                         marker = list(color = ~P1SpecColor, line = list(color = "white", width = 2)),
                         hovertext = ~paste("<b>", P1PlotText, "</b> <i>", round(Player1/KillTime*100), "%</i>", sep = ""),
                         hoverinfo = "text"
    ) %>%
      add_trace(x = ~Player2,
                marker = list(color = ~P2SpecColor, line = list(color = "white", width = 2)),
                showlegend = FALSE,
                hovertext = ~paste("<b>", P2PlotText, "</b> <i>", round(Player2/KillTime*100), "%</i>", sep = "")
      ) %>%
      add_trace(x = ~Player3,
                marker = list(color = ~P3SpecColor, line = list(color = "white", width = 2)),
                showlegend = FALSE,
                hovertext = ~paste("<b>", P3PlotText, "</b> <i>", round(Player3/KillTime*100), "%</i>", sep = "")
      ) %>%
      add_trace(x = ~Player4,
                marker = list(color = ~P4SpecColor, line = list(color = "white", width = 2)),
                showlegend = FALSE,
                hovertext = ~paste("<b>", P4PlotText, "</b> <i>", round(Player4/KillTime*100), "%</i>", sep = "")
      ) %>%
      add_trace(x = ~Player5,
                marker = list(color = ~P5SpecColor, line = list(color = "white", width = 2)),
                showlegend = FALSE,
                hovertext = ~paste("<b>", P5PlotText, "</b> <i>", round(Player5/KillTime*100), "%</i>", sep = "")
      ) %>%
      add_trace(x = ~Player6,
                marker = list(color = ~P6SpecColor, line = list(color = "white", width = 2)),
                showlegend = FALSE,
                hovertext = ~paste("<b>", P6PlotText, "</b> <i>", round(Player6/KillTime*100), "%</i>", sep = "")
      ) %>%
      add_trace(x = ~Player7,
                marker = list(color = ~P7SpecColor, line = list(color = "white", width = 2)),
                showlegend = FALSE,
                hovertext = ~paste("<b>", P7PlotText, "</b> <i>", round(Player7/KillTime*100), "%</i>", sep = "")
      ) %>%
      add_trace(x = ~Player8,
                marker = list(color = ~P8SpecColor, line = list(color = "white", width = 2)),
                showlegend = FALSE,
                hovertext = ~paste("<b>", P8PlotText, "</b> <i>", round(Player8/KillTime*100), "%</i>", sep = "")
      ) %>%
      add_trace(x = ~Player9,
                marker = list(color = ~P9SpecColor, line = list(color = "white", width = 2)),
                showlegend = FALSE,
                hovertext = ~paste("<b>", P9PlotText, "</b> <i>", round(Player9/KillTime*100), "%</i>", sep = "")
      ) %>%
      add_trace(x = ~Player10,
                marker = list(color = ~P10SpecColor, line = list(color = "white", width = 2)),
                showlegend = FALSE,
                hovertext = ~paste("<b>", P10PlotText, "</b> <i>", round(Player10/KillTime*100), "%</i>", sep = "")
      ) %>%
      add_annotations(text = ~paste("Power DPS: ", format(PowerDPS, big.mark = ','), " Condi DPS: ", format(CondiDPS, big.mark = ','), " <b>Total DPS: </b>", format(TotalDPS, big.mark = ','), "<br><b> Team Count:</b> ", TeamCount, " <b>(", KillTime, "s)</b>", sep = ""),
                      x = ~KillTime,
                      y = ~Date,
                      font = list(family = 'helvetica',
                                  size = 14,
                                  color = 'black'),
                      align = 'right',
                      showarrow = FALSE,
                      xanchor = 'right') %>%
      layout(
        yaxis = list(title = "", 
                     fixedrange = TRUE),
        xaxis = list(title = 'Kill Time (s)', 
                     fixedrange = TRUE),
        plot_bgcolor = 'rgba(222,222,222,1)',
        paper_bgcolor = 'rgba(222,222,222,1)',
        margin = list(t = 20, l = 75),
        barmode = 'stack',
        hovermode = 'closest') %>%
      config(displayModeBar = FALSE)
  })
})