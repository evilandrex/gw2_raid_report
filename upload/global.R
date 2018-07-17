#Load packages
library("rvest")
library('httr')
library('jsonlite')
library('RMySQL')

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

htmlParser <- function(parsed, team = 'NoTeam') {
  html <- read_html(parsed$permalink) %>% html_nodes('body')
  
  # Encounter data frame with some blanks
  encounterData <- data.frame(
    fight_id = parsed$id,
    team = team,
    boss = parsed$encounter$boss,
    success = as.numeric(parsed$encounter$success),
    date = parsed$encounterTime, 
    duration = NA, 
    team_dps = NA, 
    damage_done = NA,
    player1 = try(parsed$players[[1]]$character_name, silent = TRUE),
    player2 = try(parsed$players[[2]]$character_name, silent = TRUE),
    player3 = try(parsed$players[[3]]$character_name, silent = TRUE),
    player4 = try(parsed$players[[4]]$character_name, silent = TRUE),
    player5 = try(parsed$players[[5]]$character_name, silent = TRUE),
    player6 = try(parsed$players[[6]]$character_name, silent = TRUE),
    player7 = try(parsed$players[[7]]$character_name, silent = TRUE),
    player8 = try(parsed$players[[8]]$character_name, silent = TRUE),
    player9 = try(parsed$players[[9]]$character_name, silent = TRUE),
    player10 = try(parsed$players[[10]]$character_name, silent = TRUE)
  )

  # Add fight duration
  duration <- html_node(html, 
                        xpath = '/html/body/div/div[1]/div[1]/div/div/blockquote/div/div[2]/p[3]') %>% 
    html_text %>% strsplit(' ')
  minutes <- substr(duration[[1]][2], 1, nchar(duration[[1]][2]) - 1) %>%
    as.numeric() * 60
  duration <- substr(duration[[1]][3], 1, nchar(duration[[1]][3]) - 1) %>%
    as.numeric() + minutes
  encounterData$duration <- duration
  
  # DPS data table
  dps_table <- html_nodes(html, xpath = '//*[@id="dps_table0"]') %>% 
    html_table() %>% data.frame()
  
  # Add total dps to encounter data and calcualte total damage
  encounterData$team_dps <- dps_table$Boss.DPS[dps_table$Name == 'Total']
  encounterData$damage_done <- encounterData$duration * encounterData$team_dps
  
  # DPS_statistic table
  damage_table <- html_nodes(html, xpath = '//*[@id="dmgstatsBoss_table0"]') %>%
    html_table() %>% data.frame()
  
  # Get composition table
  composition <- html_nodes(html, 'div table')[1] %>% html_children()
  
  # Create empty dataframe
  playerData <- data.frame()
  
  # Get data for each player
  playerNames <- names(parsed$players)
  for (player in playerNames) {
    playerInfo <- eval(parse(text = paste('parsed$player$"', player, '"', 
                                          sep = '')))
    playerTemp <- data.frame(
      fight_id = parsed$id,
      player_name = playerInfo$display_name,
      char_name = player,
      #NOTE: Need to figure out what happens to core professions
      specialization = read_json(
        paste('https://api.guildwars2.com/v2/specializations', 
              playerInfo$elite_spec, 
              sep = '/'))$name, 
      total_dps = dps_table$Boss.DPS[dps_table$Name == player],
      power_dps = dps_table$Power[dps_table$Name == player],
      condi_dps = dps_table$Condi[dps_table$Name == player],
      crit_percent = gsub('%', '', 
                          damage_table$Var.4[damage_table$Name == player]) %>%
        as.numeric()/100,
      above90_percent = gsub('%', '', 
                             damage_table$Var.5[damage_table$Name == player]) %>%
        as.numeric()/100,
      downed_count = dps_table$Var.11[dps_table$Name == player],
      dead_at = timeToSeconds(dps_table$Var.12[dps_table$Name == player]),
      subgroup = dps_table$Sub[dps_table$Name == player],
      wep1 = NA,
      wep2 = NA,
      wep3 = NA,
      wep4 = NA, 
      gear_stats = NA
    )
    
    # Determine which tile to look at for build info
    buildBlocks <- composition[playerTemp$subgroup] %>% html_children()
    buildStrings <- buildBlocks[which(grepl(substr(player, 1, 10), 
                                            html_text(buildBlocks)))] %>%
      html_nodes('img') %>% html_attr('alt') %>% .[2:length(.)]
    
    # Make a vector of all weapons to check against
    weapons <- c('Axe', 'Dagger', 'Mace', 'Pistol', 'Sword', 'Scepter', 'Focus',
                 'Shield', 'Torch', 'Warhorn', 'Greatsword', 'Hammer', 
                 'Longbow', 'Rifle', 'Short Bow', 'Staff')
    
    # Loop through the strings
    gear_stats = ''
    weapon_counter <- 0
    for (string in buildStrings) {
      if (string %in% weapons) { # It is a weapon
        # Increment counter
        weapon_counter <- weapon_counter + 1
        
        # Apply string to the correct weapon slot
        weaponStringer <- paste('playerTemp$wep', 
                                weapon_counter, 
                                ' <- "', 
                                string, 
                                '"', sep = '')
        eval(parse(text = weaponStringer))
      } else { # It is a gear stat
        gear_stats <- paste(gear_stats, string, sep = ',')
      }
    }
    
    # Apply gear stat to dataframe
    if (gear_stats == '') {
      playerTemp$gear_stats <- 'Power'
    } else {
      playerTemp$gear_stats <- substring(gear_stats, 2, nchar(gear_stats))
    }
    
    # Append temporary to full player frame
    playerData <- rbind(playerData, playerTemp)
  }
  
  # Generate buff tables
  boonTable <- html_node(html, xpath = '//*[@id="boons_table0"]') %>% 
    html_table() %>% .[[1]] %>% .[3:length(.)]
  names(boonTable) <- html_node(html, xpath = '//*[@id="boons_table0"]') %>% 
    html_nodes('th img') %>% html_attr('alt') %>% c('Name', .)
  boonTable <- boonTable[1:parsed$encounter$numberOfPlayers,]
  
  offensiveTable <- html_node(html, xpath = '//*[@id="offensive_table0"]') %>% 
    html_table() %>% .[[1]] %>% .[3:length(.)]
  names(offensiveTable) <- html_node(html, 
                                     xpath = '//*[@id="offensive_table0"]') %>% 
    html_nodes('th img') %>% html_attr('alt') %>% c('Name', .)
  offensiveTable <- offensiveTable[1:parsed$encounter$numberOfPlayers,]
  
  defensiveTable <- html_node(html, xpath = '//*[@id="defensive_table0"]') %>% 
    html_table() %>% .[[1]] %>% .[3:length(.)]
  names(defensiveTable) <- html_node(html, 
                                     xpath = '//*[@id="defensive_table0"]') %>% 
    html_nodes('th img') %>% html_attr('alt') %>% c('Name', .)
  defensiveTable <- defensiveTable[1:parsed$encounter$numberOfPlayers,]
  
  # Bind all tables together
  buffTable <- merge(boonTable, offensiveTable, by = 'Name')
  buffTable <- merge(buffTable, defensiveTable, by = 'Name')
  
  # Rename columns for sql
  names(buffTable)[1] <- 'char_name'
  names(buffTable) <- gsub(' ', '_', tolower(names(buffTable)))
  
  # Get index of percentage columns
  percentageIndex <- which(sapply(buffTable, typeof) == 'character')
  
  # Remove name column
  percentageIndex <- percentageIndex[2:length(percentageIndex)]
  
  # Convert all percentage columns into numerics
  buffTable[,percentageIndex] <- lapply(buffTable[,percentageIndex], 
                                        function(x) 
                                          as.numeric(gsub('%', '', x))/100)
  # Add fight_id to table
  buffTable$fight_id <- parsed$id
  
  return(list(encounterData = encounterData,
              playerData = playerData,
              buffData = buffTable))
}

sendData <- function(parsedResults) {
  # Piece out dataframes from parsedResults
  enc <- parsedResults$encounterData
  play <- parsedResults$playerData
  buff <- parsedResults$buffData
  
  # Connect to database
  conn <- dbConnect(
    drv = MySQL(),
    dbname = "raid_report",
    host = '127.0.0.1',
    port = 3306,
    username = "root",
    password = "CHANGEME")
  
  # Forces exit when database finishes
  on.exit(dbDisconnect(conn), add = TRUE)
  
  # Create query string with empty spots
  query <- paste(
    'INSERT INTO encounter_data SET ',
    'fight_id = ?fight_id, ',
    'team = ?team, ',
    'boss = ?boss, ',
    'success = ?success, ',
    'date = ?date, ',
    'duration = ?duration, ',
    'team_dps = ?team_dps, ',
    'damage_done = ?damage_done, ',
    'player1 = ?player1, ',
    'player2 = ?player2, ',
    'player3 = ?player3, ',
    'player4 = ?player4, ',
    'player5 = ?player5, ',
    'player6 = ?player6, ',
    'player7 = ?player7, ',
    'player8 = ?player8, ',
    'player9 = ?player9, ',
    'player10 = ?player10',
    sep = '')
  query <- sqlInterpolate(conn, query, 
                          fight_id = as.character(enc$fight_id),
                          team = as.character(enc$team),
                          boss = as.character(enc$boss),
                          success = enc$success,
                          date = enc$date,
                          duration = enc$duration,
                          team_dps = enc$team_dps,
                          damage_done = enc$damage_done,
                          player1 = as.character(enc$player1),
                          player2 = as.character(enc$player2),
                          player3 = as.character(enc$player3),
                          player4 = as.character(enc$player4),
                          player5 = as.character(enc$player5),
                          player6 = as.character(enc$player6),
                          player7 = as.character(enc$player7),
                          player8 = as.character(enc$player8),
                          player9 = as.character(enc$player9),
                          player10 = as.character(enc$player10)
                          )
  dbGetQuery(conn, query)
  
  playQuery <- paste(
    'INSERT INTO player_data SET ',
    'fight_id = ?fight_id, ',
    'player_name = ?player_name, ',
    'char_name = ?char_name, ',
    'specialization = ?specialization, ',
    'total_dps = ?total_dps, ',
    'power_dps = ?power_dps, ',
    'condi_dps = ?condi_dps, ',
    'crit_percent = ?crit_percent, ',
    'above90_percent = ?above90_percent, ',
    'downed_count = ?downed_count, ',
    'dead_at = ?dead_at, ',
    'subgroup = ?subgroup, ',
    'wep1 = ?wep1, ',
    'wep2 = ?wep2, ',
    'wep3 = ?wep3, ',
    'wep4 = ?wep4, ',
    'gear_stats = ?gear_stats',
    sep = '')
  
  for (player in 1:nrow(play)) {
    query <- sqlInterpolate(conn, playQuery,
                            fight_id = as.character(play$fight_id[player]),
                            player_name = as.character(play$player_name[player]),
                            char_name = as.character(play$char_name[player]),
                            specialization = as.character(play$specialization[player]),
                            total_dps = play$total_dps[player],
                            power_dps = play$power_dps[player],
                            condi_dps = play$condi_dps[player],
                            crit_percent = play$crit_percent[player],
                            above90_percent = play$above90_percent[player],
                            downed_count = play$downed_count[player],
                            dead_at = play$dead_at[player],
                            subgroup = play$subgroup[player],
                            wep1 = as.character(play$wep1[player]),
                            wep2 = as.character(play$wep2[player]),
                            wep3 = as.character(play$wep3[player]),
                            wep4 = as.character(play$wep4[player]),
                            gear_stats = as.character(play$gear_stats[player]))
    dbGetQuery(conn, query)
  }
  
  # Check if any new columns need to be made
  buffList <- dbGetQuery(conn, 'SHOW COLUMNS FROM buff_data')$Field
  buffToAdd <- names(buff)[!names(buff) %in% buffList]
  
  # Add columns as necessary
  for (newColumn in buffToAdd) {
    query <- 'ALTER TABLE buff_data ADD ?newColumn FLOAT;'
    query <- sqlInterpolate(conn, query, newColumn = as.factor(newColumn))
    dbGetQuery(conn, query)
  }
  
  # Create buff query
  buffQuery <- paste(
    'INSERT INTO buff_data SET ',
    'fight_id = ?fight_id, ',
    'player_name = ?player_name, ',
    'char_name = ?char_name, ',
    'specialization = ?specialization, ',
    'total_dps = ?total_dps, ',
    'power_dps = ?power_dps, ',
    'condi_dps = ?condi_dps, ',
    'crit_percent = ?crit_percent, ',
    'above90_percent = ?above90_percent, ',
    'downed_count = ?downed_count, ',
    'dead_at = ?dead_at, ',
    'subgroup = ?subgroup, ',
    'wep1 = ?wep1, ',
    'wep2 = ?wep2, ',
    'wep3 = ?wep3, ',
    'wep4 = ?wep4, ',
    'gear_stats = ?gear_stats',
    sep = '')
  
  # Get new buffList
  buffList <- dbGetQuery(conn, 'SHOW COLUMNS FROM buff_data')$Field
  
  # Create new query to insert data based on available columns
  buffQuery = 'INSERT INTO buff_data SET '
  buffQuery = paste(buffQuery, 
                    paste(buffList, " = ?", buffList, sep = '', collapse = ', '), 
                    sep = '')
  buffList <- buffList[3:length(buffList)]
  
  # Add buff data per player
  for (player in 1:nrow(play)) {
    # Create strings for query
    baseString <- paste('query <- sqlInterpolate(conn, buffQuery, ',
                        'fight_id = buff$fight_id[player], ',
                        'char_name = buff$char_name[player], ', sep = '')
    buffString <- paste(buffList, ' = buff$', buffList, '[player]', 
                       sep = '', collapse = ', ')
    fullString <- paste(baseString, buffString, ')', sep = '')
    eval(parse(text = fullString))
    
    dbGetQuery(conn, query)
  }
}

# Tester block
# setwd('~/gw2_raid_report/upload')
# parsed <- content(POST(url = 'https://dps.report/uploadContent',
#                        body = list(json = 1,
#                                    generator = 'ei',
#                                    userToken = 'kltu2he26nvdrk0451atc1s2p2',
#                                    file = upload_file('./20180713-222530.evtc.zip')
#                        )))
# results <- htmlParser(parsed)