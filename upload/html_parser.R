library(rvest)
library(httr)

# Time to seconds functions
timeToSeconds <- function(time) {
  time <- as.character(time)
  
  if (time == '0') {
    return(0)
  }
  
  timeString <- strsplit(time, split = '(?<=[a-z])', perl = TRUE)[[1]]
  seconds <- 0
  for (string in timeString) {
    if (grepl('m', string)) {
      temp <- as.numeric(substring(string, 1, nchar(string) - 2)) * 60
      seconds <- seconds + temp
    } else {
      seconds <- seconds + 
        as.numeric(substring(string, 1, nchar(string) - 2))
    }
  }
  return(seconds)
}

htmlParser <- function(parsed, team_info, team_code) {
  # Get just the relevant team
  team <- team_info[team_info$team_code == team_code,]
  
  # Read HTML link
  html <- read_html(parsed$permalink) %>% html_nodes('body')
  
  # Check fight duration
  duration <- html_node(html, 
                        xpath = '/html/body/div/div[1]/div[1]/div/div/blockquote/div/div[2]/p[3]') %>% 
    html_text %>% strsplit(' ')
  minutes <- substr(duration[[1]][2], 1, nchar(duration[[1]][2]) - 1) %>%
    as.numeric() * 60
  duration <- substr(duration[[1]][3], 1, nchar(duration[[1]][3]) - 1) %>%
    as.numeric() + minutes
  
  # Return too short if duration is less than 60 seconds
  if (duration < 60) {
    return('Too short!')
  }
  
  # Get team players
  teamPlayers <- team[4:13]
  
  # Get percent damage done by boss
  bossOutputString <- html_nodes(html, xpath = '//*[@id="bossSummary0"]') %>% 
    html_nodes('div') %>% .[length(.)] %>% html_text()
  bossOutputString <- strsplit(bossOutputString, '%')[[1]][1] %>% 
    strsplit(' ') %>% .[[1]]
  bossOutputString <- bossOutputString[length(bossOutputString)] %>% 
    as.numeric() / 100
  
  # Turn players into a vector of account names and find missing players
  present_players <- sapply(parsed$players, function(x) x$display_name)
  missing_players <- paste(teamPlayers[!teamPlayers %in% present_players], 
                           collapse = ',')
  
  # Encounter data frame with some blanks
  encounterData <- data.frame(
    fight_id = parsed$id,
    log_link = parsed$permalink,
    team = team$team_name,
    boss = parsed$encounter$boss,
    success = as.numeric(parsed$encounter$success),
    date = parsed$encounterTime, 
    duration = duration, 
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
    player10 = NA,
    missing_players = missing_players,
    boss_damage = bossOutputString
  )
  
  # Add players
  counter <- 0
  for (player in parsed$players) {
    counter <- counter + 1
    playerAssigner <- paste('encounterData$player', counter, 
                            ' <- player$character_name', sep = '')
    eval(parse(text = playerAssigner))
  }
  
  # Get boss condi/boon table
  bossStatus <- html_nodes(html, xpath = '//*[@id="condi_table0"]') %>% 
    html_table() %>% .[[1]]
  
  # Get condi/boon names
  statusNames <- html_nodes(html, xpath = '//*[@id="condi_table0"]') %>% 
    html_nodes('img') %>% html_attr('alt')
  
  # Add status names
  names(bossStatus) <- c('Name', tolower(statusNames))
  
  # Change percentages into floats
  bossStatus[,2:length(bossStatus)] <- lapply(bossStatus[,2:length(bossStatus)], 
                                              function(x) if (grepl('%', x)) 
                                              {as.numeric(gsub('%', '', x)) / 
                                                  100} else {x})
  
  # Bind statuses to encounter data
  encounterData <- cbind(encounterData, bossStatus[,2:length(bossStatus)])
  
  # Check if there is a separate boon table
  bossBoons <- html_nodes(html, xpath = '//*[@id="boss_boon_table0"]')
  if (length(bossBoons) == 1) {
    # Get names
    boonNames <- bossBoons %>% html_nodes('img') %>% html_attr('alt')
    # Get dataframe
    bossBoons <- bossBoons %>% html_table() %>% .[[1]]
    
    # Rename columns
    names(bossBoons) <- c('Name', tolower(boonNames))
    
    # Change percentages into floats
    bossBoons[,2:length(bossBoons)] <- lapply(bossBoons[,2:length(bossBoons)], 
                                              function(x) if (grepl('%', x)) 
                                              {as.numeric(gsub('%', '', x)) / 
                                                  100} else {x})
    
    # Bind boons to encounter data
    encounterData <- cbind(encounterData, bossBoons[,2:length(bossBoons)])
  }
  
  
  # DPS data table
  dps_table <- html_nodes(html, xpath = '//*[@id="dps_table0"]') %>% 
    html_table() %>% data.frame()
  
  # Add total dps to encounter data and calcualte total damage
  encounterData$team_dps <- dps_table$Boss.DPS[nrow(dps_table)]
  encounterData$damage_done <- encounterData$duration * encounterData$team_dps
  
  # Check if no damage was done to boss 
  # NOTE: Temporary measure
  if (encounterData$damage_done < 15000) {
    return('Not enough damage!')
  }
  
  # DPS_statistic table
  damage_table <- html_nodes(html, xpath = '//*[@id="dmgstatsBoss_table0"]') %>%
    html_table() %>% data.frame()
  
  # Get composition table
  composition <- html_nodes(html, 'div table')[1] %>% html_children()
  
  # Get incoming damage table
  incoming_table <- html_node(html, xpath = '//*[@id="defstats_table0"]') %>% 
    html_table() %>% .[[1]]
  
  # Get indexed subgroups
  indexedSubs <- unique(dps_table$Sub) %>% .[!is.na(.)] %>% sort()
  
  # Get classes
  professions <- unlist(read_json('https://api.guildwars2.com/v2/professions'))
  
  # Create empty dataframe
  playerData <- data.frame()
  
  # Get data for each player
  playerNames <- names(parsed$players)
  for (player in playerNames) {
    playerInfo <- eval(parse(text = paste('parsed$player$"', player, '"', 
                                          sep = '')))
    
    # Get elite spec but with main class backup
    specialization <- professions[playerInfo$profession][[1]]
    try({specialization <- read_json(paste('https://api.guildwars2.com/v2/specializations', 
                                           playerInfo$elite_spec, 
                                           sep = '/'))$name}, silent = T)
    
    # Calculate dead at in seconds
    dead_at <- timeToSeconds(dps_table$Var.12[dps_table$Name == player])
    # If run was a failure and dead_at is 0, change it to end of run
    if (dead_at == 0 & encounterData$success == 0) {
      dead_at <- encounterData$duration
    }
    
    playerTemp <- data.frame(
      fight_id = parsed$id,
      player_name = playerInfo$display_name,
      char_name = player,
      specialization = specialization,
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
      dead_at = dead_at,
      subgroup = dps_table$Sub[dps_table$Name == player],
      wep1 = NA,
      wep2 = NA,
      wep3 = NA,
      wep4 = NA, 
      gear_stats = NA,
      dmg_taken = incoming_table$'Dmg Taken'[incoming_table$Name == player]
    )
    
    # Determine which tile to look at for build info
    buildBlocks <- composition[match(playerTemp$subgroup, indexedSubs)] %>% 
      html_children()
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
        weaponStringer <- paste('playerTemp$wep', weapon_counter, ' <- "', 
                                string, '"', sep = '')
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
  
  # Check if boon table is populated to give games
  if (!length(boonTable) == 1) {
    names(boonTable) <- html_node(html, xpath = '//*[@id="boons_table0"]') %>% 
      html_nodes('th img') %>% html_attr('alt') %>% c('Name', .)
    boonTable <- boonTable[1:parsed$encounter$numberOfPlayers,]
  }
  
  offensiveTable <- html_node(html, xpath = '//*[@id="offensive_table0"]') %>% 
    html_table() %>% .[[1]] %>% .[3:length(.)]
  
  # Check if offensive table is populated to give games
  if (!length(offensiveTable) == 1) {
    names(offensiveTable) <- html_node(html, 
                                       xpath = '//*[@id="offensive_table0"]') %>% 
      html_nodes('th img') %>% html_attr('alt') %>% c('Name', .)
    offensiveTable <- offensiveTable[1:parsed$encounter$numberOfPlayers,]
  }
  
  defensiveTable <- html_node(html, xpath = '//*[@id="defensive_table0"]') %>% 
    html_table() %>% .[[1]] %>% .[3:length(.)]
  
  # Check if defensive table is populated to give games
  if (!length(defensiveTable) == 1) {
    names(defensiveTable) <- html_node(html, 
                                       xpath = '//*[@id="defensive_table0"]') %>% 
      html_nodes('th img') %>% html_attr('alt') %>% c('Name', .)
    defensiveTable <- defensiveTable[1:parsed$encounter$numberOfPlayers,] 
  }
  
  # Bind all tables together
  buffTable <- merge(boonTable, offensiveTable, by = 'Name')
  buffTable <- merge(buffTable, defensiveTable, by = 'Name')
  
  # Rename columns for sql
  names(buffTable)[1] <- 'char_name'
  names(buffTable) <- gsub(' ', '_', tolower(names(buffTable)))
  names(buffTable) <- gsub("'", '', names(buffTable))
  
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
  
  # Find boss attack table index
  if (length(bossBoons) == 0) {
    attackIndex <- 2
  } else {
    attackIndex <- 3
  }
  
  # Get boss damage
  bossAttacks <- html_node(html, xpath = '//*[@id="bossSummary0"]') %>% 
    html_nodes('table') %>% html_table() %>% .[[attackIndex]]
  bossAttacks <- bossAttacks[!is.na(bossAttacks$Damage) & 
                               !bossAttacks$Skill == 'Total',]
  
  # Remove unwanted columns
  bossAttacks <- bossAttacks[c('Skill', 'Damage', 'Hits per Cast')]
  
  # Rename columns
  names(bossAttacks) <- c('skill_name', 'damage_dealt', 'hits_percast')
  
  # Add fight_id
  bossAttacks$fight_id <- parsed$id
  
  return(list(encounterData = encounterData,
              playerData = playerData,
              buffData = buffTable,
              bossAttacks = bossAttacks))
}

# Test block
# if (interactive()) {
#   setwd('~/gw2_raid_report/upload')
# parsed <- content(POST(url = 'https://dps.report/uploadContent',
#                        body = list(json = 1,
#                                    generator = 'ei',
#                                    userToken = 'kltu2he26nvdrk0451atc1s2p2',
#                                    file = upload_file('./20180417-195626.evtc')
#                        )))
#   results <- htmlParser(parsed, 'Potatos')
# }