#Load packages
library("rvest")
library('httr')
library('jsonlite')
library('RMySQL')

htmlParser <- function(parsed, team = 'NoTeam') {
  html <- read_html(parsed$permalink) %>% html_nodes('body')
  
  # Encounter data frame with some blanks
  encounterData <- data.frame(
    fight_id = parsed$id,
    team = team,
    boss = parsed$encounter$boss,
    success = parsed$encounter$success,
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
      crit_percent = damage_table$Var.4[damage_table$Name == player],
      above90_percent = damage_table$Var.5[damage_table$Name == player],
      downed_count = dps_table$Var.11[dps_table$Name == player],
      dead_at = dps_table$Var.12[dps_table$Name == player],
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
  
  offensiveTable <- html_node(html, xpath = '//*[@id="offensive_table0"]') %>% 
    html_table() %>% .[[1]] %>% .[3:length(.)]
  names(offensiveTable) <- html_node(html, 
                                     xpath = '//*[@id="offensive_table0"]') %>% 
    html_nodes('th img') %>% html_attr('alt') %>% c('Name', .)
  
  defensiveTable <- html_node(html, xpath = '//*[@id="defensive_table0"]') %>% 
    html_table() %>% .[[1]] %>% .[3:length(.)]
  names(defensiveTable) <- html_node(html, 
                                     xpath = '//*[@id="defensive_table0"]') %>% 
    html_nodes('th img') %>% html_attr('alt') %>% c('Name', .)
  
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

# Tester blocker
# setwd('~/gw2_raid_report/upload')
# parsed <- content(POST(url = 'https://dps.report/uploadContent',
#                        body = list(json = 1,
#                                    generator = 'ei',
#                                    userToken = 'kltu2he26nvdrk0451atc1s2p2',
#                                    file = upload_file('./20180713-222530.evtc.zip')
#                        )))
# results <- htmlParser(parsed)