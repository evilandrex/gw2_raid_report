#Load packages
library("rvest")
library('httr')

colNames <- c('Entry', 
              'Boss', 
              'PlayerName', 
              'Specialization', 
              'BuildType', 
              'TotalDPS', 
              'PowerDPS', 
              'CondiDPS', 
              'KillTime', 
              'Subgroup', 
              'Date', 
              'Weapons')

buffNames <- c('Aegis',
               'Fury',
               'Might',
               'Protection',
               'Quickness',
               'Regeneration',
               'Resistance',
               'Retaliation',
               'Stability',
               'Swiftness',
               'Vigor',
               'Strength in Numbers',
               'Virtue of Resolve',
               'Signet of Resolve',
               'Bane Signet',
               'Signet of Judgment',
               'Signet of Wrath',
               'Signet of Mercy',
               'Signet of Courage',
               'Wrath of Justice',
               "Assassin's Presence",
               'Naturalistic Resonance',
               'Rite of the Great Dwarf',
               'Banner of Strength',
               'Banner of Tactics',
               'Banner of Discipline',
               'Banner of Defense',
               'Empower Allies',
               'Pinpoint Distribution',
               'Water Spirit',
               'Spirit of Frost',
               'Stone Spirit',
               'Storm Spirit',
               'Sun Spirit',
               'Spirit of Nature',
               'Spotter',
               'Grace of the Land',
               'Glyph of Empowerment',
               'Soothing Mist',
               'Alacrity',
               'Illusion of Defense',
               'Vampiric Presence'
)

#Skil List
Guardian <- list(Greatsword = c('Whirling Wrath', 'Leap of Faith', 'Symbol of Wrath', 'Binding Blade'),
                 Hammer = c('Mighty Blow', "Zealot's Embrace", 'Banish', 'Ring of Warding'),
                 Staff = c('Orb of Light', 'Symbol of Swiftness', 'Empower', 'Line of Warding'),
                 MaceMain = c('Symbol of Faith', "Protector's Strike"),
                 ScepterMain = c('Symbol of Punishment', "Chains of Light"),
                 SwordMain = c('Symbol of Blades', "Zealot's Defense"),
                 FocusOff = c('Ray of Judgment', 'Shield of Wrath'),
                 ShieldOff = c('Shield of Judgment', 'Shield of Absorption'),
                 TorchOff = c("Zealot's Flame", 'Cleansing Flame', "Zealot's Fire"))
Dragonhunter <- list(Longbow = c('True Shot', 'Deflecting Shot', 'Symbol of Energy', "Hunter's Ward"))
Dragonhunter <- c(Guardian, Dragonhunter)
Firebrand <- list(AxeMain = c('Symbol of Vengeance', 'Blazing Edge'))
Firebrand <- c(Guardian, Firebrand)

Warrior <- list(Greatsword = c('Hundred Blades', 'Whirlwind Attack', 'Bladetrail', 'Rush', 'Arcing Slice', 'Arc Divider'),
                Hammer = c('Fierce Blow', 'Hammer Shock', 'Staggering Blow', 'Backbreaker', 'Earthshaker', 'Rupturing Smash'),
                Longbow = c('Fan of Fire', 'Arcing Arrow', 'Smoldering Arrow', 'Pin Down', 'Combustive Shot', 'Scorched Earth'),
                Rifle = c('Aimed Shot', 'Volley', 'Brutal Shot', 'Rifle Butt', 'Kill Shot', 'Gun Flame'),
                AxeMain = c('Cyclone Axe', 'Axe Throw', 'Eviscerate', 'Decapitate'),
                AxeOff = c('Dual Strike', 'Whirling Axe'),
                MaceMain = c('Counterblow', 'Pommel Bash', 'Skull Crack', 'Skull Grinder'),
                MaceOff = c('Crushing Blow', 'Tremor'),
                SwordMain = c('Savage Leap', 'Final Thrust', 'Flurry', 'Flaming Flurry'),
                SwordOff = c('Impale', 'Riposte'),
                ShieldOff = c('Shield Bash', 'Shield Stance'),
                WarhornOff = c('Charge', 'Call to Arms'))
Berserker <- list(TorchOff = c('Blaze Breaker', 'Flames of War'))
Berserker <- c(Warrior, Berserker)
Spellbreaker <- list(DaggerMain = c('Aura Slicer', 'Disrupting Stab'),
                     DaggerOff = c("Wastrel's Ruin", 'Bladestorm'))
Spellbreaker <- c(Warrior, Spellbreaker)

Revenant <- list(Hammer = c('Coalescence of Ruin', 'Phase Smash', 'Field of the Mists', 'Drop the Hammer'), 
                 Staff = c('Punishing Sweep', 'Warding Rift', 'Renewing Wave', 'Surge of the Mists'), 
                 SwordMain = c('Precision Strike', 'Unrelenting Assault'), 
                 SwordOff = c("Duelist's Preparation", 'Grasping Shadow'), 
                 MaceMain = c('Searing Fissure', 'Echoing Eruption'), 
                 AxeOff = c('Frigid Blitz', 'Temporal Rift'))
Herald <- list(ShieldOff = c('Envoy of Exuberance', 'Crystal Hibernation'))
Herald <- c(Revenant, Herald)
Renegade <- list(Shortbow = c('Bloodbane Path', 'Sevenshot', 'Spiritcrush', 'Scorchrazor'))
Renegade <- c(Revenant, Renegade)

Engineer <- list(Rifle = c('Net Shot', 'Blunderbuss', 'Overcharged Shot', 'Jump Shot'),
                 PistolMain = c('Poison Dart', 'Static Shot'),
                 PistolOff = c('Blowtorch', 'Glue Shot'),
                 ShieldOff = c('Magnetic Shield', 'Static Shield'))
Scrapper <- list(Hammer = c('Electro-whirl', 'Rocket Charge', 'Shock Shield', 'Thunderclap'))
Scrapper <- c(Engineer, Scrapper)
Holosmith <- list(SwordMain = c('Refraction Cutter', 'Radiant Arc'))
Holosmith <- c(Engineer, Holosmith)

Ranger <- list(Greatsword = c('Maul (Greatsword)', 'Swoop', 'Counterattack', 'Hilt Bash'),
               Longbow = c('Rapid Fire', "Hunter's Shot", 'Point Blank Shot', 'Barrage'),
               Shortbow = c('Poison Volley', 'Quick Shot', 'Crippling Shot', 'Concussion Shot'),
               SwordMain = c('Hornet Sting', "Serpent's Strike"),
               AxeMain = c('Splitblade', "Winter's Bite"),
               AxeOff = c('Path of Scars', 'Whirling Defense'),
               DaggerOff = c("Stalker's Strike", 'Crippling Talong'),
               TorchOff = c('Throw Torch', 'Bonfire'),
               WarhornOff = c("Hunter's Call", 'Call of the Wild'))
Druid <- list(Staff = c('Astral Wisp',
                        'Ancestral Grace',
                        'Vine Surge',
                        'Sublime Conversion'))
Druid <- c(Ranger, Druid)
Soulbeast <- list(DaggerMain = c('Double Arc', 'Instinctive Engage'))
Soulbeast <- c(Ranger, Soulbeast)

Thief <- list(Shortbow = c('Cluster Bomb', 'Disabling Shot', 'Choking Gas', "Infiltrator's Arrow"),
              SwordMain = c("Infiltrator's Strike", 'Flanking Strike', 'Pistol Whip'),
              DaggerMain = c('Heartseeker', 'Death Blossom', 'Shadow Shot'),
              DaggerOff = c('Dancing Dagger', 'Cloak and Dagger', 'Death Blossom', 'Shadow Strike', 'Flanking Strike'),
              PistolMain = c('Body Shot', 'Pistol Whip', 'Shadow Strike', 'Unload'),
              PistolOff = c('Headshot', 'Black Powder', 'Unload', 'Shadow Shot'))
Daredevil <- list(Staff = c('Weakening Charge', 'Debilitating Arc', 'Dust Strike', 'Vault'))
Daredevil <- c(Thief, Daredevil)
Deadeye <- list(Rifle = c("Skirmisher's Shot", 'Double Tap', "Death's Retreat", 'Kneel'))
Deadeye <- c(Thief, Deadeye)

Elementalist <- list(Staff = c('Lava Font', 'Flame Burst', 'Burning Retreat', 'Meteor Shower', 'Ice Spike', 'Geyser', 'Frozen Ground', 'Healing Rain', 'Gust', 'Static Field', 'Magnetic Aura', 'Shock Wave'),
                     ScepterMain = c("Dragon's Tooth", 'Phoenix', 'Lightning Strike', 'Rock Barrier'),
                     DaggerMain = c("Drake's Breath", 'Burning Speed', 'Shocking Aura'),
                     DaggerOff = c('Frost Aura', 'Ride the Lightning', 'Updraft', 'Earthquake', 'Churning Earth'),
                     FocusOff = c('Flamewall', 'Fire Shield', 'Comet', 'Swirling Winds', 'Gale', 'Magnetic Wave', 'Obsidian Flesh'))
Tempest <- list(WarhornOff = c('Heat Sync', 'Wildfire', 'Tidal Surge', 'Cyclone', 'Lightning Orb', 'Sand Squall', 'Dust Storm'))
Tempest <- c(Elementalist, Tempest)
Weaver <- list(SwordMain = c('Flame Uprising', 'Cauterizing Strike', 'Riptide', 'Aqua Siphon', 'Polaric Leap', 'Quantum Strike', 'Earthen Vortex', 'Rust Frenzy'))
Weaver <- c(Elementalist, Weaver)

Mesmer <- list(Greatsword = c('Mirror Blade', 'Mind Stab', 'Phantasmal Berserker', 'Illusionary Wave'),
               Staff = c('Phase Retreat', 'Phantasmal Warlock', 'Chaos Armor', 'Chaos Storm'),
               ScepterMain = c('Illusionary Counter', 'Confusing Images'),
               SwordMain = c('Blurred Frenzy', 'Illusoinary Leap'),
               SwordOff = c('Illusionary Riposte', 'Phantasmal Swordsman'),
               FocusOff = c('Temporal Curtain', 'Phantasmal Warden'),
               PistolOff = c('Phantasmal Duelist', 'Magic Bullet'),
               TorchOff = c('The Prestige', 'Phantasmal Mage'))
Chronomancer <- list(ShieldOff = c('Echo of Memory', 'Tides of Time'))
Chronomancer <- c(Mesmer, Chronomancer)
Mirage <- list(AxeMain = c('Lingering Thoughts', 'Axes of Symmetry'))
Mirage <- c(Mesmer, Mirage)

Necromancer <- list(Staff = c('Marker of Blood', 'Chillbains', 'Putrid Mark', "Reaper's Mark"),
                    AxeMain = c('Ghastly Claws', 'Unholy Feast'),
                    ScepterMain = c('Grasping Dead', 'Feast of Corruption'),
                    DaggerMain = c('Life Siphon', 'Dark Pact'),
                    DaggerOff = c('Deathly Swarm', 'Enfeebling Blood'),
                    FocusOff = c("Reaper's Touch", 'Spinal Shivers'),
                    WarhornOff = c('Wail of Doom', 'Locust Swarm'))
Reaper <- list(Greatsword = c('Gravedigger', 'Death Spiral', 'Nightfall', 'Grasping Darkness'))
Reaper <- c(Necromancer, Reaper)
Scourge <- list(TorchOff = c('Harrowing Wave', 'Oppressive Collapse'))
Scourge <- c(Necromancer, Scourge)

htmlParser <- function(htmlFile) {
  #Function to check rotation skills to be used in lapply
  skillChecker <- function(toCheck) {
    target <- charSkills$X1[skill]
    toCheck == target
  }
  
  # Check file size, and return error if too small
  fileInfo <- file.info(htmlFile)
  if (fileInfo$size < 500000) {
    return(paste(htmlFile, " did not parse correctly.", paste = ""))
  }
  
  bossFile <- read_html(htmlFile)
  
  #Look for center nodes for pull information
  centerNodes <- bossFile %>%
    html_nodes("center")
  
  #Boss name
  boss <- substr(htmlFile, 13, nchar(htmlFile) - 5)
  
  #Kill date
  date <- as.Date(substr(htmlFile, 1, 11), "%b-%d-%Y")
  date <- format(date, format = "%Y%m%d")
  
  #Kill time prep
  pNodes <- centerNodes[1] %>% html_nodes('p')
  killTimeString <- pNodes[3] %>% html_text()
  killTimeInfo <- data.frame(strsplit(killTimeString, ' minutes '))
  
  #Minutes component
  match <- gregexpr('([[:digit:]]+)', killTimeInfo[1,])
  killTime <- as.numeric(regmatches(killTimeInfo[1,], match)) * 60 
  
  #Seconds component
  killTime <- killTime + as.numeric(sub(' seconds', '', killTimeInfo[2,]))
  
  # Check success based on boss name
  if (boss == 'dei') {
    #Looks up total DPS calculates total damage done to boss
    mainTable <- bossFile %>% html_nodes('body') %>% html_nodes('div') %>% html_nodes('.tab-content') %>% html_nodes('#s_glob') %>% html_nodes('table') %>% html_table(fill = TRUE) %>% data.frame()
    
    if (killTime * mainTable$Boss.DPS[nrow(mainTable)] < 32264000) {
      return('Not enough damage dealt to Deimos.')
    }
  } else {
    if (!grepl('Success', pNodes[3] %>% html_text())) {
      return('Run was not successful.')
    }
  }
  
  #Player table
  mainTable <- bossFile %>% html_nodes('body') %>% html_nodes('div') %>% html_nodes('.tab-content') %>% html_nodes('#s_glob') %>% html_nodes('table') %>% html_table(fill = TRUE) %>% data.frame()
  mainTable <- mainTable[1:length(mainTable) - 1,]
  mainTable <- mainTable[-which(grepl('(Group )[[:digit:]]', mainTable$Character)),]
  mainTable$Character <- sub(' PoV', '', mainTable$Character)
  names(mainTable)[2] <- 'Specialization'
  
  #Add Specializations to player table
  specList <- bossFile %>% html_nodes('body') %>% html_nodes('div') %>% html_nodes('.tab-content') %>% html_nodes('#s_glob') %>% html_nodes('table') %>% html_nodes('img')
  specList <- specList[3:length(specList)]
  specList <- specList %>% html_attr('alt')
  specList <- specList[!specList == 'Disconnect']
  if (any(grepl('Dead', specList))) {
    mainTable$Specialization <- specList[-which(grepl('Dead', specList))]
  } else {
    mainTable$Specialization <- specList 
  }
  
  #Gear box list
  gearTable <- bossFile %>% html_nodes('body') %>% html_nodes('div') %>% html_nodes('table')
  gearBoxes <- gearTable[1] %>% html_nodes('td')
  gearBoxNames <- gearBoxes %>% html_text()
  
  #Boon table
  boonTable <- bossFile %>% html_nodes('body') %>% html_nodes('div') %>% html_nodes('.tab-content') %>% html_nodes('#bs_glob') %>% html_nodes('table') %>% html_table() %>% data.frame()
  boonTable <- boonTable[1:nrow(mainTable),3:ncol(boonTable)]
  
  boonNames <- c('Character', bossFile %>% html_nodes('body') %>% html_nodes('div') %>% html_nodes('.tab-content') %>% html_nodes('#bs_glob') %>% html_nodes('thead') %>% html_nodes('img') %>% html_attr('alt'))
  names(boonTable) <- boonNames 
  
  #GotL bug expcetion
  names(boonTable)[which(names(boonTable) == 'id:34062')] <- 'Grace of the Land'
  boonNames <- sub("id:34062", "Grace of the Land", boonNames)
  
  #Setup skill lists for rotations
  skillRotation <- bossFile %>% html_nodes('div')
  skillRotation <- skillRotation[which(grepl('(<div id="prot_glob)', skillRotation))]
  skillRotation <- skillRotation[3:length(skillRotation)]
  
  # Empty data frame
  outputData <- data.frame()
  #Iterate through all players
  for (player in 1:length(mainTable[,1])) {
    #Isolates row on table
    playerRow <- mainTable[player,]
    
    #Find charName with PoV correction
    charName <- sub(' PoV', '', playerRow$Character)
    charName <- substr(playerRow$Character, 1, 10)
    
    #? correction
    if (grepl('?', charName) || grepl('?', charName)) {
      charName <- substr(charName, 1, 9)  
    }
    
    #Find stat type with multiple match correction
    charBox <- gearBoxes[which(grepl(charName, gearBoxNames))] 
    if (length(charBox) > 1) {
      charBox <- charBox[which(grepl(playerRow$Specialization, charBox))]
    }
    
    #Write the build type using if statements and concatenate
    buildType <- c()
    
    try({
      if (grep('Condition Damage', charBox)) {
        buildType <- paste(buildType, "CD.", sep = "")
      } 
    }, silent = TRUE)
    
    try({
      if (grep('Toughness', charBox)) {
        buildType <- paste(buildType, "To.", sep = "")
      }
    }, silent = TRUE)
    
    try({
      if (grep('Healing Power', charBox)) {
        buildType <- paste(buildType, "HP.", sep = "")
      }
    }, silent = TRUE)
    
    try({
      if (is.null(buildType) && playerRow$Physical < 1000) {
        buildType <- paste(buildType, "Other.", sep = "")
      } else if (is.null(buildType)) {
        buildType <- paste(buildType, "Po.", sep = "")
      }
    }, silent = TRUE)
    
    #Weapon Info
    skillRotationIndices <- skillRotation %>% html_nodes('center') %>% html_nodes('h4') %>% html_text()
    charSkills <- skillRotation[which(grepl(paste("^", playerRow$Character, "$", sep = ''), skillRotationIndices))] %>% html_nodes('img') %>% html_attr('title')
    charSkills <- strsplit(charSkills, " [", fixed = TRUE)
    charSkills <- data.frame(t(data.frame(charSkills)))
    skillCount <- length(charSkills$X1)
    
    #Dataframe of weapons
    equippedFramer <- paste('equippedFrame <- data.frame(names(', playerRow$Specialization, '))', sep = "")
    eval(parse(text = equippedFramer))
    equippedFrame$Equipped <- FALSE
    
    skills = eval(parse(text = playerRow$Specialization))
    
    for (skill in 1:skillCount) {
      #Compare to skill list
      weaponTruer <- lapply(skills, skillChecker)
      if (any(grepl('TRUE', weaponTruer))) {
        equippedFrame[grep('TRUE', weaponTruer), 2] = TRUE
      }
    }

          weapons <- paste(equippedFrame[,1][which(equippedFrame$Equipped == 'TRUE')], collapse = ",")
    
    #Boons and buffs
    boonRow <- boonTable[which(boonTable$Character == playerRow$Character),]
    boonRow <- boonRow[2:length(boonRow)]
    boonData <- data.frame(t(data.frame(buffNames)), stringsAsFactors = FALSE)
    names(boonData) <- buffNames
    boonData[1,] <- '-'
    boonChecker <- paste('boonData$`', names(boonRow), '` <- ', 'boonRow$`', names(boonRow), '`', sep = '')
    eval(parse(text = boonChecker))
    
    #Adds info to the big dataframe
    generalData <- data.frame(Boss = boss, 
                              PlayerName = playerRow$Display.Name, 
                              Specialization = playerRow$Specialization, 
                              BuildType = buildType, 
                              TotalDPS = playerRow$Boss.DPS, 
                              PowerDPS = playerRow$Physical, 
                              CondiDPS = playerRow$Condi, 
                              KillTime = killTime, 
                              Subgroup = playerRow$Group, 
                              Date = date, 
                              Weapons = weapons)
    
    # Bind general data and boon data together
    temp <- cbind(generalData, boonData)
    #Bind it to output data
    outputData <- rbind(outputData, temp)
  }
  outputData
}
