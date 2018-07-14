library(plyr)

#Core Team List
coreTeam <- c('Trindine', 
              'Vice', 
              'blitzcreek', 
              'Elestian', 
              'MereLynn', 
              'kratox', 
              'Evil Andrex', 
              'sleepysoul', 
              'LucianTheAngelic',
              'Burkid')

#Colours based on specialization via function for dpply
spec <- c('Guardian', 'Dragonhunter', 'Firebrand',
          'Revenant', 'Herald', 'Renegade',
          'Warrior', 'Berserker', 'Spellbreaker',
          'Engineer', 'Scrapper', 'Holosmith',
          'Ranger', 'Druid', 'Soulbeast',
          'Thief', 'Daredevil', 'Deadeye',
          'Elementalist', 'Tempest', 'Weaver',
          'Mesmer', 'Chronomancer', 'Mirage',
          'Necromancer', 'Reaper', 'Scourge')
colour <- c('rgba(127,229,235,1)', 'rgba(127,229,235,1)', 'rgba(127,229,235,1)', #Guardian
            'rgba(135,167,255,1)', 'rgba(135,167,225,1)', 'rgba(135,167,225,1)', #Revenant
            'rgba(255,247,40,1)', 'rgba(255,247,40,1)', 'rgba(255,247,40,1)', #Warrior
            'rgba(255,172,20,1)', 'rgba(255,172,20,1)', 'rgba(255,172,20,1)', #Engineer
            'rgba(167,229,50,1)', 'rgba(167,229,50,1)', 'rgba(167,229,50,1)', #Ranger
            'rgba(183,172,172,1)', 'rgba(183,172,172,1)', 'rgba(183,172,172,1)', #Thief
            'rgba(225,158,158,1)', 'rgba(255,158,158,1)', 'rgba(255,158,158,1)', #Elementlist
            'rgba(252,127,255,1)', 'rgba(252,127,255,1)', 'rgba(252,127,255,1)', #Mesmer
            'rgba(70,214,121,1)', 'rgba(70,214,121,1)', 'rgba(70,214,121,1)') #Necromancer

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

#Fuctions to be used to generate graph visuals
SpecColours <- function(x) {colour[which(spec == x)]}
standardError <- function(x) {sqrt(var(x)/length(x))}

#Import data set and add column names
dataset <- read.table(file = 'allData.txt', header = FALSE, sep = '\t')
names(dataset) <- c(colNames, buffNames)

nameList <- strsplit(as.character(dataset$PlayerName), '.', fixed = TRUE)
dataset$PlayerName <- unlist(nameList)[c(TRUE,FALSE)]

plotColumn <- ddply(dataset, .(Entry), summarize, plotColumn = paste(Entry, PlayerName, Specialization, BuildType, Weapons, sep = "-"), SpecColour = SpecColours(Specialization))
plotColumn <- plotColumn[,2:3]

dashFinder <- regexpr('-', plotColumn$plotColumn)
plotColumn$plotText <- substr(plotColumn$plotColumn, dashFinder + 1, nchar(plotColumn$plotColumn))
dataset <- cbind(dataset, plotColumn)

dataset <- data.frame(lapply(dataset, function(x) if(is.character(x)|is.factor(x)) gsub('^-$', NA, x) else x))
dataset <- data.frame(lapply(dataset, function(x) if(is.character(x)|is.factor(x)) gsub('%', '', x) else x))

#Add core team values
for (row in 1:nrow(dataset)) {
  if (any(grepl(dataset$PlayerName[row], coreTeam))) {
    dataset$Core[row] <- 1
  } else {
    dataset$Core[row] <- 0
  }
}
