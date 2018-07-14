#system('raid_heroes "D:/Documents/GW2CombatLogs/arcdps.cbtlogs/20171016-232743.evtc.zip"')

library(shiny)


shinyUI(fluidPage(
  fileInput('evtc', 'EVTC Upload',
            accept = 'application/zip',
            multiple = TRUE),
  tableOutput("fileName")
))