library(shiny)

shinyUI(fluidPage(
  passwordInput('team_code', 'Team Code: ', value = "", width = NULL,
                placeholder = 'Enter your unique team code'),
  fileInput('evtc', 'EVTC Upload',
            accept = 'application/zip',
            multiple = TRUE),
  tableOutput("fileName")
))