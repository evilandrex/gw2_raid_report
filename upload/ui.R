library(shiny)

shinyUI(fluidPage(
  fileInput('evtc', 'EVTC Upload',
            accept = 'application/zip',
            multiple = TRUE),
  tableOutput("fileName")
))