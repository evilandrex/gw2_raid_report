library(shiny)

shinyUI(fluidPage(
  conditionalPanel(condition = 'input.submitButton == 0',
                   inputPanel(passwordInput('team_code', 'Team Code: ', value = "", 
                                            placeholder = 'Enter your unique team code'),
                              actionButton('submitButton', 'Submit'),
                              tags$script('
                                $(document).keyup(function(event) {
                                  if ($("#team_code").is(":focus")) {
                                    if (event.keyCode == 13) {
                                      $("#submitButton").click();
                                    }
                                  }
                                });'))),
  conditionalPanel(condition = 'input.submitButton == 1',
                   fileInput('evtc', 'EVTC Upload',
                             accept = 'application/zip',
                             multiple = TRUE)),
  conditionalPanel(condition = 'input.submitButton == 1',
                   tableOutput("fileName"))
))