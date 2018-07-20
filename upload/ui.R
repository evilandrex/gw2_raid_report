library(shiny)

shinyUI(fluidPage(
  conditionalPanel(condition = 'input.submitButton == 0 || output.teamName == "Bad code"',
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
                                });'),
                              conditionalPanel(condition = 'input.submitButton > 0 && output.teamName == "Bad code"',
                                               HTML('<div>Invalid code, please try again!</div>')))),
  conditionalPanel(condition = 'input.submitButton > 0 && output.teamName != "Bad code"',
                   fluidRow(
                     column(4, 
                            fileInput('evtc', 'EVTC Upload',
                                      accept = 'application/zip',
                                      multiple = TRUE)),
                     column(8,
                            h1(textOutput('teamName')))),
                     fluidRow(
                       dataTableOutput("fileName")))
  ))