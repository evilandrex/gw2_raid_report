library(shiny)
library(plotly)

shinyUI(fluidPage(includeCSS('../assets/style.css'),
                  
                  #Title Text
                  navbarPage("Raid Logs - Firebrand Nerf Patch",
                             position = 'fixed-top'),
                  
                  #Boss selection dropdown
                  inputPanel(id = 'inputPanelTop',
                             inputPanel(id = 'selectBoss',
                                        selectInput(
                                          "bossName",
                                          label = NULL,
                                          choices = list(
                                            'Spirit Vale' = c('Vale Guardian', 'Gorseval', 'Sabetha'),
                                            'Salvation Pass' = c('Slothasor', 'Matthias Gabrel'),
                                            'Stronghold of the Faithful' = c('Keep Construct', 'Xera'),
                                            'Bastion of the Penitent' = c('Cairn the Indomitable', 'Mursaat Overseer', 'Samarog', 'Deimos')
                                          ),
                                          selected = "Vale Guardian",
                                          width = "200px")),
                             inputPanel(id = 'selectRun', 
                                        inputPanel(htmlOutput('selectRun')))
                  ),
                  
                  #Single Run Block  
                  fluidRow(id = 'chartHeader', class = 'firstHeader',
                           column(3, textOutput('formattedDate'))),
                  mainPanel( id = 'selectRunTab',
                             tabsetPanel(
                               tabPanel('Overview', fluidRow(id = 'overview', 
                                                             column(4, HTML('<b><center>DPS Breakdown</center></b>'), plotlyOutput("singleRun")),
                                                             column(8, dataTableOutput('players'))
                               )
                               ),
                               tabPanel('Buffs', dataTableOutput('buffs'))
                             ),
                             fillRow(id = 'overviewBar',
                                     htmlOutput('killTime'),
                                     htmlOutput('diffFromBest'),
                                     htmlOutput('killTimeRank'),
                                     htmlOutput('coreTeamCount'),
                                     htmlOutput('playerMissing'),
                                     htmlOutput('DPSTotals'),
                                     height = '50px')
                  ),
                  
                  #Averages and highscore block
                  fluidRow(id = 'chartHeader', 
                           column(6, 'Averages by Player and Build'),
                           column(6, 'Highscores by Player and Build')),
                  fluidRow(column(6, plotlyOutput("averages", height = '800px')),
                           column(6, plotlyOutput("highscores", height = '800px'))),
                  
                  #Weekly composition block
                  fluidRow(id = 'chartHeader', 
                           column(12, 'Weekly Composition by Kill Times')),
                  fluidRow(column(12, plotlyOutput("weeklyComp", height = "900px"))),
                  
                  #Colour key
                  fillRow(HTML('<div id = "colourKey" style = "background-color: rgba(127,229,235,1)">Guardian</div>'),
                          HTML('<div id = "colourKey" style = "background-color: rgba(135,167,255,1)">Revenant</div>'),
                          HTML('<div id = "colourKey" style = "background-color: rgba(255,247,40,1)">Warrior</div>'),
                          HTML('<div id = "colourKey" style = "background-color: rgba(255,172,20,1)">Engineer</div>'),
                          HTML('<div id = "colourKey" style = "background-color: rgba(167,229,50,1)">Ranger</div>'),
                          HTML('<div id = "colourKey" style = "background-color: rgba(183,172,172,1)">Thief</div>'),
                          HTML('<div id = "colourKey" style = "background-color: rgba(255,158,158,1)">Elementalist</div>'),
                          HTML('<div id = "colourKey" style = "background-color: rgba(252,127,255,1)">Mesmer</div>'),
                          HTML('<div id = "colourKey" style = "background-color: rgba(70,214,121,1)">Necromancer</div>'))
))