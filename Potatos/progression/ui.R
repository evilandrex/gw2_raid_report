library(shiny)
library(plotly)

shinyUI(fluidPage(
  inputPanel(id = 'selectBoss',
             selectInput(
               "bossName",
               label = NULL,
               choices = list(
                 'Spirit Vale' = c('Vale Guardian', 'Gorseval', 'Sabetha'),
                 'Salvation Pass' = c('Slothasor', 'Matthias Gabrel'),
                 'Stronghold of the Faithful' = c('Keep Construct', 'Xera'),
                 'Bastion of the Penitent' = c('Cairn the Indomitable', 
                                               'Mursaat Overseer', 'Samarog', 
                                               'Deimos'),
                 'Hall of Chains' = c('Souless Horror', 'Dhuum')
               ),
               selected = "Vale Guardian",
               width = "200px")),
  plotlyOutput('progression')
))