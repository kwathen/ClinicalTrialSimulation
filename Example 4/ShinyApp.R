

library( shiny)
library( shinydashboard)
library( shinyBS )
library( ggplot2 )

source( "SimulatePatientOutcome.R")
source( "SimulateTrial.R")
source( "AnalysisMethods.R")
source( "Functions.R")
source( "Randomizer.R")
source( "StoppingRules.R")
source( "PlotVirtualTrial.R" )
source( "SimulateScenario.R" )
source( "ShinyUI.R" )
source( "ShinyServer.R" )

glVirtualTrial <<- NA
gBaseSize      <<- 18
#set.seed(123)

shinyApp(
    ui = dashboardPage(
        dashboardHeader( title="Virtual Trial Simulator"),
        dashboardSidebar(disable=TRUE),
        body
    ),
    server = server
)