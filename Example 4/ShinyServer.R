

library( shiny)

server <- function(input, output,session) { 
    plotServer <- function() 
    {
        nSeed <- as.numeric( input$nSeed )
        set.seed( nSeed )
        lScenarioResults <- SimulateScenario( input$nMaxQtyOfPats,  input$nMinQtyOfPats,
                                              input$dPriorAS, input$dPriorBS,
                                              input$dPriorAE, input$dPriorBE,
                                              input$dPU,
                                              input$dMinRandProb,
                                              input$dExponent, 
                                              input$dTrueRespRateS,
                                              input$dTrueRespRateE,
                                              input$nQtyReps ) 
    
        lVirtualTrial    <- lScenarioResults$lVirtualTrial
        glVirtualTrial   <<- lVirtualTrial
        vPlots           <- PlotVirtualTrial(lVirtualTrial[[1]], nTrialID = 1, bPlotResults = FALSE)
        
        output$ctrlPlotOCs <- renderPlot(  PlotOCs( lScenarioResults ) )
        
        output$ctrlPlotSampSize <- renderPlot( PlotSampleSizeDist( lScenarioResults$mQtyPats ) )
        
        output$ctrlPlotEnroll1 <- renderPlot( print( vPlots[[1]]))
        output$ctrlPlotEnroll2 <- renderPlot( print( vPlots[[2]]))
        output$ctrlPlotVT3 <- renderPlot( print( vPlots[[3]]))
        
        output$ctrlPlotVT4 <- renderPlot( print( vPlots[[4]]))
        
        output$ctrlPlotVT5 <- renderPlot( print( vPlots[[5]]))
        
        updateSelectInput( session, "ddlVirtualTrialIDN", choices=1:input$nQtyReps )
        return( lVirtualTrial )
        
    }
    PlotTrial <- function( )
    {
        if( !anyNA( glVirtualTrial ))
        {
            nTrialIDN <- as.numeric( input$ddlVirtualTrialIDN )
            vPlots <- PlotVirtualTrial( glVirtualTrial[[nTrialIDN]], nTrialID = nTrialIDN, bPlotResults = FALSE)
            
            output$ctrlPlotEnroll1 <- renderPlot( print( vPlots[[1]]))
            output$ctrlPlotEnroll2 <- renderPlot( print( vPlots[[2]]))
            output$ctrlPlotVT3     <- renderPlot( print( vPlots[[3]]))
            
            output$ctrlPlotVT4     <- renderPlot( print( vPlots[[4]]))
            
            output$ctrlPlotVT5     <- renderPlot( print( vPlots[[5]]))
        }
    }
    SetDesign <- function() 
    {
        strDesignOpt <- input$ddlDesign
        ResetDesign()
        if( strDesignOpt == "Fixed Sample")
        {
            updateNumericInput( session, "nMinQtyOfPats", value=200)
        }
        else if( strDesignOpt == "AR - No Early Stopping" )
        {
            updateNumericInput( session, "nMinQtyOfPats", value=0)
            updateNumericInput( session, "dPU", value=1.0)
            
        }
        else if( strDesignOpt ==  "Quick AR - No Early Stopping")
        {
            updateNumericInput( session, "nMinQtyOfPats", value=0)
            updateNumericInput( session, "dPU", value=1.0)
            updateNumericInput( session, "dExponent", value=2.0)
            
        }
        else if( strDesignOpt ==  "AR - Minimum N, No Early Stopping")
        {
            updateNumericInput( session, "nMinQtyOfPats", value=200)
            updateNumericInput( session, "dPU", value=1.0)
            updateNumericInput( session, "dExponent", value=1.0)
            
        }
        else if( strDesignOpt == "AR - Minimum N, Early Stopping" )
        {
            updateNumericInput( session, "nMinQtyOfPats", value=20)
            updateNumericInput( session, "dPU", value=0.95)
            updateNumericInput( session, "dExponent", value=1.0)
            
        }
        else if( strDesignOpt == "Augmented Control 100% Prior Data" )
        {
            updateNumericInput( session, "nMinQtyOfPats", value=20)
            updateNumericInput( session, "dPU", value=0.95)
            updateNumericInput( session, "dExponent", value=1.0)
            updateNumericInput( session, "dPriorAS", value=205)
            updateNumericInput( session, "dPriorBS", value=795)
            
        }
        else if( strDesignOpt == "Augmented Control 10% Prior Data" )
        {
            updateNumericInput( session, "nMinQtyOfPats", value=20)
            updateNumericInput( session, "dPU", value=0.95)
            updateNumericInput( session, "dExponent", value=1.0)
            updateNumericInput( session, "dPriorAS", value=20.5)
            updateNumericInput( session, "dPriorBS", value=79.5)
            
        }
        
       
    }
    ResetDesign <- function()
    {
        updateNumericInput( session, "nMinQtyOfPats", value=20)
        updateNumericInput( session, "dPU", value=0.95)
        
    }
    
    observeEvent( input$btnSimulate, {
        updateButton( session, "btnSimulate", label="Running Simulations", style="danger", block=F, disable=TRUE, size="large")
        glVirtualTrial <<- plotServer()
        updateButton( session, "btnSimulate", label="Run Simulations", style="success", block=F, size="large", disable=FALSE)
    })
    observeEvent( input$ddlVirtualTrialIDN, {PlotTrial( )} )
    observeEvent( input$ddlDesign, SetDesign())
    
    
}