
body <- dashboardBody(
    
    tabsetPanel(
        tabPanel("Setup",
                 splitLayout(
                     box( width="300px",title="Trial Design Options",
                          selectInput( "ddlDesign", "Select Design Option", c("Custom", 
                                                                "Fixed Sample",
                                                                "AR - No Early Stopping",
                                                                "Quick AR - No Early Stopping",
                                                                "AR - Minimum N, No Early Stopping",
                                                                "AR - Minimum N, Early Stopping",
                                                                "Augmented Control 100% Prior Data",
                                                                "Augmented Control 10% Prior Data")),
                          
                          numericInput( "nMaxQtyOfPats", "Maximum Number of Patients: ", value = 200, max = 1000, step = 1, min=10),
                          numericInput( "nMinQtyOfPats", "Minimum Number of Patients: ", value = 20, max = 1000, step = 1, min=10),
                          
                          numericInput( "dPU", "Decision Cutoff ", value = 0.95, max = 1, step = 0.01, min=0),
                          numericInput( "dMinRandProb", "Minimum Randomization Probability", value = 0.1, max = 0.5, step = 0.01, min=0.01),
                          numericInput( "dExponent", "Tunning Parameter", value = 1.0, max = 1.0, step = 0.1, min=0.0),
                          
                          box( width = 100, title="Prior S",
                               splitLayout("Q_S ~ Beta( ", 
                                           numericInput( "dPriorAS", NULL, value = 0.2, max = 1000, step = .1, min=.0001, width='75px'),
                                           ",  ",
                                           numericInput( "dPriorBS", NULL, value = 0.8, max = 1000, step = .1, min=.0001, width='75px'),
                                           " )", cellWidths=c(85, 90, 4, 90, 10))
                          ),
                          box( width = 100,  title="Prior E",
                               splitLayout("Q_E ~ Beta( ", 
                                           numericInput( "dPriorAE", NULL, value = 0.2, max = 1000, step = .1, min=.0001, width='75px'),
                                           ",  ",
                                           numericInput( "dPriorBE", NULL, value = 0.8, max = 1000, step = .1, min=.0001, width='75px'),
                                           " )", cellWidths=c(85, 90, 4, 90, 10))
                          )
                     ),
                     box(width="300px", title ="Simulation Options",
                         numericInput( "nQtyReps", "Number of Virtual Trials: ", value = 10, max = 10000, step = 5, min=5),
                         numericInput( "dTrueRespRateS", "True Response Rate S ", value = 0.2, max = 1, step = 0.05, min=0),
                         numericInput( "dTrueRespRateE", "True Response Rate E ", value = 0.2, max = 1, step = 0.05, min=0),
                         numericInput( "nSeed", "Seed", value = 123, max = 100000, step = 100, min=1)
                         
                     ),
                     cellWidths=c("50%","50%")
                 ), bsButton( "btnSimulate", label="Run Simulation", style="success", block=F, size="large")
        ),
        
        tabPanel("Selection Probabilities",
                 plotOutput("ctrlPlotOCs", height="600px")
        ),
        tabPanel("Sample Size",
                 plotOutput("ctrlPlotSampSize", height="600px")
        ),
        tabPanel( "Virtual Trials",
                  selectInput( "ddlVirtualTrialIDN", "Select a virtual trial ID to view", c("1","2","3")),
                  tabsetPanel(
                      tabPanel("Enrollment",
                               plotOutput("ctrlPlotEnroll1"),
                               tags$hr(),
                               tags$h1("Zoom In"),
                               plotOutput("ctrlPlotEnroll2")
                      ),
                      tabPanel("Randomization",
                               plotOutput("ctrlPlotVT3"),
                               tags$hr(),
                               plotOutput("ctrlPlotVT4")
                      ),
                      tabPanel("Posterior Probability",
                               plotOutput("ctrlPlotVT5",height="700px")
                      )
                  )
        )
    )
    
)
