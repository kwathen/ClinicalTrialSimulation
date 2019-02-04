########################################################################
#   This file contains functions for simulating a virtual trial.
########################################################################

SimulateSingleTrial <- function( nMaxQtyOfPats, nMinQtyOfPats, dQtyPatsPerMonth,
                                 dPriorAS, dPriorBS,
                                 dPriorAE, dPriorBE,
                                 dPU,
                                 dTrueRespRateS,
                                 dTrueRespRateE
                                 )
{
    #Setup the variables needed in this function
    vPatOutcome <- rep( NA, nMaxQtyOfPats )       # Vector that contains the patients outcome
    vTreat      <- rep( NA, nMaxQtyOfPats )       # Vector that contains the patients treatment S = 0, E = 1
    vQtyPats    <- rep( 0, 2 )                    # Vector to keep track of the number of patients on S and E 
    
    #Simulate arrival times and times the outcomes are observed
    vStartTime  <- SimulateArrivalTimes( dQtyPatsPerMonth, nMaxQtyOfPats )
    vObsTime    <- SimulateOutcomeObservedTime( vStartTime )
    
    
    vRandProbE <- rep( NA, nMaxQtyOfPats ) #This vector is used to store the randomization probabilities before each patient is enrolled.
                       # This is very educational to view when doing adaptive randomization.
    
    #For loop to randomize and simulate the patient outcomes.
    #Note: In this example this could be done easier, but this is a building block for when we want to update randomization before each patient
    for( i in 1:nMaxQtyOfPats )
    {
        #The following line should be commented out before running a simulation, as it is intended to help perform a basic check
        print( paste( "Randomizing patient " ,i ) )
        dCurrentTime        <- vStartTime[ i ]
        dProbSGrtE          <- RunAnalysis( dCurrentTime, vPatOutcome, vTreat, vObsTime, dPriorAS,dPriorBS, dPriorAE, dPriorBE )
        lRand               <- GetTreatmentAdaptiveRandomization( i,  nMinQtyOfPats, dProbSGrtE ) 
        vTreat[ i ]         <- lRand$nTrt
        vRandProbE[ i ]     <- lRand$dRandProbE
        vPatOutcome[ i ]    <- SimulatePatientOutcome( vTreat[ i ], dTrueRespRateS, dTrueRespRateE ) 
        
        vQtyPats[ vTreat[ i ] + 1 ] <- vQtyPats[ vTreat[ i ] + 1 ] + 1
    }
    dCurrentTime    <- vObsTime[ nMaxQtyOfPats ] + 0.00001  #Adding 0.0001 to make sure all patient outcomes are observed
    
    #The following line should be commented out before running a simulation, as it is intended to help perform a basic check
    print( paste( "Running the final analysis " ,i ) )
    dProbSGrtE      <- RunAnalysis( dCurrentTime, vPatOutcome, vTreat, vObsTime, dPriorAS,dPriorBS, dPriorAE, dPriorBE )
    dProbEGrtS      <- 1.0 - dProbSGrtE
    
    nDecision       <- MakeDecision( dPU, dProbSGrtE, dProbEGrtS )
    
    #Build the return list - in a large scale simulation you may not want to return the patient data 
    lRet <- list( nDecision = nDecision, dProbEGrtS = dProbEGrtS, vQtyPats = vQtyPats, 
                  vPatOutcome = vPatOutcome, vTreat = vTreat, vStartTime = vStartTime, vObsTime = vObsTime,
                  vRandProbE = vRandProbE )
    return( lRet )
    
}

