################################################################################################################################################
#
#   Code Template
#
#   Description:    This file contains functions for simulating a virtual trial.  This example is included as a starting point 
#                   and is not complete and will not run
#
#   Author: J. Kyle Wathen, PhD
#           kylewathen@gmail.com
################################################################################################################################################

SimulateSingleTrial <- function( nMaxQtyOfPats, dQtyPatsPerMonth )
{
    #Setup the variables needed in this function
   
    #Simulate arrival times and times the outcomes are observed
    vStartTime  <- SimulateArrivalTimes(dQtyPatsPerMonth, nMaxQtyOfPats )
    vObsTime    <- vStartTime 

    #For loop to randomize and simulate the patient outcomes.
    #Note: In this example this could be done easier, but this is a building block for when we want to update randomization before each patient
    for( i in 1:nMaxQtyOfPats )
    {
        vTreat[ i ]         <- GetTreatment( 0.5 ) 
        vPatOutcome[ i ]    <- SimulatePatientOutcome( ) 
        
        vQtyPats[ vTreat[ i ] + 1 ] <- vQtyPats[ vTreat[ i ] + 1 ] + 1
    }
    
    dCurrentTime    <- vObsTime[ nMaxQtyOfPats ] 
    dAnalysis       <- RunAnalysis( vTreat, vPatOutcome )
    
    nDecision       <- MakeDecision(dAnalysis )
    
    #Build the return list - in a large scale simulation you may not want to return the patient data 
    lRet <- list( nDecision = nDecision )
    return( lRet )
    
}

