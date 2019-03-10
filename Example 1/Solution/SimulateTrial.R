########## File Header #########################################################################################################################
#
#   Example 1
#
#   Description:    This file contains functions for simulating a virtual trial.
#
#   Input           nMaxQtyOfPatient    The maximum number of patients in the trial
#                   dQtyPatsPerMonth    The expected number of patients accrued each month
#                   dPriorAS, dPriorBS  Prior parameters for S where Q_S ~ Beta( dPriorAS, dPriorBS ) a priori 
#                   dPriorAE, dPriorBE  Prior parameters for S where Q_S ~ Beta( dPriorAE, dPriorBE ) a priori 
#                   dPU                 Select treatment i if Pr( Q_i > Q_j | data ) > dPU
#                   dTrueRespRateS      The true response rate for S
#                   dTrueRespRateE      The true response rate for E
#
#                   Return              A list with the following items
#                                       nDecision with 1 if no treatment is selected, 2 if E is selected 3 if S is selected, 
#                                       dProbEGrtS = Probability that Q_E > Q_E based on the simulated trial
#                                       vQtyPats = Number of patients on S and E 
#                                       vPatOutcome = Vector of patient outcome with 1 for response, 0 otherwise, 
#                                       nTreat = Treatment the patient received, 0 for S and 1 for E
#                                       vStartTime = vector of start times, in months, for each patient
#                                       vObsTime = a vector of times, in months, that each patient outcome is observed
#   Author: J. Kyle Wathen, PhD
#           kylewathen@gmail.com
################################################################################################################################################.

SimulateSingleTrial <- function( nMaxQtyOfPats, dQtyPatsPerMonth,
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
    vStartTime  <- SimulateArrivalTimes(dQtyPatsPerMonth, nMaxQtyOfPats )
    vObsTime    <- vStartTime + 1  # Note: In this example we observe the outcome 1 month after they enroll (or are treated)
    

    #For loop to randomize and simulate the patient outcomes.
    #Note: In this example this could be done easier, but this is a building block for when we want to update randomization before each patient
    for( i in 1:nMaxQtyOfPats )
    {
        vTreat[ i ]         <- GetTreatment( 0.5 ) 
        vPatOutcome[ i ]    <- SimulatePatientOutcome( vTreat[ i ], dTrueRespRateS, dTrueRespRateE ) 
        
        vQtyPats[ vTreat[ i ] + 1 ] <- vQtyPats[ vTreat[ i ] + 1 ] + 1
    }
    
    dCurrentTime    <- vObsTime[ nMaxQtyOfPats ] + 0.00001  #Adding 0.0001 to make sure all patient outcomes are observed
    dProbSGrtE      <- RunAnalysis( dCurrentTime, vPatOutcome, vTreat, vObsTime, dPriorAS,dPriorBS, dPriorAE, dPriorBE )
    dProbEGrtS      <- 1.0 - dProbSGrtE
    
    nDecision       <- MakeDecision( dPU, dProbSGrtE, dProbEGrtS )
    
    #Build the return list - in a large scale simulation you may not want to return the patient data 
    lRet <- list( nDecision = nDecision, dProbEGrtS = dProbEGrtS, vQtyPats = vQtyPats, 
                  vPatOutcome = vPatOutcome, vTreat = vTreat, vStartTime = vStartTime, vObsTime = vObsTime )
    return( lRet )
    
}

