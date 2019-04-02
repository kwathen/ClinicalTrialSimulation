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
                                 dTrueRespRateE   )
{
    # Add code here to setup the variables needed in this function
    vPatOutcome <- rep( NA, _________ )           # Vector that contains the patients outcome
    vTreat      <- rep( NA, _________ )           # Vector that contains the patients treatment S = 0, E = 1
    vQtyPats    <- rep( 0, 2 )                    # Vector to keep track of the number of patients on S and E, column 1 is is, column 2 is E 
    
    # Simulate arrival times and times the outcomes are observed
    # Add code to simulate start times for each patient (vStartTime) and observed time of outcome 1 month after starting the trial (vObsTime) #####
    # Note: Make sure to make use of SimulateArrivalTimes(..)
    vStartTime  <- SimulateArrivalTimes( ____________, ____________ )
    vObsTime    <- _________ + 1  # Note: In this example we observe the outcome 1 month after they enroll (or are treated)
    

    #For loop to randomize and simulate the patient outcomes.
    #Note: In this example this could be done easier, but this is a building block for when we want to update randomization before each patient
    for( i in 1:nMaxQtyOfPats )
    {
        # Add code here to fill in vTreat, vPatOutcome and vQtyPats #####
        # Hint: Use the functions GetTreatment and SimulatePatientOutcome
        vTreat[ i ]                 <- ___________( 0.5 )   #For this example the randomization probability is always 0.5
        vPatOutcome[ i ]            <- ___________( ________[ i ], dTrueRespRateS, ________________ ) 
        
        vQtyPats[ vTreat[ i ] + 1 ] <- vQtyPats[ vTreat[ i ] + 1 ] + 1
      
    }
    
    # Add code here to to compute the time of the analysis after the last outcome is observed ####
    # Also compute dProbSGrtE, dProbEGrtS and make a decision based on this probabilities
    # Hint: Use the functions RunAnalysis and MakeDecision and check the functions to make sure the parameters are in the correct order
    dCurrentTime    <- vObsTime[ nMaxQtyOfPats ] + 0.00001  #Adding 0.0001 to make sure all patient outcomes are observed
    dProbSGrtE      <- _________( _______, _______, ______, __________, dPriorAS,dPriorBS, dPriorAE, dPriorBE )
    dProbEGrtS      <- 1.0 - ___________
    
    nDecision       <- ___________( dPU, __________, ________ )

    #Build the return list - in a large scale simulation you may not want to return the patient data but could
    # be good to return it so you could plot each virtual trial.  
    lRet <- list( nDecision = nDecision, dProbEGrtS = dProbEGrtS, vQtyPats = vQtyPats, 
                  vPatOutcome = vPatOutcome, vTreat = vTreat, vStartTime = vStartTime, vObsTime = vObsTime )
    return( lRet )
    
}

