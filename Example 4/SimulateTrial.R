################################################################################################################################################
#   This file contains functions for simulating a virtual trial.
#
#   Input:
#       dMinRandProb: the minimum randomization probability that is desired.
#           All randomization probabilities will be in the range [ dMinRandProb, 1-dRandProb ]
#       dExponent:  Randomization Probability to S (dRandProbTrtS)
#           dRandProbTrtS = (dProbSGrtE)^dExponent/( (dProbSGrtE)^dExponent + (1 - dProbSGrtE)^dExponent )
#           dRandProbTrtE = 1 - dRandProbTrtS
#       nQtyPats: The quantity of patients in trial (must be >= nMinQtyOfPats before adapting)
#       nMinQtyOfPats: The minimum quantity of patients before adapting begins.
#		dTrueRespRateS - The true response rate for S
#       dTrueRespRateE - True response rate for E
################################################################################################################################################

SimulateSingleTrial <- function( nMaxQtyOfPats, nMinQtyOfPats, vQtyPatsPerMonth,
                                 dPriorAS, dPriorBS,
                                 dPriorAE, dPriorBE,
                                 dPU,
                                 dMinRandProb, 
                                 dExponent,
                                 dTrueRespRateS,
                                 dTrueRespRateE
                                 )
{
    #Set up the variables needed in this function
    vPatOutcome         <- rep( NA, nMaxQtyOfPats )       # Vector that contains the patients outcome
    vTreat              <- rep( NA, nMaxQtyOfPats )       # Vector that contains the patients treatment S = 0, E = 1
    vQtyPats            <- rep( 0, 2 )                    # Vector to keep track of the number of patients on S and E 
    
    #Simulate arrival times and times the outcomes are observed
    vStartTime          <- SimulateArrivalTimes( vQtyPatsPerMonth, nMaxQtyOfPats )
    vObsTime            <- SimulateOutcomeObservedTime( vStartTime )
    
    
    vRandProbE          <- rep( NA, nMaxQtyOfPats )     # This vector is used to store the randomization probabilities before each patient is enrolled.
                                                        # This is very educational to view when doing adaptive randomization.
    vProbEGrtS          <- rep( NA, nMaxQtyOfPats )
    nEarlyStop          <- 0                            # If the trial is stopped early due to crossing the boundary this variable will be set to 1
         
    #For loop to randomize and simulate the patient outcomes.
    #Note: In this example this could be done easier, but this is a building block for when we want to update randomization before each patient
    
    for( i in 1:nMaxQtyOfPats )
    {
        #The following line should be commented out before running a simulation, as it is intended to help perform a basic check
        #print( paste( "Randomizing patient " ,i ) )
        
        dCurrentTime        <- vStartTime[ i ]
        dProbSGrtE          <- RunAnalysis( dCurrentTime, vPatOutcome, vTreat, vObsTime, dPriorAS,dPriorBS, dPriorAE, dPriorBE )
        vProbEGrtS[ i ]     <- 1 - dProbSGrtE 
        nDecision           <- CheckStoppingRule( nMinQtyOfPats, dPU, i, dProbSGrtE )
        
        #If nDecision > 1 then the trial has reached a stopping point and no need to continue randomizing patients
        if( nDecision > 1 )
        {
            # The trial is stopping early we do not want to return a vector of patient data with the NAs created above so subset them
            i               <- i - 1  # i-1 because the ith patient never enrolled, the trial ended first
            vPatOutcome     <- vPatOutcome[ 1:i ]
            vTreat          <- vTreat[ 1:i ]
            vStartTime      <- vStartTime[ 1:i ]
            vObsTime        <- vObsTime[ 1:i ]
            vRandProbE      <- vRandProbE[ 1:i ]
            vProbEGrtS      <- vProbEGrtS[ 1:i ]
            
            nEarlyStop       <- 1
            break           # Break the for loop and do not add any more patients
        }
        lRand               <- GetTreatmentAdaptiveRandomization( dMinRandProb,  dExponent, i, nMinQtyOfPats, dProbSGrtE ) 
        vTreat[ i ]         <- lRand$nTrt
        vRandProbE[ i ]     <- lRand$dRandProbE
        vPatOutcome[ i ]    <- SimulatePatientOutcome( vTreat[ i ], dTrueRespRateS, dTrueRespRateE ) 
        
        vQtyPats[ vTreat[ i ] + 1 ] <- vQtyPats[ vTreat[ i ] + 1 ] + 1
    }
    
    #In this example we will not run the analysis again if we cross a stopping boundary the trial ends and that treatment is selected.
    if( nEarlyStop == 0 )
    {
        dCurrentTime    <- vObsTime[ nMaxQtyOfPats ] + 0.00001  #Adding 0.0001 to make sure all patient outcomes are observed
        
        #The following line should be commented out before running a simulation, as it is intended to help perform a basic check
        #print( paste( "Running the final analysis " ,i ) )
        dProbSGrtE      <- RunAnalysis( dCurrentTime, vPatOutcome, vTreat, vObsTime, dPriorAS,dPriorBS, dPriorAE, dPriorBE )
        dProbEGrtS      <- 1.0 - dProbSGrtE
        
        nDecision       <- MakeDecision( dPU, dProbSGrtE, dProbEGrtS )
        
    }
  
    # New output items to help with visual display of trials
    nQtyEnrolled  <- length( vTreat )
    vPatientIndex <- 1:nQtyEnrolled
    vQtyPatsOnE   <- cumsum( vTreat)
    vQtyPatsOnS   <- vPatientIndex - cumsum( vTreat)
    
    #Build the return list - in a large scale simulation you may not want to return the patient data 
    lRet <- list( nDecision = nDecision, dFinalProbEGrtS = 1.0 - dProbSGrtE, nQtyPatsS = vQtyPats[1],  nQtyPatsE = vQtyPats[2],
                  vPatOutcome = vPatOutcome, vTreat = vTreat, vStartTime = vStartTime, vObsTime = vObsTime,
                  vRandProbE = vRandProbE, nEarlyStop = nEarlyStop, nMaxQtyOfPats = nMaxQtyOfPats, nMinQtyOfPats = nMinQtyOfPats,
                  dPU = dPU,
                  dMinRandProb = dMinRandProb,
                  vPatientIndex = vPatientIndex,
                  vQtyPatsOnE   = vQtyPatsOnE, 
                  vQtyPatsOnS   = vQtyPatsOnS,
                  vProbEGrtS    = vProbEGrtS)
    return( lRet )
    
}

