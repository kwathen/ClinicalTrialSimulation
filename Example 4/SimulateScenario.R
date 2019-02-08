##### File Header ##################################################################################################  
#   
#       Description: This function will simulate 1 scenario nQtyReps virtual trials
#
#       Input Parameters:
#       nMaxQtyOfPats: The maximum number of patients in the trial
#       nMinQtyOfPats: The minimum number of patients before the trial can stop.  nMinQtyOfPats patients 
#                       are randomized fairly before altering the randomization
#       dPriorAS, dPriorBS: Prior parameters for S: Q_S ~ Beta( dPriorAS, dPriorBS ) 
#       dPriorAE, dPriorBE: Prior parameters for E: Q_E ~ Beta( dPriorAE, dPriorBE ) 
#       dPU: Decision criteria parameter, 
#           Stop if Pr( Q_E > Q_S | data ) > dPU --> Select E or  Pr( Q_S > Q_E | data ) > dPU --> Select S
#       dMinRandProb: Randomization probability is constrained between (dMinRandProb, 1-dMinRandProb)
#       dExponent: Adaptive randomization tunning parameter
#           Randomize to S with probability dRandProbTrtS = (dProbSGrtE)^dExponent/( (dProbSGrtE)^dExponent + (1 - dProbSGrtE)^dExponent )
#           Randomize to E with probability dRandProbTrtE = 1 - dRandProbTrtS     
#
#   Author: J. Kyle Wathen, PhD
#           kylewathen@gmail.com
######################################################################################################################.

SimulateScenario <- function(  nMaxQtyOfPats, nMinQtyOfPats, 
                               dPriorAS, dPriorBS, 
                               dPriorAE, dPriorBE,
                               dPU,
                               dMinRandProb,
                               dExponent, 
                               dTrueRespRateS,
                               dTrueRespRateE, 
                               nQtyReps)
{
    
    vQtyPatsPerMonth    <- c( 1, 1.5, 2, 3, 5, 7, 10, 15, 22, 25 )  #Each element represents the expected # of patients recruited, then the recruitment stays 25/month
    
     

    ##### Setup Simulation Results #####
    # Initialize variables that are used for tracking simulation results
    vResults        <- rep( NA, nQtyReps )                  # Which arm is selected, 1 = no arm, 2 = S, 3 = E
    mQtyPats        <- matrix( NA, ncol=2, nrow = nQtyReps) # The number of patient on each arm
    vStopEarly      <- rep( NA, nQtyReps )                  # Keep track of early stopping; 0 = no early stopping, 1 = early stopping 
    lVirtualTrials  <- list( vVirtualTrial = rep( NA, nQtyReps))
    
    i<-1
    # For loop - each replication in the loop will simulate 1 virtual trial. 
    for( i in 1:nQtyReps )
    {
        #It is often nice to provide feedback to users what stage of the simulation we are on.
        nNotify <- round( nQtyReps*0.1,0)
        if( i %% nNotify == 0 )
            print( paste( "Simulating virtual trial ", i, " of ", nQtyReps, " virtual trials."))
        
        lSimulatedTrial <- SimulateSingleTrial( nMaxQtyOfPats,  nMinQtyOfPats, vQtyPatsPerMonth,  dPriorAS,  dPriorBS, dPriorAE, dPriorBE,  
                                                dPU, dMinRandProb, dExponent, dTrueRespRateS, dTrueRespRateE  )
        
        lVirtualTrials[[i]] <- lSimulatedTrial
        #Capture the results for the simulated virtual trial 
        vResults[ i ]   <- lSimulatedTrial$nDecision
        mQtyPats[ i, ]  <- c(lSimulatedTrial$nQtyPatsS, lSimulatedTrial$nQtyPatsE )
        vStopEarly[ i ] <- lSimulatedTrial$nEarlyStop
        
    }
    
    
    #Create simple summaries
    return( list( lVirtualTrials = lVirtualTrials, vResults = vResults, mQtyPats = mQtyPats, vStopEarly = vStopEarly ) )
}
