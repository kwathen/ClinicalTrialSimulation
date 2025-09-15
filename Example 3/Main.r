####### File Header####################################################################################################
#   This is the main file for conducting the simulation.
#
#   Example 1
#
#   Description: 		A fixed sample clinical trial utilizing fair randomization to compare standard of care (S) 
#                       and experimental (E).	The primary outcome is binary and is observed at the time a patient is treated.
#
#   Analysis Method:    Bayesian Beta-Binomial model
#                       Assume patients receiving treatment j = S or E have a response probability Q_j
#                       Prior: Q_j ~ Beta( a_j, b_j )
#
#   Decision Criteria:  At the end of the study, treatment j= S or E will be selected if
#                       Pr( Q_j > Q_{i!= j} | data ) > dPU
#                       Otherwise, no treatment is selected
#
#   Author: J. Kyle Wathen, PhD
#           kylewathen@gmail.com
######################################################################################################################.

# The next line will remove the contents of the work space.  This is done to 
# reduce the likelihood of bugs or inadvertently using a variable in the global 
# environment rather than a local function due to a typo
remove( list=ls() )

##### Source files needed for project #####
source( "SimulatePatientOutcome.r")
source( "SimulateTrial.R")
source( "AnalysisMethods.R")
source( "Functions.R")
source( "Randomizer.r")
source( "StoppingRules.R")

#####  Trial Design - Input Parameters #####
# Define the input variables.  Helpful to define variables so they can easily be modified without
# changing values in the program

nMaxQtyOfPats       <- 200      # The maximum quantity of patients to enroll in the study
nMinQtyOfPats       <- 20       # The minimum number of patients enrolled before the trail adapts or stops for futility/superiority
vQtyPatsPerMonth    <- c( 1, 1.5, 2, 3, 5, 7, 10, 15, 22, 25 )  #Each element represents the expected # of patients recruited, then the recruitment stays 25/month

#Prior for S Q_S ~ Beta( 0.2, 0.8 ) 
dPriorAS        <- 0.2  
dPriorBS        <- 0.8

#Prior for E Q_E ~ Beta( 0.2, 0.8 )
dPriorAE        <- 0.2  
dPriorBE        <- 0.8

# Parameters that impact the design of the study
dPU             <- 0.95     # Pr( Q_E > E_S | data ) > dPU
dMinRandProb    <- 0.1      # Minimum randomization probability to either arm during the adapting phase
dExponent       <- 1.0      # Exponent used to "tune" the randomization; dProbSGrtE = Prob S is "Better" than E
                            # Randomize to S with probability dRandProbTrtS = (dProbSGrtE)^dExponent/( (dProbSGrtE)^dExponent + (1 - dProbSGrtE)^dExponent )
                            # Randomize to E with probability dRandProbTrtE = 1 - dRandProbTrtS      


#####  Simulation Design - Input Parameters #####
# Create the "true" parameter values for a scenario -  for this example we are simulating the null case, eg both treatments
# have the same true response rate.  
dTrueRespRateS  <- 0.2      # A true response rate of 0.2 for S
dTrueRespRateE  <- 0.2      # A true response rate of 0.2 for E

nQtyReps        <- 1000     # The number of virtual trials to simulate

####################################################################################################################################.
#It is often best to simulate a single trial and look at the result many times, before launching a loop with many virtual trial
####################################################################################################################################.
set.seed( 123)
lSimulatedTrial <- SimulateSingleTrial( nMaxQtyOfPats,  nMinQtyOfPats, vQtyPatsPerMonth,  dPriorAS,  dPriorBS, dPriorAE, dPriorBE,  
                                        dPU, dMinRandProb, dExponent,  dTrueRespRateS, dTrueRespRateE  )

#It is often very educational to simulate several trials and plot the randomization probabilities
nQtyPatsEnrolled <- sum( lSimulatedTrial$vQtyPats )
plot( 1:nQtyPatsEnrolled, lSimulatedTrial$vRandProbE, type='l', xlab="Patient", ylab="Randomization Probability E", ylim=c(0,1), xlim=c(1,nMaxQtyOfPats), lwd=2 )
abline( h=c(0.5, dMinRandProb, 1- dMinRandProb), v=20, lty=3)


####################################################################################################################################.

##### Setup Simulation Results #####
# Initialize variables that are used for tracking simulation results
vResults        <- rep( NA, nQtyReps )                  # Which arm is selected, 1 = no arm, 2 = S, 3 = E
mQtyPats        <- matrix( NA, ncol=2, nrow = nQtyReps) # The number of patient on each arm
vStopEarly      <- rep( NA, nQtyReps )                  # Keep track of early stopping; 0 = no early stopping, 1 = early stopping 

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
   
    #Capture the results for the simulated virtual trial 
    vResults[ i ]   <- lSimulatedTrial$nDecision
    mQtyPats[ i, ]  <- lSimulatedTrial$vQtyPats
    vStopEarly[ i ] <- lSimulatedTrial$nEarlyStop
}


#Create simple summaries
PrintSummary( vResults, mQtyPats )
