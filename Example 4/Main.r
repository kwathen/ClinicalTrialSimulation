####### File Header####################################################################################################
#   This is the main file for conducting the simulation.
#
#   Example 4
#
#   Description: 		A clinical trial  to compare standard of care (S) and experimental (E).	 
#                       The primary outcome is binary and is observed at the time a patient is treated.
#                       This example code base is designed to allow users to simulate a range of design options
#                       ranging from a fixed sample design to a continusuly monitored, Bayesain outcome adaptively 
#                       randomized study with early selection for futility/success.  See Detials for more information.
#
#   Requirements:       R >  V3.5; ggplot2
#                       
#
#   Analysis Method:    Bayesian Beta-Binomial model
#                       Assume patients receiving treatment j = S or E have a response probability Q_j
#                       Prior: Q_j ~ Beta( a_j, b_j )
#
#   Decision Criteria:  At any decision point in the study, treatment j= S or E will be selected if
#                       Pr( Q_j > Q_{i!= j} | data ) > dPU
#                       Otherwise, no treatment is selected such that at IA the trial continues and at the FA
#                       the trial would select no treatment.
#
#   Design Parameters   nMaxQtyOfPats: The maximum number of patients in the trial
#                       nMinQtyOfPats: The minimum number of patients before the trial can stop.  nMinQtyOfPats patients 
#                                       are randomized fairly before altering the randomization
#                       dPriorAS, dPriorBS: Prior parameters for S: Q_S ~ Beta( dPriorAS, dPriorBS ) 
#                       dPriorAE, dPriorBE: Prior parameters for E: Q_E ~ Beta( dPriorAE, dPriorBE ) 
#                       dPU: Decision criteria parameter, 
#                           Stop if Pr( Q_E > Q_S | data ) > dPU --> Select E or  Pr( Q_S > Q_E | data ) > dPU --> Select S
#                       dMinRandProb: Ranomization proabiltiy is constranined between (dMinRandProb, 1-dMinRandProb)
#                       dExponent: Adaptive randomization tunning parameter
#                           Randomize to S with probability dRandProbTrtS = (dProbSGrtE)^dExponent/( (dProbSGrtE)^dExponent + (1 - dProbSGrtE)^dExponent )
#                           Randomize to E with probability dRandProbTrtE = 1 - dRandProbTrtS     
#
#   Details:            The Trial Design - Input Parameters code section block containes the trial design parameters.
#                       By alterting the trial design parameters a wide range of designs can be simulated. 
#                       
#                       Examples Trial Designs
#                       Design 1 - Fixed sample, no early stopping, equal randomization for entire trial
#                           By setting the minimum number of patients equal to the max number of patients no addaptions occur, but
#                           you can still plot virtual trials to see posterior proabilities.
#                           nMinQtyOfPats <- nMaxQtyOfPats 
#
#                       Design 2 - Utilize adaptive randomization (AR), no early stopping
#                           Set nMinQtyOfPats less than nMaxQtyOfPats
#                           dExponent     <- 1.0   #Any value > 0 will utilize AR
#                           dPU           <- 1.0   #Setting to 1 prevents early stopping
#
#                       Design 3 - Utilize adaptive randomization, adapting randomization quickly, no early stopping
#                           Set nMinQtyOfPats less than nMaxQtyOfPats
#                           dPU           <- 1.0   #Setting to 1 prevents early stopping
#                           dExponent     <- 2.0   # Adapt the randomization quickly, use dExponent in (0,1) to adapt less quickly
#
#                       Design 4 - Require 20 patients before adaptive randomization, no early stopping
#                           Set nMinQtyOfPats less than nMaxQtyOfPats
#                           dPU           <- 1.0   #Setting to 1 prevents early stopping
#                           nMinQtyOfPats <- 20
#                           dExponent     <- 1.0  
#
#                       Design 5 - Require 20 patients before adaptive randomization, 
#                                   stop early if there is 95% chance one treatment is the best
#                           Set nMinQtyOfPats less than nMaxQtyOfPats
#                           dPU           <- 1.0   #Setting to 1 prevents early stopping
#                           nMinQtyOfPats <- 20
#                           dExponent     <- 0.95    
#
#                       Design 6 - Augumented control, utilize prior data on S ( N=1000, # responders = 201), 
#                                   Require 20 patients before adaptive randomization, 
#                                   stop early if there is 95% chance one treatment is the best
#                           Set nMinQtyOfPats less than nMaxQtyOfPats
#                           dPU           <- 1.0   #Setting to 1 prevents early stopping
#                           nMinQtyOfPats <- 20
#                           dExponent     <- 0.95  
#
#                           #Prior option 1 - include all data into prior for S
#                           dPriorAS      <- 205  
#                           dPriorBS      <- 795
#
#                           #Prior option 2 - include 10% of data into prior for S
#                           dPriorAS      <- 20.5  
#                           dPriorBS      <- 79.5
#                       
#           
#
#   Author: J. Kyle Wathen, PhD
#           kylewathen@gmail.com
######################################################################################################################.

# The next line will remove the contents of the work space.  This is done to 
# reduce the likelihood of bugs or inadvertently using a variable in the global 
# environment rather than a local function due to a typo
remove( list=ls() )

##### Source files needed for project #####
source( "SimulatePatientOutcome.R")
source( "SimulateTrial.R")
source( "AnalysisMethods.R")
source( "Functions.R")
source( "Randomizer.R")
source( "StoppingRules.R")
source( "PlotVirtualTrial.R" )

library(ggplot2)

#####  Trial Design - Input Parameters #####
# Define the input variables.  Helpful to define variables so they can easily be modified without
# changing values in the program

nMaxQtyOfPats       <- 200      # The maximum quantity of patients to enroll in the study
nMinQtyOfPats       <- 200      # The minimum number of patients enrolled before the trail adapts or stops for futility/superiority

#Prior for S Q_S ~ Beta( 0.2, 0.8 ) 
dPriorAS            <- 0.2  
dPriorBS            <- 0.8

#Prior for E Q_E ~ Beta( 0.2, 0.8 )
dPriorAE            <- 0.2  
dPriorBE            <- 0.8

# Parameters that impact the design of the study
#For first run of example use 0.95
dPU                 <- 0.95     # Stop if Pr( Q_E > Q_S | data ) > dPU --> Select E or  Pr( Q_S > Q_E | data ) > dPU --> Select S
dMinRandProb        <- 0.1      # Minimum randomization probability to either arm during the adapting phase
dExponent           <- 1.0      # Exponent used to "tune" the randomization; dProbSGrtE = Prob S is "Better" than E
                                # Randomize to S with probability dRandProbTrtS = (dProbSGrtE)^dExponent/( (dProbSGrtE)^dExponent + (1 - dProbSGrtE)^dExponent )
                                # Randomize to E with probability dRandProbTrtE = 1 - dRandProbTrtS      


#####  Simulation Design - Input Parameters #####
# Create the "true" parameter values for a scenario -  for this example we are simulating the null case, eg both treatments
# have the same true response rate.  
dTrueRespRateS      <- 0.2      # A true response rate of 0.2 for S
dTrueRespRateE      <- 0.2      # A true response rate of 0.2 for E

vQtyPatsPerMonth    <- c( 1, 1.5, 2, 3, 5, 7, 10, 15, 22, 25 )  #Each element represents the expected # of patients recruited, then the recruitment stays 25/month

nQtyReps            <- 500    # The number of virtual trials to simulate

####################################################################################################################################.
#It is often best to simulate a single trial and look at the result many times, before launching a loop with many virtual trials
####################################################################################################################################.
set.seed( 123 )
nTrialID <- 1

lSimulatedTrial <- SimulateSingleTrial( nMaxQtyOfPats,  nMinQtyOfPats, vQtyPatsPerMonth,  dPriorAS,  dPriorBS, dPriorAE, dPriorBE,  
                                        dPU, dMinRandProb, dExponent,  dTrueRespRateS, dTrueRespRateE  )


#It is often very educational to simulate several trials and plot the randomization probabilities
PlotVirtualTrial( lSimulatedTrial , nTrialID )
nTrialID <- nTrialID + 1
####################################################################################################################################.

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
PrintSummary( vResults, mQtyPats )

#Plot a few of the virtual trials
nTrialID <- 4 #Interesting tail becuase it goes from favoring S to favoring E and back to S
PlotVirtualTrial( lVirtualTrials[[ nTrialID ]], nTrialID = nTrialID ) 


nTrialID <- 6 #Interesting tail becuase it goes from favoring S to favoring E and back to S
PlotVirtualTrial( lVirtualTrials[[ nTrialID ]], nTrialID = nTrialID ) 
