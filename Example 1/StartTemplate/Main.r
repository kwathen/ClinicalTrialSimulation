###### File Header ##############################################################################################################################
#   This is the main file for conducting the simulation.
#
#   Example 1
#
#   Description: 		A fixed sample clinical trial utilizing fair randomization to compare standard of care (S) and experimental (E).
#                   	The primary outcome is binary and is observed at the time a patient is treated.
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
###############################################################################################################################################.

# The next line will remove the contents of the work space.  This is done to 
# reduce the likelihood of bugs or inadvertently using a variable in the global 
# environment rather than a local function due to a typo
remove( list=ls() )

#Source the files in the project
source( "SimulatePatientOutcome.R")
source( "SimulateTrial.R")
source( "AnalysisMethods.R")
source( "Functions.R")
source( "Randomizer.R")



# Setup Design Parameters #####

# Helpful to define variables so they can easily be modified without changing values in the program

# Add code here  to define parameters for the maximum number of patient and expected number of patient accrued each month ####

nMaxQtyOfPats       <- ___      # The maximum quantity of patients to enroll in the study that will be fairly randomized
dQtyPatsPerMonth    <- ___      # Number of patients that will be enrolled each month, expectation is 7.5 patients per month


#Priors: Q_S ~ Beta( 0.2, 0.8 ); Q_E ~ Beta( 0.2, 0.8 )
dPriorAS     <- 0.2  
dPriorBS     <- 0.8

# Add code here  to define prior parameters for E ####
dPriorAE     <- _____
dPriorBE     <- _____

# Decision criteria  At the end of the study E will be selected if
# Pr( Q_E > E_S | data ) > dPU
# Add code here  to define cutoff dPU ####
dPU          <- ______

# Setup Simulation Parameters #####
#Create the "true" parameter values for a scenario

# Add code here to define the parameters #####
dTrueRespRateS <- _____
______________ <- _____   #Hint: Choose a consistent naming convention for the true response rate for E

#It is often best to simulate a single trial and look at the result many times, before launching a loop with many virtual trial
lSimulatedTrial <- SimulateSingleTrial( nMaxQtyOfPats,  dQtyPatsPerMonth,  dPriorAS,  dPriorBS, dPriorAE, dPriorBE,  
                                        dPU, dTrueRespRateS, dTrueRespRateE  )

#Results variables
vResults        <- rep( NA, nQtyReps )                   # Keep track of the decision for each simulated trial
mQtyPats        <- matrix( NA, ncol=2, nrow = nQtyReps)  # Vector to keep track of the number of patients on S and E  

# Start Simulation  #####

for( i in 1:nQtyReps )
{
    
    lSimulatedTrial <- SimulateSingleTrial( nMaxQtyOfPats,  dQtyPatsPerMonth,  dPriorAS,  dPriorBS, dPriorAE, dPriorBE,  
                                            dPU, dTrueRespRateS, dTrueRespRateE  )
   
    
    # Add code here to capture the results based on lSimulatedTrial #####
    
    vResults[ i ]   <- lSimulatedTrial$___________
    mQtyPats[ i, ]  <- lSimulatedTrial$___________
}

#   Print the Operating Characteristics #####
# Add code here to use vResults and mQtyPats to calculate the operating characteristics. 

print( paste( "The probability the trial will select no treatment is ", _____________ ))
print( paste( "The probability the trial will select S is ", ___________ ))
print( paste( "______________ select E is ", _________ ))

vAveQtyPats <- apply( ________, 2, mean)
print( paste("The average number of patient on S is ", _____________ ))
print( paste("The average number of patient on E is ", ______________ ))



