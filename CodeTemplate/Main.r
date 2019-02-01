################################################################################################################################################
#   This is the main file for conducting the simulation.
#
#   Code Template
#
#   Description:        This project is a code template for where to start.  It is intended as a template and does not run
#                       as many functions are currently incomplete. 
#
#   Analysis Method:    TBD - For now we plan on a function named RunAnalysis() that will execute the analysis, eg compare posteriors 
#
#   Decision Criteria:  TBD - For now we plan on a function named MakeDecision() that will take in the analysis results and make
#                       a decision about the success/failure of the trial.
#
#   Author: J. Kyle Wathen, PhD
#           kylewathen@gmail.com
################################################################################################################################################

# The next line will remove the contents of the work space.  This is done to 
# reduce the likelihood of bugs or inadvertently using a variable in the global 
# environment rather than a local function due to a typo
remove( list=ls() )

#Source the files in the project - These files just contain the function structures to be implemented
source( "SimulatePatientOutcome.R")
source( "SimulateTrial.R")
source( "AnalysisMethods.R")
source( "Functions.R")
source( "Randomizer.R")


# Define the input variables.  Helpful to define variables so they can easily be modified without
# changing values in the program

nMaxQtyOfPats       <- 200      # The maximum quantity of patients to enroll in the study that will be fairly randomized
dQtyPatsPerMonth    <- 7.5      # Number of patients that will be enrolled each month, expectation is 7.5 patients per month

nQtyReps            <- 10       # The number of virtual trials to simulate

#It is often best to simulate a single trial and look at the result many times, before launching a loop with many virtual trial
lSimulatedTrial <- SimulateSingleTrial( nMaxQtyOfPats,  dQtyPatsPerMonth  )

# Setup the variables that will contain the results of the simulation.
vResults <- rep( NA, nQtyReps )
mQtyPats <- matrix( NA, ncol=2, nrow = nQtyReps)


for( i in 1:nQtyReps )
{
    
    lSimulatedTrial <- SimulateSingleTrial( nMaxQtyOfPats,  dQtyPatsPerMonth )
   
    vResults[ i ]   <- lSimulatedTrial$nDecision
    mQtyPats[ i, ]  <- lSimulatedTrial$vQtyPats
}


#Create simple summaries

print( paste( "The probability the trial will select no treatment is ", length( vResults[ vResults == 1])/ nQtyReps))
print( paste( "The probability the trial will select S is ", length( vResults[ vResults == 2])/ nQtyReps))
print( paste( "The probability the trial will select E is ", length( vResults[ vResults == 3])/ nQtyReps))

vAveQtyPats <- apply( mQtyPats, 2, mean)
print( paste("The average number of patient on S is ", vAveQtyPats[1]))
print( paste("The average number of patient on E is ", vAveQtyPats[2]))
