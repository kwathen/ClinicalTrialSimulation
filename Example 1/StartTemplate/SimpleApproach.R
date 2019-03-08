####### File Header ################################################################################################################# 
#   Description:        This file is to demonstrate how Example 1 could be done in a much simpler fashion but it would be difficult
#                       to extend this version the accommodate all the adaptive features planned.  
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
#           KyleWathen@gmail.com
#####################################################################################################################################.

#####################################################################################################################################.
#   Define any functions needed
#####################################################################################################################################.

#####################################################################################################################################.
#    Name:  		Inequality Calculator – Compare distributions of various types, Beta
#    Keywords:  	Inequality calculator, compare posteriors, posterior probability
#    Description:   Suppose you have q1 ~ Beta(a, b), q2 ~ Beta( c, d) and you want to calculate Pr( q1 > q2 ).  
#
#    R code was copied from AnalysisMethods.R
#####################################################################################################################################.

# The next line will remove the contents of the work space.  This is done to 
# reduce the likelihood of bugs or inadvertently using a variable in the global 
# environment rather than a local function due to a typo
remove( list=ls() )

# Develop a function to compare 2 Beta distributions
# Assume 
# Q1 ~ Beta( dA1, dB1 )
# Q2 ~ Beta( dA2, dB2 )
# ProbabilityX1GreaterX2  computes the Probability( Q1 > Q2 ) and returns it
ProbabilityX1GreaterX2 <- function(dA1,dB1,dA2,dB2) 
{
    # Add code here to perform this calculation #####
    # Hint: This can be done in by simulating from both distributions and comparing or by integration
}



#####################################################################################################################################.
#     Example 1 - Simple approach for simulating example 1                                                                      #####
#####################################################################################################################################.


# Setup Design Parameters #####
# Design parameters are the variables that determine the design, such as, number of patient, prior parameters and cutoff for decision making

nMaxQtyOfPats   <- 200      # The maximum quantity of patients to enroll in the study that will be equally randomized

#Priors: Q_S ~ Beta( 0.2, 0.8 ); Q_E ~ Beta( 0.2, 0.8 )

# Add code here to define parameters #####

# Decision criteria  At the end of the study E will be selected if
# Pr( Q_E > E_S | data ) > dPU

# Add code here to define cutoff #####



# Setup Simulation Parameters #####
# Create the "true" parameter values for a scenario, such as the true response rate on S or E and the number of virtual trials to simulate

# Add code here to define the parameters #####

# Start Simulation  #####
# Because this is a fixed sample trial we only need to simulate the number of patients in each treatment
# the number of responders/non-responders on each treatment and then compute the posterior probabilities.
# Note: this approach would be much more difficult if the design included frequent monitoring. 

#Simulate the number of patients on each treatment
vQtyPatsE       <- rbinom( nQtyReps, nMaxQtyOfPats, 0.5 )   #Fairly randomize patients, each element in the vector represents 
                                                            #the number of patient receiving E in the virtual trial
vQtyPatsS       <- nMaxQtyOfPats - vQtyPatsE


# Simulate the number of responses on each treatment

# Add code here to simulate responses #####

#For the Beta-Binomial model the posterior is Beta( dPriorA + #success, dPriorB + # of failures)

# Add code here to compute the posteriors based on the above simulated data #####
# Name the vectors for the posteriors vPostAE, vPostBE for E and vPostAS and vPostBS for S to utilize the code below

mPostParams     <- cbind( vPostAE, vPostBE, vPostAS, vPostBS )  #The order combined into columns means we will calculate Pr( Q_E > E_S | data ) 
vPostProbs      <- rep( -1, nQtyReps )


#Go through each element (simulated trial) and compute  Pr( Q_E > E_S | data )
for( nRep in 1:nQtyReps )
{
    vPostProbs[ nRep ] <- ProbabilityX1GreaterX2( vPostAE[ nRep ], vPostBE[ nRep ], vPostAS[ nRep ], vPostBS[ nRep ] )
}


#Summarize based on vPostProbs - eg compute the probability the values were above the cutoff. 
# Note: this approach allows you to try different dPU cutoff values without simulating and computing again.

vProbSelE       <- mean( ifelse( vPostProbs > dPU, 1, 0 ))

# Add code here to compute the probability of select S (vProbSelS) and probability of selecting no treatment (vProbNoTrt)#####

#   Print the Operating Characteristics #####
print( paste( "The probability the trial will select no treatment is ", vProbNoTrt ))
print( paste( "The probability the trial will select S is ", vProbSelS ))
print( paste( "The probability the trial will select E is ", vProbSelE ))

# Add code here to compute the average number of patients on each treatment #####

