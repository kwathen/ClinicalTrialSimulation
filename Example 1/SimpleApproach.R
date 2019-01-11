################################################################################################################################################
#
#   Description:        This file is to demonstrate how Example 1 could be done in a much simpler fashion but it would be difficult
#                       to extend this version the accomodate all the adaptive features planned.  
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
################################################################################################################################################

#####################################################################################################################################
#   Define any functions needed
#####################################################################################################################################

#####################################################################################################################################
#    Name:  		Inequality Calculator – Compare distributions of various types, Beta
#    Keywords:  	Inequality calculator, compare posteriors, posterior probability
#    Description:   Suppose you have q1 ~ Beta(a, b), q2 ~ Beta( c, d) and you want to calculate Pr( q1 > q2 ).  
#
#    R code was coppied from AnalysisModthods.R
#####################################################################################################################################

# Compare 2 Beta distributions
# calculate the probability that one beta dist. is greater than another
IneqCalcBeta <- function(dA1,dB1,dA2,dB2) 
{
    ## 
    
    res <- integrate(fBetaIneqCalc,0,1, dA1 = dA1, dB1 = dB1, dA2 = dA2, dB2 = dB2)
    res$value
}

#Helper functions 
fBetaIneqCalc <- function(x, dA1, dB1, dA2, dB2){x**(dA1-1) * (1-x)**(dB1-1) * pbeta(x,dA2,dB2) / beta(dA1,dB1)}


# This version is implemented to allow apply to be used rather than looping through each virual trial
IneqCalcBetaVect <- function( vParams )
{
    return( IneqCalcBeta( vParams[1], vParams[2], vParams[3], vParams[4]) )
}


#####################################################################################################################################
# Start the simple approach for simulating example 1
#####################################################################################################################################

nMaxQtyOfPats   <- 200      # The maximum quantity of patients to enrol in the study that will be fairly randomized

#Priors: Q_S ~ Beta( 0.2, 0.8 ); Q_E ~ Beta( 0.2, 0.8 )
dPriorAS        <- 0.2  
dPriorBS        <- 0.8

dPriorAE        <- 0.2  
dPriorBE        <- 0.8

# Decision criteria  At the end of the study E will be selected if
# Pr( Q_E > E_S | data ) > dPU
dPU             <- 0.90   


#Create the "true" parameter values for a scenario
dTrueRespRateS  <- 0.2      # A true response rate of 0.2 for S
dTrueRespRateE  <- 0.4      # A true response rate of 0.4 for E

nQtyReps        <- 1000     # The number of virtual trials to simulate

#Simulate the number of patients on each treatment
vQtyPatsE       <- rbinom( nQtyReps, nMaxQtyOfPats, 0.5 )   #Fairly randomize patients, each element in the vector represents 
                                                            #the number of patient receiving E in the virtual trial
vQtyPatsS       <- nMaxQtyOfPats - vQtyPatsE

#Simulate the number of responses
vRespE          <- rbinom( rep(1, nQtyReps), vQtyPatsE, rep( dTrueRespRateE, nQtyReps) )
vFailE          <- vQtyPatsE - vRespE

vRespS          <- rbinom( rep(1, nQtyReps), vQtyPatsS, rep( dTrueRespRateS, nQtyReps) )
vFailS          <- vQtyPatsS - vRespS

#For the Beta-Binomial model the posterior is Beta( dPriorA + #success, dPriorB + # of failures)
vPostAS         <- dPriorAS + vRespS
vPostBS         <- dPriorBS + vFailS

vPostAE         <- dPriorAE + vRespE
vPostBE         <- dPriorBE + vFailE

mPostParams     <- cbind( vPostAE, vPostBE, vPostAS, vPostBS )  #The order combined into columns means we will calculate Pr( Q_E > E_S | data ) 
vPostProbs      <- apply( mPostParams, 1, IneqCalcBetaVect )
#Note - if you are not familar with apply, this could be accomplised via a for or repeat loop

#Summarize based on vPostProbs
vProbSelE       <- mean( ifelse( vPostProbs > dPU, 1, 0 ))
vProbSelS       <- mean( ifelse( ( 1 -vPostProbs ) > dPU, 1, 0 ))
vProbNoTrt      <- 1.0 - vProbSelE  - vProbSelS


print( paste( "The probability the trial will select no treatment is ", vProbNoTrt ))
print( paste( "The probability the trial will select S is ", vProbSelS ))
print( paste( "The probability the trial will select E is ", vProbSelE ))

print( paste("The average number of patient on S is ", mean( vQtyPatsS )))
print( paste("The average number of patient on E is ", mean( vQtyPatsE )))