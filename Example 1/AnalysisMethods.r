################################################################################################################################################
#
#   Example 1
#
#   Description: This file contains functions for running any analysis. 
#
#   Author: J. Kyle Wathen, PhD
#           kylewathen@gmail.com
################################################################################################################################################

RunAnalysis <- function(  dCurrentTime, vPatOutcome, vTreat, vObsTime, dPriorAS,dPriorBS, dPriorAE, dPriorBE  )
{
    #Set the posterior parameters = priors parameters
    dPostAS <- dPriorAS
    dPostBS <- dPriorBS
    
    dPostAE <- dPriorAE
    dPostBE <- dPriorBE
    
    # Need to loop through the data and update the prior parameters to the posterior parameters.
    # Typically, if building the dataset for analysis is more complicated it is best to create a function to perform that task.
    nQtyPats <- length( vPatOutcome )
    for( iPat in 1:nQtyPats )
    {
        #We only want to include patient outcomes that were observed prior to dCurrentTime
        #In Example 1 this is less important because we only want to run the analysis after all patient outcomes are observed.
        if( vObsTime[ iPat ] <= dCurrentTime )
        {
            if( vTreat[ iPat ] == 0 )  #Treatment S
            {
                dPostAS <- dPostAS + vPatOutcome[ iPat ]
                dPostBS <- dPostBS + (1 - vPatOutcome[ iPat ])
                
            }
            else if( vTreat[ iPat ] == 1 )  #Treatment E
            {
                dPostAE <- dPostAE + vPatOutcome[ iPat ]
                dPostBE <- dPostBE + (1 - vPatOutcome[ iPat ])
                
            }
            else
                stop( paste( "Error: In function RunAnalysis an invalid value in vTreat of ", vTreat[ iPat ], " was sent into the function. ") )
        }  
    }
  
    
    dProbSGrtE <- ProbabilityX1GreaterX2( dPostAS, dPostBS, dPostAE, dPostBE )
    return( dProbSGrtE )
}

#####################################################################################################################################
#    Name:  		ProbabilityX1GreaterX2 – Compare distributions of various types, Beta
#    Keywords:  	Inequality calculator, compare posteriors, posterior probability
#    Description:   Suppose you have q1 ~ Beta(a, b), q2 ~ Beta( c, d) and you want to calculate Pr( q1 > q2 ).  
#
#####################################################################################################################################

# Compare 2 Beta distributions
# calculate the probability that one beta dist. is greater than another
ProbabilityX1GreaterX2 <- function( dA1, dB1, dA2, dB2 ) 
{
    ## 
    
    res <- integrate(fBetaIneqCalc,0, 1, dA1 = dA1, dB1 = dB1, dA2 = dA2, dB2 = dB2)
    res$value
}

#Helper functions 
fBetaIneqCalc <- function(x, dA1, dB1, dA2, dB2){x**(dA1-1) * (1-x)**(dB1-1) * pbeta(x,dA2,dB2) / beta(dA1,dB1)}
