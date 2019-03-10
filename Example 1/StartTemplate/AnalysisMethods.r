######## File Header ###########################################################################################################################
#
#   Example 1
#
#   Description: This file contains functions for running any analysis. 
#
#   Input Arguments:
#       dCurrentTime    The current time in the virtual trial
#       vPatOutcome     A vector of patient outcomes, elements are 1 of the patient is a response and 0 otherwise.   
#       vTreat          A vector of the treatment the patient received, 0 for S, 1 for E
#       vObsTime        A vector of the times the outcome are observed, patient outcomes are observed sometime after enrollment
#       dPriorAS, dPriorBS  prior parameters for S where Q_S ~ Beta( dPriorAS, dPriorBS ) a priori 
#       dPriorAE, dPriorBE  prior parameters for S where Q_S ~ Beta( dPriorAE, dPriorBE ) a priori 
#   
#   Return: The Pr( Q_S > Q_E | data )
#   Author: J. Kyle Wathen, PhD
#           kylewathen@gmail.com
################################################################################################################################################.

RunAnalysis <- function(  dCurrentTime, vPatOutcome, vTreat, vObsTime, dPriorAS,dPriorBS, dPriorAE, dPriorBE  )
{
    #Set the posterior parameters = priors parameters then update the parameters
    #Example for S
    dPostAS <- dPriorAS
    dPostBS <- dPriorBS
    
    # Add code here to define posterior parameters for E #####
    
    
    # Need to loop through the data and update the prior parameters to the posterior parameters.
    # Typically, if building the dataset for analysis is more complicated it is best to create a function to perform that task.
   
    nQtyPats <- length( vPatOutcome )
    for( iPat in 1:nQtyPats )
    {
        #We only want to include patient outcomes that were observed prior to dCurrentTime
        #In Example 1 this is less important because we only want to run the analysis after all patient outcomes are observed.
        
        if( vObsTime[ iPat ] <= dCurrentTime )
        {
            #Add code here to update the posterior parameters based on what treatment the patient receives
            #Hint:  
            #if( treatment is S )
            #    Update the posterior parameters of S
            #else
            #   Update the posterior parameters of E
                         
        }  
    }
  
    #Add code here compute dProbSGrtE = Pr( Q_S > Q_E | data ) given the updated posterior parameters
    
    return( dProbSGrtE )
}

#####################################################################################################################################.
#    Name:  		ProbabilityX1GreaterX2 – Compare distributions of various types, Beta
#    Keywords:  	Inequality calculator, compare posteriors, posterior probability
#    Description:   Suppose you have q1 ~ Beta(a, b), q2 ~ Beta( c, d) and you want to calculate Pr( q1 > q2 ).  
#
#####################################################################################################################################.

# Compare 2 Beta distributions
# calculate the probability that one beta dist. is greater than another
ProbabilityX1GreaterX2 <- function( dA1, dB1, dA2, dB2 ) 
{
    ## 
    
    res <- integrate(fBetaIneqCalc,0, 1, dA1 = dA1, dB1 = dB1, dA2 = dA2, dB2 = dB2)
    res$value
}

#Helper functions 
fBetaIneqCalc <- function(x, dA1, dB1, dA2, dB2){x**(dA1 - 1) * (1 - x)**( dB1 - 1) * pbeta(x,dA2,dB2) / beta(dA1,dB1)}
