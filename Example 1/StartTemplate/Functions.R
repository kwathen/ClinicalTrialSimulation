########### File Header #######################################################################################################################
#   Example 1
#
#   Description: This function contains generic functions used in the simulation
#   
#   Input:  dPatsPerMonth   The expected number of patients accrued each month. Recruitment will follow a Poisson process
#           nQtyPats        The quantity of patient arrival times to simulate
#
#   Author: J. Kyle Wathen, PhD
#           kylewathen@gmail.com
################################################################################################################################################.

SimulateArrivalTimes <- function( dPatsPerMonth, nMaxQtyPats )
{ 
    # Add code here to simulate a vector of time vTime that are the arrival times of the nMaxQtyPats #####
    # Hint: if x1, x2,..., are simulated from an exponential( rate = patients per month) distribution 
    # then the sum( x1...) would represent Poisson process with correct number of patients arriving each time unit
    
    return( vTimes )
    
}



################################################################################################################################################.
#   Input   dPU         Select treatment i if Pr( Q_i > Q_j | data ) > dPU
#           dProbSGrtE  Pr( Q_S > Q_E | data )
#           dProbEGrtS  Pr( Q_E > Q_S | data )
#
#   Return  1 if no treatment is selected
#           2 if E is selected
#           3 if S is selected
################################################################################################################################################.
MakeDecision <- function( dPU, dProbSGrtE, dProbEGrtS )
{
    nDecision <- 1
    
    # Add code here to if a treatment is selected #####
    
    return( nDecision )
}

