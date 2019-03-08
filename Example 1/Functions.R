################################################################################################################################################.
#
#   Example 1
#
#   Description: This function contains generic functions used in the simulation
#
#   Author: J. Kyle Wathen, PhD
#           kylewathen@gmail.com
################################################################################################################################################.

SimulateArrivalTimes <- function( dPatsPerMonth, nMaxQtyPats )
{ 
    vTimes <- cumsum(rexp(nMaxQtyPats ,dPatsPerMonth))
    return( vTimes )
    
}


MakeDecision <- function( dPU, dProbSGrtE, dProbEGrtS )
{
    nDecision <- 1
    if( dProbSGrtE > dPU )
        nDecision <- 2
    else if( dProbEGrtS > dPU )
        nDecision <- 3
    return( nDecision )
}

