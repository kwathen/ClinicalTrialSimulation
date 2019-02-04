
#####################################################################################
#   Function to check the stopping rule.  Stopping rule is only applied
#   after the nMinQtyOfPats is enrolled.
#  
#   Input:
#####################################################################################
CheckStoppingRule <- function( nMinQtyOfPats, dPU, nQtyOfPats, dProbSGrtE )
{
    nDecision <- 1  
    if( nQtyOfPats > nMinQtyOfPats )
    {
        nDecision <- MakeDecision( dPU, dProbSGrtE, 1-dProbSGrtE)
    }
    return( nDecision )
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
