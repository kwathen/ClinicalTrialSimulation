
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



#  Return the decision based on the posterior probabilities.
#	Returns: 1--> No treatment selected, 2 --> S was selected, 3 --> E was selected
MakeDecision <- function( dPU, dProbSGrtE, dProbEGrtS )
{
    nDecision <- 1
    if( dProbSGrtE > dPU )
        nDecision <- 2
    else if( dProbEGrtS > dPU )
        nDecision <- 3
    return( nDecision )
}
