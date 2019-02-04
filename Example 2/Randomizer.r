
#####################################################################################
#   Function to get the treatment for a patient.
#   dProbTrt1 = probability that patients receive treatment 1 (in this case E)
#####################################################################################
GetTreatment <- function( dProbTrt1 )
{
    nTrt <- rbinom( 1, 1, dProbTrt1 )
    return( nTrt )
}

GetTreatmentAdaptiveRandomization <- function( nQtyPats, nMinQtyOfPats,  dProbSGrtE ) 
{
    nTreat     <- NA
    dRandProbE <- 0.5  # We want to keep track of the randomization probabilities 
    if( nQtyPats <= nMinQtyOfPats )
        nTreat     <- GetTreatment( dRandProbE )
    else
    {
        #In this example we randomize to S with probability dProbSGrtE, but we need probability of E for GetTreatment 
        dRandProbE <-  1 - dProbSGrtE
        nTreat     <- GetTreatment( dRandProbE )
        
    }
    return( list( nTrt = nTreat, dRandProbE = dRandProbE)  )
}