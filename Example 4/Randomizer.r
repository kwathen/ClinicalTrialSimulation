
#####################################################################################################################
#   Function to get the treatment for a patient.
#   dProbTrt1 = probability that patients receive treatment 1 (in this case E)
#####################################################################################################################
GetTreatment <- function( dProbTrt1 )
{
    nTrt <- rbinom( 1, 1, dProbTrt1 )
    return( nTrt )
}

#####################################################################################################################
#   Function to get the treatment for a patient.
#   Description: 
#       This randomizer utilizes the probability that the standard treatment (S)
#       is "better" than the experimental (E) to adapt the randomization probability
#       away from equal randomization.  Allows for an exponent to tune the randomization
#       and allows for the randomization probabilities to be constrained to an interval
#
#   Input:
#       dMinRandProb: the minimum randomization probability that is desired.
#           All randomization probabilities will be in the range [ dMinRandProb, 1-dRandProb ]
#       dExponent:  Randomization Probability to S (dRandProbTrtS)
#           dRandProbTrtS = (dProbSGrtE)^dExponent/( (dProbSGrtE)^dExponent + (1 - dProbSGrtE)^dExponent )
#           dRandProbTrtE = 1 - dRandProbTrtS
#       nQtyPats: The quantity of patients in trial (must be >= nMinQtyOfPats before adapting)
#       nMinQtyOfPats: The minimum quantity of patients before adapting begins.
#
#   Return:
#   dProbTrt1 = probability that patients receive treatment 1 (in this case E)
#####################################################################################################################
GetTreatmentAdaptiveRandomization <- function( dMinRandProb, dExponent, nQtyPats, nMinQtyOfPats,  dProbSGrtE ) 
{
  
    nTreat     <- NA
    dRandProbE <- 0.5  # We want to keep track of the randomization probabilities 
    if( nQtyPats <= nMinQtyOfPats )
        nTreat     <- GetTreatment( dRandProbE )
    else
    {
        #In this example we randomize to S with probability dProbSGrtE, but we need probability of E for GetTreatment 
        #We also need to implement the use of dExponent and dMinRandProb 
        dRandProbS <- (dProbSGrtE^dExponent) / ( dProbSGrtE^dExponent + (1 - dProbSGrtE )^dExponent )
        
        # The following 2 lines make sure that the randomization probabilities are in the interval [dMinRandProb, 1-dMinRandProb]
        # This could be accomplished in 1 line combining but is less readable and more difficult to follow.  
        dRandProbS <- max( dMinRandProb, dRandProbS)
        dRandProbS <- min( 1-dMinRandProb, dRandProbS)
        dRandProbE <- 1 - dRandProbS 
        nTreat     <- GetTreatment( dRandProbE )
        
    }
    return( list( nTrt = nTreat, dRandProbE = dRandProbE)  )
}
