########################################################################
#   This file contains functions for simulating the patient data.
########################################################################
SimulatePatientOutcome <- function( nTreat, dTrueRespRateS, dTrueRespRateE )
{
    nOutcome <- NA
    if( nTreat == 0 )  # Patient received S
        nOutcome <- rbinom(1, 1, dTrueRespRateS )
    else if( nTreat == 1 )# Patient received E
        nOutcome <- rbinom(1, 1, dTrueRespRateE )
    else #There was an error in the input
        stop( paste( "Error: In function SimulatePatientOutcome an invalid nTeat = ", nTeat, " was sent into the function.  nTreat must be 0 or 1.") )
    
    return( nOutcome )
}

########################################################################
#   This function simulates the time the patient outcomes are observed
########################################################################
SimulateOutcomeObservedTime <- function( vStartTime )
{
    #Note: A better practice would be to call this functions with inputs for the mean and variance
    #Simulate the average 2 months from treatment to the time the outcome is observed
    vTimeToOutcome <- rnorm( length( vStartTime ), 2, 0.3 )
    
    vObsTime <- vStartTime  + vTimeToOutcome
    return( vObsTime )
}