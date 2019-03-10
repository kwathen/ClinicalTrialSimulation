################################################################################################################################################
#   Example 1
#
#   Description: 	This file contains functions for simulating the patient data.
#
#   Input           nTreat          Treatment the patient received, 0 for S and 1 for E
#                   dTrueRespRateS  The true response rate for S
#                   dTrueRespRateE  The true response rate for E
#
#   Return          nOutcome a 1 if the patient response, 0 otherwise
#
#   Author: J. Kyle Wathen, PhD
#           kylewathen@gmail.com
################################################################################################################################################

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
