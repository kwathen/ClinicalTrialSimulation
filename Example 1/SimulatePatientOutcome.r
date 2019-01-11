################################################################################################################################################
#   Example 1
#
#   Description: 		This file contains functions for simulating the patient data.
#
#   Analysis Method:    Bayesian Beta-Binomial model
#                       Assume patients receiving treatment j = S or E have a response probability Q_j
#                       Prior: Q_j ~ Beta( a_j, b_j )
#
#   Decision Criteria:  At the end of the study E will be selected if
#                       Pr( Q_E > E_S | data ) > dPU
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
