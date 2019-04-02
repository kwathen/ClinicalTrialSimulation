########## File Header #########################################################################################################################
#   Example 1
#
#   Description: 	This file contains an example of text driven development for testing SimulatePatientOutcome.  This version
#                   utilizes a simple testing approach and in future versions this will be extended to use the testthat package
#
#   Author: J. Kyle Wathen, PhD
#           kylewathen@gmail.com
################################################################################################################################################.


source( "SimulatePatientOutcome.R" )

nQtyTestsPassed     <- 0
nQtyTests           <- 0

# Test 1 ######################################################
nQtyTests           <- nQtyTests + 1
nTreatment          <- -1
dTrueRespRateS      <- 0.0  
dTrueRespRateE      <- 0.99
nExpectedOutcome    <- NA 

nRet <- SimulatePatientOutcome(  nTreatment, dTrueRespRateS, dTrueRespRateE)
if( is.na( nRet ) ) 
{
    nQtyTestsPassed <- nQtyTestsPassed + 1
}



# Test 2 ######################################################
nQtyTests           <- nQtyTests + 1
nTreatment          <- 0       # Treatment S (0)
dTrueRespRateS      <- 0.00    # Due to low response rate would expected the simulated patient outcome to be 0
dTrueRespRateE      <- 1.0   
nExpectedOutcome    <- 0 

nRet <- SimulatePatientOutcome(  nTreatment, dTrueRespRateS, dTrueRespRateE)
if( nExpectedOutcome == nRet ) 
{
    nQtyTestsPassed <- nQtyTestsPassed + 1
}



# Test 3 ######################################################
nQtyTests           <- nQtyTests + 1
nTreatment          <- 1       # Treatment E (1)
dTrueRespRateS      <- 0.00    # Due to high response rate would expected the simulated patient outcome to be 1
dTrueRespRateE      <- 1.0   
nExpectedOutcome    <- 1 

nRet <- SimulatePatientOutcome(  nTreatment, dTrueRespRateS, dTrueRespRateE)
if( nExpectedOutcome == nRet ) 
{
    nQtyTestsPassed <- nQtyTestsPassed + 1
}


print( paste( "Test SimulatePatientOutcome: Number of Tests: ", nQtyTests, ", Number of tests passed: ", nQtyTestsPassed ))