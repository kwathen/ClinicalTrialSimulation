################################################################################################################################################
#
#   Example 1
#
#   Description: This file will contain various randomization functions
#
#   Author: J. Kyle Wathen, PhD
#           kylewathen@gmail.com
################################################################################################################################################


#####################################################################################
#   Function to get the treatment for a patient.
#   dProbTrt1 = probability that patients receive treatment 1 (in this case E)
#   Return: 0 for S and 1 for E
#####################################################################################
GetTreatment <- function( dProbTrt1 )
{
    #Note: This function should just return 1 treatment.   In the next versions the dProbTrt1 will change with every patient.
    nTrt <- rbinom( 1, 1, dProbTrt1 )
    return( nTrt )
}