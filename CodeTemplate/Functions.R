################################################################################################################################################
#
#   Code Template
#
#   Description:    Include any non-specific functions here.  
#   
#   Author: J. Kyle Wathen, PhD
#           kylewathen@gmail.com
################################################################################################################################################

#Simulate patient arrival times in months (basic version)
SimulateArrivalTimes <- function( dPatsPerMonth, nMaxQtyPats )
{ 
    vTimes <- cumsum(rexp(nMaxQtyPats ,dPatsPerMonth))
    return( vTimes )
    
}


MakeDecision <- function(  )
{

}

