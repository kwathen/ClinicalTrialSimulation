#####################################################################################
#   Simulate a month of recruitment times   
#   Input:
#       dPatsPerMonth - the expected number of patients accrued in the month
#       dStartMonth - The month you want recruitments to be in.  The return vector
#           of recruitment times will in be in [dStartMonth, dStartMonth + 1]
#####################################################################################

SimulateAMonthOfAccrualTimes <- function( dPatsPerMonth , dStartMonth )
{
    # All we know is the rate so using a Poisson to generate the number of patients
    # that are accrued this month.  Note, this does not guarantee that nQtyPats will be
    # Enrolled we just don't want to simulate way more than needed in the subsequent lines, but to make 
    # we simulate enough times from the exponential we increase the number by 20% (eg multiply by 1.2)
    nQtyPats    <- 1.2 * qpois(0.9999,dPatsPerMonth)
    
    vTimes      <- cumsum( rexp( nQtyPats, dPatsPerMonth ) )
    vTimes      <- vTimes[ vTimes < 1 ]
    vTimes      <- vTimes + dStartMonth
    return( vTimes )
    
}

SimulateArrivalTimes <- function( vPatsPerMonth, nMaxQtyPats )
{ 
    vTimes <- c()
    if( length( vPatsPerMonth ) == 1 )
    {
        #There is a constant rate, eg no ramp up in accrual
        vTimes <- cumsum(rexp(nMaxQtyPats ,vPatsPerMonth))
    }
    else 
    {
        #There is a ramp up in accrual.  
        #General idea: SimulateAMonthOfAccrualTimes will simulate 1 month of accrual and a loop will keep concatenate the results
        dStartMonth <- 0
        nMonth      <- 1
        while( length( vTimes ) < nMaxQtyPats  )
        {
                
            vTimes      <- c( vTimes, SimulateAMonthOfAccrualTimes( vPatsPerMonth[ nMonth ], dStartMonth ))
            dStartMonth <- dStartMonth + 1
            
            if( nMonth < length( vPatsPerMonth ) )
                nMonth <- nMonth +  1
        }
        vTimes <- vTimes[ 1:nMaxQtyPats ]
        
        
    }
    
    return( vTimes )
    
}


#####################################################################################
#   Function print out basic simulation results
#####################################################################################

PrintSummary <- function( vResults, mQtyPats, vStopEarly  )
{
    nQtyReps <- length( vResults )
    
    #Compute some simple summaries.  This code block could be easily abstracted to a function to print result.
    # By creating the ComputeOC() function to compute the summaries it will be easier to test and use from other places
    print( paste( "The probability the trial will select no treatment is ", length( vResults[ vResults == 1])/ nQtyReps))
    print( paste( "The probability the trial will select S is ", length( vResults[ vResults == 2])/ nQtyReps))
    print( paste( "The probability the trial will select E is ", length( vResults[ vResults == 3])/ nQtyReps))
    print( paste( "The probability the trial will stop early is ", length( vStopEarly[ vStopEarly == 1] )/ nQtyReps))

    
    vAveQtyPats      <- round( apply( mQtyPats , 2, mean), 1)
    vConfInt1        <- round( quantile( mQtyPats[,1], c( 0.025, 0.975)), 1)
    vConfInt2        <- round( quantile( mQtyPats[,2], c( 0.025, 0.975)), 1)
    vTotalSampleSize <- mQtyPats[,1] + mQtyPats[,2]
    dMeanSampleSize  <- round( mean( vTotalSampleSize), 1 ) 
    vConfIntSS       <- round( quantile( vTotalSampleSize, c(0.025, 0.975)), 1)
    
    print( paste("The average number of patient on S is ", vAveQtyPats[1], " ( ", vConfInt1[1], ", ", vConfInt1[2], " )", sep=""))
    print( paste("The average number of patient on E is ", vAveQtyPats[2], " ( ", vConfInt2[1], ", ", vConfInt2[2], " )", sep=""))
    print( paste("The average number of patient enrolled and 95% CI are ", dMeanSampleSize, " ( ", vConfIntSS[1], ", ", vConfIntSS[2], " )", sep=""))
    
    
    PlotSampleSizeDist( mQtyPats )
}

# Function to compute the operating characteristics 
ComputeOC <- function( vResults, mQtyPats, vStopEarly )
{
    nQtyReps <- length( vResults )
    
    
    dProbNoTreatment <- length( vResults[ vResults == 1])/ nQtyReps
    dProbSelectS     <- length( vResults[ vResults == 2])/ nQtyReps
    dProbSelectE     <- length( vResults[ vResults == 3])/ nQtyReps
    dProbStopEarly   <- length( vStopEarly[ vStopEarly == 1] )/ nQtyReps
    
    #Compute some simple summaries.  
    vAveQtyPats      <- round( apply( mQtyPats, 2, mean), 1)
    vConfInt1        <- round( quantile( mQtyPats[,1], c( 0.025, 0.975)), 1)
    vConfInt2        <- round( quantile( mQtyPats[,2], c( 0.025, 0.975)), 1)
    vTotalSampleSize <- mQtyPats[,1] + mQtyPats[,2]
    dMeanSampleSize  <- round( mean( vTotalSampleSize), 1 ) 
    vConfIntSS       <- round( quantile( vTotalSampleSize, c(0.025, 0.975)), 1)
    return( list( dProbNoTreatment = dProbNoTreatment, 
                  dProbSelectS     = dProbSelectS, 
                  dProbSelectE     = dProbSelectE, 
                  dProbStopEarly   = dProbStopEarly, 
                  dAveQtyPatsS     = vAveQtyPats[1], 
                  dAveQtyPatsE     = vAveQtyPats[2],
                  dAveQtyPatsSLL   = vConfInt1[1],
                  dAveQtyPatsSUL   = vConfInt1[2],
                  dAveQtyPatsSUL   = vConfInt2[1],
                  dAveQtyPatsSUL   = vConfInt2[2],
                  dMeanSampleSize  = dMeanSampleSize))
    
}



