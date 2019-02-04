##### Description ########################################################################
#  This function will take in a virtual trial and plot visualizations of the trial.
##### Description ########################################################################.

PlotVirtualTrial <- function( lVirtualTrial, nTrialID = NA )
{
   
  
    dfVT <- data.frame( lVirtualTrial )
    
    strTrialID  <- ""
    if( ! is.na( nTrialID ) )
        strTrialID <- paste("\nTrial ID ", nTrialID )
    ##### Plot Enrollment #####
    # Plot the trial enrollment and available data
    gPlot <- ggplot( dfVT, aes( y= vPatientIndex)) + ylim(0,nMaxQtyOfPats)
    gPlot <- gPlot + theme_minimal()  + theme(legend.position = "top")
    gPlot <- gPlot + geom_line( aes(  x = vStartTime, colour="Enrollment"))
    gPlot <- gPlot + geom_line( aes(  x = sort(vObsTime), colour="Outcome"))
    gPlot <- gPlot + labs(title=paste("Virtual Trial Enrollment",strTrialID), x= "Time (Months)", y="Number of Patients")
    gPlot <- gPlot + geom_hline( yintercept = c( nMinQtyOfPats), linetype="dotted")
    gPlot <- gPlot + scale_colour_manual("", 
                                         breaks = c("Enrollment", "Outcome"),
                                         values = c("black", "green")) 
    gPlot <- gPlot + geom_vline( xintercept=max( dfVT$vStartTime), linetype ="dotted")
    print( gPlot )
    

    ##### Plot Enrollment - Zoomed in #####
    # Plot the trial enrollment and available data
    gPlot <- ggplot( dfVT, aes( y= vPatientIndex)) 
    gPlot <- gPlot + theme_minimal( ) + theme(legend.position = "top")
    gPlot <- gPlot + ylim(0, max(dfVT$vPatientIndex))
    gPlot <- gPlot + geom_line( aes(  x = vStartTime, colour="Enrollment"))
    gPlot <- gPlot + geom_line( aes(  x = sort(vObsTime), colour="Outcome"))
    gPlot <- gPlot + labs(title=paste("Virtual Trial Enrollment",strTrialID), x= "Time (Months)", y="Number of Patients")
    gPlot <- gPlot + geom_hline( yintercept = c( nMinQtyOfPats), linetype="dotted")
    gPlot <- gPlot + scale_colour_manual("", 
                                         breaks = c("Enrollment", "Outcome"),
                                         values = c("black", "green")) 
    gPlot <- gPlot + geom_vline( xintercept=max( dfVT$vStartTime), linetype ="dotted")
    print( gPlot )
    
    ##### Plot # of Patients on Each Treatment #####
    # Plot the number of patients on each treatment
    gPlot <- ggplot( dfVT, aes( x= vStartTime)) 
    gPlot <- gPlot + theme_minimal() + theme(legend.position = "top")
    gPlot <- gPlot + ylim(0, max(dfVT$vPatientIndex))
    gPlot <- gPlot + geom_step( aes(  y = vQtyPatsOnE, colour="# on E"))
    gPlot <- gPlot + geom_step( aes(  y = vQtyPatsOnS, colour="# on S"))
    gPlot <- gPlot + labs(title=paste("Virtual Trial Randomization",strTrialID), x= "Time (Months)", y="Number of Patients")
    gPlot <- gPlot + geom_hline( yintercept = c( nMinQtyOfPats), linetype="dotted")
    gPlot <- gPlot + scale_colour_manual("", 
                                         breaks = c("# on E", "# on S"),
                                         values = c("black", "green")) 
    gPlot <- gPlot + geom_vline( xintercept=max( dfVT$vStartTime), linetype ="dotted")
    print( gPlot )
    
    ##### Plot Randomization Probability #####
    # Plot the trial enrollment and available data
    gPlot <- ggplot( dfVT,  aes( x= vPatientIndex, y = vRandProbE) ) + xlim(0,lVirtualTrial$nMaxQtyOfPats) + ylim(0,1)
    gPlot <- gPlot + geom_line(colour="black") + geom_point( colour="blue")
    gPlot <- gPlot + geom_hline( yintercept = c( dMinRandProb, 1-dMinRandProb, 0.5), linetype="dashed")
    gPlot <- gPlot + theme_minimal()  + theme(legend.position = "top")
    gPlot <- gPlot + geom_vline( xintercept = c( nMinQtyOfPats), linetype="dotted")
    gPlot <- gPlot + labs(title= paste("Randomization Probabilities",strTrialID), x="Patient Number", y="Randomization Probability for E")
    print( gPlot )
    
    ##### Plot Pr( Q_E > Q_S | data) #####
    # Plot the trial enrollment and available data
    vTmpIndx        <- dfVT$vPatientIndex
    vTmpProbEGrtS   <- dfVT$vProbEGrtS
    if( lVirtualTrial$nEarlyStop == 1 )  # If the trial stopped early add the final Pr( Q_E > Q_S for display)
    {
        vTmpIndx      <- c( vTmpIndx, max(vTmpIndx+1))
        vTmpProbEGrtS <- c( vTmpProbEGrtS, lVirtualTrial$dFinalProbEGrtS )
        dfVT          <- rbind( dfVT, NA)
    }
   
    dfVT$vTmpIndx      <- vTmpIndx
    dfVT$vTmpProbEGrtS <- vTmpProbEGrtS 
    gPlot <- ggplot( dfVT,  aes( x= vTmpIndx, y =vTmpProbEGrtS) ) + xlim(0,lVirtualTrial$nMaxQtyOfPats) 
    gPlot <- gPlot + geom_line(colour="black") + geom_point( colour="blue")
    gPlot <- gPlot + geom_hline( yintercept = c( dPU, 1-dPU, 0.5), linetype="dashed")
    gPlot <- gPlot + theme_minimal()  + theme(legend.position = "top")
    gPlot <- gPlot + geom_vline( xintercept = c( nMinQtyOfPats), linetype="dotted")
    gPlot <- gPlot + labs(title= paste("Prob( Q_E > Q_S | Data) ",strTrialID), x="Patient Number", y="Prob( Q_E > Q_S | Data)")
    gPlot <- gPlot + annotate("text", x=100, y=0, label = "Stop Select S", colour="Red")
    gPlot <- gPlot + annotate("text", x=100, y=1, label = "Stop Select E", colour="Red")
    gPlot <- gPlot + geom_ribbon(  ymin = c(-0.05), ymax = 1-lVirtualTrial$dPU, alpha=0.2,
                                   x=c(lVirtualTrial$nMinQtyOfPats,rep(lVirtualTrial$nMaxQtyOfPats,length(vTmpIndx)-1)))
    gPlot <- gPlot + geom_ribbon(  ymax = 1.05, ymin = lVirtualTrial$dPU, alpha=0.2,
                                   x=c(lVirtualTrial$nMinQtyOfPats,rep(lVirtualTrial$nMaxQtyOfPats,length(vTmpIndx)-1)))
    print( gPlot )
    
    
}

