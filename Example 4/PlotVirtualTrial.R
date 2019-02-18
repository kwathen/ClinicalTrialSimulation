##### Description ########################################################################
#  This function will take in a virtual trial and plot visualizations of the trial.
##### Description ########################################################################.

PlotVirtualTrial <- function( lVirtualTrial, nTrialID = NA, bPlotResults = TRUE )
{
   
  
    dfVT <- data.frame( lVirtualTrial )
    
    strTrialID  <- ""
    if( ! is.na( nTrialID ) )
        strTrialID <- paste("\nTrial ID ", nTrialID )
    
    nMaxQtyOfPats <- dfVT$nMaxQtyOfPats[1]
    nMinQtyOfPats <- dfVT$nMinQtyOfPats[1]
    dMinRandProb  <- dfVT$dMinRandProb[1]
    dPU           <- lVirtualTrial$dPU
    
    lPlots <- list( )
    ##### Plot Enrollment #####
    # Plot the trial enrollment and available data
    gPlot <- ggplot( dfVT, aes( y= vPatientIndex)) + ylim(0,nMaxQtyOfPats)
    gPlot <- gPlot + theme_minimal(base_size=gBaseSize)  + theme(legend.position = "top", plot.title=element_text( hjust=0.5, size=16))
    gPlot <- gPlot + geom_line( aes(  x = vStartTime, colour="Enrollment"), size =1.5)
    gPlot <- gPlot + geom_line( aes(  x = sort(vObsTime), colour="Outcome"), size =1.5)
    gPlot <- gPlot + ggtitle(paste("Virtual Trial Enrollment",strTrialID))
    gPlot <- gPlot + labs( x= "Time (Months)", y="Number of Patients", hajust =0.5)
    gPlot <- gPlot + geom_hline( yintercept = c( nMinQtyOfPats), linetype="dotted")
    gPlot <- gPlot + scale_colour_manual("", 
                                         breaks = c("Enrollment", "Outcome"),
                                         values = c("green","black" )) 
    gPlot <- gPlot + geom_vline( xintercept=max( dfVT$vStartTime), linetype ="dotted")
    lPlots[[1]] <- gPlot
    #print( gPlot )
    

    ##### Plot Enrollment - Zoomed in #####
    # Plot the trial enrollment and available data
    gPlot <- ggplot( dfVT, aes( y= vPatientIndex)) 
    gPlot <- gPlot + theme_minimal(base_size=gBaseSize ) + theme(legend.position = "top", plot.title=element_text( hjust=0.5, size=16))
    gPlot <- gPlot + ylim(0, max(dfVT$vPatientIndex))
    gPlot <- gPlot + geom_line( aes(  x = vStartTime, colour="Enrollment"), size =1.5)
    gPlot <- gPlot + geom_line( aes(  x = sort(vObsTime), colour="Outcome"), size =1.5)
    gPlot <- gPlot + labs(title=paste("Virtual Trial Enrollment",strTrialID), x= "Time (Months)", y="Number of Patients")
    gPlot <- gPlot + geom_hline( yintercept = c( nMinQtyOfPats), linetype="dotted")
    gPlot <- gPlot + scale_colour_manual("", 
                                         breaks = c("Enrollment", "Outcome"),
                                         values = c("green", "black")) 
    gPlot <- gPlot + geom_vline( xintercept=max( dfVT$vStartTime), linetype ="dotted")
    #print( gPlot )
    lPlots[[2]] <- gPlot
    
    ##### Plot # of Patients on Each Treatment #####
    # Plot the number of patients on each treatment
    gPlot <- ggplot( dfVT, aes( x= vStartTime)) 
    gPlot <- gPlot + theme_minimal(base_size=gBaseSize) + theme(legend.position = "top", plot.title=element_text( hjust=0.5, size=16))
    gPlot <- gPlot + ylim(0, max(dfVT$vPatientIndex))
    gPlot <- gPlot + geom_step( aes(  y = vQtyPatsOnE, colour="# on E"), size =1.5)
    gPlot <- gPlot + geom_step( aes(  y = vQtyPatsOnS, colour="# on S"), size =1.5)
    gPlot <- gPlot + labs(title=paste("Virtual Trial Randomization",strTrialID), x= "Time (Months)", y="Number of Patients")
    gPlot <- gPlot + geom_hline( yintercept = c( nMinQtyOfPats), linetype="dotted")
    gPlot <- gPlot + scale_colour_manual("", 
                                         breaks = c("# on E", "# on S"),
                                         values = c("green", "black")) 
    gPlot <- gPlot + geom_vline( xintercept=max( dfVT$vStartTime), linetype ="dotted")
    
    lPlots[[3]] <- gPlot
    #print( gPlot )
    
    ##### Plot Randomization Probability #####
    # Plot the trial enrollment and available data
    gPlot <- ggplot( dfVT,  aes( x= vPatientIndex, y = vRandProbE) ) + xlim(0,nMaxQtyOfPats) + ylim(0,1)
    gPlot <- gPlot + geom_line(colour="black", size =0.5) + geom_point( colour="blue", size =1.7)
    gPlot <- gPlot + geom_hline( yintercept = c( dMinRandProb, 1-dMinRandProb, 0.5), linetype="dashed")
    gPlot <- gPlot + theme_minimal(base_size=gBaseSize)  + theme(legend.position = "top", plot.title=element_text( hjust=0.5, size=16))
    gPlot <- gPlot + geom_vline( xintercept = c( nMinQtyOfPats), linetype="dotted")
    gPlot <- gPlot + labs(title= paste("Randomization Probabilities",strTrialID), x="Patient Number", y="Randomization Probability for E")
    
    lPlots[[4]] <- gPlot
    #print( gPlot )
    
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
    gPlot <- ggplot( dfVT,  aes( x= vTmpIndx, y =vTmpProbEGrtS) ) + xlim(0,nMaxQtyOfPats) 
    gPlot <- gPlot + geom_line(colour="black", size =0.5) + geom_point( colour="blue", size =1.75)
    gPlot <- gPlot + geom_hline( yintercept = c( dPU, 1-dPU, 0.5), linetype="dashed")
    gPlot <- gPlot + theme_minimal( base_size=gBaseSize)  + theme(legend.position = "top", plot.title=element_text( hjust=0.5, size=16))
    gPlot <- gPlot + geom_vline( xintercept = c( nMinQtyOfPats), linetype="dotted")
    gPlot <- gPlot + labs(title= paste("Prob( Q_E > Q_S | Data) ",strTrialID), x="Patient Number", y="Prob( Q_E > Q_S | Data)")
    gPlot <- gPlot + annotate("text", x = ( 200 + nMinQtyOfPats )/2, y=-0.02, label = "Stop Select S ", colour="Red",size=4 )
    gPlot <- gPlot + annotate("text", x=( 200 + nMinQtyOfPats )/2, y=1.02, label = "Stop Select E", colour="Red",size=4 )
    gPlot <- gPlot + geom_ribbon(  ymin = c(-0.05), ymax = 1-dPU, alpha=0.2,
                                   x=c(nMinQtyOfPats,rep(nMaxQtyOfPats,length(vTmpIndx)-1)))
    gPlot <- gPlot + geom_ribbon(  ymax = 1.05, ymin = dPU, alpha=0.2,
                                   x=c(nMinQtyOfPats,rep(nMaxQtyOfPats,length(vTmpIndx)-1)))
   
   
    lPlots[[5]] <- gPlot
    
    if( bPlotResults )   #We want the plot not the return list
    {
        for( i in 1:length(lPlots))
            print( lPlots[[ i ]])
    }
    else
    {
        return( lPlots )
    }
    
}

# Plot the operating characteristics 
PlotOCs <- function(  lScenarioResults )
{
    lOCs  <- ComputeOC( lScenarioResults$vResults, lScenarioResults$mQtyPats, lScenarioResults$vStopEarly)
    
    dfOCs <- data.frame( stat = factor(c( "Pr( Select None )", "Pr( Select E )", "Pr( Select S )",  "Pr( Stop Early )"),
                                       levels =c( "Pr( Select None )", "Pr( Select E )", "Pr( Select S )",  "Pr( Stop Early )")), 
                         Probability =c(   lOCs$dProbNoTreatment, lOCs$dProbSelectE, lOCs$dProbSelectS, lOCs$dProbStopEarly ) )
    
    gPlot <- ggplot( dfOCs, aes( x =stat, y = Probability, fill = stat))  + ylim( 0, 1)+
                geom_bar( stat="identity", show.legend = FALSE)    + theme_minimal( base_size=gBaseSize) +
                scale_fill_manual( values = c("yellow", "green","red", "black")) +
                labs(title="", x= "", y="Probability")
    return( gPlot )
}


# Plot sample size distributions 
PlotSampleSizeDist <- function( mQtyPats )
{ 
    plot( density(mQtyPats[,1], from= 0), type='l', lwd = 2, xlab = "# of Patients", ylab="Density", "Sample Size\n S (Solid), E (Dashed)")
    lines( density(mQtyPats[,2], from = 0), lty=2,  col=2,lwd=2)
    #abline( h=seq(0, 0.1, 0.01), v=seq( -200,200, 20), lty=9, col=8)
    
    # Add the mean and CI for SS
    vMean       <- apply( mQtyPats, 2, mean )
    vConfInt1   <- round( quantile( mQtyPats[,1], c( 0.025, 0.975)), 1)
    vConfInt2   <- round( quantile( mQtyPats[,2], c( 0.025, 0.975)), 1)
    abline( v = vMean, col = c(1,2), lty = 3)
    abline( v = vConfInt1, col = c(1,1), lty = 3, lwd=1.5)
    abline( v = vConfInt2, col = c(2,2), lty = 3, lwd=1.5)
    
}

