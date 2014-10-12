plot.param.vertical <- function(samps, params, with.density = FALSE,
                                CI.level = 0.5, show.level = 0.95){
  dim.samps <- dim(samps) #nIter, nChain, nParam
  if(length(params) == 0)
    params = dimnames(samps)$parameters[1:min(10, dim.samps[3])]
  nParams <- length(params)
  nIter <- dim.samps[1] * dim.samps[2]
  samps.use <- array(samps[,,params], c(nIter, nParams))
  colnames(samps.use) <- params
  
  samps.mean <- colMeans(samps.use)
  samps.median <- apply(samps.use, 2, median)
  probs.use <- c(0.5 - show.level / 2, 
                 0.5 - CI.level / 2, 
                 0.5,
                 0.5 + CI.level / 2, 
                 0.5 + show.level / 2)
  samps.quantile <- t(apply(samps.use, 2, quantile, probs = probs.use))
  
  y <- seq(nParams, 1, by = -1)
  xlim <- c(min(samps.quantile[,1]), max(samps.quantile[,5])) 
  xrange <- diff(xlim)
  xlim[1] <- xlim[1] - 0.05 * xrange
  xlim[2] <- xlim[2] + 0.05 * xrange
  par(mar = c(1.5,5,3,1))
  plot(samps.median, y, bty = "n", type = "n",
       xlim = xlim, pch = 20, ylim = c(0.5, nParams + 1),
       xaxt = "n", yaxt = "n",
       xlab = "", ylab = "")
  abline(h = y, lty = 2, col = "lightgray")
  grid(nx = NULL, ny = 0, lty = 2)
  points(samps.median, y, pch = 20)
  segments(samps.quantile[,1], y,
           samps.quantile[,5], y)

  #axis(side = 2, at = y, labels = params, las = 1)
  mtext(text = params, side = 2, las = 1, at = y)
  axis(side = 3)
  
  
  if(with.density){
    for(i in 1:nParams){
      d.temp <- density(samps.use[,i], 
                        from = samps.quantile[i,1],
                        to = samps.quantile[i,5])
      y.max <- max(d.temp$y)
      x.plot <- d.temp$x
      y.plot <- d.temp$y / y.max * 0.8 + y[i]
      lines(x.plot, y.plot)
      d.line <- density(samps.use[,i], 
                        from = samps.quantile[i,2], 
                        to = samps.quantile[i,4], n = 2)
      x.plot <- d.line$x
      y.plot <- d.line$y / y.max * 0.8 + y[i]
      segments(x.plot, y[i], x.plot, y.plot, lty = 3)
    }
  }
  else{
    segments(samps.quantile[,2], y,
             samps.quantile[,4], y, lwd = 3)
  }
}

plot.param.vertical(samps, NULL, TRUE,
                    CI.level = 0.68, show.level = 0.95)

