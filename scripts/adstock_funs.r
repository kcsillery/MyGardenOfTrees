## https://medium.com/@kyliefu/implementation-of-the-advertising-adstock-theory-in-r-62c2cc4b82fd
## Author: Kylie Fu, Nov 27, 2021

## lambda is the decay rate
GeometricSimpleAdstock <- function(advertising, lambda){
  adstock <- as.numeric(stats::filter(advertising, lambda, method="recursive"))
  return(adstock)   
}
GeometricLogAdstock <- function(advertising, lambda){
  adstock <- as.numeric(stats::filter(log(advertising), lambda, method="recursive"))
  return(adstock)
}
DelayedSimpleAdstock <- function(advertising, N, lambda, theta, L){
  ##   '''
  ## Return the advertising adstock using Delayed Decay Model.  ---
  ## Inputs:
  ## advertising: A sequence.
  ## N: Length of the advertising sequence.
  ## lambda: Adstock decay rate.
  ## theta: The delay of the peak effect.
  ## L: The maximum duration of carryover effect.  ---
  ## Returns:
  ## adstock: Advertising adstock
  ## '''
    weights <- matrix(0, N, N)
  for (i in 1:N){
    for (j in 1:N){
      k = i - j
      if (k < L && k >= 0){
        weights[i, j] = lambda ** ((k - theta) ** 2)
      }
    }
  }
  
  adstock <- as.numeric(weights %*% matrix(advertising))
  
  return(adstock)
}
## using the delayed function
## N <- 8
## lambda <- 0.9
## theta <- 1
## L <- matrix(0, N, N)
## weights <- matrix(0, N, N)
## for (i in 1:N){
##     for (j in 1:N){
##         k = i - j
##         if (k < L & k >= 0){
##             weights[i, j] = lambda ** ((k - theta) ** 2)
##         }
##     }
## }


adstock_model <- function(df, t, L = 13, theta = 1, lambda=0.8){
  ##   '''
  ## Find the optimal Adstock model.
  
  ## --- 
  ## Inputs:
  ## df: Data Frame.
  ## t: Model type. Options: "GeoSimple", "Delayed", "GeoLog".
  ## L: The maximum duration of carryover effect.
  ## theta: The delay of the peak effect.
  
  ## ---
  ## Returns:
  ## A model.
  ## '''
  
  if (t == 'GeoSimple'){model <- nls(data = df, sales ~ alpha + beta * GeometricSimpleAdstock(advertising, lambda), start = c(alpha = 1, beta = 1, lambda = 0))
    
    if (summary(model)$coef[3, 1] < 0 || summary(model)$coef[3, 1] >= 1){
      
      model <- nlsLM(data = df, sales ~ alpha + beta * GeometricSimpleAdstock(adstock, lambda), start = list(alpha = 1, beta = 1, lambda = 0), lower = c(alpha = -Inf, beta = -Inf, lambda = 0), upper = c(alpha = Inf, beta = Inf, lambda = 1)) 
    }
    
  } else if (t == 'Delayed'){
    
    N <- dim(df)[1]
    model <- nlsLM(data = df, sales ~ alpha + beta * DelayedSimpleAdstock(adstock, N, lambda, theta, L), start = list(alpha = 1, beta = 1, lambda = 0), lower = c(alpha = -Inf, beta = -Inf, lambda = 0), upper = c(alpha = Inf, beta = Inf, lambda = 1)) 
    
  } else if (t == 'GeoLog'){
    
    model <- nls(data = spots, sales ~ alpha + beta * GeometricLogAdstock(adstock, lambda), start = c(alpha = 1, beta = 1, lambda = 0))
    
    if (summary(model)$coef[3, 1] < 0 || summary(model)$coef[3, 1] >= 1){
      
      model <- nlsLM(data = df, sales ~ alpha + beta * GeometricLogAdstock(adstock, lambda), start = list(alpha = 1, beta = 1, lambda = 0), lower = c(alpha = -Inf, beta = -Inf, lambda = 0), upper = c(alpha = Inf, beta = Inf, lambda = 1)) 
    }
  }
  
  return(model)
}


Plot_ErrorBars<-function(x, y, x.err, y.err, xbar = F,
                         ybar = F, cap.scaling = 50, xcolor = "black",
                         ycolor = "black", lwidth = rep(1,length(x)), ...){
  ycap.width <- (par("usr")[4] - par("usr")[3])/cap.scaling
  xcap.width <- (par("usr")[2] - par("usr")[1])/cap.scaling
  if(xbar == T){
    for(i in 1:length(x.err)){
      segments(x0 = x[i] + x.err[i], y0 = y[i],
               x1= x[i] - x.err[i],  y1 = y[i],
               lwd = lwidth[i], col = xcolor[i])
      segments(x0 = x[i] + x.err[i], y0 = y[i] + ycap.width,
               x1=x[i] + x.err[i],   y1 = y[i] - ycap.width,
               lwd = lwidth[i], col = xcolor[i])
      segments(x0 = x[i] - x.err[i], y0 = y[i] + ycap.width,
               x1=x[i] - x.err[i],   y1 = y[i] - ycap.width,
               lwd = lwidth[i], col = xcolor[i])
    }
  }
  if(ybar == T){
    for(i in 1:length(y.err)){
      segments(x0 = x[i], y0 = y[i] + y.err[i],
               x1= x[i],  y1 = y[i] - y.err[i],
               lwd = lwidth, col = ycolor)
      segments(x0 = x[i] + xcap.width, y0 = y[i] + y.err[i],
               x1=x[i] - xcap.width,   y1 = y[i] + y.err[i],
               lwd = lwidth, col = ycolor)
      segments(x0 = x[i] + xcap.width, y0 = y[i] - y.err[i],
               x1=x[i] - xcap.width,   y1 = y[i] - y.err[i],
               lwd = lwidth, col = ycolor)
    }
  }
}
# E
