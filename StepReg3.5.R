rm(list = ls())
# STEP 0.1
# Download all related packages
all.pcg <- c("xts", "openxlsx", "xlsxjars", "xlsx","openxlsx",
              "car", "MASS", "lmtest", "zoo", "plyr", "tidyr", "dplyr",
              "lubridate", "ggplot2", "Rcpp", "colorspace", "parallel",
              "foreach", "doParallel","readr","lars","glmnet")

# req.pkg <- function(pkg, install.Flag = F){
#   new.pkg <- pkg[(!(pkg %in% installed.packages()[, "Package"]))|
#                    (pkg %in% old.packayges()[, "Package"])]
#   
#   if (length(new.pkg) & install.Flag) install.packages(new.pkg, dependencies = TRUE)
#   sapply(pkg, library, y
#          quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)
# }
# Sys.setenv(R_ZIPCMD= "C:/StatTools/Rtools/bin/zip")
# if Rtools is not one of the environment variables, do this:
ini.Rtools <- function(){
  repeat{
    if(is.null(Sys.getenv("R_ZIPCMD"))|Sys.getenv("R_ZIPCMD")==""){
      # try c:/ root first
      if ("zip.exe" %in% list.files("C:/Rtools/bin/")){
        Sys.setenv(R_ZIPCMD= "C:/Rtools/bin/zip.exe")
        message('Please enter "StepReg3()" to start modeling.')
        break
      }else{
        message("\n", paste(rep("-",60),collapse=""),"\n")
        message('| "Rtools\\bin\\zip" is not set to R_ZIPCMD (environment variable)!\n')
        repeat{
          message('| Please set R_ZIPCMD parameter after installing Rtools:')
          message('| Example 1: C:/StatTools/Rtools/bin/zip')
          message('| Example 2: C:\\\\StatTools\\\\Rtools\\\\bin\\\\zip\n')
          message('| If the path contains space, use "\\" to escape the space:')
          message('| Example 3: C:/Program\\ files/Rtools/bin/zip')
          message('| Example 4: C:\\\\Program\\ files\\\\Rtools\\\\bin\\\\zip\n')
          message(paste(rep("-",60),collapse=""),"\n")
          
          zipcmd <- readline('| Please enter the full path of "Rtools\\bin\\zip": ')
          zippath <- gsub("zip", "", zipcmd)
          cat("\n")
          
          if ("zip.exe" %in% list.files(zippath)){
            Sys.setenv(R_ZIPCMD= zipcmd)
            message('Please enter "StepReg3()" to start modeling.')
            break

          }else{
            message("| Cannot find zip.exe in the entered path.\n")
          }
        }
     }
    }else{
      message('Please enter "StepReg3()" to start modeling.')
      break
    }
  }
  # create new envoirment
  e <<- new.env()
}

# STEP 0.2
# Initialization
# setting the R-tools path
ini.Rtools()


# STEP 0.3
# Functions Definition

##==================================================================================
## FUNDAMENTAL FUNCTIONS                        | 
## 0. lg(x, no.lag)                             | ~ lag transformation
## 1. co(x, co.rate)                            | ~ carryover effect transformation
## 2. sc(x, lambda1, lambda2)                   | ~ s-curve transformation
## 3. pc(x, exponent) #abandonned               | ~ power curve transformation
## 4. cs(x, co.rate, lambda1, lambda2)          | ~ carryover + s-curve
## 5. cp(x, co.rate, expnent)                   | ~ carryover + power curve
## 6. lc(x, no.lag, co.rate)                    | ~ lag + carryover
## 7. lcp(x, no.lag, co.rate, exponent)         | ~ lag + carryover + power curve
## 8. lcs(x, no.lag, co.rate, lambda1, lambda2) | ~ lag + carryover + s curve
##==================================================================================

#------------------#
# 0. lg(x, no.lag) #
#------------------#
# generate the x with lag
# lg <- function(x, no.lag){
#   x <- c(rep(0, no.lag), x[1:(length(x)-no.lag)])
#   return(x)
# }

# replace the old function by dplyr::lag(x, n, default) 
lg <- function(x, no.lag, default = 0){
  dplyr::lag(x, n = no.lag, default)
}

#-------------------#
# 1. co(x, co.rate) #
#-------------------#
co <- function(x, co.rate){
  #---------------------------------
  # carry over effect
  # formula: period2 = period1 * carryover rate + period2
  # x: variable to be transformed
  # co.rate: carry over rate
  # make sure the 'x' already exists in the environment
  #---------------------------------
  
  for (p in 2:length(x)){
    if (is.na(x[p-1])){
      # p: position of the element
      x[p] = x[p]
    }else{
      x[p] = x[p-1] * co.rate + x[p]
    }
  }
  
  return(x)
}  

#=====================================================================

#--------------------------#
# 2. sc(x, lambda1, lambda2)
sc <- function(x, lambda1, lambda2) 1 - exp(-lambda1 * x^lambda2)
# s-curve transformation
# formula: 1-e^(-lambda1 * x^lambda2)
# x: variable to be transformed
# lambda1 & lambda2: 2 parameters of the s-curve
# make sure the 'x' already exists in the environment

#--------------------#
# 3. pc(x, exponent) 
# pc <- function(x,exponent) x^exponent
# power curve transformation
# formula: x^i
# x: variable to be transformed
# exponent: exponent...
# make sure the 'x' already exists in the environment

#-----------------------------------#
# 4. cs(x, co.rate, lambda1, lambda2) 
cs <- function(x, co.rate, lambda1, lambda2){
  # carryover + s-curve
  # x <- sc(co(x, co.rate), lambda1, lambda2)
  x <- 1 - exp(-lambda1 * co(x, co.rate)^lambda2)
  #x
}

#-----------------------------------#
# 5. cp(x, co.rate, exponent) 
cp <- function(x, co.rate, exponent){
  # carryover + power curve
  # x <- pc(co(x, co.rate), exponent)
  x <- co(x, co.rate)^exponent
  #x
}

#-----------------------------------#
# 6. lc(x, no.lag, co.rate)
lc <- function(x, no.lag, co.rate){
  # lag + carryover
  x <- co(lg(x, no.lag), co.rate)
  return(x)
}

#-----------------------------------#
# 7. lcp(x, no.lag, co.rate, exponent)
lcp <- function(x, no.lag, co.rate, exponent){
  # lag + carryover + power curve
  lg(cp(x, co.rate, exponent), no.lag)
}

#-----------------------------------#
# 8. lcs(x, no.lag, co.rate, lambda1, lambda2)
lcs <- function(x, no.lag, co.rate, lambda1, lambda2){
  # lag + carryover + s curve
  lg(cs(x, co.rate, lambda1, lambda2), no.lag)
}

#=====================================================================

#-------------------#
# trial()           #
#-------------------#

trial <- function(data, resp, fit = NULL, action = 1, pred) {            
  # Build the model				
  # based on NULL or fit1, add/remove a predictor/the intercept, output summary				
  
  # action:  1 means add a predictor
  #         -1 means delete a predictor
  
  message("go to trial")
  if(is.null(fit)){ # if(exists("fit", mode = "list"))
    if(action == 1) {
      fit.new <- lm(as.formula(sprintf('%s ~ %s', resp, pred)), data = data, na.action = na.exclude)
    } else if(action == -1) {
      message("There's no existed model to let you delete ", pred, " from!", sep = "")
      cat("\n")
      fit.new <- fit
    }
    
  } else { # if(!is.null(fit))
    if(pred %in% names(coef(fit))) {
      if(action == -1) {
        fit.new <- update(fit, as.formula(sprintf('~. - %s', pred)), data = data)
      } else {
        message("The ", pred, " is already in the model!", sep = "")
        cat("\n")
        fit.new <- fit
      }
    } else {
      if(action == 1){
        fit.new <- update(fit, as.formula(sprintf('~. + %s', pred)), data = data)
      } else {
        message("The ", pred, " isn't in the model!", sep = "")
        cat("\n")
        fit.new <- fit
      }
    }
  }
  return(fit.new)
} # end of function trial()

#=====================================================================

#---------#
# recom() #
#---------#

recom <- function(pred, resp, df, type, fit = NULL, st.row){
  #---------------------------------
  # pred: predictor to be inserted to the model, to be quoted. i.e.: "cly" (mtcars)
  # resp: response variable in the model, to be quoted. i.e.: "mpg" (mtcars)
  # df: data base for the model
  # type: transformation method for the predictor
  # fit: default value is NULL; if a model exists, then put the name of fit list
  # st.row: modeling starting row
  #---------------------------------
  # When type = 1: test all combinations of parameters of carryover & s-curve
  # When type = 2: test all combinations of parameters of carryover & power curve
  # When type = 3: do both of the actions above
  # Check the best R-square / p-value of coef, give recommendations
  #---------------------------------
  
  #----------------------#
  # PART I - Preparation #
  #----------------------#
  #require needed packages
  
  message("go to recom")
  
  if(!"plyr" %in% installed.packages()){
    install.packages("plyr")
    require(plyr)
  }
  
  if(!"dplyr" %in% installed.packages()){
    install.packages("dplyr")
    require(dplyr)
  }
  
  #---------------------------------
  
  # Load the data
  df1 <- df[st.row:nrow(df), ]  # df to be modeled
  x0 <- df[[pred]]
  y0 <- df[[resp]]
  x1 <- x0[st.row:length(x0)]
  y1 <- y0[st.row:length(y0)]
  
  #---------------------------------#
  # PART II - Fundamental functions #
  #---------------------------------#
  message("go to part2")
  
  # The default parameter ranges are as below:
  # (To be modified on demand)
  e$co.range <- seq(0.05, 0.95, 0.05)               # 19 steps
  e$lambda1.range <- seq(0.0001, 0.0007, 0.0001)    # 7 steps
  e$lambda2.range <- seq(1.1, 1.7, 0.1)             # 7 steps
  e$pc.range <- seq(0.4, 0.95, 0.05)                # 12 steps
  e$lag.range <- seq(1, 91)                         # 91 steps
  e$lag.range.mon.d <- c(1, 30, 60, 90)             # 4 steps
  e$lag.range.week.d <- seq(1, 91, by = 7)          # 13 steps
  e$lag.range.mon.w <- c(1,4,8,12)                  # 4 steps
  e$lag.range.week.w <- seq(1, 12, by = 1)          # 13 steps
  

  if (e$lag.granularity == "d") {
    e$lag.range <- seq(1, 91)
  } else if (e$granularity == "daily") {
    if (e$lag.granularity == "m") {
      e$lag.range <- e$lag.range.mon.d
    } else if (e$lag.granularity == "w") {
      e$lag.range <- e$lag.range.week.d
    }
  } else if (e$granularity == "weekly") {
    if (e$lag.granularity == "m") {
      e$lag.range <- e$lag.range.mon.w
    } else if (e$lag.granularity == "w") {
      e$lag.range <- e$lag.range.week.w
    }
  } else{
    e$lag.range <- seq(1, 91)
  }

  # Function 1.1
  #---------------------------------
  # Set sub functions (cs.trans & cp.trans) for generating predictor matrix 
  # (transformed by different group of parameters)
  cstrans <- function(x){
    cs.mat <- t(mdply(expand.grid(co.rate = e$co.range, 
                                  lambda1 = e$lambda1.range, 
                                  lambda2 = e$lambda2.range),
                      cs, x))
    # return(cs.mat)
    # 7 * 7 * 19 = 931 combinations
  }
  
  # Function 1.2
  #---------------------------------
  cptrans <- function(x){
    cp.mat <- t(mdply(expand.grid(co.rate = e$co.range, 
                                  exponent = e$pc.range),
                      cp, x))
    # return(cp.mat)
    # 12 * 19 = 228 combinations
  }
  
  # Function 1.3
  #---------------------------------
  ltrans <- function(x){
    l.mat <- t(mdply(expand.grid(no.lag = e$lag.range),
                     lg, x))
  }
  
  # Function 1.4
  #---------------------------------
  lctrans <- function(x){
    lc.mat <- t(mdply(expand.grid(no.lag = e$lag.range,
                                  co.rate = e$co.range),
                      lc, x))
    # 91 * 19 = 1729 combinations
  }
  
  # Function 1.4 bis
  #---------------------------------
  # lctrans with parallel computation
  lctrans2 <- function(x){
    foreach(b = e$co.range, .combine = cbind)                        %:%
      foreach(a = e$lag.range, .combine = cbind)                     %dopar%
      c(a, b, lc(x, a, b))
  }
  
  # Function 1.5
  #---------------------------------
  lcptrans <- function(x){
    lcp.mat <- t(mdply(expand.grid(no.lag = e$lag.range,
                                   co.rate = e$co.range,
                                   exponent = e$pc.range),
                       lcp, x))
    # 91 * 19 * 12 = 20748 combinations
  }
  
  # Function 1.5 bis
  #---------------------------------
  # lcptrans with parallel computation
  lcptrans2 <- function(x){
    foreach(c = e$pc.range, .combine = cbind)                        %:%
      foreach(b = e$co.range, .combine = cbind)                      %:%
      foreach(a = e$lag.range, .combine = cbind)                     %dopar%
      c(a, b, c, lcp(x, a, b, c))
  }
  
  lcptrans2.test <- function(x){
    foreach(c = e$pc.range, .combine = cbind)                        %:%
      foreach(b = e$co.range, .combine = cbind)                      %:%
      foreach(a = e$lag.range, .combine = cbind)                     %dopar%
      c(a, b, c, lcp(x, a, b, c))
  }
  
  # Function 1.6
  #---------------------------------
  lcstrans <- function(x){
    lcs.mat <- t(mdply(expand.grid(no.lag = e$lag.range,
                                   co.rate = e$co.range,
                                   lambda1 = e$lambda1.range,
                                   lambda2 = e$lambda2.range),
                       lcs, x))
    # 91 * 19 * 7 * 7 = 84721 combinations
  }
  
  # Function 1.6 bis
  #---------------------------------
  # lcstrans with parallel computation
  lcstrans2 <- function(x){
    foreach(d = e$lambda2.range, .combine = cbind)                   %:%    
      foreach(c = e$lambda1.range, .combine = cbind)                 %:%
      foreach(b = e$co.range, .combine = cbind)                      %:%
      foreach(a = e$lag.range, .combine = cbind)                     %dopar%
      c(a, b, c, d, lcp(x, a, b, c))
  }
  
  # Function 2
  #---------------------------------
  # Check if a fit exists, update the fit if it does, otherwise create one
    fit.update <- function(resp, x, fit.check = NULL, pred, df){
      df[[pred]] <- x
      fit.new <-  if (is.null(fit.check)){
        lm(as.formula(sprintf("%s ~ %s", resp, pred)), data = df)
      }else{
        update(fit.check, as.formula(sprintf("~. + %s", pred)), data = df)
      }
      # return(fit.new)
    }
  
  # Function 3
  #---------------------------------
  # Extract the key modeling summary statatistics
  # Call Function 2: fit.update()
  summary.stats <- function(resp, x, fit.coef = fit, pred, df){
    smr.fit <- fit.update(resp, x, fit.coef, pred, df)
    smr <- summary(smr.fit)
    simulation <-  t(t(as.matrix(x)) * as.vector(coef(smr.fit)[[pred]]))
    contri.d <- sum(simulation)/sum(smr.fit$fitted.values)
    smr.coef <- subset(smr$coefficients, rownames(smr$coefficients)==pred)[c(1,4)]
    smr.key <- c(smr.coef, smr$r.squared, smr$adj.r.squared, contri.d)
    names(smr.key) <- c("Coefficient", "P.value", "R.squared", "Adj.R.squared", "Contribution")
    # to generate a group of key statistics, how many variables corresponding to how many groups
    return(t(smr.key))
  }
  
  # Function 4
  #---------------------------------
  # Test all the combinations of parameters & the respective modeling result
  testall <- function(resp, x, pred, fit.coef = fit, type, df){
    # type <- if(as.numeric(type) == 6){as.numeric(type) + 1}else{as.numeric(type)}
    
    
    # assign variables
    clusterExport(e$cl, c("co.range", "lambda1.range", "lambda2.range","pc.range", 
                        "lag.range",
                        "lag.range.mon.d","lag.range.week.d",
                        "lag.range.mon.w","lag.range.week.w"), envir = e)
    clusterExport(e$cl, c("co", "cp", "cs", "lg", "lc", "lcp", "lcs"))
    
    opt <- LETTERS[type]
    #both <- function(x)list(cstrans(x),cptrans(x))
    
    
    system.time({
    mat <- switch(opt,
                  A = cstrans(x),
                  B = cptrans(x),
                  D = ltrans(x),
                  E = lctrans2(x),
                  # G = lcstrans2(x),
                  G = lcptrans2.test(x))
    })[3] -> e$cal.duration.trans
    
    len <- switch(opt,
                  A = 3,
                  B = 2,
                  D = 1,
                  E = 2,
                  # G = 4,
                  G = 3)
    
    nam <- switch(opt,
                  A = "cs",
                  B = "cp",
                  D = "lg",
                  E = "lc",
                  # G = "lcs",
                  G = "lcp")
    
    met <- switch(opt,
                  A = "Carryover + S-curve",
                  B = "Carryover + Power curve",
                  D = "Lag",
                  E = "Lag + Carryover",
                  # G = "Lag + Carryover + S-curve",
                  G = "Lag + Carryover + Power curve")
    
    # @@@@
    prmt <- as.data.frame(mat[1:len, , drop = F])    # capture the parameter combinations
    var0 <- as.data.frame(mat[(len+1):nrow(mat),])    # capture the transformed variables
    #colnames(var) <- paste(pred, 1:ncol(var), sep = "")
    var <- var0[st.row:dim(var0)[1], ]
    
    test.stats <- t(sapply(var, summary.stats, resp = resp, pred = pred, 
                           fit.coef = fit.coef, df = df))
    # coef <- test.stats[1:2]
    # rsq <- test.stats[3]
    # adj.rsq <- test.stats[4]
    # cat(dim(t(prmt)), dim(test.stats), "\n")
    prmt.all <- as.data.frame(cbind(t(prmt), test.stats))
    prmt.all <- prmt.all[complete.cases(prmt.all), ]
    if(nrow(prmt.all) == 0){
      op.recom <- as.data.frame(matrix(1, nrow = 1, ncol = 9))
      message("| ", e$pred, " cannot get into the model!\n")
      message("| Please test another variable!\n")
      
      curve.prmt <- as.data.frame(matrix(rep(NA, 6), nrow = 1))
    } else {
      # curve.prmt <- if(len == 2) "pc.r" else c("sc.1","sc.2")
      curve.prmt <- if(type == 1){
        c("co.r", "sc.1","sc.2")
      }else if(type == 2){
        c("co.r", "pc.r")
      }else if(type == 4){
        "no.lag"
      }else if(type == 5){
        c("no.lag", "co.r")  
      # }else if(type == 7){
      #   c("no.lag", "co.r", "sc.1", "sc.2")
      }else if(type == 7){
        c("no.lag", "co.r", "pc.r")
      }
        
      colnames(prmt.all) <- c(curve.prmt, "coef", "p-value", 
                              "r.squared", "adj.r.squared", "contribution")
      rownames(prmt.all) <- NULL
      prmt.all <- prmt.all[order(prmt.all[["adj.r.squared"]], decreasing = T),]
      
      # add by william at 2017-03-31
      e$prmt.all[[e$pred]] <- prmt.all

      # export all parameters and related model statistics to local working directory
      write.csv(prmt.all, paste("prmt", nam, pred, "csv",sep = "."), row.names = FALSE)
      message(paste("\nThe parameter reference: 'prmt", nam, 
                    pred, "csv' is exported!",sep = "."))
      
      # capture the best group of parameters
      # get the best adj. r squared first then allocate the position
      posi <- which.max(as.numeric(prmt.all[["adj.r.squared"]]))
      
      best.stats <- prmt.all[posi,]
      rownames(best.stats) <- NULL   # remove the row index assigned automatically by the program
      
      # print(paste("the size of prmt.all is ", paste(dim(prmt.all),collapse=" and "),sep=""))
      # print(paste("the value of best is ", best,sep=""))
      # print(paste("the best row number is ", which(round(prmt.all$adjusted.r.squared,6)==round(best,6)), sep =""))
      # print(paste("the size of best.stats is ", paste(dim(best.stats),collapse=" and "),sep=""))
      
      # print the message indicating the best transformation parameters and results
      cat("",
          paste(" For the transformation method ", met, ":", sep=""),
          paste(" ", paste(rep("-",40), collapse = ""),sep = ""),
          sep = "\n")
      
      if(as.character(type)=="1"){
        cat(paste(" - The recommended carryover rate is ", best.stats[1], sep=""),
            paste(" - The recommended lambda1(S-curve) is ", best.stats[2], sep = ""),
            paste(" - The recommended lambda2(S-curve) is ", best.stats[3], sep = ""),
            paste(" - The coefficient after transformation is ", 
                  if (round(best.stats[4],4)==0){
                    format(best.stats[4], scientific = T)
                  }else{
                    format(round(best.stats[4],4),nsmall = 4)
                  }, sep = ""),
            sep = "\n")
        curve.prmt <- c(NA, best.stats[1], NA, best.stats[2], best.stats[3],
                        best.stats[ncol(prmt.all)-1])
      }else if(as.character(type)=="2"){
        cat(paste(" - The recommended carryover rate is ", best.stats[1], sep=""),
            paste(" - The recommended power rate is ", best.stats[2], sep = ""),
            paste(" - The coefficient after transformation is ", 
                  if (round(best.stats[3],4)==0){
                    format(best.stats[3], scientific = T)
                  }else{
                    format(round(best.stats[3],4),nsmall = 4)
                  }, sep = ""),
            sep = "\n")
        curve.prmt <- c(NA, best.stats[1], best.stats[2], NA, NA,
                        best.stats[ncol(prmt.all)-1])
      }else if(as.character(type)=="4"){
        cat(paste0(" - The recommended number of lag is ", best.stats[1]),
            paste0(" - The coefficient after transformation is ",
                   if (round(best.stats[2],4)==0){
                     format(best.stats[2], scientific = T)
                   }else{
                     format(round(best.stats[2],4),nsmall = 4)
                   }), sep = "\n")
        curve.prmt <- c(best.stats[1], NA, NA, NA, NA,
                        best.stats[ncol(prmt.all)-1])
      }else if(as.character(type)=="5"){
        cat(paste0(" - The recommended number of lag is ", best.stats[1]),
            paste0(" - The recommended carryover rate is ", best.stats[2]),
            paste0(" - The coefficient after transformation is ",
                   if (round(best.stats[3],4)==0){
                     format(best.stats[3], scientific = T)
                   }else{
                     format(round(best.stats[3],4),nsmall = 4)
                   }), sep = "\n")
        curve.prmt <- c(best.stats[1], best.stats[2], NA, NA, NA,
                        best.stats[ncol(prmt.all)-1])
      # }else if(as.character(type)=="7"){
        # cat(paste0(" - The recommended number of lag is ", best.stats[1]),
        #     paste0(" - The recommended carryover rate is ", best.stats[2]),
        #     paste0(" - The recommended lambda1(S-curve) is ", best.stats[3]),
        #     paste0(" - The recommended lambda2(S-curve) is ", best.stats[4]),
        #     paste0(" - The coefficient after transformation is ",
        #            if (round(best.stats[5],4)==0){
        #              format(best.stats[5], scientific = T)
        #            }else{
        #              format(round(best.stats[5],4),nsmall = 4)
        #            }), sep = "\n")
        # curve.prmt <- c(best.stats[1], best.stats[2], NA, best.stats[3], best.stats[4],
        #                 best.stats[ncol(prmt.all)-1])
      }else if(as.character(type)=="7"){
        cat(paste0(" - The recommended number of lag is ", best.stats[1]),
            paste0(" - The recommended carryover rate is ", best.stats[2]),
            paste0(" - The recommended power rate is ", best.stats[3]),
            paste0(" - The coefficient after transformation is ",
                   if (round(best.stats[4],4)==0){
                     format(best.stats[4], scientific = T)
                   }else{
                     format(round(best.stats[4],4),nsmall = 4)
                   }), sep = "\n")
        curve.prmt <- c(best.stats[1], best.stats[2], best.stats[3], NA, NA,
                        best.stats[ncol(prmt.all)-1])
        
      }
      
      cat(paste(" ", paste(rep("-",40), collapse = ""),sep = ""),"\n")
      
      # add by william at 2017-03-31
      # if we want postive coef
      e$prmt.all.coef.positive[[e$pred]] <- e$prmt.all[[e$pred]] %>% dplyr::filter(coef > 0 & `p-value` <= 0.3) %>% dplyr::arrange(desc(contribution))
      e$prmt.all.coef.negative[[e$pred]] <- e$prmt.all[[e$pred]] %>% dplyr::filter(coef <= 0 & `p-value` <= 0.3) %>% dplyr::arrange(desc(contribution))
      if(nrow(e$prmt.all.coef.positive[[e$pred]])>0){
        e$prmt.all.coef.flag[[e$pred]] = T # T means positive , F means Negative
      }else{
        e$prmt.all.coef.flag[[e$pred]] = F
      }
      # end of change
      
      if(is.na(best.stats[["p-value"]])){
        message("| ", e$pred, " is not advised to be added to the model!\n")
      }else if (best.stats[["p-value"]] > 0.2){
        message("| Please be aware that the p-value of predictor coefficient is larger than 0.2!")
        message("| The estimate of coefficient is not significant!\n")
      }
    }
    names(curve.prmt) <- c("no.lag", "co.r","pc.r","sc.1","sc.2","Adj.r2")
    return(curve.prmt)
    
  } # end of function testall()
  
  #-------------------------------------#
  # PART III - Application of functions #
  #-------------------------------------#
  system.time({
  if(as.character(type) == "1"){
    # carryover + s-curve only
    prmt.rec <- testall(resp, x0, pred, fit.coef = fit, type = 1, df = df1)
  }else if(as.character(type) == "2"){
    # carryover + power curve only
    prmt.rec <- testall(resp, x0, pred, fit.coef = fit, type = 2, df = df1)
  }else if(as.character(type) == "3"){
    # compary carryover + s-curve and carryover + power curve 
    prmt.cs <- testall(resp, x0, pred, fit.coef = fit, type = 1, df = df1)
    prmt.cp <- testall(resp, x0, pred, fit.coef = fit, type = 2, df = df1)
    if(is.na(prmt.cs[1])|is.na(prmt.cs[1])){
      prmt.rec <- rep(NA, 5)
      message("There is no recommendation!\n")
    }else if(as.numeric(prmt.cs[5]) > as.numeric(prmt.cp[5])){
      prmt.rec <- prmt.cs
      message("Concerning r-squared, the method **CARRY-OVER + S-CURVE** is preferred.\n")
    }else if(as.numeric(prmt.cs[5]) < as.numeric(prmt.cp[5])){
      prmt.rec <- prmt.cp
      message("Concerning r-squared, the method **CARRY-OVER + POWER CURVE** is preferred.\n")
    }else if(as.numeric(prmt.cs[5]) == as.numeric(prmt.cp[5])){
      prmt.rec <- prmt.cp
      message("Both transformation methods are OK for the model.\n")
    }
  }else if(as.character(type) == "4"){
    # lag
    prmt.rec <- testall(resp, x0, pred, fit.coef = fit, type = 4, df = df1)
  }else if(as.character(type) == "5"){
    # lag + carryover
    prmt.rec <- testall(resp, x0, pred, fit.coef = fit, type = 5, df = df1)
  # }else if(as.character(type) == "6"){
  #   # lag + carryover + s curve
  #   prmt.rec <- testall(resp, x0, pred, fit.coef = fit, type = 7, df = df1)
  }else if(as.character(type) == "7"){
    # lag + carryover + power curve
    prmt.rec <- testall(resp, x0, pred, fit.coef = fit, type = 7, df = df1)
  }
  })-> e$cal.duration.test
  
  return(prmt.rec[1:5])
  
} # end of funciton recom()




#=====================================================================
#------------------#
# other functions()
#------------------#
# other functions()
term.clean <- function(x){
  # elminiate "/,/ and restructure it into vector, c("a","b","c")  
  kwd <- "\"|'"
  x <- trimws(x) %>% toupper()
  x <- gsub(kwd,"",x)
  x = strsplit(x,",") %>% unlist() %>% as.vector()
  return(x)
}
merge.txt = function(x){
  txt1 = ""
  txt2 = ""
  # x = brands
  for(i in 1:length(x)){
    txt1 = paste0("",x[i],"")
    if(i == 1) {
      txt2 = txt1
    }else {
      txt2 = paste(txt2,txt1,sep = "|")
    }
  }
  return(txt2)
}


#=====================================================================
#------------#
# hmodif()   #
#------------#

hmodif <- function(pred, resp, data){
  #---------------------------------
  # transform specified variable with(out) parameters
  # this function returns **a dataset** with selected variable transformed
  # make sure the raw data is already loaded into global environment
  #---------------------------------
  # pred: the name of variable to be transormed of class "character"
	# resp: the name of the respondent variable of the model
  # data: data frame to store the variable transformed (original var. will be covered)
  #---------------------------------
  df <- data
    
	if(substr(pred, nchar(pred)-3, nchar(pred)) == ".PRE"){
		for(i in which(data[[pred]] == 1)){
			if(i < 4){
				df[[pred]][i] <- mean(data[[resp]][(i+3):(i+6)], na.rm = TRUE)
			}else{
				df[[pred]][i] <- mean(c(data[[resp]][(i-4):(i-1)], data[[resp]][(i+3):(i+6)]), na.rm = TRUE)
			}
		}
	}else if(substr(pred, nchar(pred)-3, nchar(pred)) == "POST"){
		for(i in which(data[[pred]] == 1)){
			if(i < 6){
				df[[pred]][i] <- mean(data[[resp]][(i+1):(i+4)], na.rm = TRUE)
			}else{
				df[[pred]][i] <- mean(c(data[[resp]][(i-6):(i-3)], data[[resp]][(i+1):(i+4)]), na.rm = TRUE)
			}
		}
	}else{
		for(i in which(data[[pred]] == 1)){
			if(i < 5){
				df[[pred]][i] <- mean(data[[resp]][(i+2):(i+5)], na.rm = TRUE)
			}else{
				df[[pred]][i] <- mean(c(data[[resp]][(i-5):(i-2)], data[[resp]][(i+2):(i+5)]), na.rm = TRUE)
			}
		}
	}

  return(df)
}

#=====================================================================

#---------#
# modif() #
#---------#

modif <- function(pred, resp, data, tm.prmt){
  #---------------------------------
  # transform specified variable with(out) parameters
  # this function returns **a dataset** with selected variable transformed
  # make sure the raw data is already loaded into global environment
  #---------------------------------
  # pred: the name of variable to be transormed of class "character"
  # data: data frame to store the variable transformed (original var. will be covered)
  # tm.prmt: transformation parameters
  #---------------------------------
  
  df <- data
  no.lag <- tm.prmt[[1]]
  co.r <- tm.prmt[[2]]
  pc.r <- tm.prmt[[3]]
  sc.1 <- tm.prmt[[4]]
  sc.2 <- tm.prmt[[5]]
  
  if(!is.na(no.lag)){
    if(is.na(co.r)){
      df[[pred]] <- lg(data[[pred]], no.lag)
    }else if (is.na(pc.r)){
      df[[pred]] <- lc(data[[pred]], no.lag, co.r)
    }else {
      df[[pred]] <- lcp(data[[pred]], no.lag, co.r,pc.r)
    }
  }else{
    if(is.na(co.r)){
      df[[pred]] <- data[[pred]]
    }else	if(co.r == 1){
      df <- hmodif(pred, resp, data)
    }else if(is.na(pc.r)){
      df[[pred]] <- cs(data[[pred]], co.r, sc.1, sc.2)
    }else{
      df[[pred]] <- cp(data[[pred]], co.r, pc.r)
    }
  }
  
  return(df)
}

#=====================================================================

#-------------#
# questions() #
#-------------#

questions <- function(index, pred = NULL, df = NULL, coef = NULL, opt = NULL){
  # index: number of question to be used
  # df: data frame, class - data.frame 
  # coef: fit$coef, class - data.frame
  # opt: add a vriable to (1) or remove one (2) from the model
  
  # index = 1
  q2.2 <- function(){
    # Question 2.2 "What's next move?"
    repeat{
      cat("| What is the next move?",
          "|  1. Continue modelling & add a predictor!",
          "|  2. Remove a predictor from the model.",
          "|  3. Stop! Show me the final model and the statistics!",
          "", sep = "\n")
      opt <- readline("| Enter an option number please: ")
      cat("\n")
      if(!opt %in% as.character(1:3)){
        message("| Only the number between 1 and 3 is acceptable, dude!\n")
      }else{
        opt <- as.numeric(opt)
        break
      }
    }
    return(opt)
  }
  
  # index = 2
  q2.2.1.2 <- function(df, coef, opt){
    # Question 2.2.1 / 2.2.2 "Indicate the predictor's name to be added to the model"
    # also applicable for question at step 2.2.2: 
    #   "Indicate the predictor's name to be removed from the model"
    repeat{
      pred.name <- readline("| Please indicate the name of predictor: ")
      cat("\n")
      
      # add by william at 2017-04-10
      if(!is.na(pred.name) & pred.name != ""){
        i = 1
        repeat{
          if(i>=2) break
          pred.name.t <- search.variables(colnames(e$df0),pred.name) #colnames(e$df0)
          pred.index <- readline("| Please show me the INDEX : ")
          pred.index <- strsplit(trimws(pred.index),",")[[1]] %>% as.numeric()
          pred.search.result <- pred.name.t[pred.index][1]
          if(length(pred.search.result) >= 1 & !is.na(pred.search.result)){
            pred.name <-  pred.search.result
            break
          }else{
            message("! Wrong input ! \n")
          }
          i = i + 1
        }
      } 
      # end of change
      
      
      if(opt == 1){
        # Check if the predictor exists in the df 
        if(!pred.name %in% names(df)){
          message('| The predictor "', pred.name, '" does not exist in the dataset!\n')
        }else if (pred.name %in% names(coef)){
          message('| The predictor "', pred.name, '" already exists in the model!\n')
        }else break
      }else{
        if(!pred.name %in% names(coef)){
          message('| The predictor "', pred.name, '" does not exist in the model!\n')
        }else break
      }
    }
    return(pred.name)
  }
  
  # index = 3
  q2.2.1.1 <- function(pred){
    # Question 2.2.1.1 "Transformation method"
    repeat{
      cat(
        paste('| Which kind of transformation does the variable "', pred, '" need to be adapted?', sep=""),
              "|  1. carry over + s curve",
              "|  2. carry over + power curve",
              "|  3. auto-selection between 1 and 2",
              "|  4. lag only",
              "|  5. lag + carry over",
							# "|  6. lag + carry over + s curve",
              "|  6. lag + carry over + power curve",
              "",
              "|  0. no transformation", "", sep="\n")
      opt <- readline("| Please enter an option: ")
      cat("\n")
      if(!opt %in% as.character(0:6)){
        message("| Only the number between 0 and 6 is acceptable, dude!\n")
      }else{
        opt <- as.numeric(opt)
        break
      }
    }
    return(opt)
  }
  
  # index = 4
  q2.2.1.3 <- function(){
    # Question 2.2.1.3 - "Are you satisfied with the recommended parameters?"
    repeat{
      cat('| Are you satisfied with the recommended parameters? ',
          '|   1. I want to change the parameters',
          '|   2. I want to change the transformation method',
          '|   3. I want to change a predictor',
          '|   4. Yes, I am OK with the transformation',
          '', sep = '\n')
      satis <- readline("| Please enter an option: ")
      cat("\n")
      if(!satis %in% as.character(1:4)){
        message("| Only a number between 1 and 4 is acceptable!\n")
      }else{
        satis <- as.numeric(satis)
        break
      }
    }
    return(satis)
  }
  
  # index = 5
  q2.2.1.3_sub <- function(){
    # Question 2.2.1.3 - sub - "Which transformation method do you want to apply? "
    # when q2.2.1.1 opt == 3 and q2.2.1.3 == 1
    repeat{
      cat('| Which transformation method do you want to apply?',
          "|   1. carry over + s curve",
          "|   2. carry over + power curve",
          "", sep="\n")
      opt <- readline("| Please enter an option: ")
      cat("\n")
      if(!opt %in% as.character(1:2)){
        message("| Only 1 and 2 is acceptable, dude!\n")
      }else{
        opt <- as.numeric(opt)
        break
      }
    }
    return(opt)
  }
  
  # index = 7
  q2.2.1.3.1_pc <- function(){
    # Question 2.2.1.3.1 - pc "Please enter the parameters (carry-over + power curve) you want to try"
    repeat{
      co.r <- readline("| Please enter the alternative carry-over rate: ")
      cat("\n")
      if(!all(strsplit(co.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    repeat{
      pc.r <- readline("| Please suggest the alternative power curve rate: ")
      cat("\n")
      if(!all(strsplit(pc.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    return(as.numeric(c(NA, co.r, pc.r, NA, NA)))
  }
  
  # index = 8
  q2.2.1.3.1_sc <- function(){
    # Question 2.2.1.3.1 - sc "Please enter the parameters (carry-over + s curve) you want to try"
    repeat{
      co.r <- readline("| Please enter the alternative carry-over rate: ")
      cat("\n")
      if(!all(strsplit(co.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    repeat{
      sc.1 <- readline("| Please suggest the 1st alternatives curve rate: ")
      cat("\n")
      if(!all(strsplit(sc.1, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    repeat{
      sc.2 <- readline("| Please suggest  the 2nd alternative s curve rate: ")
      cat("\n")
      if(!all(strsplit(sc.2, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    return(as.numeric(c(NA, co.r, NA, sc.1, sc.2)))
  }
  
  # index = 9
  q2.2.2.2 <- function(){
    # Question 2.2.2.2 "Do you confirm removing this variable?"
    repeat{
      conf.remv <- readline(paste("| Do you confirm removing", e$pred, "(Y/N)? ", sep = " "))
      cat("\n")
      if(!toupper(conf.remv) %in% c("Y","N")){
        message("| Only Y/y and N/n is acceptable!\n")
      }else break
    }
    return(toupper(conf.remv))
  }
  
  # index = 10
  q2.2.3.2 <- function(){
    # Question 2.2.3.2 "Do you want to perform stepwise regression check?"
    repeat{
      stepaic <- readline("| Do you want to do stepwise regression check (Y/N)? ")
      cat("\n")
      if(!toupper(stepaic) %in% c("Y","N")){
        message("| Only Y/y and N/n is acceptable!\n")
      }else{
        aic <- ifelse(toupper(stepaic) == "Y", T, F)
        break
      } 
    }
    return(aic)
  }
  
  # index = 11
  q2.2.1.3.2_lg <- function(){
    # Question 2.2.1.3.2 - lg "Please enter the parameters no.lag you want to try"
    repeat{
      no.lag <- readline("| Please enter the alternative number of lag: ")
      cat("\n")
      if(!all(strsplit(no.lag, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    return(as.numeric(c(no.lag, NA, NA, NA, NA)))
  }
  
  # index = 12
  q2.2.1.3.2_lc <- function(){
    # Question 2.2.1.3.2 - lc "Please enter the parameters (no.lag + co.r) you want to try"
    repeat{
      no.lag <- readline("| Please enter the alternative number of lag: ")
      cat("\n")
      if(!all(strsplit(no.lag, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    repeat{
      co.r <- readline("| Please enter the alternative carry-over rate: ")
      cat("\n")
      if(!all(strsplit(co.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    return(as.numeric(c(no.lag, co.r, NA, NA, NA)))
  }
  
  # index = 13
  q2.2.1.3.2_lcs <- function(){
    # Question 2.2.1.3.2 - lc "Please enter the parameters (no.lag + co.r + 2 sc.r) you want to try"
    repeat{
      no.lag <- readline("| Please enter the alternative number of lag: ")
      cat("\n")
      if(!all(strsplit(no.lag, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    repeat{
      co.r <- readline("| Please enter the alternative carry-over rate: ")
      cat("\n")
      if(!all(strsplit(co.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    repeat{
      sc.1 <- readline("| Please suggest the 1st alternatives curve rate: ")
      cat("\n")
      if(!all(strsplit(sc.1, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    repeat{
      sc.2 <- readline("| Please suggest  the 2nd alternative s curve rate: ")
      cat("\n")
      if(!all(strsplit(sc.2, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    return(as.numeric(c(no.lag, co.r, NA, sc.1, sc.2)))
  }
  
  # index = 14
  q2.2.1.3.2_lcp <- function(){
    # Question 2.2.1.3.2 - lc "Please enter the parameters (no.lag + co.r + pc.r) you want to try"
    repeat{
      no.lag <- readline("| Please enter the alternative number of lag: ")
      cat("\n")
      if(!all(strsplit(no.lag, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    repeat{
      co.r <- readline("| Please enter the alternative carry-over rate: ")
      cat("\n")
      if(!all(strsplit(co.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    repeat{
      pc.r <- readline("| Please suggest the alternative power curve rate: ")
      cat("\n")
      if(!all(strsplit(pc.r, split = "")[[1]] %in% c(as.character(0:9), "."))) {
        message("| Please do enter a number!\n")
      } else break
    }
    return(as.numeric(c(no.lag, co.r, pc.r, NA, NA)))
  }
  
  
  
  L_index <- LETTERS[index]
  switch(L_index,
         A = q2.2(),
         B = q2.2.1.2(df, coef, opt),
         C = q2.2.1.1(pred),
         D = q2.2.1.3(),
         E = q2.2.1.3_sub(),
         G = q2.2.1.3.1_pc(),
         H = q2.2.1.3.1_sc(),
         I = q2.2.2.2(),
         J = q2.2.3.2(),
         K = q2.2.1.3.2_lg(),
         L = q2.2.1.3.2_lc(),
         M = q2.2.1.3.2_lcs(),
         N = q2.2.1.3.2_lcp())
}
#=====================================================================

#--------#
# warn() #
#--------#
warn <- function(fit1, fit2, p.cons = 0.2) {
  # 1. warn user when there is some coefficient which changes **sign** between two models
  # 2. warn user when there is big gap between **p-value** of one pred in two models 
  
  message("go to warn")
  
  if(!is.null(names(coef(fit2)))) {
    
    coef2 <- coef(fit2)
    name2 <- names(coef2)
    p.value2 <- coef(summary(fit2))[, 4]
    
    if(sum(p.value2 > p.cons) > 0){
      message("P-value of the following predictor", 
              if(sum(p.value2 > p.cons) > 1) "s are" else " is", 
              " larger than ", as.character(p.cons), sep = "")
      cat(paste(names(p.value2)[p.value2 > p.cons], collapse = ", "), "", sep = "\n")
      cat(paste(rep("-", 40), collapse = ""), "", sep = "\n")
    }
    
    
    if(!is.null(names(coef(fit1)))){
      coef1 <- coef(fit1)    
      name1 <- names(coef1)
      p.value1 <- coef(summary(fit1))[, 4]
      
      name <- intersect(name1, name2)
      
      sign1 <- sign(coef1[which(name1 %in% name)])
      sign1 <- sign1[order(names(sign1))]
      
      sign2 <- sign(coef2[which(name2 %in% name)])
      sign2 <- sign2[order(names(sign2))]
      
      if(any(sign1 * sign2 == -1)) {
        message("Sign of the following predictor's coefficient has changed!", sep = "")
        cat(paste(names(sign1)[sign1 * sign2 == -1], collapse = ", "), "", sep = "\n")
        cat(paste(rep("-", 40), collapse = ""), "", sep = "\n")
      }
    } 
    
  } 
  
} # end function warn()



#=====================================================================
#------------------#
# show.filename() #
#-----------------#

show.filename <- function(filetype = "raw data", show.related = T, 
                          x.include  = c("DATA|SALES|TRAFFIC|WINPIN|WIN|PIN|RAW|EQR"), 
                          x.exclude = c("PRMT")){
  # add by william at 2017-03-30
  message("go to show.filename")
  repeat{
    message("| Working dictionary files are listed below")
    message("| ---------------------------------------------")
    
    if(show.related){
      message("showrelated1")
      wd.rawfile <- list.files() 
      wd.rawfile <- wd.rawfile[grep(filetype,wd.rawfile)] %>% as.data.frame()
    }else{
      message("showrelated2")
      wd.rawfile <- list.files() 
      wd.rawfile <- wd.rawfile[dplyr::setdiff(c(1:length(wd.rawfile)),grep(x.exclude,wd.rawfile %>% toupper()))] 
      #wd.rawfile <- wd.rawfile[grep(x.include,wd.rawfile %>% toupper())]
      wd.rawfile <- wd.rawfile %>% as.data.frame()
    }
    
    wd.rawfile.name <- show.related
    message("wd.rawfile.name:")
    message(wd.rawfile.name)
    #modified by ren 2018/3/29
    #wd.rawfile <- wd.rawfile %>% mutate(ID = wd.rawfile.name)
    wd.rawfile <- wd.rawfile %>% mutate(ID = c(1:nrow(wd.rawfile)))
    names(wd.rawfile) <- c("filename","id")
    wd.rawfile.print <- paste(wd.rawfile$id,wd.rawfile$filename, sep = " -> ")
    
    message(paste("| ",wd.rawfile.print,collapse = "\n"))
    cat("\n")
    data.name <- readline(paste0("| Please enter the name of ",filetype," file: "))
    message(data.name)
        
    if(!is.na(as.numeric(data.name))){
      message("go to as.numeric(data.name)")
      data.no <- as.numeric(data.name)
      message("data.no")
      message(data.no)
      if(data.no > nrow(wd.rawfile)){
        message()
        message('| no."', data.name, '" file does not exist in StepReg3 system work directory!')
        message('| Please re-check it in the file explorer!\n')
        data.name <- "invalid_file_name"
      }else{
        message("go to data.no <")
        message(wd.rawfile)
        message(wd.rawfile$id)
       data.name <- wd.rawfile %>% dplyr::filter( id %in% data.no) %>% dplyr::select(filename) %>% unlist() %>% as.vector()
       message("data.name")
       message(data.name)
       }
      #e$filename <- data.name
    }else {
      message("go to no as.numeric(data.name)")
      data.name <- ""
    }
    message (data.name)
    if(!data.name %in% c("","invalid_file_name")){
      return(data.name)
      break
    }
  }
}


#=====================================================================
#--------------------#
# search.variables() #
#--------------------#
# searching variables by using liklihood match..
# example : search.variables(x, "TV;OOH;DGT")

search.variables <- function(cols, txt){
  message("search variable")
  cols <- cols %>% toupper()
  txt <- txt %>% toupper()
  term.like <- "%"
  term.sep <- ";"
  #txt <- c("LAUN%0.95;ABS%SD")
  s1 <- strsplit(txt,term.sep)[[1]];
  s1.n <- length(s1);
  s2 <- strsplit(s1,term.like);
  s2.n <- list();

  fx.match <- function(cols, txt){  
    if.txt.there <- grepl(txt,paste(cols, collapse = ";"))
    if(if.txt.there){
      cols.id <- grep(txt,cols)
      cols.names <- cols[cols.id]
      # show.message <- paste("| ",cols.id," -> ", cols.names, sep = "" , collapse = "\n")
      # message("| Searching vars... like >>> '",txt,"'")
      # message("| ",rep("-",45))
      # message(show.message,"\n")
    }else{
      # message("| There doesn't exists the variable.\n")
    }
    return(cols.names)
  }
  
  for(i in 1:s1.n){s2.n[[i]] <- length(s2[[i]])};
  s2.n <- s2.n %>% dplyr::combine()
  cols.output <- list()
  for(i in 1:s1.n){
    s <- s2[[i]]
    for (j in 1:s2.n[i]) {
      if(j == 1){
        try(cols.next <- fx.match(cols = cols, txt = s[j]), silent = T)
        # try(cols.output[[i]] <- cols.next, silent = T)
      }else{
        try(cols.next<-  fx.match(cols = cols.next, txt = s[j]),silent = T)
      }
      try(cols.output[[i]] <- cols.next,silent = T)
    }
  }
  # merge the result
  cols.output <- cols.output %>% combine() %>% unique() 
  
  if(length(cols.output) > 0){
  cols.index <- cols.output%>% as.data.frame() %>% rownames()
  
  show.message <- paste("| ",cols.index," -> ", cols.output, sep = "" , collapse = "\n")
  message("| Searching vars... like >>> '",txt,"'")
  message("| ",rep("-",45))
  message(show.message,"\n")
  return(cols.output)
  }else{
    message("There doesn't exist the variable !")
    return(F)
  }
  
}

#=====================================================================
#----------------#
# read.rawdata() #
#----------------#

read.rawdata <- function(){
  # add by william at 2017-03-30
  message("go to read.rawdata")
  
  data.name <- show.filename()
  endstr <- substr(data.name, nchar(data.name) - 3, nchar(data.name))
  
  # loop controller
  loop.flag = F
  
  cat("\n")
  if (!endstr %in% c(".csv", "xlsx", ".xls")){
    message('| Only "*.csv", "*.xlsx" or "*.xls" file is expected!\n')
  }else if(!file.exists(data.name)){
    message('| "', data.name, '" does not exist in StepReg3 system work directory!')
    message('| Please re-check it in the file explorer!\n')
  }else {
    # data can be input correctly
    if(endstr == ".csv"){
      output.data <- read.csv(data.name, stringsAsFactors = F)
    }else{ # if(endstr %in% c("xlsx", ".xls"))
      sht.names <- names(getSheets(loadWorkbook(data.name)))
      repeat{
        sht.name <- readline("| Please enter the name of the worksheet: ")
        cat("\n")
        if (!sht.name %in% sht.names){
          message('| The worksheet "', sht.name, '" is not found!\n')
        }else{
          break
        }
      }
      output.data <- read.xlsx(data.name, sheetName = sht.name, stringsAsFactors = F)
      loop.flag = T
    }
    
    message('| "',data.name, '" is loaded.') 
    message('| There are ', dim(output.data)[1], ' observations and ', 
            dim(output.data)[2], ' variables in the raw dataset.')
    cat("\n")
    
    # Check NA in the input data frame
    check.na <- function(x)sum(is.na(x))
    num.na <- sapply(output.data, check.na)
    prmt.flag <- grepl("prmt", tolower(data.name))
    
    if(sum(num.na)!=0 & !prmt.flag){
      message(paste('| There ', if(sum(num.na!=0)==1)'is ' else 'are ',
                    sum(num.na!=0),
                    if(sum(num.na!=0)==1)' variable' else ' variables',
                    ' with NAs!', sep = ""))
      cat("\n")
      message(paste('| The', if(sum(num.na!=0)==1)' variable is: ' else ' variables are: ', sep = " "))
      message("| ", paste(names(which(num.na != 0)), sep = ", "), ". \n")
      message("| Please double check your raw data!\n")
      loop.flag = F 
    }else if(prmt.flag){
      loop.flag = T
    }else {
      loop.flag = T
    }
  }

  output <- list(data.name, output.data, loop.flag)
  return(output)
  # end of change  
}



#=====================================================================
#------------------------#
# select variables set() #
#------------------------#

subset.rawdata <- function(){
  
}


#=====================================================================
#------------------------#
# download from Database() #
#------------------------#

download.rawdata <- function(){
  
}



#=====================================================================
#-----------#
# ini.lasso() #
#-----------#

ini.lasso.lars <- function( x.tvar, y.resp , x.data , x.row ){
  
  # Initiation
  #################################################################################
  # x.data = e$data
  # y.resp = e$resp
  # x.tvar = e$tvar
  # x.row = 20
  
  # Data reshape
  #################################################################################
  df0 = x.data;
  resp = y.resp;
  
  x.data <- x.data[x.row:nrow(x.data),]
  x.data[is.na(x.data)]<-0
  y = x.data %>% dplyr::select(one_of(y.resp)) %>% unlist() %>% as.matrix()
  x.data = x.data %>% dplyr::select( -one_of(x.tvar, y.resp)) %>% as.matrix()
  y.resp = y
  x.data <-apply(x.data,2,function(x){
    x = as.numeric(x)
    x[is.na(x)] = 0
    return(x)})
  
  message(paste("| response name  = ",resp))
  message(paste("| response obs count = ",length(y.resp)))
  message(paste("| indep obs count = ",nrow(x.data)))  
  message(paste("| indep vars count = ",ncol(x.data)))
  cat("\n| $y[1:10]\n")
  print(y.resp[1:10])
  cat("| $x[1:10,1:10]\n")
  print(x.data[1:10,1:10])  
  
  # Lasso
  #################################################################################
  i = 1
  repeat{

    lars = NULL
    best = NULL
    # Runing the lasso
    try(las <- lars::lars(x.data, y.resp, type="lasso"),silent = F)
    # K = 10 or 5
    try(cvlas <- lars::cv.lars(x.data, y.resp, K = 10, plot.it = TRUE, se = TRUE), silent = F)
    # get the best model
    try(best <- cvlas$index[which.min(cvlas$cv)], silent = F)
    best = ifelse(is.na(best)|is.null(best),0,best)
    
    message(paste0("| Lasso # ",i," ... ","lambda.min = ",round(best,2)))    
    if(best > 0){break}
    if(i > 9) break
    i = i + 1
  }
  
  ## Plot ##
  #################################################################################
  predlas <- lars::predict.lars(las, newx= x.data, type="fit", mode="fraction", s=best)
  p.fit <-predlas$fit
  p.result <- data.frame(cbind(data.frame(y.resp),p.fit))
  
  # lasso. fit vs. actual
  x_axis = data.frame(1:nrow(p.result))
  fit.resp <- p.result$p.fit
  act.resp <- p.result$y.resp
  fit_to_plot <- data.frame(x_axis, fit.resp, act.resp)
  if(!class(fit_to_plot$x_axis)[1] %in% c("POSIXct", "POSIXt", "Date", "numeric", "integer")){
    fit_to_plot$x_axis <- seq_along(1:nrow(fit_to_plot))
  }
  
  fit_vs_act <- fit_to_plot                                       %>%
    ggplot(aes(x = x_axis))                                       +
    geom_line(aes(y = act.resp), colour = "darkgrey", size = 1)   +
    geom_line(aes(y = fit.resp), colour = "red", size = 1)        +
    ggtitle("Lasso. Modeled vs. Actual Variable")                +
    ylab("Count / Volume")                                        +
    theme(legend.position = "right", 
          axis.title.x = element_blank())
  print(fit_vs_act)
  
  
  # calculation coef
  #################################################################################
  coef <- coef.lars(las, mode = "fraction", s = best) %>%  unlist() %>% as.data.frame() 
  coef$VAR = rownames(coef);rownames(coef) = NULL
  colnames(coef) = c("coefficent","variable")
  coef = coef[,2:1]
  
  coef <- coef %>% arrange(desc(coefficent))
  coef.tbl <- coef %>% 
    dplyr::filter(coefficent !=0) %>% 
    dplyr::select(variable) %>% 
    dplyr::mutate(no.lag = NA, co.r = NA, 
                  pc.r = NA, sc.1 = NA, sc.2 = NA)
  
  
  
  # load to e.prmt
  #################################################################################
  coef.tbl$variable = coef.tbl[[1]]
  coef.tbl$no.lag = coef.tbl[[2]] %>% as.numeric()
  coef.tbl$co.r = coef.tbl[[3]] %>% as.numeric()
  coef.tbl$pc.r = coef.tbl[[4]] %>% as.numeric()
  coef.tbl$sc.1 = coef.tbl[[5]] %>% as.numeric()
  coef.tbl$sc.2 = coef.tbl[[6]] %>% as.numeric()
  coef.tbl$status = rep("alive",nrow(coef.tbl))
  
  
  
  # write to local folder
  #################################################################################
  coef %>% print()
  message("\n| Please find the prmt.lasso.csv in the folder.\n| ",getwd(),"\n| ",nrow(coef)," Variables in the sheet.", sep = "")
  write.csv(coef.tbl,"prmt.lasso.csv", row.names = F)
  write.csv(coef,"prmt.lasso.coef.csv", row.names = F)
  
  
  
  # rebuild pass the parameter
  #################################################################################
  e$rebuild.Flag = F
  if(e$rebuild.Flag){
    # skip the following codes
    e$prmt <- coef.tbl
    for(i in 1:nrow(coef.tbl)){
      pred <- as.character(coef.tbl[[1]][i])
      no.lag <- as.numeric(coef.tbl[[2]][i])
      co.r <- as.numeric(coef.tbl[[3]][i])
      pc.r <- as.numeric(coef.tbl[[4]][i])
      sc.1 <- as.numeric(coef.tbl[[5]][i])
      sc.2 <- as.numeric(coef.tbl[[6]][i])
      
      df0 <- modif(pred, resp, df0, c(no.lag, co.r, pc.r, sc.1, sc.2))
    }
    e$df1 <- df0
    e$fit1 <- lm(as.formula(paste(resp, paste(coef.tbl[[1]], collapse = " + "), sep = " ~ ")), 
                 data = e$df1, na.action = na.exclude)
    # coef is na -> 0 
    e$fit1$coefficients -> coef.tmp
    coef.tmp[is.na(coef.tmp)] <-0
    e$fit1$coefficients <- coef.tmp
  }else{
    repeat{
      rec <- readline("| Do you want to continue testing with lasso ini-model (Y/N)? ")
      cat("\n")
      if (!toupper(rec) %in% c("Y","N")){
        message("| Only Y/y and N/n is acceptable!\n")
      } else if (toupper(rec) == "Y"){
        e$prmt.lasso.flag = T
        rebuild(e$resp, e$data, e$st.row)
        loop.output(e$resp, e$df1, e$fit1, pred = NULL, e$tvar)
        warn(fit1 = NULL, fit2 = e$fit1, p.cons = 0.2)
        e$prmt.lasso.flag = F
        break
      } else break
    }
  }
  
}

ini.lasso.glmnet <- function( x.tvar, y.resp , x.data , x.row ,x.penalty.vars ){
  
  # Initiation
  ################################################################################
  # x.data = e$data
  # y.resp = e$resp
  # x.tvar = e$tvar
  # x.row = 20
  
  # Data reshape
  #################################################################################
  df0 = x.data;
  resp = y.resp;
  
  x.data <- x.data[x.row:nrow(x.data),]
  x.data[is.na(x.data)]<-0
  y = x.data %>% dplyr::select(one_of(y.resp)) %>% unlist() %>% as.matrix()
  x.data = x.data %>% dplyr::select( -one_of(x.tvar, y.resp)) %>% as.matrix()
  y.resp = y
  x.data <-apply(x.data,2,function(x){
    x = as.numeric(x)
    x[is.na(x)] = 0
    return(x)})
  
  # done ->
  x = x.data
  y = y.resp
  
  # Penalty control
  ##################################################################################
  if(length(x.penalty.vars)==0){
    e$penalty.flag = F}
  
  if(e$penalty.flag){
      spare.var = x.penalty.vars
      p.fac = rep(1,ncol(x))
      col <-colnames(x)
      p.fac[grep(merge.txt(spare.var),col)] <- 0
  }else{
    p.fac = rep(1,ncol(x))
  }
  
  # Lasso
  #################################################################################
  i = 1
  repeat{
    message(paste0("| Lasso # ",i," ..."))
    glmnet.fit = NULL
    best = NULL
    
    # Runing the lasso
    try(glmnet.fit <- glmnet(x,y, 
                        family = "gaussian", 
                        nlambda = 50, 
                        penalty.factor = p.fac, alpha = 1),silent = F)
    # fit %>% plot( xvar = "lambda", label = T)
    glmnet.fit
    
    # K determine the model's stablity, if the obs is not enough < 500 please set the K = 5
    try(cvfit <-cv.glmnet(x,y,family = "gaussian", 
                                   type.measure = "mse" ,
                                   penalty.factor = p.fac,
                                   nfolds = 10, 
                                   parallel = T))
    # get the best model
    try(best <- cvfit$lambda.min, silent = F)
    best = ifelse(is.na(best)|is.null(best),0,best)
    
    if(best > 0){break}
    if(i > 10) break
    i = i + 1
  }
  
  # calculation coef
  #################################################################################
  # get the coef
  pred <- predict(cvfit, newx = x, s = "lambda.min") %>% as.vector()
  coef <- coef(cvfit,s=c("lambda.min"))
  coef <- coef %>% as.matrix() %>% as.data.frame()
  coef$varialbe = rownames(coef)
  rownames(coef) = NULL
  coef = coef[,2:1]
  colnames(coef) = c("variable","coefficent")
  coef <- coef %>% mutate(coefficent = round(coefficent,3))%>% arrange(desc(coefficent))
  # remark penalty vars
  
  if(e$penalty.flag){
  coef$penalty <- apply(coef %>% as.matrix(),1, function(x){
                                 a = x[1] 
                                 b = x[2] %>% as.numeric()
                                  if(a %in% spare.var & abs(b)>=1){
                                    return("**")
                                  }else if(a %in% spare.var & abs(b)<1){
                                    return("*")
                                  }else{
                                    return("")
                                  }
                                  })
  }else{
    coef$penalty = ""
  }

  # prmt file
  coef.tbl <- coef %>% 
    dplyr::filter(coefficent !=0 & !variable %in% c("Intercept","(Intercept)")) %>% 
    dplyr::select(variable) %>% 
    dplyr::mutate(no.lag = NA, co.r = NA, 
                  pc.r = NA, sc.1 = NA, sc.2 = NA)
  
  ## Plot ##
  #################################################################################
  # lasso. fit vs. actual
  x.n = 180
  x.max = nrow(x)
  x_axis = data.frame((x.max-x.n+1):x.max)
  fit.resp <- pred[(x.max-x.n+1):x.max]
  act.resp <- y[(x.max-x.n+1):x.max]
  fit_to_plot <- data.frame(x_axis, fit.resp, act.resp)
  if(!class(fit_to_plot$x_axis)[1] %in% c("POSIXct", "POSIXt", "Date", "numeric", "integer")){
    fit_to_plot$x_axis <- seq_along(1:nrow(fit_to_plot))
  }
  fit_vs_act <- fit_to_plot                                       %>%
    ggplot(aes(x = x_axis))                                       +
    geom_line(aes(y = act.resp), colour = "darkgrey", size = 1)   +
    geom_line(aes(y = fit.resp), colour = "red", size = 1)        +
    ggtitle("Lasso. Modeled vs. Actual Variable")                 +
    ylab("Count / Volume")                                        +
    theme(legend.position = "right", 
          axis.title.x = element_blank())
  print(fit_vs_act)
  

  # load to e.prmt
  #################################################################################
  coef.tbl$variable = coef.tbl[[1]]
  coef.tbl$no.lag = coef.tbl[[2]] %>% as.numeric()
  coef.tbl$co.r = coef.tbl[[3]] %>% as.numeric()
  coef.tbl$pc.r = coef.tbl[[4]] %>% as.numeric()
  coef.tbl$sc.1 = coef.tbl[[5]] %>% as.numeric()
  coef.tbl$sc.2 = coef.tbl[[6]] %>% as.numeric()
  coef.tbl$status = rep("alive",nrow(coef.tbl))
  
  
  # write to local folder
  #################################################################################
  coef %>% dplyr::filter(coefficent != 0) %>% print()
  message("\n| Please find the prmt.lasso.csv in the folder.\n| ",getwd(),"\n| ",nrow(coef)," Variables in the sheet.", sep = "")
  write.csv(coef.tbl,"prmt.lasso.csv", row.names = F)
  write.csv(coef,"prmt.lasso.coef.csv", row.names = F)
  
  
  # rebuild pass the parameter
  #################################################################################
  e$rebuild.Flag = F
  if(e$rebuild.Flag){
    # skip the following codes
    e$prmt <- coef.tbl
    for(i in 1:nrow(coef.tbl)){
      pred <- as.character(coef.tbl[[1]][i])
      no.lag <- as.numeric(coef.tbl[[2]][i])
      co.r <- as.numeric(coef.tbl[[3]][i])
      pc.r <- as.numeric(coef.tbl[[4]][i])
      sc.1 <- as.numeric(coef.tbl[[5]][i])
      sc.2 <- as.numeric(coef.tbl[[6]][i])
      
      df0 <- modif(pred, resp, df0, c(no.lag, co.r, pc.r, sc.1, sc.2))
    }
    e$df1 <- df0
    
    # lm model
    e$fit1 <- lm(as.formula(paste(resp, paste(coef.tbl[[1]], collapse = " + "), sep = " ~ ")), 
                 data = e$df1, na.action = na.omit)
    
  }else{
    repeat{
      rec <- readline("| Do you want to continue testing with lasso ini-model (Y/N)? ")
      cat("\n")
      if (!toupper(rec) %in% c("Y","N")){
        message("| Only Y/y and N/n is acceptable!\n")
      } else if (toupper(rec) == "Y"){
        e$prmt.lasso.flag = T
        e$prmt.bug.vif = T
        rebuild(e$resp, e$data, e$st.row)
        loop.output(e$resp, e$df1, e$fit1, pred = NULL, e$tvar)
        warn(fit1 = NULL, fit2 = e$fit1, p.cons = 0.2)
        e$prmt.lasso.flag = F
        break
      } else break
    }
  }
  
}


#=====================================================================
#-----------#
# rebuild() #
#-----------#

rebuild <- function(resp, data, st.row) {
  
  # resp and data is already in the global environment
  # data is raw without modification
  # source(modif)
  
  # need one step to confirm the model, then 
  # fit <- fit.temp; df <- df.temp
  
  message("go to rebuild")
  
  message("| You're expected to input the Parameters file. ")
  message("| Make sure the full data file and Parameters file are RELATED! ")
  message("| Make sure the data structure is the same as StepReg3 TOOL OUTPUTS! \n")
  
  repeat{
    
    # alter by william in 2017-03-28
    if(e$prmt.lasso.flag){
      message('| loading file --> prmt.lasso.csv ...')
      prmt.file = "prmt.lasso.csv"}else{
        # william modified at 2017-04-05
        prmt.file <- show.filename("prmt", show.related = T)
        # prmt.file <- readline('| Please enter the name of "prmt" file: ')
      }
    
    cat("\n")
    endstr <- substr(prmt.file, nchar(prmt.file)-3, nchar(prmt.file))
    if (!endstr %in% c(".csv", "xlsx", ".xls")){
      message('| Only ".csv", "*.xlsx" or "*.xls" file is expected!\n')
    }else if(file.exists(prmt.file)){
      if(endstr == ".csv"){
        e$prmt <- read.csv(prmt.file, stringsAsFactors = FALSE)
        # names(e$prmt) <- c("variable", "co.r", "pc.r", "sc.1", "sc.2", "status")
      } else {
        sht.names <- names(getSheets(loadWorkbook(prmt.file)))
        repeat{
          sht.name2 <- readline("| Please enter the name of Parameters worksheet: ")
          cat("\n")
          if (!sht.name2 %in% sht.names){
            message('| The worksheet "', sht.name2, '" is not found!\n')
          }else break
        }
        e$prmt <- read.xlsx(prmt.file, sheetName = sht.name2, stringsAsFactors = F)
        # names(e$prmt) <- c("variable", "co.r", "pc.r", "sc.1", "sc.2", "status")
      }
      break
    }else if(!file.exists(prmt.file)){
      message('| "', prmt.file, '" does not exist in the work directory!')
      message('| Please re-check it in the file explorer!\n')
    }
  }
  
  e$prmt$status <- factor(e$prmt$status, levels = c("alive", "dead"))
  prmt.alive <- e$prmt[which(e$prmt$status == "alive"),]
  df0 <- data
  
  for(i in 1:nrow(prmt.alive)){
    pred <- as.character(prmt.alive[[1]][i])
    no.lag <- as.numeric(prmt.alive[[2]][i])
    co.r <- as.numeric(prmt.alive[[3]][i])
    pc.r <- as.numeric(prmt.alive[[4]][i])
    sc.1 <- as.numeric(prmt.alive[[5]][i])
    sc.2 <- as.numeric(prmt.alive[[6]][i])
    
    df0 <- modif(pred, resp, df0, c(no.lag, co.r, pc.r, sc.1, sc.2))
  }
  e$df0 <- df0
  e$df1 <- df0[st.row:dim(df0)[1], ]
  e$fit1 <- lm(as.formula(paste(resp, paste(prmt.alive[[1]], collapse = " + "), sep = " ~ ")), 
               data = e$df1, na.action = na.exclude)
  
  
  # updated by william at 2017-03-29
  #########################################################################
  # regenerate coef , this is a bug , wait to be fixed
  # vif(fit) error
  if(e$prmt.bug.vif){
    coef <- e$fit1$coefficients %>% as.matrix() %>% as.data.frame()
    coef$varialbe = rownames(coef)
    rownames(coef) = NULL
    coef = coef[,2:1]
    colnames(coef) = c("variable","coefficent")
    coef <- coef %>% mutate(coefficent = round(coefficent,3))%>% arrange(desc(coefficent))
    # remark penalty vars
    
    if(e$penalty.flag){
    coef$penalty <- apply(coef %>% as.matrix(),1, function(x){
      a = x[1]
      b = x[2] %>% as.numeric()
      if(a %in% e$penalty.vars & abs(b)>=1){
        return("**")
      }else if(a %in% e$penalty.vars & abs(b)<1){
        return("*")
      }else{
        return("")
      }
    })}else{
      coef$penalty = ""
    }
    # prmt file
    coef.tbl <- coef %>% 
      dplyr::filter(coefficent !=0 & !variable %in% c("Intercept","(Intercept)")) %>% 
      dplyr::select(variable) %>% 
      dplyr::mutate(no.lag = NA, co.r = NA, 
                    pc.r = NA, sc.1 = NA, sc.2 = NA)
    
    # load to e.prmt
    #################################################################################
    coef.tbl$variable = coef.tbl[[1]]
    coef.tbl$no.lag = coef.tbl[[2]] %>% as.numeric()
    coef.tbl$co.r = coef.tbl[[3]] %>% as.numeric()
    coef.tbl$pc.r = coef.tbl[[4]] %>% as.numeric()
    coef.tbl$sc.1 = coef.tbl[[5]] %>% as.numeric()
    coef.tbl$sc.2 = coef.tbl[[6]] %>% as.numeric()
    coef.tbl$status = rep("alive",nrow(coef.tbl))
    
    # lm model
    e$fit1 <- lm(as.formula(paste(resp, paste(coef.tbl[[1]], collapse = " + "), sep = " ~ ")), 
                 data = e$df1, na.action = na.exclude)
    
    e$prmt <- coef.tbl
    write.csv(coef.tbl,"prmt.lasso.csv", row.names = F)
    # end of the bug vif(fit)
  }
  
  
  # update by william at 2017-03-29
  # e$fit1$coefficients <- na.omit(e$fit1$coefficients)
  # e$fit1$ <- na.omit(e$fit1$coefficients)  
  
  
} # end of function rebuild()

#=====================================================================

#-------------------#
# arrange_ggplot2() #
#-------------------#

## Function for arranging ggplots. use png(); arrange(p1, p2, ncol=1); dev.off() to save.
suppressMessages(require(grid))
vp.layout <- function(x, y) {
  viewport(layout.pos.row=x, layout.pos.col=y)
}
arrange_ggplot2 <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
  dots <- list(...)
  n <- length(dots)
  if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
  if(is.null(nrow)) { nrow = ceiling(n/ncol)}
  if(is.null(ncol)) { ncol = ceiling(n/nrow)}
  ## NOTE see n2mfrow in grDevices for possible alternative
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
  ii.p <- 1
  for(ii.row in seq(1, nrow)){
    ii.table.row <- ii.row    
    if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
    for(ii.col in seq(1, ncol)){
      ii.table <- ii.p
      if(ii.p > n) break
      print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
      ii.p <- ii.p + 1
    }
  }
} # end of function arrange_ggplot2()

#=====================================================================

#---------------#
# lag.freq.set() #
#---------------#

lag.freq.ini <- function(){
    # interactive choosing 
    lag.freq.set <- function(){
      # lag freq
      repeat{
        cat("| What kind of lag frequency do you want ? \n")
        cat("|   a. daily\n|   b. weekly\n|   c. monthly\n")
        lag.freq <- tolower(readline("| Please choose a format: "))
        cat("\n")
        if(!lag.freq %in% c("a","b","c")){
          message('| Only "a", "b" or "c" is acceptable!\n')
        }else break
      }
  
      # pass the lag granularity
      e$lag.granularity.new <- switch(lag.freq, 
                                      a = "d", 
                                      b = "w",
                                      c = "m")
      
      # setting loop controller
      if( e$lag.granularity == e$lag.granularity.new ){
        message("| Lag frequency didn't change...(Y/N) \n")
        lag.break.opt<- readline("| or if you want to skip press <ENTER> ")
        
        # Loop control
        if(lag.break.opt == "") {
          e$jump.loop = T
          }else{e$jump.loop = F }
      
      }else{
        
        e$lag.granularity = e$lag.granularity.new
        message("| We are going to re-run prmt.all with lag freq ", e$lag.granularity)
        e$cal.duration.recom <- system.time({e$tm.prmt <- recom(e$pred, e$resp, e$df0, e$tm, e$fit1, e$st.row)})[3]
        e$cal.duration.modif <- system.time({e$df.temp0 <- modif(e$pred, e$resp, e$df0, e$tm.prmt)})[3]   
        
        cat("\n")
        message("| Glimpse prmt.test.all ...\t",nrow(e$prmt.all[[e$pred]])," rows")
        print(e$prmt.all[[e$pred]] %>% head())
        cat("\n")
        
        cat("\n")
        message("| Cal prmt.test.all cost ...   \t",round(e$cal.duration.trans,0)," s")
        message("| Get recommonded prmt cost ...\t",round(e$cal.duration.recom,0)," s")
        cat("\n")
      }
      # end of lag freq
    }
    
    # ini the prmt rerun flag
    # bug bug bug here for loop
    e$prmt.rerun.flag = T
    prmt.rerun.opt.2 = ""
    repeat{
      
      if(prmt.rerun.opt.2 == "Y"){
        prmt.rerun.opt <- prmt.rerun.opt.2
        cat("\n")
      }else{
        prmt.rerun.opt <- readline("| Do you want to change the lag frequency to re-run the prmt.all? (Y/N) ")
        cat("\n")
      }
      
      if(!toupper(prmt.rerun.opt) %in% c("Y", "N")){
        message("| Only Y/y and N/n is acceptable!\n")
      } else if (toupper(prmt.rerun.opt) %in% c("Y")) {
        if(e$prmt.rerun.flag){
          
          lag.freq.set()
          if(e$jump.loop) break
          
          repeat{   
          prmt.rerun.opt.2 <- readline("| Do you want to try again ? (Y/N) ")
          cat("\n")
            if(!toupper(prmt.rerun.opt.2) %in% c("Y", "N")){
              message("| Only Y/y and N/n is acceptable!\n")
            } else if (toupper(prmt.rerun.opt.2) %in% c("Y")) {
              e$prmt.rerun.flag = T
              break
            }else if (toupper(prmt.rerun.opt.2) %in% c("N")) {
              e$prmt.rerun.flag = F
              break
            }else{
              message("| Only Y/y and N/n is acceptable!\n")
            }
            }
          
        } else break
      }else if(prmt.rerun.opt %in% c("","N")){
        break
        }
    }
}


#=====================================================================

#---------------#
# loop.output() #
#---------------#

loop.output <- function(resp, data, fit, pred, tvar) {                  
  # Input: resp(name), data(modified, st.row:dim(df0)[1]), fit, tvar(name)  				
  # Output: only on memory and screen, nothing to files
  
  # Consists of 4 parts:  Part. I  Summary of Fit & MAPE
  #                       Part. II  Plots
  #                       Part. III  DW-test
  #                       Part. IV  Contribution Ranking
  
  # Needed Packages: "zoo" (for library(lmtest)), "lmtest" (for dwtest())
  
  cat(paste(rep("-+-",20),collapse=""),"\n\n")
  readline('You can find the output of the model below.\nIn next 5 steps, please press <Enter> to continue...')
  cat("\n")
  
  #---------------------------------------------------------------
  # Part. I  Summary of Fit
  #---------------------------------------------------------------
  
  readline("Part. I  Summary of Fit")
  print(summary(fit))	
  cat("\n")
  
  #---------------------------------------------------------------
  # Part. II  MAPE
  #---------------------------------------------------------------
  
  readline("Part. II  MAPE")
  resp.temp <- data[[resp]]
  resp.temp[which(resp.temp == 0)] <- mean(data[[resp]]) 
  mape <- mean(abs(fit$residuals/resp.temp))
  e$mape <- mape
  cat("MAPE of the model is ", round(mape, 4),"\n", sep = "")
  cat("\n")
  
  
  #---------------------------------------------------------------
  # Part. III  Plots
  #---------------------------------------------------------------
  
  na <- readline("Part. III  Plots")
  message("Please look at the Plots area!\n")
  
  x_axis <- data[[tvar]]
  df_to_plot <- data.frame(x_axis, res = summary(fit)$residuals)
  # cat(dim(df_to_plot))
  # cat(head(df_to_plot$x_axis))
  if(!class(df_to_plot$x_axis)[1] %in% c("POSIXct", "POSIXt", "numeric", "Date", "integer")){
    df_to_plot$x_axis <- seq_along(1:nrow(df_to_plot))
  }
  # 1. scatterplot of residuals
  scat_res <- df_to_plot                                %>%
    ggplot(aes(x = x_axis, y = res))                     +
    geom_point(colour = "orange", size = 3)              +
    theme(legend.position = "none")                      +
    geom_hline(yintercept = 0, colour = "red")           +
    ggtitle("Plot 1. Scatterplot of Model Residuals")    +
    xlab("Observation Index")                            +
    ylab("Residuals")
  
  # 2. histogram of residuals
  max_res <- max(df_to_plot$res)
  min_res <- min(df_to_plot$res)
  mean_res <- mean(df_to_plot$res)
  binrange <- (max_res - min_res)/40
  
  hist_res <- df_to_plot                                           %>%
    ggplot(aes(x = res))                                            +
    geom_histogram(stat = "bin", fill = "orange", 
                   colour = "white", binwidth = binrange)           +
    geom_vline(xintercept = mean_res, colour = "red")               +
    ggtitle("Plot 2. Histogram of Model Residuals")                 +
    xlab("Residual Interval")                                       +
    ylab("Residuals")
  
  # 3. latest predictor
  if(!is.null(pred)){
    modif_pred <- data.frame(x_axis, predictor = data[[pred]])
    pred_line <- ggplot(modif_pred, aes(x_axis))                    +
      geom_line(aes(y = predictor), colour = "darkgrey", size = 1)  +
      ggtitle(paste("Plot 3. Modified Value of ", pred, sep = " ")) +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank())
  }
  
  # 4. fit vs. actual
  fit.resp <- fit$fitted.values
  act.resp <- data[,resp]
  fit_to_plot <- data.frame(x_axis, fit.resp, act.resp)
  if(!class(fit_to_plot$x_axis)[1] %in% c("POSIXct", "POSIXt", "Date", "numeric", "integer")){
    fit_to_plot$x_axis <- seq_along(1:nrow(fit_to_plot))
  }
  
  fit_vs_act <- fit_to_plot                                       %>%
    ggplot(aes(x = x_axis))                                       +
    geom_line(aes(y = act.resp), colour = "darkgrey", size = 1)   +
    geom_line(aes(y = fit.resp), colour = "red", size = 1)        +
    ggtitle("Plot 4. Modeled vs. Actual Variable")                +
    ylab("Count / Volume")                                        +
    theme(legend.position = "right", 
          axis.title.x = element_blank())
  
  if(is.null(pred)){
    arrange_ggplot2(scat_res, hist_res, fit_vs_act, ncol = 1)
  } else{
    arrange_ggplot2(scat_res, hist_res, ncol = 1)
    arrange_ggplot2(pred_line, fit_vs_act, ncol = 1)
  }
  
  #---------------------------------------------------------------
  # Part. IV  DW-test
  #---------------------------------------------------------------
  
  na <- readline("Part. IV  DW-test")
  print(dwtest(fit))
  
  #---------------------------------------------------------------
  # Part. V  Contribution Rates
  #---------------------------------------------------------------    
  
  na <- readline("Part. V  Top Contribution")
  cat("\n")
  
  # contri.d: draft    
  simulation <- cbind(coef(fit)[1], t(t(as.matrix(data[, names(coef(fit))[-1]])) * as.vector(coef(fit)[-1])))
  colnames(simulation) <- names(coef(fit))
  contri.d <- colSums(simulation)/sum(fit$fitted.values)
  
  # contri.p: positive
  contri.p <- contri.d[which(contri.d >= 0)]
  contri.p <- as.matrix(contri.p[order(contri.p, decreasing = T)])
  
  contri.top10 <- as.matrix(head(contri.p[which(rownames(contri.p) != "(Intercept)")], 10))
  colnames(contri.top10) <- "Top POSITIVE Contributors"
  rn.p <- rownames(contri.p)[which(rownames(contri.p) != "(Intercept)")]
  rownames(contri.top10) <- if (length(rn.p)>10) rn.p[1:10] else rn.p
  
  
  # contri.n: negative
  contri.n <- contri.d[which(contri.d < 0)]
  contri.n <- as.matrix(contri.n[order(contri.n, decreasing = F)])
  
  contri.bot5 <- as.matrix(head(contri.n[which(rownames(contri.n) != "(Intercept)")], 5))
  colnames(contri.bot5) <- "Top NEGATIVE Contributors"
  rn.n <- rownames(contri.n)[which(rownames(contri.n) != "(Intercept)")]
  rownames(contri.bot5) <- if (length(rn.n)>5) rn.n[1:5] else rn.n
  
  
  # message('Voila the top contributor(s) to the variable "', resp, '":', sep ="")
  if(nrow(contri.top10) > 0){
    print(contri.top10)
    cat("\n")
  }
  if(nrow(contri.bot5) > 0){
    print(contri.bot5)
    cat("\n")
  }
  
  
  #---------------------------------------------------------------
  # Part. VI  VIF (Updated on Friday 12/19/2014)
  #---------------------------------------------------------------  

  if(length(coef(fit)) > 2) {
    vif <- as.matrix(vif(fit))
    colnames(vif) <- "VIF"
    vif.var <- rownames(vif)
    vif.sig <- apply(vif,1,function(x){if(x>=10){
                        return(">= 10")
                      }else if(x>=5){
                        return(">= 5")
                      }else if(x<5 & x>2){
                        return("*")
                      }else{
                        return("**")
                        }
                        })
    
    vif2 <-vif %>% as.data.frame()  %>% dplyr::mutate(SIG = vif.sig, VAR = vif.var) %>% dplyr::select(VAR,VIF,SIG) %>% dplyr::arrange(VIF)
    na <- readline("Part. VI  VIF")
    cat("\n")
    print(vif2)
    cat("\n")
    
    if(any(vif >= 10)){
      message(paste("VIF of", paste(rownames(vif)[which(vif >= 10)], collapse = ", "), 
                    if(length(which(vif >= 10)) > 1) "are" else "is", 
                    "bigger than 10! ", sep = " "))
      cat("\n")
    }
  }
  
  cat(paste(rep("-+-",20),collapse=""),"\n\n")
  
  
} # end of function loop.output()

#=====================================================================

#----------------#
# final.output() #
#----------------#

final.output <- function(resp, data, rawdata, tvar, fit, prmt, aic = FALSE) {
  # prmt is a dataframe including 8 columns: 
  #       variable, trans.meth, co.r, sc.1, sc.2, pc.r, oth, status
  # data = df1 # Transformed Data
  
  # Needed Packages: "MASS" for stepAIC(), "car" for vif(), "xlsx" for export
  # these 3 packages will be loaded outside this function
  
  message("go to final.output")
  message(resp)
  message(tvar)
  message(fit)
  message(prmt)
  message(aic)
  if(aic) aic.ind <- "aic." else aic.ind <- NULL
  # 1.Transformation (trans.meth, co.r, sc.1, sc.2, pc.r, oth)
  # directly output as a csv file
	# prmt.csv: for convenience of importing model next time
	write.csv(prmt, paste(aic.ind, "prmt.csv", sep = ""), row.names = FALSE)
	message(paste('| The variable parameters history is exported to "',
								paste(aic.ind, "prmt.csv", sep = ""),
								'". \n',sep=""))
	cat(paste(rep("-+-",20),collapse=""),"\n\n")

  # 2. Coefficients
	message("Coefficients check")
  coef <- coef(summary(fit))     # Estimate  Std. Error    t value   Pr(>|t|)
  
  # 3. VIF
  message("VIF check")
  if(length(coef(fit)) > 2) {
    vif <- vif(fit)
  } else {
    vif <- NA
  }
  
  message("coef,VIF are finished")
  
  # 4. Contribution    
  simulation <- cbind(coef(fit)[1], t(t(as.matrix(data[, names(coef(fit))[-1]])) * as.vector(coef(fit)[-1])))
  colnames(simulation) <- names(coef(fit))
  contri.d <- colSums(simulation)/sum(fit$fitted.values)

  # 5. Residuals
  resid <- cbind(data[, c(tvar, resp)], fit$fitted.values, summary(fit)$residuals)
  rownames(resid) <- NULL
  colnames(resid) <- c(tvar, resp, "Prediction", "Residuals")
	resid <- cbind(resid, simulation)
	
  # Merge model information into one data frame
  # Output csv
  pos <- which(colnames(prmt) == "status")
  prmt.alive <- prmt[which(prmt[[pos]] == "alive"), -pos]
  
  if(rownames(coef)[1] == "(Intercept)"){
    prmt.alive <- rbind(NA, prmt.alive)
    prmt.alive[1, 1] <- "(Intercept)"
    vif <- rbind(NA, as.matrix(vif))
  }
  model <- data.frame(prmt.alive[, 1], coef[, 1], prmt.alive[, -1], coef[, -1], 
                      vif = vif, contribution = contri.d, stringsAsFactors = F)
  names(model) <- c("variable", "coefficient", "no.lag", "co.r", "pc.r",	"sc.1", "sc.2", 
                    "std.error", "t.value", "p.value", "vif", "contribution")
  
	# Make convenience for EFFICIENCY Calculation
	repeat{
		message("| Please enter the variable names whose EFFICIENCY needs to be computed: ")
		message("| If none, press <Enter> without typing any letters. \n")
		effi <- readline('| Please specify (use comma "," to split variable names): ')
		cat("\n")
		effivar <- strsplit(gsub(" ", "", toupper(effi)), ",")[[1]]
		
		# add by william 
		e$effivar <- effivar
		if(!all(effivar %in% names(coef(fit)))) {
			message("| Some variables you specified doesn't exist in the model! \n")
		} else break
	}
	
  if (length(effivar) == 0){
    effidf <- NULL
  }else{
    
    # modifed by Angela at 2017-04-10
    # 1 year refers to 365 days or 56 weeks
    if(e$granularity == "daily"){
      zerodf <- data.frame(matrix(data = 0, nrow = 365, ncol = length(effivar)), stringsAsFactors = FALSE)      
    }else if(e$granularity == "weekly"){
      zerodf <- data.frame(matrix(data = 0, nrow = 54, ncol = length(effivar)), stringsAsFactors = FALSE)        
    }else{
      zerodf <- data.frame(matrix(data = 0, nrow = 365, ncol = length(effivar)), stringsAsFactors = FALSE)        
    }
    
    colnames(zerodf) <- effivar
    # add by william at 2017-04-03
    effidfraw <- rbind(rawdata[, effivar], zerodf) # e$st.row:nrow(rawdata)
    effidf <- effidfraw
    # prmt is a dataframe including 8 columns: 
    #       variable, trans.meth, co.r, sc.1, sc.2, pc.r, oth, status
    for(var in effivar){
      tm.prmt <- dplyr::filter(prmt.alive, variable == var)[2:6]
      no.lag <- tm.prmt[[1]]
      co.r <- tm.prmt[[2]]
      pc.r <- tm.prmt[[3]]
      sc.1 <- tm.prmt[[4]]
      sc.2 <- tm.prmt[[5]]
      
      if(is.na(no.lag)){
        if(is.na(co.r)){
          effidf[[var]] <- effidf[[var]]
        }else	if(co.r == 1){
          effidf <- hmodif(var, resp, effidf)
        }else if(is.na(pc.r)){
          effidf[[var]] <- cs(effidf[[var]], co.r, sc.1, sc.2)
        }else{
          effidf[[var]] <- cp(effidf[[var]], co.r, pc.r)
        }
        effidf[[var]] <- effidf[[var]]*coef(fit)[var]
      }else{
        if(is.na(co.r)){
          effidf[[var]] <- lg(effidf[[var]], no.lag)
        }else if (is.na(pc.r)){
          effidf[[var]] <- lc(effidf[[var]], no.lag, co.r)
        }else {
          effidf[[var]] <- lcp(effidf[[var]], no.lag, co.r,pc.r)
        }
        effidf[[var]] <- effidf[[var]]*coef(fit)[var]
      }
      
    }
    
    
    # # add by william , this part is to show the sales\traffic per sales ...
    # e$effidf <- effidf
    # e$effi.raw <- rawdata
    # 
    # # the response variables , sales or traffic
    # sales <- e$effidf %>% tidyr::gather(var, value) %>% 
    #   dplyr::group_by(var) %>% dplyr::summarise( `sales/traffic.driven` = sum(value)) %>% 
    #   dplyr::ungroup()
    # 
    # sales.subset <- e$effidf[e$st.row:nrow(e$effidf),] %>% tidyr::gather(var, value) %>% 
    #   dplyr::group_by(var) %>% dplyr::summarise( `sales/traffic.driven` = sum(value)) %>% 
    #   dplyr::ungroup()
    # 
    # # the ATL\BTL spends or any cost related to money
    # spends <- e$effi.raw %>% dplyr::select(one_of(e$tvar, e$effivar)) %>%
    #   tidyr::gather(var,spends, -c(1)) %>% group_by(var) %>% 
    #   dplyr::summarise(spends = sum(spends)) %>% 
    #   ungroup()
    # 
    # spends.subset <- e$effi.raw[e$st.row:nrow(e$effi.raw),] %>% dplyr::select(one_of(e$tvar, e$effivar)) %>%
    #   tidyr::gather(var,spends, -c(1)) %>% group_by(var) %>% 
    #   dplyr::summarise(spends = sum(spends)) %>% 
    #   ungroup()
    # 
    # # coef
    # coef.dt <- e$fit1$coefficients %>% as.data.frame()
    # colnames(coef.dt) = c("coef");
    # coef.dt$var = rownames(coef.dt);
    # rownames(coef.dt) = NULL;
    # coef.dt = coef.dt[,2:1] %>% dplyr::filter(var %in% e$effivar) %>% dplyr::select(var,coef)
    # 
    # # summuary
    # e$efficency.sum <- dplyr::left_join(sales,spends, by = c("var") ) %>% dplyr::left_join(coef.dt, by = "var") %>% 
    #   dplyr::mutate( spends = round(spends,0),
    #                  `cost.per.sale/traffic` = round(spends/(`sales/traffic.driven`*coef),2),
    #                  `sales/traffic.driven` = round(`sales/traffic.driven`*coef,0),
    #                  `coef` = round(coef,2)
    #   ) %>% 
    #   dplyr::select(var, spends, `sales/traffic.driven`,`coef`, `cost.per.sale/traffic`) %>% 
    #   dplyr::rename(Pred.Variable = var,
    #                 `Total.Spends` = spends,
    #                 `Coefficent` = coef,
    #                 `Sales/Traffic.Driven` = `sales/traffic.driven`,
    #                 `Cost.per.Sales/Traffic` = `cost.per.sale/traffic`
    #   ) %>% 
    #   dplyr::arrange(desc(Total.Spends)) %>% 
    #   as.data.frame()
    # 
    # # subset
    # e$efficency.sum.subset <- dplyr::left_join(sales.subset,spends.subset, by = c("var") ) %>% dplyr::left_join(coef.dt, by = "var") %>% 
    #   dplyr::mutate( spends = round(spends,0),
    #                  `cost.per.sale/traffic` = round(spends/(`sales/traffic.driven`*coef),2),
    #                  `sales/traffic.driven` = round(`sales/traffic.driven`*coef,0),
    #                  `coef` = round(coef,2)
    #   ) %>% 
    #   dplyr::select(var, spends, `sales/traffic.driven`,`coef`, `cost.per.sale/traffic`) %>% 
    #   dplyr::rename(Pred.Variable = var,
    #                 `Total.Spends` = spends,
    #                 `Coefficent` = coef,
    #                 `Sales/Traffic.Driven` = `sales/traffic.driven`,
    #                 `Cost.per.Sales/Traffic` = `cost.per.sale/traffic`
    #   ) %>% 
    #   dplyr::arrange(desc(Total.Spends)) %>% 
    #   as.data.frame()
    # 
    # 
    # cat("\n")
    # message("Start Row = 1 ", "(",e$df0[1,1],")")
    # print(e$efficency.sum)
    # cat("\n")
    # message("Start Row = ",e$st.row," (",e$df0[e$st.row,1],")")
    # print(e$efficency.sum.subset)
    # cat("\n")
    # 
    # # end of change.
    
  }
  

  
  
  
  # write to local excel work book
  if(suppressMessages(require(openxlsx))){

    e$wb <- openxlsx::createWorkbook()
    
    openxlsx::addWorksheet(e$wb, sheetName = "Model_Results")
    openxlsx::addWorksheet(e$wb, sheetName = "Residuals")
    openxlsx::addWorksheet(e$wb, sheetName = "Raw_Data")
    openxlsx::addWorksheet(e$wb, sheetName = "Efficiency")
    openxlsx::addWorksheet(e$wb, sheetName = "Parameters")
    
    openxlsx::writeData(e$wb, "Model_Results", model, rowNames = FALSE)
    openxlsx::writeData(e$wb, "Residuals", resid, rowNames = FALSE)
    openxlsx::writeData(e$wb, "Raw_Data", rawdata[, c(tvar, resp, names(coef(fit))[-1])], rowNames = FALSE)
    openxlsx::writeData(e$wb, "Efficiency", effidf, rowNames = FALSE)
    # add by william
    openxlsx::writeData(e$wb, "Efficiency", e$efficency.sum , startRow =  1, startCol = ncol(effidf) + 3, borders = c("surrounding"), rowNames = F, colNames = T)
    openxlsx::writeData(e$wb, "Efficiency", e$efficency.sum.subset , startRow =  1, startCol = ncol(effidf)*2 + 3*2, borders = c("surrounding"), rowNames = F, colNames = T)
    
    openxlsx::writeData(e$wb, "Parameters", prmt, rowNames = FALSE)
    e$output.name <- paste("model_result", ifelse(aic, "_aic", "")," ",gsub("-|:", "", as.character(Sys.time())), ".xlsx", sep = "")
    openxlsx::saveWorkbook(e$wb,e$output.name , overwrite = T)
    message('| The model results are saved as "',e$output.name,'.xlsx".')

  }else{
    # 1. residuals.csv
    write.csv(resid, paste(aic.ind, "residuals.csv", sep = ""), row.names = FALSE)
    message(paste('| Value of response variable, prediction and residuals are exported to',
                  paste('the file "',paste(aic.ind, "residuals.csv", sep = ""),
                        '". ', sep=""), sep="\n"))
    cat(paste(rep("-+-",20),collapse=""))
    
    
    # 3. model.results.csv
    write.csv(model, paste(aic.ind, "model.results.csv", sep = ""), row.names = FALSE)
    message(paste('The modeling result is exported to "', 
                  paste(aic.ind, "model.results.csv", sep = ""),'".',sep=""))
    cat(paste(rep("-+-",20),collapse=""),"\n")
    
    # 4. transformed.data.csv
    # output transformed data to local file
    write.csv(rawdata[, c(tvar, resp, names(coef(fit))[-1])], paste(aic.ind, "transformed.data.csv", sep = ""))
		message(paste('The transformed data is exported to "', 
							paste(aic.ind, "transformed.data.csv", sep = ""),'".',sep=""))
    cat(paste(rep("-+-",20),collapse=""),"\n")
		
		# 5. efficiency.csv
		write.csv(effidf, paste(aic.ind, "efficiency.csv", sep = ""))
		message(paste('The efficiency calculation is exported to "', 
							paste(aic.ind, "efficiency.csv", sep = ""),'".',sep=""))
    cat(paste(rep("-+-",20),collapse=""),"\n")
  }
} # end of function final.output()

#=====================================================================

#-------------#
# check.cor() #
#-------------#
check.cor <- function(df){
  message("go to check.cor")
  cor.df <- cor(df)
  col.no <- ncol(df)
  mat <- as.data.frame(matrix(ncol = 3))
  colnames(mat) <- c("Var1","Var2","Correlation")
  for (i in 2:col.no){
    for (j in 1:(i-1)){
      if (is.na(mat[1,1])){
        mat[1,1] <- rownames(cor.df)[i]
        mat[1,2] <- colnames(cor.df)[j]
        mat[1,3] <- cor.df[i,j]
      }else{
        row.no <- nrow(mat)
        mat[row.no+1, 1] <- rownames(cor.df)[i]
        mat[row.no+1,2] <- colnames(cor.df)[j]
        mat[row.no+1,3] <- cor.df[i,j]
      }
    }
  }
  
  mat <- mat[order(abs(mat[,3]), decreasing =T),]
  rownames(mat) <- NULL
  n.cor <- if(nrow(mat)>20) 20 else nrow(mat)
  message("| The top ", n.cor, " (absolute) correlation rates are listed below: \n")
  print(mat[1:n.cor,])
  write.csv(mat, "correlation_pairs.csv", row.names = F)
  message('\n| The correlation pairs and rates are exported to "correlation_pairs.csv".\n')
}

#=====================================================================

# Structure Function: StepReg3()
StepReg3 <- function(){
  
  message("go to StepReg3")
  
  # STEP 0.5 
  # save original work directory for final recover
  if("wd_rcv" %in% ls(e)) setwd(e$wd_rcv) else e$wd_rcv <- getwd()
  
  # create computation cores and clusters
  e$core.number <- parallel::detectCores()
  e$core.number.active <- parallel::detectCores() - 1
  if(e$core.number.active == 0 ){
    e$core.number.active = 1
  }else if(e$core.number.active == 1){
    e$core.number.active = 2
  }else{
    e$core.number.active = e$core.number.active
  }
  
  e$cl <- parallel::makeCluster(e$core.number.active)
  registerDoParallel(e$cl)
  
  # STEP 0.6
  # set working directory
  if(substr(e$wd_rcv, nchar(e$wd_rcv) - 9, nchar(e$wd_rcv))  %in% c("StepReg3_WD","StepReg3_WD/") |
     substr(e$wd_rcv, nchar(e$wd_rcv) - 9, nchar(e$wd_rcv))  %in% c("tepReg3_WD","tepReg3_WD/") ){
    e$wd_rcv <- substr(e$wd_rcv, 1, nchar(e$wd_rcv) - 11)
  } else {
    if(!file.exists("StepReg3_WD")) dir.create("StepReg3_WD")
    setwd(paste(e$wd_rcv, "StepReg3_WD", sep = "/"))
  }
  
  
  e$fit1 <- NULL
  e$prmt <- data.frame(variable = character(),
                       no.lag = numeric(),
                       co.r = numeric(),
                       pc.r = numeric(),
                       sc.1 = numeric(),
                       sc.2 = numeric(),
                       status = character(),  #indicate the availability of variable for the model
                       stringsAsFactors = F)
  
  # add by william at 2017-03-31
  # the list is created to put the all lag+ c + s +.. parameter
  # T means positive , F means Negative
  e$prmt.all = list() 
  e$prmt.all.coef.flag = list() 
  e$prmt.all.coef.positive = list()
  e$prmt.all.coef.negative = list()
  e$prmt.lasso.flag = F
  e$prmt.bug.vif = F
  e$jump.loop = F
  # end of change
  
  # e$prmt[[6]] <- factor(prmt[[6]], levels = c("alive", "dead"))
  
  # STEP 1.1
  # Welcome!
  cat("\n")
  message("| Welcome to StepReg3 system!")
  message("| Please follow instructions to finish the regression.\n")
  message("| Be aware that in StepReg3 system the working directory is set as: ")
  message("| ", getwd(), ".\n")
  message("| It will be recovered to your default directory after the system is closed. \n")
  message("| there are ",e$core.number," cores in your PC, ",e$core.number.active," of them would be clustered into computing. \n")
  
  # STEP 1.2
  # Read Data
  repeat{
    rawdata <- read.rawdata()
    e$filename <- rawdata[[1]]
    e$data <- rawdata[[2]]
    e$read.raw.flag <- rawdata[[3]]
    if(e$read.raw.flag) break
  }
   
  
  # # backup - william at 2017-04-05 
  # message("| Working dictionary files are listed below")
  # message("| ---------------------------------------------")
  # wd.rawfile <- list.files() %>% as.data.frame()
  # wd.rawfile.name <- wd.rawfile %>% rownames()
  # wd.rawfile <- wd.rawfile %>% mutate(ID = wd.rawfile.name)
  # names(wd.rawfile) <- c("filename","id")
  # wd.rawfile.print <- paste(wd.rawfile$id,wd.rawfile$filename, sep = " -> ")
  # 
  # message(paste("| ",wd.rawfile.print,collapse = "\n"))
  # cat("\n")
  # data.name <- readline("| Please enter the name of raw data file: ")
  # 
  # # add by william at 2017-03-30
  # if(!is.na(as.numeric(data.name))){
  #   data.no <- as.numeric(data.name)
  #   if(data.no > nrow(wd.rawfile)){ 
  #     message('| no."', data.name, '" file does not exist in StepReg3 system work directory!')
  #     message('| Please re-check it in the file explorer!\n')
  #     dataname <- "invalid_file_name"
  #   }
  #   data.name <- wd.rawfile %>% dplyr::filter( id %in% data.no) %>% dplyr::select(filename) %>% unlist() %>% as.vector()
  #   e$filename <- data.name
  # }
  # # end of change  
  # 
  # endstr <- substr(data.name, nchar(data.name) - 3, nchar(data.name))
  # cat("\n")
  # if (!endstr %in% c(".csv", "xlsx", ".xls")){
  #   message('| Only "*.csv", "*.xlsx" or "*.xls" file is expected!\n')
  # }else if(!file.exists(data.name)){
  #   message('| "', data.name, '" does not exist in StepReg3 system work directory!')
  #   message('| Please re-check it in the file explorer!\n')
  # }else {
  #   # data can be input correctly
  #   if(endstr == ".csv"){
  #     e$data <- read.csv(data.name, stringsAsFactors = F)
  #   }else{ # if(endstr %in% c("xlsx", ".xls"))
  #     sht.names <- names(getSheets(loadWorkbook(data.name)))
  #     repeat{
  #       sht.name <- readline("| Please enter the name of the worksheet: ")
  #       cat("\n")
  #       if (!sht.name %in% sht.names){
  #         message('| The worksheet "', sht.name, '" is not found!\n')
  #       }else{
  #         break
  #       }
  #     }
  #     e$data <- read.xlsx(data.name, sheetName = sht.name, stringsAsFactors = F)
  #   }
  #   
  #   message('| "',data.name, '" is loaded.') 
  #   message('| There are ', dim(e$data)[1], ' observations and ', 
  #           dim(e$data)[2], ' variables in the raw dataset.')
  #   
  #   # Check NA in the input data frame
  #   check.na <- function(x)sum(is.na(x))
  #   num.na <- sapply(e$data, check.na)
  #   if(sum(num.na)!=0){
  #     message(paste('| There ', if(sum(num.na!=0)==1)'is ' else 'are ',
  #                   sum(num.na!=0),
  #                   if(sum(num.na!=0)==1)' variable' else ' variables',
  #                   ' with NAs!', sep = ""))
  #     cat("\n")
  #     message(paste('| The', if(sum(num.na!=0)==1)' variable is: ' else ' variables are: ', sep = ", "))
  #     message("| ", paste(names(which(num.na != 0)), sep = ", "), ". \n")
  #     message("| Please double check your raw data!\n")
  #   } else {
  #     break
  #   }
  # }
  
  
  # STEP 1.3 
  # choose the response variable
  repeat{
    data.names.print <-names(e$data) %>% as.vector()
    # message("| ------------------------------------------------------------------")
    # e$resp <- readline("| Type <Enter> to set response variable (sales/traffic): ") %>% toupper()
    # message("| Please enter the name of response variable: ") %>% toupper()
   
    message("| The response variables (sales/traffic/other) are listed below: ")
    # cat("\n")
    e$resp <- ""
    
    if(nchar(e$resp) == 0){
      # message("| We are going to match a response var in following list ")
      resp.guess <- grep("SALES|WINPIN|TRAFFIC|EQR",names(e$data))
      if(length(resp.guess) > 0){
        if(length(resp.guess) == 1){
          e$resp = names(e$data)[resp.guess]
          message(paste("| '",e$resp,"' is taken as response variable", sep = ""))
          cat("\n")
          break
          }else if(length(resp.guess) > 1){
          message("| Please pick one as response variable: ")
          message("| ",rep("-",60))
          message(paste("| ",
                  names(e$data)[resp.guess] %>% as.data.frame() %>% rownames()%>% unlist() %>% as.vector(),
                  " -> ",
                  names(e$data)[resp.guess] %>% unlist() %>% as.vector(),
                  collapse = "\n"))
          cat("\n")
          resp.no <- readline("| Please enter the name of response variable: ") %>% as.numeric()
          if(resp.no > dim(e$data)[2] | resp.no <0 |is.na(resp.no) |resp.no == ""){
            e$resp <- "NA"
          }else{
            e$resp <- names(e$data)[resp.guess][resp.no]
          }
          cat("\n")
          if (e$resp %in% names(e$data)){
            break
          }else{
            message(paste('| The variable "', e$resp, 
                          '" does not exist in the database!', sep=""))
            cat("\n")
          }    
        }
      }
    }else if (e$resp %in% names(e$data)){
      break
    }else{
      message(paste('| The variable "', e$resp, 
                    '" does not exist in the database!', sep=""))
      cat("\n")
    }    
  }
  
  # STEP 1.4
  # choose the timeline variable and its format
  repeat{
    e$tvar <- readline("| Please enter the name of time variable: ") %>% toupper()
    cat("\n")
    
    if(nchar(e$tvar) == 0){
      tvar.guess <- grep("DATE",names(e$data)[1:20])
      if(length(tvar.guess) > 0){
        if(length(tvar.guess) == 1){
          e$tvar = names(e$data)[tvar.guess]
          message(paste("| '",e$tvar,"' is token as time variable", sep = ""))
          cat("\n")
          break
        }else if(length(tvar.guess) > 1){
          message("| Please pick one as time variable: ")
          print(names(e$data)[tvar.guess])
          e$tvar <- readline("| Please enter the name of time variable: ")
          cat("\n")
          if (e$tvar %in% names(e$data)){
            break
          }else{
            message(paste('| The variable "', e$tvar, 
                          '" does not exist in the database!', sep=""))
            cat("\n")
          }    
        }
      }
    }else if (e$tvar %in% names(e$data)){
      break
    }else{
      message(paste('| The variable "', e$tvar, 
                    '" does not exist in the database!', sep=""))
      cat("\n")
    }    
    
  }
  
  # STEP 1.4.1
  # add by william at 2017-03-30
  # auto detect the time format
  tmp.date <- e$data[[e$tvar]]
  if(typeof(e$data[[e$tvar]]) == "character"){
    guess.format <- guess_formats(tmp.date, c("mdY", "Ymd", "dmY"))
    guess.format <- data.frame(guess.format) %>% group_by(guess.format) %>% tally() %>% arrange(desc(n))
    guess.format <- guess.format[1,1] %>% unlist() %>% as.vector()
    e$data[[e$tvar]] <- as.Date(strptime(tmp.date, guess.format))
    
    cat(paste0("| The data type of the time variable is '", class(e$data[[e$tvar]]), "'."),
        paste0("| The data format is ",guess.format,"."),
        paste0("| The first element of this variable is '", e$data[[e$tvar]][1], "'.\n"),
        sep = "\n")
    
  }else if(typeof(e$data[[e$tvar]]) == "numeric"){
    e$data[[e$tvar]] <- e$data[[e$tvar]]  %>% as.Date(origin = "1899-12-30")
    message("| Data format is Y%m%d")
    cat(paste0("| The data type of the time variable is '", class(e$data[[e$tvar]]), "'."),
        paste0("| The data format is Y%m%d"),
        paste0("| The first element of this variable is '", e$data[[e$tvar]][1], "'.\n"),
        sep = "\n")
    
  }else{
    repeat{
      cat("| Which kind of date format is to be adapted? \n")
      cat("|   a. m/d/y\n|   b. d/m/y\n|   c. y/m/d\n")
      t_f <- tolower(readline("\n| Please choose a format: "))
      cat("\n")
      if(!t_f %in% c("a","b","c")){
        message('| Only "a", "b" or "c" is acceptable!\n')
      }else break
    }
    e$data[[e$tvar]] <- switch(t_f, 
                               a = mdy(e$data[[e$tvar]]), 
                               b = dmy(e$data[[e$tvar]]), 
                               c = ymd(e$data[[e$tvar]]))
  }

  # add by william to control lag + carryover related stuff.
  repeat{
    cat("| What kind of data frequency it is? \n")
    cat("|   a. daily\n|   b. weekly\n")
    gran <- tolower(readline("\n| Please choose a format: "))
    cat("\n")
    if(!gran %in% c("a","b")){
      message('| Only "a", "b" is acceptable!\n')
    }else break
  }
  e$granularity <- switch(gran, 
                          a = "daily", 
                          b = "weekly"
  )
  # lag freq
  repeat{
    cat("| What kind of lag frequncy do you want ? PLEASE choose a \n")
    cat("|   a. daily\n|   b. weekly\n|   c. monthly")
    lag.freq <- tolower(readline("\n| Please choose a format: "))
    cat("\n")
    if(!lag.freq %in% c("a","b","c")){
      message('| Only "a", "b" or "c" is acceptable!\n')
    }else break
  }
  e$lag.granularity <- switch(lag.freq, 
                          a = "d", 
                          b = "w",
                          c = "m"
  )
  ### end of change
  # } else {
  #   e$data[[e$tvar]] <- ymd(e$data[[e$tvar]])
  # }
  e$df0 <- e$data
  
  # STEP 1.5
  # choose start row: e$st.row
  repeat{
    st.row <- readline("| Please enter the row number you want to start modeling: ")
    cat("\n")
    if( st.row == ""){
      st.row <- readline("| If you want to proceed with all observations, please press <ENTER>, otherwise please provide an number. \t")
      if(st.row == ""){
        e$st.row = 1
        try(message("| Start date is ",e$df0[e$st.row,e$tvar]),silent = F)
        try(message("|  End  date is ",e$df0[nrow(e$df0),e$tvar]),silent = F)
        cat("\n")
        break
      }else{
        if(all(strsplit(st.row, split = "")[[1]] %in% as.character(0:9))) {
          e$st.row <- as.numeric(st.row)
          try(message("| Start date is ",e$df0[e$st.row,e$tvar]),silent = F)
          try(message("|  End date is ",e$df0[nrow(e$df0),e$tvar]),silent = F)
          cat("\n")
          break
        }
      }
    } else if(all(strsplit(st.row, split = "")[[1]] %in% as.character(0:9))) {
      e$st.row <- as.numeric(st.row)
      try(message("| Start date is ",e$df0[e$st.row,e$tvar]),silent = F)
      try(message("|  End  date is ",e$df0[nrow(e$df0),e$tvar]),silent = F)
      cat("\n")
      break
    }else{
      message("Please do enter an integer!\n")
    }    
  }
  
  
  e$df1 <- e$df0[e$st.row:nrow(e$df0), ]
  
  # Check the correlations
  repeat{
    cor.opt <- readline("| Do you want to check correlations between variables (Y/N) ? ")
    cat("\n")
    
    if(cor.opt == ""){
      break
    }else if(!toupper(cor.opt) %in% c("Y", "N")){
      message("| Only Y/y and N/n is acceptable!\n")
    } else if (toupper(cor.opt) == "Y") {
      check.cor(e$data[e$st.row:nrow(e$data),-which(colnames(e$data) == e$tvar)])
      break
    } else break
  }
  
  
  # STEP 1.5.1 - lasso 
  # initiate the model by using lasso
  repeat{
    rec <- readline("| Do you want to initiate the model by using lasso? PLEASE input N ")
    cat("\n")
    if (!toupper(rec) %in% c("Y","N")){
      message("| Only Y/y and N/n is acceptable!\n")
    } else if (toupper(rec) == "Y"){
      
    # choose the lasso type
    repeat{
      cat("| Which kind of lasso is to be applied to select variables ? \n")
      cat("|   a. glmnet(with penalty option)\n|   b. randomforest(precision best)\n|   c. lars\n\n")
      lasso.opt <- tolower(readline("| Please choose the method: \n"))
      cat("\n")
      if(!lasso.opt %in% c("a","b","c")){
        message('| Only "a", "b" or "c" is acceptable!\n')
        }else if(lasso.opt %in% c("a")){
          # add by William at 2017-03-29
          message('| lasso-glmnet is initiating ... \n')
          repeat{
        		message("| Please enter the variable names whose PENALTY needs to be 0: ")
            message("| In another word, they are expected to show-up in model")
        		message("| If none, press <Enter> without typing any letters. \n")
        		penalty.vars <- readline('| Please specify (use comma "," to split variable names): ')
        		cat("\n")

        	  penalty.vars <- penalty.vars %>% term.clean() %>% as.vector()
        		if(!all(penalty.vars %in% names(e$df0))) {
        			message("| Some variables you specified doesn't exist in the model! \n")
        		} else break
          }
          e$penalty.vars <- penalty.vars
          e$penalty.flag = T
          break
        }else if(lasso.opt %in% c("b")){
          message('| Random-forest is not ready yet ! \n')
        }else break
    }
    
    switch(lasso.opt,
           a = ini.lasso.glmnet(e$tvar, e$resp, e$data, e$st.row, e$penalty.vars), 
           c = ini.lasso.lars(e$tvar, e$resp, e$data, e$st.row)
           )
    break
    }else break
  }
  
  # STEP 1.6 
  # rebuild the model if any model existed already
  repeat{
    rec <- readline("| Do you want to continue testing an existed model (Y/N)? ")
    cat("\n")
    if (!toupper(rec) %in% c("Y","N")){
      message("| Only Y/y and N/n is acceptable!\n")
    } else if (toupper(rec) == "Y"){
        rebuild(e$resp, e$data, e$st.row)
        repeat{
          rec <- readline("| Do you want to pull-out the data (Y/N)? ")
          cat("\n")
          if (!toupper(rec) %in% c("Y","N")){
            message("| Only Y/y and N/n is acceptable!\n")
          } else if (toupper(rec) == "Y"){
            try(write.csv(e$df0, paste0(substr(e$filename, 1, nchar(e$filename)-4),"_trasformed.csv"), row.names = F),silent = F)
            message('| "',paste0(substr(e$filename, 1, nchar(e$filename)-4),"_trasformed.csv"), '" is exported') 
            message('| There are ', dim(e$df0)[1], ' observations and ', 
                    dim(e$df0)[2], ' variables in the raw dataset.')
            cat("\n")
            repeat{
              rec <- readline("| Do you want to continue (Y/N)? ")
              cat("\n")
                if (!toupper(rec) %in% c("Y","N")){
                message("| Only Y/y and N/n is acceptable!\n")
                } else if (toupper(rec) == "Y"){
                message('| Terminated, and Goodbye !')
                cat("\n")
                break
                }
            }
          } else break
        }
        loop.output(e$resp, e$df1, e$fit1, pred = NULL, e$tvar)
        warn(fit1 = NULL, fit2 = e$fit1, p.cons = 0.2)
        break
    } else break
  }
  
  # big loop start: What's the next move? 
  repeat{
    a_nm <- questions(1) # STEP 2.2 answer for the next move
    if(a_nm == 1){ # opt1. Add a new predictor
      repeat{
        # STEP 2.2.1 predictor name to add
        e$pred <- questions(2, df = e$df1, coef = e$fit1$coef, opt = 1) 
        repeat{
          # STEP 2.2.1.1 transformation method
          e$tm <- questions(3, pred = e$pred) 
          if(e$tm %in% 1:6){ 
          # 0-no trans; 1-s curve; 2-power curve; 3-1 or 2;
            # 4-lag only; 5-lag + carryover;
            # 6-lag + carryover + power curve;
            # 7-lag + carryover + s curve
            # if(e$tm == 6) message("| This process takes around 10+ mins, please wait...\n")
            if(e$tm == 6){
              # modified by william at 2017-03-30, add e$lag options
              message("| This process takes around 5-8 mins, please wait...")
              e$tm <- e$tm + 1 # the choice is 6 applied to function G with index 7 
            } 
            
            
            # modified by william at 2017-03-30
            e$cal.duration.recom <- system.time({e$tm.prmt <- recom(e$pred, e$resp, e$df0, e$tm, e$fit1, e$st.row)})[3]
            e$cal.duration.modif <- system.time({e$df.temp0 <- modif(e$pred, e$resp, e$df0, e$tm.prmt)})[3]
            
            cat("\n")
            message("| Glimpse prmt.test.all ...\t",nrow(e$prmt.all[[e$pred]])," rows")
            print(e$prmt.all[[e$pred]] %>% head())
            cat("\n")
            message("| Cal prmt.test.all cost ...   \t",round(e$cal.duration.trans,0)," s")
            message("| Get recommonded prmt cost ...\t",round(e$cal.duration.recom,0)," s")
            cat("\n")
            
            if(e$prmt.all.coef.flag[[e$pred]]){
              message("| There have positive coefficent in prmt.all")
              message("| (only p-val <= 0.3 and positive rows being kept)")
              message("| ------------------------------------------------------------------")
              print(e$prmt.all.coef.positive[e$pred])
              cat("\n")
              e$prmt.rerun.flag = T
              lag.freq.ini()
              
            }else{
              message("| There doesn't have positive coefficent in prmt.all")
              e$prmt.rerun.flag = T
              lag.freq.ini()
            }            
            # end of modification
            
            
            
              # following code has been packaged into lag.freq.ini()
              # # +_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
              # repeat{
              #   prmt.rerun.opt <- readline("| Do you want to change the lag granularity to re-run the prmt.all? (Y/N)")
              #   cat("\n")
              #   
              #   if(!toupper(prmt.rerun.opt) %in% c("","Y", "N")){
              #     message("| Only Y/y and N/n is acceptable!\n")
              #   } else if (toupper(prmt.rerun.opt) %in% c("Y","")) {
              #     if(e$prmt.rerun.flag){
              #       lag.freq.set()
              #       prmt.rerun.opt <- readline("| Do you want to try again ? (Y/N)")
              #       cat("\n")
              #       if(!toupper(prmt.rerun.opt) %in% c("Y", "N")){
              #         message("| Only Y/y and N/n is acceptable!\n")
              #       } else if (toupper(prmt.rerun.opt) %in% c("Y")) {
              #         e$prmt.rerun.flag = T
              #       }else if (toupper(prmt.rerun.opt) %in% c("N")) {
              #         e$prmt.rerun.flag = F
              #       }else{
              #         message("| Only Y/y and N/n is acceptable!\n")
              #       }
              #     }
              #     break
              #   } else break
              # }
              # # +_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
              

            
          } else if(e$tm == 0){
            e$tm.prmt <- rep(NA, 5)
            e$df.temp0 <- e$df0
          } # else if(e$tm == 6){
# 						if(!all(e$df0[[e$pred]] %in% 0:1)){
# 							message("| The predictor contains values besides 0 or 1! ")
# 							message("| The transformation method is changed to No Transformation! \n")
# 							e$tm <- 0
# 							e$tm.prmt <- rep(NA, 5)
# 							e$df.temp0 <- e$df0
# 						} else {
# 							e$tm.prmt <- rep(1, 5)
# 							e$df.temp0 <- modif(e$pred, e$resp, e$df0, e$tm.prmt)
# 						}
# 					}
          e$df.temp1 <- e$df.temp0[e$st.row:nrow(e$df.temp0), ]
          e$fit.temp <- trial(e$df.temp1, e$resp, e$fit1, action = 1, e$pred)
            
          repeat{
            loop.output(e$resp, e$df.temp1, e$fit.temp, e$pred, e$tvar)
            warn(fit1 = e$fit1, fit2 = e$fit.temp, p.cons = 0.2)
            
            # STEP 2.2.1.3 answer for Satisfactory Question
            e$a_stsf <- questions(4) 
            if(e$a_stsf == 1){ # opt1 change transformation prmt
              if(e$tm == 3){
                e$tm.prmt <- if(questions(5) == 1) questions(8) else questions(7)
              } else if(e$tm == 1){
                e$tm.prmt <- questions(8)
              } else if(e$tm == 2){
                e$tm.prmt <- questions(7)
              } else if(e$tm == 4){
                e$tm.prmt <- questions(11)
              } else if(e$tm == 5){
                e$tm.prmt <- questions(12)
              #} else if(e$tm == 6){
              #  e$tm.prmt <- questions(13)
              } else if(e$tm == 7){
                e$tm.prmt <- questions(14)
              }
              
              e$df.temp0 <- modif(e$pred, e$resp, e$df0, e$tm.prmt)
              e$df.temp1 <- e$df.temp0[e$st.row:nrow(e$df.temp0), ]
              e$fit.temp <- trial(e$df.temp1, e$resp, e$fit1, action = 1, e$pred)             
            } else break # if(e$a_stsf %in% 2:4){ 
          }
          # if(e$a_stsf == "2"){ # change transformation method
          if(e$a_stsf %in% 3:4){
            break
          }   
        }
        # if(e$a_stsf == 3){ # change a predictor
        if(e$a_stsf == 4){ # satisfied
          e$df0 <- e$df.temp0
          e$df1 <- e$df.temp1
          e$fit1 <- e$fit.temp
          prmt.line <- data.frame(variable = e$pred, no.lag = e$tm.prmt[1],
                                  co.r = e$tm.prmt[2], pc.r = e$tm.prmt[3], 
                                  sc.1 = e$tm.prmt[4], sc.2 = e$tm.prmt[5], 
                                  status = "alive", stringsAsFactors = F)
          # names(e$prmt) <- c("variable", "no.lag", "co.r", "pc.r", "sc.1", "sc.2", "status")
          e$prmt <- rbind(e$prmt, prmt.line)
          break
        }
      }
    } else if(a_nm == 2){ # 2. Remove a predictor
      e$pred <- questions(2, df = e$df1, coef = e$fit1$coef, opt = 2) # predictor name to remove
      e$fit.temp <- trial(e$df1, e$resp, e$fit1, -1, e$pred)
      loop.output(e$resp, e$df1, e$fit.temp, e$pred, e$tvar)
      warn(fit1 = e$fit1, fit2 = e$fit.temp, p.cons = 0.2)
      e$a_conf <- questions(9) # STEP 2.2.2.2 confirm the removal or not
      if(e$a_conf == "Y"){
        e$fit1 <- e$fit.temp
        e$df0[[e$pred]] <- e$data[[e$pred]]
        e$df1 <- e$df0[e$st.row:nrow(e$df0), ]
        e$prmt[which((e$prmt[[1]] == e$pred) & (e$prmt[[7]] == "alive")), 7] <- "dead"
      } # else if(e$a_conf == "N") do nothing
    } else if(a_nm == 3){ # 3. Stop modeling
      final.output(e$resp, e$df1, e$data, e$tvar, e$fit1, e$prmt, questions(10))
      break
    }
  }

  message("| Thank you for using StepReg3 tool! Goodbye! \n")
  
  stopCluster(e$cl)
  
  # reset wd to the default after StepReg3 finished
  setwd(e$wd_rcv)
  
} # end of function StepReg3()


# Updated by william at 2017-03-28
####################################################################################
StepReg3.ini <- function(pkg = all.pcg){
  
  message("go to StepReg3.ini")
  # load packages
  req.pkg <- function(pkg){
    # detect the packages status
    new.pkg <- pkg[(!(pkg %in% installed.packages()[, "Package"]))|(pkg %in% old.packages()[, "Package"])]
    exist.pkg <- pkg[(!(pkg %in% installed.packages()[, "Package"]))]
    load.pkg <-function(pkg){sapply(pkg, library, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)}
    install.pkg <- function(pkg){if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)}
    
    if(length(exist.pkg) == 0) {
      load.pkg(pkg)
      return(T)
    }else{
      repeat{
        message('| the following r packages need to be installed in your PC')
        message('| Y = Yes, please update it automaticly')
        message('| N = I want to download it mannually')
        pkg.opt <- readline('| Please choose (Y/N) ...')
        cat("\n")
        if(!toupper(pkg.opt) %in% c("Y", "N")){
          message("| Only Y/y and N/n is acceptable!\n")
        } else if (toupper(pkg.opt) == "Y") {
          install.pkg(pkg)
          load.pkg(pkg)
          return(T)
          break
        } else break
      }
    }
    
  }
  
  # load packages, and start StepReg3
  StepReg3.load <- function(){
    message("go to StepReg3.load")
    repeat{
      message('| Now, the packages are well-loaded !')
      ini.opt <- readline('| Are you ready to start modeling right now ?\t')
      cat("\n")
      if(!toupper(ini.opt) %in% c("Y", "N")){
        message("| Only Y/y and N/n is acceptable!\n")
      } else if (toupper(ini.opt) == "Y") {
        message('| Initiating StepReg3()...')
        StepReg3()
        break
      } else break
    }
  }
  
  repeat{
    message('| Welcome to StepReg3 System !')
    message('| We are going to Load following packages, please make sure these packages are well-installed')
    req.pkg.flag <- req.pkg(pkg)    
    if(req.pkg.flag){
      StepReg3.load()
      break
    }
  }
} # end of function StepReg3.ini
StepReg3.ini(all.pcg)

