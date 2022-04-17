library(plotly)
#library(parallel)
library(foreach)
#library(doParallel)

max_table_entries = 100
L = 0
userInput = rbind(c(10, 12, 14),
                  c(0.05, 0.075, 0.1),
                  c(10.25, 12.5, 15))
colnames(userInput) = c("LoB1", "LoB2", "LoB3")
rownames(userInput) = c("mean", "CV", "P")
input_global = userInput
corr_global = rbind(c(1, 0.75, -0.25),
                    c(0.75, 1, 0.25),
                    c(-0.25, 0.25, 1))
n_global = 10000


numCores = detectCores() - 1
registerDoParallel(numCores)  # use multicore, set to the number of our cores

#testCorr = rbind(c(1, 0.75, -0.25),
#                 c(0.75, 1, 0.25),
#                 c(-0.25, 0.25, 1))


generateCorrRndNum = function(n, corr) {
  #browser()
  # Cholesky decomposition
  D = chol(corr)
  
  # Uncorrelated Random Normal numbers
  X = cbind(rnorm(n))
  for(i in 1:(nrow(corr)-1)) {
    X = cbind(X, rnorm(n))
  }

  # Correlated Random Normal numbers
  Y = X %*% D
  
  # Correlated Uniform Random numbers
  Y = pnorm(Y)
  return(Y)
  
}

get_sim_L = function(input) {
  
  mean = input[1,]
  CV = input[2,]
  stdDev = mean*CV
  m = log(mean) - (1/2)*log(1 + stdDev^2/mean^2)
  s = (log(1 + (stdDev^2)/(mean^2)))^(1/2)
  
  return(rbind(mean, CV, stdDev, m, s))
}

# Run monte carlo to get loss numbers
get_L = function() {
  # input variables
  mean = input_global['mean',]
  CV = input_global['CV',]
  
  #####################
  # Monte Carlo
  dim = dim(corr_global)[1]  # how many LoB's
  stdDev = mean*CV
  m = log(mean) - (1/2)*log(1 + stdDev^2/mean^2)
  s = (log(1 + (stdDev^2)/(mean^2)))^(1/2)
  
  # generate Scenarios
  Y = generateCorrRndNum(n_global, corr_global)
  
  # loss numbers (lognormal)
  for (i in 1:dim) {
    Y[,i] = qlnorm(Y[,i], m[i], s[i])
  }
  
  return(Y)
}

# Recalculate monte carlo numbers
reCalc_L = function() {
  L <<- get_L()
}

verify_fn = function() {
  reCalc_L()
  agg_stop_loss(input, L, c(10.25, 12.5, 15), 0.1, 0.1, returnOption = "separate_z", nonNegativeOutput = FALSE)
}

# Calculate the reinsurance premium
# .... L : Loss numbers for reinsurer
# .... type : "EVP", "SDP", "CoCP"
# .... (Expected value principle, Standard deviation principle, Cost of capital principle)
# .... args : (lambda, alpha, [r_CoC, "ES"/"VaR", alpha]) respectively
calculate_premium = function(L, type="EVP", args=NULL) {
  # dimensions
  n = dim(L)[1]
  n_LoB = dim(L)[2]
  
  # principles
  if (type == "EVP") { # Expected value principle
    if (is.null(args)) {
      lambda = 0
    } else {
      lambda = as.numeric(args)
    }
    premium = apply(L, 2, mean) * (1 + lambda)
    
  } else if (type == "SDP") { # Standard deviation principle
    if (is.null(args)) {
      alpha = 2.33
    } else {
      alpha = as.numeric(args)
    }
    premium = apply(L, 2, mean) + alpha * apply(L, 2, sd)
    
  } else if (type == "CoCP") { # Cost of capital principle
    if (is.null(args)) {
      r_CoC = 0.1
      r_type = "ES"
      alpha = 0.99
    } else {
      r_CoC = as.numeric(args[1])
      r_type = args[2]
      alpha = as.numeric(args[3])
    }
    L_mean = apply(L, 2, mean)
    sorted = t(t(apply(L, 2, sort)) - L_mean)
    if (r_type == "ES") {
      rho = apply((sorted[floor(alpha*n):n,]), 2, mean)
    } else if (r_type == "VaR") {
      #browser()
      rho = sorted[floor(alpha*n),]
    }
    premium = L_mean + r_CoC * rho
  }
  
  return(premium)
}


# Differential Evolution
# .... f : function to maximize
# .... D : dimension of problem
# .... R : initial bounds of variables
# .... args : optional additional arguments for the function
# .... FE : maximum amount of function evaluations
# .... popSize : population size
# .... s : scalar
# .... pXO : cross over probability
# .... nonNegative : True if input variables should be non-negative
# .... parallel : True if population within a generation should be computed in parallel
# .... fOutputDimensions : Number of output dimensions of f
# .... optIndex : Index of output of f to be optimized (0 for sum)
DEmax = function(f, D, R, args = NULL, FE = 1000, popSize = 20, s = 0.7, pXO = 0.5,
                 nonNegative = TRUE, parallel = FALSE,
                 fOutputDimensions = 1, optIndex = 0) {
  # browser()
  # initialize
  cat("opt index ... ", optIndex)
  xC = matrix(runif(popSize * D, R[1], R[2]), nrow = popSize, ncol = D)
  
  xC[1,] = c(10, 12.5, 14.5, 1.5, 2, 2.5)
  
  fC_sep = matrix(0, nrow = popSize, ncol = fOutputDimensions)
  fC_prem = matrix(0, nrow = popSize, ncol = 3)
  for (i in 1:popSize) {
    res = do.call(f, c(list(xC[i,]), args))
    fC_sep[i,] = res[1:fOutputDimensions]
    fC_prem[i,] = res[7:9]
  }
  if (optIndex == 0) {
    fC = apply(fC_sep, 1, sum)
  } else {
    fC = fC_sep[, optIndex]
  }
  
  
  # browser()
  iOpt = which.max(fC)
  xOpt = xC[iOpt,]
  fOpt = fC[iOpt]
  fOpt_sep = fC_sep[iOpt,]
  
  cat("Starting DE maximation with FE: ", FE,", Popsize: ", popSize, ", s: ", s, "\n")
  cat("Current opt (1): ", fOpt, " (", fOpt_sep, ") at ", xOpt, "\n")
  start_time = Sys.time()
  
  # evolve
  for (gen in 2:(FE %/% popSize)) {
    # generate new candidates:
    # .... linear combination
    m = matrix(0, nrow = 3, ncol = popSize)
    for (i in 1:3) {
      m[i,] = sample(1:popSize)
    }
    
    xN = xC[m[1,],] + s * (xC[m[2,],] - xC[m[3,],])
    
    
    if (nonNegative) {
      xN = replace(xN, xN < 0, 0)
    }
    
    # .... cross over:
    XO = matrix(runif(popSize * D), nrow = popSize, ncol = D) < pXO
    xN = replace(xC, XO, xN[XO])
    
    # evaluate and determine new population
    fN_sep = matrix(0, nrow = popSize, ncol = fOutputDimensions)
    fN_prem = matrix(0, nrow = popSize, ncol = 3)
    
    if (parallel) { # Parallel
      fN_sep = foreach (i=1:popSize, .combine = rbind) %dopar% {
        do.call(f, c(list(xN[i,]), args))[1:fOutputDimensions]
      }
    } else { # Non Parallel
      for (i in 1:popSize) {
        res = do.call(f, c(list(xN[i,]), args))
        fN_sep[i,] = res[1:fOutputDimensions]
        fN_prem[i,] = res[7:9]
      }
    }
    
    if (optIndex == 0) {
      fN = apply(fN_sep, 1, sum)
    } else {
      fN = fN_sep[, optIndex]
    }
    
    cat("x_11 ... ", xC[1,1], " ... ", xN[1,1], "\n")
    cat("x_12 ... ", xC[1,2], " ... ", xN[1,2], "\n")
    cat("x_13 ... ", xC[1,3], " ... ", xN[1,3], "\n")
    cat("x_14 ... ", xC[1,4], " ... ", xN[1,4], "\n")
    cat("x_15 ... ", xC[1,5], " ... ", xN[1,5], "\n")
    cat("x_16 ... ", xC[1,6], " ... ", xN[1,6], "\n")
    cat("f(x_1) C ... ", fC_sep[1,], "\n")
    cat("prem C ... ", fC_prem[1,], "\n")
    cat("f(x_1) N ... ", fN_sep[1,], "\n")
    cat("prem N ... ", fN_prem[1,], "\n", "\n")
    cat("fC ... ", fC[1], "\n")
    cat("fN ... ", fN[1], "\n")
    
    replace = fN > fC
    
    xC[replace,] = xN[replace,]
    fC[replace] = fN[replace]
    fC_sep[replace,] = fN_sep[replace,]
    
    # new acting optimum?
    iOpt = which.max(fN)
    if (fN[iOpt] > fOpt) {
      xOpt = xN[iOpt,]
      fOpt = fN[iOpt]
      fOpt_sep = fN_sep[iOpt,]
    }
    
    cat("Current opt (", gen, "):", fOpt, " (", fOpt_sep, ") at ", xOpt, "\n")
    cat("prem opt ... ", fN_prem[iOpt,], "\n", "\n")
  }
  
  end_time = Sys.time()
  cat("Time elapsed: ", end_time - start_time, "\n")
  cat("Optimum is: ", fOpt, " (", fOpt_sep, ") at \n", xOpt, "\n \n")
  
  res = do.call(f, c(list(xOpt), args))
  
  return(list("xOpt" = xOpt, "fOpt" = fOpt_sep, "fOptAll" = res))
}

# aggregate stop loss reinsurance function that gives out economic profitability
# .... input : c(deductible, limit, premium)
# .... L : Loss monte carlo numbers
# .... premium_ins : Premium profit of insurer
# .... coc_ins : Cost of Capital of insurer
# .... coc_reins : Cost of Capital of reinsurer
# .... premium_fn_type : Premium function of reinsurer ("EVP", "SDP", "CoCP")
# .... premium_fn_args : (lambda, alpha, [r_CoC, "ES"/"VaR", alpha]) respectively
# .... alpha : alpha used for risk measure (expected shortfall and value at risk)
# .... reins_risk_measure : Risk measure of reinsurer ("VaR" or "ES")
# .... returnOption : "total_z", "separate_z", "total_RoRBC", "separate_RoRBC", "all"
# .... punish : punish function
# .... nonNegativeOutput : add punishment term if one of the outputs is negative
agg_stop_loss = function(input, L, premium_ins, coc_ins, coc_reins,
                         premium_fn_type=NULL,
                         premium_fn_args=NULL,
                         alpha=0.99,
                         reins_risk_measure = "VaR",
                         returnOption = "all",
                         punish = NULL,
                         nonNegativeOutput = TRUE) {
  # Input
  n_LoB = dim(L)[2]
  deductible = input[1:n_LoB]
  limit = input[(n_LoB+1):(2*n_LoB)]
  # take premium numbers for reinsurer if given, else premium will be calculated later
  if (is.null(premium_fn_type)) {
    if (length(input) < n_LoB * 3) {
      print("ERROR: premium_fn or input[premium] are missing")
      return(0)
    } else {
      p_reins = input[(2*n_LoB+1):(3*n_LoB)]
    }
  }
  
  # Parameters
  n = dim(L)[1]
  max_reins = deductible + limit
  deductible_matrix = matrix(deductible, ncol=n_LoB, nrow=n, byrow=TRUE)
  limit_matrix = matrix(limit, ncol=n_LoB, nrow=n, byrow=TRUE)
  max_reins_matrix = matrix(max_reins, ncol=n_LoB, nrow=n, byrow=TRUE)
  L_ins = L
  
  # Apply aggregate stop loss reinsurance
  # .... reinsurance given, but limit not reached:
  case_1 = (L>deductible_matrix) & (L<=max_reins_matrix)
  L_ins = replace(L_ins, case_1, deductible_matrix[case_1])
  # .... maximum reinsurance given, limit is reached:
  case_2 = L>max_reins_matrix
  L_ins = replace(L_ins, case_2, (L-limit_matrix)[case_2])
  
  # Calculate P_L for reinsurer
  L_reins = L - L_ins
  # Calculate premium for reinsurance if not given
  if (!is.null(premium_fn_type)) {
    p = calculate_premium(L_reins, premium_fn_type, premium_fn_args)
  }
  p_reins = matrix(p, ncol = n_LoB, nrow = n, byrow = TRUE)
  x_reins = p_reins - L_reins
  
  # Calculate P_L for insurer
  x_ins = matrix(premium_ins, ncol = n_LoB, nrow = n, byrow=TRUE) - L_ins - p_reins
  
  # Calculate risk based capital for both insurer and reinsurer
  x_ins_total = apply(x_ins, 1, sum)
  x_reins_total = apply(x_reins, 1, sum)
  
  x_ins_total_sorted = sort(x_ins_total)
  x_reins_total_sorted = sort(x_reins_total)
  
  # Risk based capital of insurer based on Expected Shortfall
  RBC_ins = - mean(x_ins_total_sorted[1:((1-alpha)*n)]) / (1-coc_ins)
  
  # Risk based capital of reinsurer based on Value at Risk or Expected Shortfall
  if (reins_risk_measure == "VaR") {
    RBC_reins = - x_reins_total_sorted[(1-alpha)*n] / (1-coc_reins)
  } else if (reins_risk_measure == "ES") {
    RBC_reins = - mean(x_reins_total_sorted[1:((1-alpha)*n)]) / (1-coc_reins)
  }
  
  # Cost of Capital
  CoC_ins = RBC_ins * coc_ins
  CoC_reins = RBC_reins * coc_reins
  
  # Underwriting Result U
  U_ins = mean(x_ins_total)
  U_reins = mean(x_reins_total)
  
  # Economic Profitability Z
  Z_ins = U_ins - CoC_ins
  Z_reins = U_reins - CoC_reins
  
  # Return on Risk Based Capital RoRBC (if RBC is negative, define RoRBC = 0)
  if (RBC_ins <= 0) {
    RoRBC_ins = 0
  } else {
    RoRBC_ins = Z_ins / RBC_ins
  }
  if (RBC_reins <= 0) {
    RoRBC_reins = 0
  } else {
    RoRBC_reins = Z_reins / RBC_reins
  }
  
  # Return the value that is requested
  return_value = c(Z_ins, Z_reins)
  if (returnOption == "total_z") {
    return_value = sum(Z_ins, Z_reins)
  } else if (returnOption == "separate_z") {
    return_value = c(Z_ins, Z_reins)
  } else if (returnOption == "total_RoRBC") {
    return_value = sum(RoRBC_ins, RoRBC_reins)
  } else if (returnOption == "separate_RoRBC") {
    return_value = c(RoRBC_ins, RoRBC_reins)
  } else if (returnOption == "all") {
    return_value = c(Z_ins, Z_reins, RBC_ins, RBC_reins, RoRBC_ins, RoRBC_reins, p)
  }
  
  # Add punishment if one of output dimension (z) is negative
  if (nonNegativeOutput) {
    if (returnOption == "all") {
      negativeOutput = return_value[1:2] < 0
      return_value[1:2] = return_value[1:2] - sum(negativeOutput)
    } else {
      negativeOutput = return_value < 0
      return_value = return_value - sum(negativeOutput)
    }
  }
  
  # Add punishment term if included
  if (!is.null(punish)) {
    if (returnOption == "all") {
      return_value[1:2] = return_value[1:2] - punish(input)
    } else {
      return_value = return_value - punish(input)
    }
  }
  
  return(return_value)
}


# aggregate stop loss punishment function, returns positive number in accordance to the
# severity of violation of feasibility of reinsurance contracts. Returns zero if feasible.
agg_stop_loss_punish = function(input, scale_violation=1, cutoff_violation=1) {
  # Input
  n_LoB = 3
  violation = 0
  punishment = 0
  deductible = input[1:n_LoB]
  limit = input[(n_LoB+1):(2*n_LoB)]
  if (length(input) == 9) {
    premium = input[(2*n_LoB+1):(3*n_LoB)]
    
    # If premium is bigger than limit, it is a unfeasible reinsurance
    violation = sum((premium - limit)[premium - limit > 0])
    punishment = violation * scale_violation + (violation > 0) * cutoff_violation
    
    # Negative premium
    violation = sum((-premium)[premium < 0])
  }
  #n_LoB = length(input) %/% 3
  # browser()
  # Any negative input makes no sense either
  if (sum(deductible[deductible < 0]) < 0) {
    violation = violation + mean((-deductible)[deductible < 0])
  }
  if (sum(limit[limit < 0]) < 0) {
    violation = violation + mean((-limit)[limit < 0])
  }
  
  punishment = punishment + violation * scale_violation + (violation > 0) * cutoff_violation
  
  return(punishment)
}


# plot the effects of the aggregate stop loss reinsurance
# deductible : deductible of reinsurance
# limit : limit of reinsurance
# p : reinsurance premium if given directly
# p_type, p_args : reinsurance premium type and arguments if to be calculated
plot_agg_stop_loss = function(deductible, limit, p=NULL, p_type=NULL, p_args=NULL) {
  if (L == 0) {
    reCalc_L()
  }
  
  # Before reinsurance
  L_plot = ecdf(apply(L, 1, sum))
  
  # After reinsurance
  # Parameters
  n = dim(L)[1]
  n_LoB = dim(L)[2]
  max_reins = deductible + limit
  deductible_matrix = matrix(deductible, ncol=n_LoB, nrow=n, byrow=TRUE)
  limit_matrix = matrix(limit, ncol=n_LoB, nrow=n, byrow=TRUE)
  max_reins_matrix = matrix(max_reins, ncol=n_LoB, nrow=n, byrow=TRUE)
  L_ins = L
  
  # Apply aggregate stop loss reinsurance
  # .... reinsurance given, but limit not reached:
  case_1 = (L>deductible_matrix) & (L<=max_reins_matrix)
  L_ins = replace(L_ins, case_1, deductible_matrix[case_1])
  # .... maximum reinsurance given, limit is reached:
  case_2 = L>max_reins_matrix
  L_ins = replace(L_ins, case_2, (L-limit_matrix)[case_2])
  
  # Calculate reinsurance premium
  L_reins = L - L_ins
  if (!is.null(p)) {
    p_reins = p
  } else {
    p_reins = calculate_premium(L_reins, p_type, p_args)
  }
  
  # add reinsurance premium to altered loss numbers of insurer
  L_ins = t(t(L_ins) + p_reins)
  L_reins = t(t(L_reins) - p_reins)
  
  #cat("Deductble: ", deductible, "\n",
  #    "limit: ", limit, "\n",
  #    "premium: ", p_reins, "\n")
  
  L_plot_after = ecdf(apply(L_ins, 1, sum))
  
  
  min_x = min(apply(L, 1, sum))
  max_x = max(apply(L_ins, 1, sum))
  
  #p = plot(L_plot, ylim=c(0,1), xlim=c(min_x, max_x), col="red", main="CDF")
  #lines(L_plot_after,lty=3,verticals=T, col="green")
  
  p = plot(L_plot, ylim=c(0,1),
           xlim=c(min_x, max_x),
           col="red", main="CDF", xlab = "Loss")
  lines(L_plot_after,lty=3,verticals=T, col="green")
  legend(max_x - 0.2*(max_x - min_x), 0.2, legend=c("CDF before", "CDF Insurer"),
         col=c("red", "green"), lty=1:2, cex=0.8)
  
  return(list("reins_plot_before" = L,
              "reins_plot_after" = L_ins,
              "reins_plot_reins" = L_reins,
              "reins_p" = p_reins))
}

plot_test = function(premium, coc_ins, coc_reins) {
  #browser()
  a = 0.99
  n = dim(P_L)[1]
  deductible = seq(10, 30, by=1)
  limit = seq(3, 10, by=1)
  
  data = matrix(0, nrow = length(deductible), ncol = length(limit))
  
  for (i in 1:length(deductible)) {
    for (j in 1:length(limit)) {
      ded = deductible[i]/3
      lim = limit[j]/3
      if (ded != 0) {
        prem = (lim/4) * (10/ded)
      } else {
        prem = (lim/4) * 10
      }
      
      input = c(ded, ded, ded, 
                lim, lim, lim,
                prem, prem, prem)
      z = agg_stop_loss(input, premium, coc_ins, coc_reins)
      data[i, j] = z[1] + z[2]
      #print(data[i, j])
    }
  }
  
  #p = plot_ly(z = data, type = "surface")
  
  return(list("data_x" = deductible, "data_y" = limit, "data_z" = data))
  
}


plot_optimize_total_profitability = function(L, premium, coc_ins, coc_reins) {
  D = 9
  R = c(0, 15)
  #args = list(premium, coc_ins, coc_reins, 0.99, "VaR", "separate_z", TRUE)
  args = list(L, premium, 0.1, 0.1, alpha=0.99, "ES", "separate_z", agg_stop_loss_punish)
  FE = 600
  popSize = 30
  s = 0.7
  pXO = 0.5
  print("begin ... ")
  xOpt = DEmax(agg_stop_loss, D, R, args, FE, popSize, s, pXO)
  fOpt = do.call(agg_stop_loss, c(list(xOpt), args))
  punish = agg_stop_loss_punish(xOpt)
  cat("optimum is at ", xOpt, "with \n", fOpt, "\n and ", punish, " punishment.")
}

repeated_experiments = function(L, premium, coc_ins, coc_reins) {
  D = 9
  R = c(0, 15)
  #args = list(premium, coc_ins, coc_reins, 0.99, "VaR", "separate_z", TRUE)
  args = list(L, premium, 0.1, 0.1, 0.99, "ES", "separate_z", agg_stop_loss_punish)
  FE = 2000
  popSize = 30
  s = 0.7
  pXO = 0.5
  print("begin ... ")
  
  # perform repeated experiments to test out best hypterparameters
  nExp = 100
  popSizeSet = c(20, 25, 30, 35, 40, 45, 50)
  sSet = c(0.6, 0.7, 0.8, 0.9)
  result = matrix(0, nrow = nExp, ncol = length(popSizeSet))
  result_add = matrix(0, nrow = nExp, ncol = 12)
  pop = 1
  DE_max = DEmax
  agg_stop_loss_fn = agg_stop_loss
  
  
  # popSize experiments
  ############
  for (popSize in popSizeSet) {
    print("Begin pop experiment")
    start_time = Sys.time()
    
    # parallel
    #col_temp = foreach (i=1:nExp, .combine = c) %dopar% {
    #  xOpt = DE_max(agg_stop_loss_fn, D, R, args, FE, popSize, s, pXO)
    #  fOpt = do.call(agg_stop_loss_fn, c(list(xOpt), args))
    #  sum(fOpt)
    #}
    
    for (exp in 1:nExp) {
      L = get_L()
      args = list(L, premium, 0.1, 0.1, 0.99, "ES", "separate_z", agg_stop_loss_punish)
      xOpt = DEmax(agg_stop_loss, D, R, args, FE, popSize, s, pXO, parallel = TRUE)
      fOpt = do.call(agg_stop_loss, c(list(xOpt), args))
      result_add[exp,] = c(sum(fOpt), fOpt, xOpt)
      result[exp, pop] = sum(fOpt)
    }
    path = paste("data/pop_", popSize, ".rds", sep = "")
    saveRDS(result_add, path)
    
    end_time = Sys.time()
    cat("Time elapsed: ", end_time - start_time)
    # result[,pop] = col_temp only for parallel
    pop = pop + 1
  }
  
  colnames(result) = popSizeSet
  saveRDS(result, "data/pop_result.rds")
  colr = palette(rainbow(length(popSizeSet)))
  boxplot(result, col = colr, main = "Boxplot popSizeSet")
  
  
  # s experiments
  ###############
  popSize = 30
  pop = 1
  result_s = matrix(0, nrow = nExp, ncol = length(sSet))
  result_s_add = matrix(0, nrow = nExp, ncol = 12)
  for (s in sSet) {
    print("Begin f experiment")
    start_time = Sys.time()
    
    # parallel
    #col_temp = foreach (i=1:nExp, .combine = c) %dopar% {
    #  xOpt = DE_max(agg_stop_loss_fn, D, R, args, FE, popSize, s, pXO)
    #  fOpt = do.call(agg_stop_loss_fn, c(list(xOpt), args))
    #  sum(fOpt)
    #}
    
    for (exp in 1:nExp) {
      L = get_L()
      args = list(L, premium, 0.1, 0.1, 0.99, "ES", "separate_z", agg_stop_loss_punish)
      xOpt = DEmax(agg_stop_loss, D, R, args, FE, popSize, s, pXO, parallel = TRUE)
      fOpt = do.call(agg_stop_loss, c(list(xOpt), args))
      result_s_add[exp,] = c(sum(fOpt), fOpt, xOpt)
      result_s[exp, pop] = sum(fOpt)
    }
    path = paste("data/s_", s*10, ".rds", sep = "")
    saveRDS(result_s_add, path)
    
    end_time = Sys.time()
    cat("Time elapsed: ", end_time - start_time)
    # result[,pop] = col_temp only for parallel
    pop = pop + 1
  }
  
  colnames(result_s) = sSet
  saveRDS(result_s, "data/s_result.rds")
  colr = palette(rainbow(length(sSet)))
  boxplot(result_s, col = colr, main = "Boxplot sSet")
  
  
}


runSimulation = function(n, corr, input, CoC_ins=0.1, CoC_reins=0.06, risk_thresh=0.9, a=0.99) {
  #browser()
  # set global variables
  n_global <<- n
  corr_global <<- corr
  input_global <<- input
  
  # input variables
  mean = input['mean',]
  CV = input['CV',]
  premium = input['P',]
  
  #####################
  # Monte Carlo
  # further variables (for log normal distribution)
  dim = dim(corr)[1]  # how many LoB's
  stdDev = mean*CV
  m = log(mean) - (1/2)*log(1 + stdDev^2/mean^2)
  s = (log(1 + (stdDev^2)/(mean^2)))^(1/2)
  
  # generate Scenarios
  L = get_L()
  
  
  
  ######################
  # underwriting result U
  # profit
  P = rep(premium[1], n)
  for (i in 2:dim) {
    P = cbind(P, rep(premium[i], n))
  }
  # P&L
  P_L = P - L
  corrUniRndNum = P_L
  # statistics
  u_mean = apply(P_L, 2, mean)
  U_stdDev = apply(P_L, 2, sd)
  u_min = apply(P_L, 2, min)
  u_max = apply(P_L, 2, max)
  u = rbind(u_mean, U_stdDev, u_min, u_max)
  colnames(u) = c("LoB1", "LoB2", "LoB3")
  
  # total
  P_L_total = apply(P_L, 1, sum)
  u_mean_total = mean(P_L_total)
  u_stdDev_total = sd(P_L_total)
  u_min_total = min(P_L_total)
  u_max_total = max(P_L_total)
  
  
  
  # capital cost
  Sorted = apply(P_L, 2, sort)  # sorted P_L scenarios
  valAtRisk = Sorted[(1-a)*n,]  # Value at risk
  expShort = apply(Sorted[1:((1-a)*n),], 2, mean)  # expected shortfall
  # total
  Sorted_total = sort(P_L_total)  # sorted P_L_total scenarios
  valAtRisk_total = Sorted_total[(1-a)*n]  # total Value at risk
  expShort_total = mean(Sorted_total[1:((1-a)*n)])  # total expected shortfall
  
  
  z_rbc = -expShort/(1-CoC_ins)  # risk based capital
  z_CoC = z_rbc * CoC_ins  # cost of capital
  # total
  z_rbc_total = -expShort_total/(1-CoC_ins)  # total risk based capital
  z_CoC_total = z_rbc_total * CoC_ins  # total cost of capital
  
  
  # economic profitability Z
  z_mean = u_mean - z_CoC
  z_RoRBC = z_mean / z_rbc  # return on risk based capital
  # total
  z_mean_total = u_mean_total - z_CoC_total
  z_RoRBC_total = z_mean_total / z_rbc_total  # total return on risk based capital
  
  # OUTPUT
  undiv_z = rbind(c(z_mean, z_mean_total),
                  c(z_rbc, z_rbc_total),
                  c(z_RoRBC, z_RoRBC_total))
  undiv_u = rbind(c(u_mean, u_mean_total),
                  c(expShort, expShort_total))
  undiv_capCost = rbind(c(z_rbc, z_rbc_total),
                        c(z_CoC, z_CoC_total))
  
  
  ####################
  # diversification
  
  # Conditional expected shortfall
  expShort_div = apply(P_L[P_L_total < valAtRisk_total,], 2, mean)
  
  z_rbc_div = -expShort_div/(1-CoC_ins)
  z_CoC_div = z_rbc_div * CoC_ins
  
  # economic profitability z (diversified)
  z_mean_div = u_mean - z_CoC_div
  z_RoRBC_div = z_mean_div / z_rbc_div  # return on risk based capital (diversified)
  
  # OUTPUT
  div_riskCap = rbind(c(-expShort, -sum(expShort)),
                      c(expShort - expShort_div, sum(expShort) - sum(expShort_div)),
                      c(-expShort_div, -sum(expShort_div)))
  div_z = rbind(c(z_mean_div, z_mean_total),
                c(z_rbc_div, z_rbc_total),
                c(z_RoRBC_div, z_mean_total / z_rbc_total))
  div_u = rbind(c(u_mean, u_mean_total),
                c(expShort_div, expShort_total))
  div_capCost = rbind(c(z_rbc_div, z_rbc_total),
                      c(z_CoC_div, z_CoC_total))
  
  
  
  ####################
  # Risk Appetite and Risk Limits
  risk_perc = apply(P_L, 2, sort)[(1-risk_thresh)*n,]
  
  
  
  ####################
  # Reinsurance
  
  # REPEATED EXPERIMENTS
  # data = plot_test(premium, CoC_ins, CoC_reins)
  # test = agg_stop_loss(c(10, 12.5, 14.5, 1.5, 2.0, 2.5, 0.3, 0.5, 0.8), L, c(10.25, 12.5, 15.0), 0.1, 0.06)
  # cat("Test: ", test, " with sum of ", sum(test))
  # opt_plot = plot_optimize_total_profitability(L, premium, CoC_ins, CoC_reins)
  # repeated_experiments(L, premium, CoC_ins, CoC_reins)
  # print("We did it")
  # print(dim(data))
  
 
  
  #browser()
  
  cat("VaR: ", valAtRisk, ". ES: ", expShort)
  cat("Total VaR: ", valAtRisk_total, ". Total ES: ", expShort_total)
  
  # Cap amount of scenarios shown on table
  corrUniRndNum = corrUniRndNum[1:max_table_entries,]
  Y = L[1:max_table_entries,]
  P_L = P_L[1:max_table_entries,]
  
  result = list("loss" = Y, "p_l" = P_L, "u" = u, "corrNum" = corrUniRndNum,
                "undiv_z" = undiv_z, "undiv_u" = undiv_u, "undiv_capCost" = undiv_capCost,
                "div_riskCap" = div_riskCap, "div_z" = div_z,
                "div_u" = div_u, "div_capCost" = div_capCost)
                #"data_x" = data["data_x"], "data_y" = data["data_y"], "data_z" = data["data_z"])
  
  return(result)
}


# Get data for reinsurance plot
# .... ded : deductible
# .... lim : limit
# .... p_type : Premium Principle ("EVP", "SDP", "CoCP")
# .... p_args : (lambda, alpha, [r_CoC, "ES"/"VaR", alpha])
makeReinsPlot = function(ded, lim, p_type, p_args) {
  
  #ded = c(10, 12.5, 14.5)
  #lim = c(1.5, 2, 2.5)
  #p_type = "SDP"
  #p_args = 2.3
  
  reins_plot = plot_agg_stop_loss(ded, lim, p_type = p_type, p_args = p_args)
  
  result = list("reins_plot_before" = reins_plot$reins_plot_before,
                "reins_plot_after" = reins_plot$reins_plot_after,
                "reins_plot_reins" = reins_plot$reins_plot_reins,
                "reins_p" = reins_plot$reins_p)
  return(result)
  
}

# Get data for 3D Reinsurance Premium surface plot
# .... bounds : Bound of the x/y variables (deductible, limit)
# .... p_type : Premium Principle ("EVP", "SDP", "CoCP")
# .... p_args : (lambda, alpha, [r_CoC, "ES"/"VaR", alpha])
makeReinsPremPlot = function(bounds, p_type, p_args) {
  if (L == 0) {
    reCalc_L()
  }
  step_size = 0.5
  x = seq(bounds[1], bounds[2], step_size) # Deductible
  y = seq(bounds[1], bounds[2], step_size) # Limit
  
  # After reinsurance
  # Parameters
  n = dim(L)[1]
  n_LoB = dim(L)[2]
  z1 = matrix(0, nrow = length(x), ncol = length(y))
  z2 = matrix(0, nrow = length(x), ncol = length(y))
  z3 = matrix(0, nrow = length(x), ncol = length(y))
  start_time = Sys.time()
  for (i in x) {
    for (j in y) {
      deductible = c(i, i, i)
      limit = c(j, j, j)
      max_reins = deductible + limit
      deductible_matrix = matrix(deductible, ncol=n_LoB, nrow=n, byrow=TRUE)
      limit_matrix = matrix(limit, ncol=n_LoB, nrow=n, byrow=TRUE)
      max_reins_matrix = matrix(max_reins, ncol=n_LoB, nrow=n, byrow=TRUE)
      L_ins = L
      
      # Apply aggregate stop loss reinsurance
      # .... reinsurance given, but limit not reached:
      case_1 = (L>deductible_matrix) & (L<=max_reins_matrix)
      L_ins = replace(L_ins, case_1, deductible_matrix[case_1])
      # .... maximum reinsurance given, limit is reached:
      case_2 = L>max_reins_matrix
      L_ins = replace(L_ins, case_2, (L-limit_matrix)[case_2])
      
      # Calculate reinsurance premium
      L_reins = L - L_ins
      p_reins = calculate_premium(L_reins, p_type, p_args)
      x_pos = (i-bounds[1])/step_size + 1
      y_pos = (j-bounds[1])/step_size + 1
      z1[x_pos, y_pos] = p_reins[1]
      z2[x_pos, y_pos] = p_reins[2]
      z3[x_pos, y_pos] = p_reins[3]
    }
  }
  
  end_time = Sys.time()
  cat("It took: ", end_time - start_time)
  
  return(list("x" = x,
              "y" = y,
              "z1" = z1,
              "z2" = z2,
              "z3" = z3))
  
}

# Run the optimization
# .... prem_ins : Reinsurance premium values (LoB number of dimensions)
# .... coc_ins : Cost of Capital rate of insurer
# .... coc_reins : Cost of Capital rate of reinsurer
# .... prem_fn_type : Reinsurance Premium function type ("EVP", "SDP", "CoCP")
# .... prem_fn_args : (lambda, alpha, [r_CoC, "ES"/"VaR", alpha])
# .... alpha : Alpha value of risk measure of both insurer and reinsurer
# .... risk_measure_reins : Risk measure of reinsurer ("ES", "VaR")
# .... f_dim : Dimension of output vector
# .... f_ind : Index of variable to be optimized
# .... .... (Z_ins, Z_reins, RBC_ins, RBC_reins, RoRBC_ins, RoRBC_reins, p)
# .... eq_ins : Total available equity Insurer
# .... eq_reins : Total available equity Reinsurer
# .... sol_tar_ins : Solvency target Insurer (equity / RBC)
# .... sol_tar_reins : Solvency target Reinsurer (equity / RBC)
# .... FE : Function evaluations for differential evolution (DE)
# .... popSize : Population size for DE
# .... s : s scalar for DE
runOptimization = function(prem_ins, coc_ins, coc_reins, prem_fn_type, prem_fn_args, alpha,
                           risk_measure_reins, f_dim, f_ind,
                           eq_ins, eq_reins, sol_tar_ins, sol_tar_reins,
                           FE = 1000, popSize = 20, s = 0.7) {
  D = 6
  R = c(0, 15)
  args = list(L, prem_ins, coc_ins, coc_reins,
              prem_fn_type, prem_fn_args, alpha,
              risk_measure_reins, "all", agg_stop_loss_punish, TRUE)
  
  cat("begin ... \n")
  opt = DEmax(agg_stop_loss, D, R, args, FE, popSize, s, parallel = FALSE,
               fOutputDimensions = f_dim, optIndex = f_ind)
  xOpt = opt$xOpt
  fOptAll = opt$fOptAll
  
  return(list("xOpt" = xOpt, "fOptAll" = fOptAll))
}


#
generateData = function() {
  reCalc_L()
  
  ded = seq(0, 16, by=2)
  lim = seq(0, 16, by=2)
  
  D = 6
  bounds = c(0, 15)
  p_insurer = c(10.25, 12.5, 15)
  FE = 1000
  popSize = 30
  s = 0.7
  pXO = 0.5
  
  args = list(L, p_insurer, 0.1, 0.06, "SDP", 2.33, 0.99, "VaR", "all", NULL, FALSE)
  row_nr = length(ded)^3 * length(lim)^3
  m = matrix(0, ncol = 15, nrow = row_nr)
  colnames(m) = c("Z_ins", "Z_reins", "RBC_ins", "RBC_reins",
                  "RoRBC_ins", "RoRBC_reins", "p1", "p2", "p3",
                  "ded1", "ded2", "ded3", "lim1", "lim2", "lim3")
  count = 1
  start_time = Sys.time()
  cat("starting ... \n")
  for (i1 in ded) {
    for (i2 in ded) {
      for (i3 in ded) {
        for (u1 in lim) {
          for (u2 in lim) {
            for (u3 in lim) {
              input = c(i1, i2, i3, u1, u2, u3)
              result = do.call(agg_stop_loss, c(list(input), args))
              m[count,] = c(result, input)
              count = count + 1
            }
          }
        }
        elapsed_time = difftime(Sys.time(), start_time, units = "mins")
        cat("Progress ... ", round(100 * (count/row_nr), 3), "% ... Elapsed Time ... ", elapsed_time, " mins \n")
      }
    }
    cat("Backing up ... \n")
    path = paste("data/data_10_06_SDP_2-33_VaR_backup_", i1,".rds", sep = "")
    saveRDS(m, path)
  }
  
  elapsed_time = difftime(Sys.time(), start_time , units = "mins")
  cat("Finished ... Elapsed Time ... ", elapsed_time, " mins \n")
  path = "data/data_10_06_SDP_2-33_VaR.rds"
  saveRDS(m, path)
  
  # c(Z_ins, Z_reins, RBC_ins, RBC_reins, RoRBC_ins, RoRBC_reins, p)
  #y = DEmax(agg_stop_loss, D, bounds, args, FE, popSize, s, pXO, TRUE, FALSE, 9, 1)
  #print(y)
}


# Make a 2D plot from Data with (dim1, dim2) as (x, y) variables
# and dimOpt as variable to be optimized
# Dimensions: c("Z_ins", "Z_reins", "RBC_ins", "RBC_reins",
# .... "RoRBC_ins", "RoRBC_reins", "p1", "p2", "p3",
# .... "ded1", "ded2", "ded3", "lim1", "lim2", "lim3")
getDataPlot2D = function(dim1, dim2, dimOpt, reCalc = F) {
  # cat("Getting plotData for (", dim1, ", ", dim2, ", ", dimOpt, ") \n")
  # Check for correct input
  dim1 = floor(dim1)
  dim2 = floor(dim2)
  dimOpt = floor(dimOpt)
  if (dim1 < 1 | dim1 > 15 | dim2 < 1 | dim2 > 15 | dimOpt < 1 | dimOpt > 15) {
    cat("Error in 'getDataPlot2D', input dimensions are incorrect! \n")
    return(0)
  }
  
  # Check if this plot has already been done
  if (!reCalc) {
    path = paste("data/plots/plot_SDP_(", dim1, "-", dim2, "-", dimOpt,").rds", sep = "")
    exi = file.exists(path)
    if (exi) {
      plotDataOpt = readRDS(path)
      return(plotDataOpt)
    }
  }
  
  # Read data if it exists
  path = "data/data_10_06_SDP_2-33_VaR.rds"
  exi = file.exists(path)
  if (exi) {
    m = readRDS(path)
  } else {
    cat("Error in 'getDataPlot2D', data file does not exist at location: \n", path, "\n")
    return(0)
  }
  
  # Order by 'x' variable, round and define new matrix with relevant columns
  m = round(m[order(m[,dim1]),], 1)
  dimOpt_original = dimOpt
  if (dimOpt == dim1) {
    plotData = m[,c(dim1, dim2)]
    dimOpt = 1
  } else if (dimOpt == dim2) {
    plotData = m[,c(dim1, dim2)]
    dimOpt = 2
  } else {
    plotData = m[,c(dim1, dim2, dimOpt)]
    dimOpt = 3
  }
  
  # For each 'x' value take the 'y' value that optimizes the 'dimOpt' value
  iOpt = 1
  newIndices = c()
  fOpt = plotData[1, dimOpt]
  xCur = plotData[1, 1]
  for (i in 2:dim(plotData)[1]) {
    currentRow = plotData[i,]
    if (currentRow[1] == xCur) {
      if (currentRow[dimOpt] > fOpt) {
        iOpt = i
        fOpt = currentRow[dimOpt]
      }
    } else {
      newIndices = c(newIndices, iOpt)
      iOpt = i
      fOpt = currentRow[dimOpt]
      xCur = currentRow[1]
      #cat("Adding ... ", currentRow, "\n")
      #cat("Current Indices ... ", length(newIndices), " ... from ... ", i, "\n")
    }
  }
  plotDataOpt = plotData[newIndices,]
  
  path = "data/data_SDP_.rds"
  path = paste("data/plots/plot_SDP_(", dim1, "-", dim2, "-", dimOpt_original,").rds", sep = "")
  saveRDS(plotDataOpt, path)
  cat("Finished getting plotData \n")
  
  return(plotDataOpt)
}

# Differential Evolution
# .... f : function to maximize
# .... D : dimension of problem
# .... R : initial bounds of variables
# .... args : optional additional arguments for the function
# .... FE : maximum amount of function evaluations
# .... popSize : population size
# .... s : scalar
# .... pXO : cross over probability
# .... nonNegative : True if input variables should be non-negative
# .... parallel : True if population within a generation should be computed in parallel
# .... fOutputDimensions : Number of output dimensions of f
# .... optIndex : Index of output of f to be optimized (0 for sum)

# aggregate stop loss reinsurance function that gives out economic profitability
# .... input : c(deductible, limit, premium)
# .... L : Loss monte carlo numbers
# .... premium_ins : Premium profit of insurer
# .... coc_ins : Cost of Capital of insurer
# .... coc_reins : Cost of Capital of reinsurer
# .... premium_fn_type : Premium function of reinsurer ("EVP", "SDP", "CoCP")
# .... premium_fn_args : (lambda, alpha, [r_CoC, "ES"/"VaR", alpha]) respectively
# .... alpha : alpha used for risk measure (expected shortfall and value at risk)
# .... reins_risk_measure : Risk measure of reinsurer ("VaR" or "ES")
# .... returnOption : "total_z", "separate_z", "total_RoRBC", "separate_RoRBC", "all"
# .... punish : punish function
# .... nonNegativeOutput : add punishment term if one of the outputs is negative