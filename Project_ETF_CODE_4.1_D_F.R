  #The objective of this project is to examine the performance of large AI ETFs and assess the risk environments.
  library("quantmod")
  library("moments")
  library("tseries")
  library(corrplot)
   # list of all ticker symbols for which data will be retrieved
  tickers <- c("ROBO","AIA","AIRR","QQQ","VGT","^GSPC", "^IRX", "USNQX")
  # ROBO: Global Robotics and Automation Index ETF
  # Index: Robo-Stox Global Robotics and Automation Index
  # AIA:  iShares S&P Asia ETF
  # Index: S&P Asia 50 Index
  # AIRR:  First Trust RBA American Industrial Renaissance ETF
  # INDEX: Richard Bernstein Advisors American Industrial Renaissance Index
  # QQQ:  Invesco QQQ
  # Index: NASDAQ-100 Index
  # VTG: Vanguard Information Technology ETF
  # Index: MSCI US Investable Market Information Technology 25/50 Index
  # Retrieve weekly time-series data for ticker symbols from 2014-04-01 to 2019-04-01
  #Column naming
  etf_names = c("ROBO", "AIA","AIRR", "QQQ", "VGT")
  etfIndex_names = c("ROBO-STOX", "S&P Asia 50","RBAAIR", "NASDAQ-100", "MSCI")
  market_names = c("S&P 500", "DJUSTC")
  
  #Data Extraction
  getSymbols(tickers, from="2014-04-01", to="2019-04-01", periodicity="weekly", return.class="xts")
  ROBO.STOX = matrix(scan("ROBO.STOX.csv", what=numeric()), nrow=261) #Data to be added #Data to be added
  SPASIA50 = matrix(scan("SPASIA50.csv", what=numeric()), nrow=261)
  RBAAIR = matrix(scan("RBAAIR.csv", what=numeric()), nrow=261)
  DJUSTC = matrix(scan("DJUSTC.csv", what=numeric()), nrow=261)
  MSCI = matrix(scan("MSCI.csv", what=numeric()), nrow=261)
  ETF.AI = cbind(ROBO[,6], AIA[,6], AIRR[,6], QQQ[,6], VGT[,6])
  colnames(ETF.AI) = etf_names
  ETF.Index =  cbind(ROBO.STOX, SPASIA50, RBAAIR, USNQX[,6], MSCI)
  colnames(ETF.Index) = etfIndex_names
  market.index = cbind(GSPC[,6], DJUSTC)
  T_yield =  IRX[,6] / 100
  colnames(market.index) = market_names
  
  # Q (a) - Start
  # General Function to calculate log returns
  get_logRet = function(prices){
    log_ret = matrix(0, nrow = dim(prices)[1], ncol = dim(prices)[2])
    for(i in 1:dim(log_ret)[2]){
      for(j in 1:dim(log_ret)[1]){
        log_ret[, i] = diff(log(prices[ ,i]))
      }
    }
    return(log_ret)
  }
  
  # Calculating Log Returns for ETF Data
  ETF.AI_logRet = get_logRet(ETF.AI)
  colnames(ETF.AI_logRet) = etf_names
  ETF.Index_logRet = get_logRet(ETF.Index)
  colnames(ETF.Index_logRet) = etfIndex_names
  market.index_logRet = get_logRet(market.index)
  colnames(market.index_logRet) = market_names
  
  # Data Compiled to be used for Analysis 
  LR_data = list(ETF.AI_logRet, ETF.Index_logRet, market.index_logRet)
  
  #Plotting Function for Histogram of returns
  plotting = function(x, names){
    x = coredata(x)
    h <- hist(x, breaks = 104, density = 10, main = names, xlab = "Weekly Log Returns")
    # and fit a normal curve
    xfit <- seq(from = min(x, na.rm = TRUE), max(x, na.rm = TRUE), length = 104)
    yfit <- dnorm(xfit, mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
    yfit <- yfit * diff(h$mids[1:2]) * length(x)
    lines(xfit, yfit, col = "red", lwd = 2)
  }
  
  #plotting ETF Log Returns
  for(l in 1:length(LR_data)){
    if(dim(LR_data[[l]])[2] == 3){
      par(mfrow = c(1, 3))
    }
    else{
      par(mfrow = c(3, 2))
    }
    for(i in 1:dim(LR_data[[l]])[2]){
      plotting(LR_data[[l]][,i], colnames(LR_data[[l]])[i])
    }
  }
  
  # Normality Test of ETFs:
  par(mfrow=c(2,3))
  for (i in 1:dim(ETF.Index_logRet)[2])
  {
    names = colnames(ETF.AI_logRet)
    qqnorm(ETF.AI_logRet[,i],datax=TRUE,main=names[i])
    qqline(ETF.AI_logRet[,i],datax=TRUE)
    print(shapiro.test(ETF.AI_logRet[,i]))
  }
  par(mfrow=c(2,3))
  for (i in 1:dim(ETF.Index_logRet)[2])
  {
    names = colnames(ETF.Index_logRet)
    qqnorm(ETF.Index_logRet[,i],datax=TRUE,main=names[i])
    qqline(ETF.Index_logRet[,i],datax=TRUE)
    print(shapiro.test(ETF.Index_logRet[,i]))
  }
  
  ###From the p-value of Shapiro test, both stocks are not normal
  
  # Summary Statistics Function
  get_stats = function(data){
    return(c(mean(data, na.rm=TRUE)*52, median(data, na.rm=TRUE)*52, sd(data, na.rm=TRUE)*sqrt(52),
             quantile(data, 0.25, na.rm=TRUE), quantile(data, 0.75, na.rm=TRUE),
             skewness(data, na.rm=TRUE), kurtosis(data, na.rm=TRUE)))
  }
  calc_ETF_stats = function(etf_data){
    summary_mat = matrix(0, nrow = 7, ncol = dim(etf_data)[2])
    for(i in 1:dim(etf_data)[2]){
      summary_mat[, i] = get_stats(etf_data[,i])
    }
    rownames(summary_mat) = c("Mean", "Median", "Standard Deviation", "q25", "q75", "Skewness", "Kurtosis")
    return(summary_mat)
  }
  ETF.AI_LR_Summary = calc_ETF_stats(LR_data[[1]])
  colnames(ETF.AI_LR_Summary) = etf_names
  ETF.Index_LR_Summary = calc_ETF_stats(LR_data[[2]])
  colnames(ETF.Index_LR_Summary) = etfIndex_names
  market.index_LR_Summary = calc_ETF_stats(LR_data[[3]])
  colnames(market.index_LR_Summary) = market_names
  
  print(ETF.AI_LR_Summary)
  print(ETF.Index_LR_Summary)
  print(market.index_LR_Summary)
  # Q (a) - End
  
  # Q (b) - Start
  
  # Covariance and Correlation Matrix for All funds
  cov(LR_data[[1]], LR_data[[1]], use="complete.obs")
  cor(LR_data[[1]], LR_data[[1]], use="complete.obs")
  # Covariance and Correlation Matrix for all funds vs Market Index
  cov(LR_data[[1]], LR_data[[3]], use="complete.obs")
  cor(LR_data[[1]], LR_data[[3]], use="complete.obs")
  
  corrplot(cor(LR_data[[1]], LR_data[[1]], use="complete.obs"),method='color')
  
  # Q (b) - End
  
  # Q(c) - Start
  
  calc_trackingError = function(rp, rb){
    te = sqrt(mean(((rp - rb) ** 2), na.rm = TRUE) / (261 - 1))
    return(te)
  }
  
  tracking_error = matrix(0, nrow = dim(LR_data[[1]])[2], ncol = 1)
  for(i in 1:dim(LR_data[[1]])[2]){
    tracking_error[i,] = calc_trackingError(LR_data[[1]][,i], LR_data[[2]][,i])
  }
  rownames(tracking_error) = etf_names
  colnames(tracking_error) = c("Tracking Error")
  print(tracking_error)
  #### Sharpe Ratios of ETFS and indices ####
  calc.sharpe.annual = function(R, r, std){
    s.r = (mean(R,na.rm = TRUE) - mean(r,na.rm = TRUE)/52/100) / sd(std,na.rm = TRUE)
    return(s.r)     
  }
  sharp.mat = matrix(0, nrow = dim(LR_data[[1]])[2], ncol = 1)
  for(i in 1:dim(LR_data[[1]])[2]){
    sharp.mat[i,] = calc.sharpe.annual(LR_data[[1]][,i], T_yield, LR_data[[1]][,i])
  }
  sharp.mat.indx = matrix(0, nrow = dim(LR_data[[1]])[2], ncol = 1)
  for(i in 1:dim(LR_data[[1]])[2]){
    sharp.mat.indx[i,] = calc.sharpe.annual(LR_data[[2]][,i], T_yield, LR_data[[2]][,i])
  }
  sharp.mat.annual = (52/sqrt(52)) * sharp.mat
  rownames(sharp.mat) = etf_names
  colnames(sharp.mat) = c("Weekly Sharpe ETFs")
  colnames(sharp.mat.annual) = c("Annual Sharpe ETFs")
  sharp.mat.indx.annual = (52/sqrt(52)) * sharp.mat.indx
  rownames(sharp.mat.indx)= etfIndex_names
  colnames(sharp.mat.indx) = c("Weekly Index Sharpe")
  colnames(sharp.mat.indx.annual) = c("Annual Index Sharpe ")
  sharpes.etf = cbind(sharp.mat,sharp.mat.annual)
  sharpes.index = cbind(sharp.mat.indx,sharp.mat.indx.annual)
  print(sharpes.etf)
  print(sharpes.index)
  #############################
  ####Hypothesis Testing######
  ###
  t.tst = function(m.etf, m.fund){
    return(t.test(m.etf,m.fund, paired = TRUE)) 
  }
  for(i in 1:dim(LR_data[[1]])[2]){
    hypo = t.tst(LR_data[[1]][,i], LR_data[[2]][,i])
  }
  print(hypo)
  
  for(m in 1:dim(LR_data[[1]])[2]){
      print(c(etf_names[m], "vs", etfIndex_names[m]))
      test.hypo.2samp = t.test(LR_data[[1]][,m],LR_data[[2]][,m], paired = TRUE)
      print(test.hypo.2samp)
    }
  # Q(c) - End
  
  # Q(d) - Start
  
  plot_regressLine = function(f_ex, m_ex, cn){
    df = data.frame( m_ex, f_ex)
    colnames(df) = cn
    plot(df, xlim=c(-0.05,0.05),ylim=c(-0.05,0.05),main="Beta")
    mod = lm(f_ex ~ m_ex)
    abline(mod, col="blue")
    text(-0.05, 0.05, coef(mod)[2], pos = 4)
  }
  
  calc_ExcessRet = function(fund){
    fund_excess = matrix(0, nrow = dim(fund)[1] - 1, ncol = dim(fund)[2])
    for(f in 1:dim(fund)[2]){
      fund_excess[,f] = fund[,f][2:length(fund[,f])] - (T_yield[2:length(T_yield)] / 52)
    }
    return(fund_excess)
  }
  
  ETF_excess = calc_ExcessRet(LR_data[[1]])
  colnames(ETF_excess) = c("ROBO_Excess", "AIA_Excess","AIRR_Excess", "QQQ_Excess", "VGT_Excess")
  
  market_excess = calc_ExcessRet(LR_data[[3]])
  colnames(market_excess) = c("S&P500_Excess", "DJUSTC_Excess")
  
  est_beta = function(ETF.x, market.x){
    estimates = matrix(0, nrow = dim(ETF.x)[2], ncol = 3)
    for(e in 1:dim(ETF.x)[2]){
      f_model = lm(ETF.x[,e]~market.x)
      estimates[e, 1] = coef(f_model)[1]
      estimates[e, 2] = coef(f_model)[2]
      estimates[e, 3] = summary(f_model)$r.squared
    }
    colnames(estimates) = c("Alpha", "Beta", "R-squared")
    rownames(estimates) = etf_names
    return(estimates)
  }
  
  # Estimating Alpha and Beta with S&P 500 Returns 
  est_SP = est_beta(ETF_excess, market_excess[,1])
  print(est_SP)
  
  par(mfrow = c(3, 2))
  for(i in 1:dim(ETF_excess)[2]){
    cname = c(colnames(market_excess)[1], colnames(ETF_excess)[i])
    plot_regressLine(ETF_excess[,i], market_excess[,1], cname)
  }
  
  # Estimating Alpha and Beta with Dow Jones Technology Index 
  est_DJ = est_beta(ETF_excess, market_excess[,2])
  print(est_DJ)
  
  par(mfrow = c(3, 2))
  for(i in 1:dim(ETF_excess)[2]){
    cname = c(colnames(market_excess)[2], colnames(ETF_excess)[i])
    plot_regressLine(ETF_excess[,i], market_excess[,2], cname)
  }
  # Q(d) - End
  
  # Q(e) - Start
  
  for(m in 1:dim(market_excess)[2]){
    for(e in 1:dim(ETF_excess)[2]){
      print("<----------------------------------------------------->")
      print(c(colnames(ETF_excess)[e], "vs", colnames(market_excess)[m]))
      anova_ETF = aov(ETF_excess[,e]~market_excess[,m])
      print(summary(anova_ETF))
      par(mfrow = c(2,3))
      plot(anova_ETF, 1:6)
    }
  }
  
  # Q(e) - End