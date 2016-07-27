# HelperFunctions for portfolio management

Stats <- function(df, cols.return)
{
  m = NULL
  v = NULL
  for(j in 1:length(cols.return)) {
    m[j] = mean(df[,cols.return[j]], na.rm=T)
    v[j] = var(df[,cols.return[j]], na.rm=T)
  }
  cv = cov(df[2:nrow(df),cols.return[1]], df[2:nrow(df),cols.return[2]])
  s = as.data.frame(cbind(m,v,cv))
  s$cv[1] = NA
  names(s) = c("Return.Average", "Variance", "Covariance")
  row.names(s) = names(df[,cols.return])
  return(s)
}

Stats.grouped = function(df, dv, iv)
{
  means = tapply(df[,dv], df[,iv], mean, na.rm=T)
  variance = tapply(df[,dv], df[,iv], var, na.rm=T)
  n = tapply(df[,dv], df[,iv], length)
  stats = as.data.frame(means)
  stats = cbind(stats, variance, n)
  return(stats)
}

pref.sample.percent = function(exp.return, variability, risk.aversion=2, v.type = c("var", "vol", "sd")) {
  exp.return = exp.return/100
  variability = variability/100
  ifelse(v.type == "vol" | v.type == "sd", (variance = variability^2), (variance = variability)) 
  print(exp.return - risk.aversion/2 * variance)
}

pref.sample = function(exp.return, variability, risk.aversion=2, v.type = c("var", "vol", "sd")) {
  exp.return = exp.return
  variability = variability
  ifelse(v.type == "vol" | v.type == "sd", (variance = variability^2), (variance = variability)) 
  print(exp.return - risk.aversion/2 * variance)
}

