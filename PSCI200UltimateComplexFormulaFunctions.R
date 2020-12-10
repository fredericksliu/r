#Welcome to the Ultimate Complex Formula Collections for PSCI 200 made by the People
#This was created to make our and possibly your life easier.
#The following functions are purely for your casual reference. 
#Do not use them on your exams or HWs

#List of functions:
#1. findciZmean
#2. findciTmean
#3. findciZproportion
#4. findpopsize
#5. hypotestZmean
#6. hypotestTmean
#7. hypotestZproportion
#8. hypotestTwoGroupsmean
#9. hypotestTwoGroupsproportion
#10.moemean
#11.moeproportion

#BELOW ARE THEIR PURPOSES:

#findciZmean --> Given the following, to find the confidence interval of 
#a large > 30 sample size based on given mean using Z score
#n --> Sample Size
#sd --> Standard Deviation
#meanYbar --> Sample mean
#confidencelevel --> Confidence Level

#findciTmean --> Given the following, to find the confidence interval of 
#a small < 30 sample size based on given mean using T score
#n --> Sample Size
#sd -->  Standard Deviation
#meanYbar --> Sample mean
#confidencelevel -->  Confidence Level

#findciZproportion --> Given the following, to find the confidence interval of 
#a large > 30 sample size based on given proportion using Z score
#n --> Sample Size
#p --> Proportion
#confidencelevel --> Confidence Level

#findpopsize --> Given the following, to find the appropriate population size
#MoE --> Margin of Error
#confidencelevel --> Confidence Level
#p --> Usually assume it to be *0.5*, so just input *0.5*

#hypotestZmean --> Given the following,
#use P-value and classical method to conduct Hypothesis test of large > 30 sample size using Z score
#u0 --> Mean of the Null Hypothesis
#n --> Sample Size
#sd --> Standard Deviation
#meanYbar --> Sample mean
#confidencelevel --> Confidence Level (usually if given alpha, use 1-alpha to find confidence level)

#hypotestTmean --> Given the following,
#use P-value and classical method to conduct Hypothesis test of small < 30 sample size using T score
#u0 --> Mean of the Null Hypothesis
#n --> Sample Size
#sd --> Standard Deviation
#meanYbar --> Sample mean
#confidencelevel --> Confidence Level (usually if given alpha, use 1-alpha to find confidence level)

#hypotestZproportion --> Given the following,
#use P-value and classical method to conduct Hypothesis test of large > 30 sample size using Z score
#pi0 --> Proportion of the Null Hypothesis
#n --> Sample Size
#p --> proportion
#confidencelevel --> Confidence Level (usually if given alpha, use 1-alpha to find confidence level)

#hypotestTwoGroupsmean --> Given the following, conduct Hypothesis test to test the mean of two groups being equal using Z score
#xbarV1mean --> mean of Variable/Group 1
#xbarV2mean --> mean of Variable/Group 2
#nV1 --> size of Variable/Group 1
#nV2 --> size of Variable/Group 2
#sdV1 --> standard deviation of Variable/Group 1
#sdV2 --> standard deviation of Variable/Group 2

#hypotestTwoGroupsproportion --> Given the following, conduct Hypothesis test to test the proportion of two groups being equal using Z score
#proportionV1 --> Proportion of Variable/Group 1
#proportionV2 --> Proportion of Variable/Group 2
#nV1 --> size of Variable/Group 1
#nV2 --> size of Variable/Group 2

#moemean --> Given the following, find the margin of error of the sample mean
#n --> Sample Size
#confidencelevel --> Confidence Level (usually if given alpha, use 1-alpha to find confidence level)
#sd --> Standard Deviation

#moeproportion  --> Given the following, find the margin of error of the sample proportion
#n --> Sample Size
#p --> proportion
#confidencelevel --> Confidence Level (usually if given alpha, use 1-alpha to find confidence level)


findciZmean = function(n = NULL, sd = NULL, meanYbar = NULL, confidencelevel = NULL){
  alpha = 1 - confidencelevel
  z = qnorm(alpha/2)
  sderror = sd/sqrt(n) 
  confidenceintervalleft = meanYbar + (z * sderror) 
  confidenceintervalright = meanYbar - (z * sderror)
  print ("Based on your sample size of: " )
  print(n)
  print("The confidence interval on the right bound is:")
  print(confidenceintervalright)
  print("The confidence interval on the left bound is:")
  print(confidenceintervalleft)
}

findciTmean = function(n = NULL, sd = NULL, meanYbar = NULL, confidencelevel = NULL){
  alpha = 1 - confidencelevel
  df = n - 1
  t = qt(alpha/2, df)
  sderror = sd/sqrt(n) 
  confidenceintervalleft = meanYbar + (t * sderror) 
  confidenceintervalright = meanYbar - (t * sderror)
  print ("Based on your sample size of: " )
  print(n)
  print("The confidence interval on the right bound is:")
  print(confidenceintervalright)
  print("The confidence interval on the left bound is:")
  print(confidenceintervalleft)
}

findciZproportion = function(n = NULL, p = NULL, confidencelevel = NULL){
  alpha = 1 - confidencelevel
  z = qnorm(alpha/2)
  sderror = sqrt(p*(1-p)/n)
  confidenceintervalleft = p + (z * sderror) 
  confidenceintervalright = p - (z * sderror)
  print ("Based on your sample size of: " )
  print(n)
  print("The confidence interval on the right bound is:")
  print(confidenceintervalright)
  print("The confidence interval on the left bound is:")
  print(confidenceintervalleft)
}

findpopsize = function (MoE = NULL, confidencelevel = NULL, p = NULL){
  alpha = 1 - confidencelevel
  z = qnorm(alpha/2)
  n = ((p*z)/MoE)^2
  print("Based on your requested Margin of Error of: ")
  print(MoE)
  print("The appropriate sample size for your experiment is: ")
  print(n)
}

hypotestZmean = function(u0 = NULL, n = NULL, sd = NULL, meanYbar = NULL, confidencelevel = NULL){
  alpha = 1 - confidencelevel
  sderror = sd/sqrt(n)
  z = (meanYbar - u0)/sderror
  zcrit = qnorm(alpha/2)
  
  print("P-value method with the P-value --> ")
  pval = pnorm(-abs(z))*2
  
  print(pval)
  if (pval < 0.05 && pval > 0.01) {
    print("We reject null hypothesis by stating it is statistically significant at the level of the p-value with alpha = 0.05")
    print("However, we DO NOT reject null hypothesis by stating it is NOT statistically significant at the at standard level of significance with alpha = 0.01")
  }
  
  if(pval > 0.05 && pval > 0.01){
    print("We DO NOT reject null hypothesis by stating it is NOT statistically significant at the at standard level of significance with alpha = 0.05 and 0.01")
  }
  
  if(pval < 0.05 && pval < 0.01){
    print("We reject null hypothesis by stating it is statistically significant at the level of the p-value with alpha = 0.05 and 0.01")
  }
  
  print("Classical method -->")
  if (z < 0){
    z = abs(z)
  }
  if (zcrit < 0){
    zcrit = abs(zcrit)
  }
  print("Z: ")
  print(z)
  print("Zcrit: ")
  print(zcrit)
  if(z > zcrit){
    print("We reject the null hypothesis at the level of alpha: ")
    print(alpha)
  } else if (zcrit > z){
    print("We do not reject the null hypothesis at the level of alpha: ")
    print(alpha)
  }
}

hypotestTmean = function(u0 = NULL, n = NULL, sd = NULL, meanYbar = NULL, confidencelevel = NULL){
  alpha = 1 - confidencelevel
  sderror = sd/sqrt(n)
  df = n -1
  t = (meanYbar - u0)/(sderror)
  tcrit = qt (alpha/2, df)
  
  print("P-value method with the P-value --> ")
  
  pval = pnorm(-abs(t))*2
  
  print(pval)
  if (pval < 0.05 && pval > 0.01) {
    print("We reject null hypothesis by stating it is statistically significant at the level of the p-value with alpha = 0.05")
    print("However, we DO NOT reject null hypothesis by stating it is NOT statistically significant at the at standard level of significance with alpha = 0.01")
  }
  
  if(pval > 0.05 && pval > 0.01){
    print("We DO NOT reject null hypothesis by stating it is NOT statistically significant at the at standard level of significance with alpha = 0.05 and 0.01")
  }
  
  if(pval < 0.05 && pval < 0.01){
    print("We reject null hypothesis by stating it is statistically significant at the level of the p-value with alpha = 0.05 and 0.01")
  }
  
  print("Classical method -->")
  if (z < 0){
    z = abs(z)
  }
  if (tcrit < 0){
    tcrit = abs(tcrit)
  }
  print("T: ")
  print(t)
  print("Tcrit: ")
  print(tcrit)
  if(t > tcrit){
    print("We reject the null hypothesis at the level of alpha: ")
    print(alpha)
  } else if (tcrit > t){
    print("We do not reject the null hypothesis at the level of alpha: ")
    print(alpha)
  }
}


hypotestZproportion = function(pi0 = NULL, n = NULL, p = NULL, confidencelevel = NULL){
  z = (p - pi0)/sqrt((pi0*(1-pi0))/n)
  alpha = 1 - confidencelevel
  zcrit = qnorm(alpha/2)
  
  print("P-value method with the P-value --> ")
  pval = pnorm(-abs(z))*2
  print(pval)
  if (pval < 0.05 && pval > 0.01) {
    print("We reject null hypothesis by stating it is statistically significant at the level of the p-value with alpha = 0.05")
    print("However, we DO NOT reject null hypothesis by stating it is NOT statistically significant at the at standard level of significance with alpha = 0.01")
  }
  
  if(pval > 0.05 && pval > 0.01){
    print("We DO NOT reject null hypothesis by stating it is NOT statistically significant at the at standard level of significance with alpha = 0.05 and 0.01")
  }
  
  if(pval < 0.05 && pval < 0.01){
    print("We reject null hypothesis by stating it is statistically significant at the level of the p-value with alpha = 0.05 and 0.01")
  }
  
  print("Classical method --> ")
  if (z < 0){
    z = abs(z)
  }
  if (zcrit < 0){
    zcrit = abs(zcrit)
  }
  print("Z: ")
  print(z)
  print("Zcrit: ")
  print(zcrit)
  
  if(z > zcrit){
    print("We reject the null hypothesis at the level of alpha: ")
    print(alpha)
  } else if (zcrit > z){
    print("We do not reject the null hypothesis at the level of alpha: ")
    print(alpha)
  }
}

hypotestTwoGroupsmean = function(xbarV1mean = NULL, xbarV2mean = NULL, nV1 = NULL, nV2 = NULL, sdV1 = NULL, sdV2 = NULL){
  u0 = 0
  standarderror = sqrt(((sdV1)/nV1) + ((sdV2)/nV2))
  z = ((xbarV1mean - xbarV2mean) - u0)/standarderror
  print("P-value method with the P-value --> ")
  pval = pnorm(-abs(z))*2
  print(pval)
  if (pval < 0.05 && pval > 0.01) {
    print("We reject null hypothesis by stating it is statistically significant at the level of the p-value with alpha = 0.05")
    print("However, we DO NOT reject null hypothesis by stating it is NOT statistically significant at the at standard level of significance with alpha = 0.01")
  }
  
  if(pval > 0.05 && pval > 0.01){
    print("We DO NOT reject null hypothesis by stating it is NOT statistically significant at the at standard level of significance with alpha = 0.05 and 0.01")
  }
  
  if(pval < 0.05 && pval < 0.01){
    print("We reject null hypothesis by stating it is statistically significant at the level of the p-value with alpha = 0.05 and 0.01")
  }
}

hypotestTwoGroupsproportion = function (proportionV1 = NULL, proportionV2 = NULL, nV1 = NULL, nV2 = NULL){
  targetV1 = proportionV1 * nV1
  targetV2 = proportionV2 * nV2
  poolestimatepihat = ((targetV1) + (targetV2))/(nV1 + nV2)
  standarderror = sqrt((poolestimatepihat*(1- poolestimatepihat)/nV1) + (poolestimatepihat*(1- poolestimatepihat)/nV2))
  z = (proportionV1 - proportionV2)/standarderror
  
  print("P-value method with the P-value --> ")
  pval = pnorm(-abs(z))*2
  print(pval)
  if (pval < 0.05 && pval > 0.01) {
    print("We reject null hypothesis by stating it is statistically significant at the level of the p-value with alpha = 0.05")
    print("However, we DO NOT reject null hypothesis by stating it is NOT statistically significant at the at standard level of significance with alpha = 0.01")
  }
  
  if(pval > 0.05 && pval > 0.01){
    print("We DO NOT reject null hypothesis by stating it is NOT statistically significant at the at standard level of significance with alpha = 0.05 and 0.01")
  }
  
  if(pval < 0.05 && pval < 0.01){
    print("We reject null hypothesis by stating it is statistically significant at the level of the p-value with alpha = 0.05 and 0.01")
  }
}

moemean = function ( n = NULL, confidencelevel = NULL, sd = NULL){
  alpha = 1 - confidencelevel
  z = qnorm(alpha/2)
  if (z < 0){
    z = abs(z)
  }
  sderror = sd/sqrt(n)
  z * sderror
}

moeproportion = function (n = NULL, confidencelevel = NULL, p = NULL){
  alpha = 1 - confidencelevel
  z = qnorm(alpha/2)
  if (z < 0){
    z = abs(z)
  }
   sderror = sqrt((p*(1-p))/n)
  z * sderror
}
