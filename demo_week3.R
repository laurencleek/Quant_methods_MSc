##############################################################################
# Week 3 Demo: Checking Assumptions / Model Diagnostics for Linear Regression
#
# Data: Prestige (from the car package), so no external data files are needed.
# How to use: run the script line by line (Ctrl/Cmd + Enter) and look at the
# output and plots after each step. Lines starting with "# Result:" describe
# what we find in this example.
##############################################################################

### 0. Setup

# Packages needed for this script
pkgs <- c("car")  # add more packages here

# Personal (user-writable) library, so no admin rights are needed
lib <- Sys.getenv("R_LIBS_USER")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)  # create it if missing
.libPaths(c(lib, .libPaths()))                           # search it first

# Install each package only if it is missing, then load it
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {  # TRUE if already installed
    install.packages(p, lib = lib, repos = "https://cloud.r-project.org")
  }
  library(p, character.only = TRUE)  # load using the name stored in p
}

### 1. Data and model
# Prestige: occupational prestige in Canada (102 occupations)
# prestige = prestige score, education = mean years of education,
# income = mean income, women = % women in the occupation
head(Prestige)
summary(Prestige)

M1 <- lm(prestige ~ education + income + women, data = Prestige)
summary(M1)
# Result: R2 = 0.80. Education (b = 4.19) and income (p < .001) predict prestige;
# women is not significant (p = .77)

### 2. Key Assumptions
# (1) Independent errors (cases)
# (2) No perfect (or very high) multicollinearity
# (3) Linearity and additivity
# (4) Homoskedasticity
# (5) Normally distributed errors
# (6) Limited impact of outliers and influential cases

## 2a. Check for (1) Independent errors (cases)

# Independence cannot be checked (with exceptions): it depends on the 'sampling' design
# Check for serial correlation over time (Durbin-Watson test) applies only
# to time series data!

## 2b. Check for (2) Multicollinearity: VIF or Tolerance

vif(M1)
1/vif(M1)              #Tolerance measure (report VIF *or* tolerance, not both)
# Result: VIF = 1.85 (education), 2.28 (income), 1.53 (women); tolerance = 0.54, 0.44, 0.66
# -> all well below common warning levels (VIF > 5-10, tolerance < 0.2/0.1): no multicollinearity problem

## 2c. Checks for (3)-(6) based on (standardized) residuals

plot(M1)          #simple way to get four diagnostic residual plots (in the console, press Enter to see the next plot)
par(mfrow=c(2,2))
plot(M1)          #same plots, but combined in a single plot (four panels)
par(mfrow=c(1,1))

## Alternative: Request six different residual plots individually
plot(M1, 1)  #residuals vs. fitted
# Result: red line is mostly flat, with a slight bend at the high end -> mild non-linearity (traced to income, see residualPlots below)
# Linear relationship -> should have a horizontal line with no distinct patterns
plot(M1, 2)  #normal Q-Q
# Result: points close to the line -> errors approximately normal (see also Shapiro test below)
# Normally distributed errors -> residuals should follow a straight line
plot(M1, 3)  #scale-location
# Result: red line is roughly flat -> no sign of heteroscedasticity (formal test: ncvTest(M1), p = .62)
# Homoscedasticity -> should have a horizontal line with equally spread points
plot(M1, 4)  #Cook's distance
# Influential cases -> identifies cases with high Cook's distance values
plot(M1, 5)  #residuals vs. leverage
# Extreme cases -> shows extreme cases for outcome (residuals) and predictors (leverage)
plot(M1, 6)  #Cook's distance vs. leverage
# Influential cases -> shows extreme (predictors) and influential cases

## 2d. Other options for residual checks/diagnostics

# (Normal) distribution of residuals
boxplot(residuals(M1), horizontal=TRUE)
plot(density(residuals(M1)))               #kernel density plot
rug(residuals(M1))                         #add a 'rug' (strip plot) to indicate number of observations
qqnorm(residuals(M1))                      #normal Q-Q plot
qqline(residuals(M1), col="red", lwd=2)    #add linear line
shapiro.test(residuals(M1))                #formal test: H0 = normal distribution
# Result: W = 0.99, p = .92 -> no evidence against normality of residuals (boxplot/density/Q-Q agree)

# Outliers/influential cases
plot(cooks.distance(M1))
influencePlot(M1)         #bubble-plot of studentized residuals against hat-values (size of bubbles = Cook's distance)
influenceIndexPlot(M1)    #all values for Cook's distance, hat-value, p-value for outlier test, and studentized residual in a spike-plot

# Predictor-specific plots
residualPlots(M1)             #Pearson residuals against each predictor (incl. curvature test)
avPlots(M1)                   #partial-regression plots: for each predictor (most useful!)
# Result: only income shows significant curvature (p < .001); Tukey test for whole model p = .16
# Fix to try: M2 <- lm(prestige ~ education + log(income) + women, data = Prestige), then re-check
# Influence: general.managers (Cook's D = 0.28, hat = 0.34) is the most influential case,
#   a high-income occupation sitting where the income curvature is strongest. No Cook's D > 1.

# Note: income shows a curved relationship with prestige: it can help to revisit the descriptive plot
plot(Prestige$income, Prestige$prestige)
lines(lowess(Prestige$income, Prestige$prestige), col="red")
# Result: relationship flattens at high incomes (diminishing returns) -> try log(income)

## 2e. Function for Residual Diagnostics for Multivariate OLS Regression Models

# Define a function that calculates several residual diagnostics in one go.
# (You do not need to understand every line: run the definition, then call it on a model.)
residual.checks <- function(model) {
    cat("--------- Residual Diagnostics for Multivariate Linear Models ---------- \n \n")
	# proportion of large absolute standardized residuals
	large.residual.2 <- rstandard(model) > 2 | rstandard(model) < -2
	large.residual.25 <- rstandard(model) > 2.5 | rstandard(model) < -2.5
	large.residual.3 <- rstandard(model) > 3 | rstandard(model) < -3
	ASR1 <- round((sum(large.residual.2) / nrow(model$model))*100, 2)
	ASR2 <- round((sum(large.residual.25) / nrow(model$model))*100, 2)
	ASR3 <- round((sum(large.residual.3) / nrow(model$model))*100, 2)
	if(ASR1 <= 5) { test1 <- "passed"
      } else test1 <- "failed"
	if(ASR2 <= 1) { test2 <- "passed"
      } else test2 <- "failed"
	if(ASR3 <= 0) { test3 <- "passed"
      } else test3 <- "failed"
      cat("Proportion of large absolute standardized residuals (ASR): \n")
      cat("    Criterion ASR>2.0 (<5%):", round(sum(large.residual.2),4),"case(s),", ASR1, "% | Check", test1, "\n")
      cat("    Criterion ASR>2.5 (<1%):", round(sum(large.residual.25),4),"case(s),", ASR2, "% | Check", test2, "\n")
      cat("    Criterion ASR>3.0 (=0%):", round(sum(large.residual.3),4),"case(s),", ASR3, "% | Check", test3, "\n")
	# Cook's value
      cook.lim <- 4/(nrow(model$model))
	cook <- cooks.distance(model) > cook.lim
	if(max(cooks.distance(model)) < cook.lim) { test1 <- "passed"
      } else test1 <- sprintf("failed for %d case(s)", sum(cook))
      cat("Cook's Distance: \n")
      cat("    Largest observed Cook's Distance =", round(max(cooks.distance(model)),4), "\n")
      cat("    Criterion 4/n:", round(cook.lim, 4), "| Check", test1, "\n")
	# leverage/hat value
	AL1.crit <- model$rank / nrow(model$model)
	AL2.crit <- 2 * model$rank / nrow(model$model)
	AL3.crit <- 3 * model$rank / nrow(model$model)
	AL1 <- hatvalues(model) > AL1.crit
	AL2 <- hatvalues(model) > AL2.crit
	AL3 <- hatvalues(model) > AL3.crit
	if(max(hatvalues(model)) < AL1.crit) { test1 <- "passed"
      } else test1 <- sprintf("failed for %d case(s)", sum(AL1))
	if(max(hatvalues(model)) < AL2.crit) { test2 <- "passed"
      } else test2 <- sprintf("failed for %d case(s)", sum(AL2))
	if(max(hatvalues(model)) < AL3.crit) { test3 <- "passed"
      } else test3 <- sprintf("failed for %d case(s)", sum(AL3))
      cat("Leverage / Hat Values: \n")
      cat("    Largest observed hat value =", round(max(hatvalues(model)),4), "\n")
      cat("    Criterion  k/n:", round(AL1.crit,4), "| Check", test1, "\n")
      cat("    Criterion 2k/n:", round(AL2.crit,4), "| Check", test2, "\n")
      cat("    Criterion 3k/n:", round(AL3.crit,4), "| Check", test3, "\n")
	# covariance ratio (CVR)	  
	CVR.lim <- 1 + AL3.crit 
	CVR <- abs(covratio(model)) > CVR.lim
	if(max(abs(covratio(model))) < CVR.lim) { test1 <- "passed"
      } else test1 <- sprintf("failed for %d case(s)", sum(CVR))
	cat("Covariance Ratio: \n")
      cat("    Largest observed absolute covariance ratio =", round(max(abs(covratio(model))),4), "\n")
      cat("    Criterion 1+(3k/n):", round(CVR.lim,4), "| Check", test1, "\n")
	# difference statistics (DFBeta/DFFit)
      dfbeta.lim <- 2/sqrt(nrow(model$model))
	dfbeta <- abs(dfbeta(model)) > dfbeta.lim
	if(max(abs(dfbeta(model))) < dfbeta.lim) { test1 <- "passed"
      } else test1 <- sprintf("failed for %d case(s)", sum(dfbeta))
	cat("Difference Statistics (for excluded cases): \n")
      cat("    DFBeta (across all coefficients): \n")
      cat("      Largest observed absolute DFBeta =", round(max(abs(dfbeta(model))),4), "\n")
      cat("      Criterion 2/sqrt(n):", round(dfbeta.lim, 4), "| Check", test1, "\n")
      dffit.lim <- 2*sqrt(model$rank/nrow(model$model))
	dffit <- abs(dffits(model)) > dffit.lim
	if(max(abs(dffits(model))) < dffit.lim) { test1 <- "passed"
      } else test1 <- sprintf("failed for %d case(s)", sum(dffit))
      cat("    DFFit: \n")
      cat("      Largest observed absolute DFFit =", round(max(abs(dffits(model))),4), "\n")
      cat("      Criterion 2*sqrt(k/n):", round(dffit.lim, 4), "| Check", test1, "\n")
      cat("------------------------------------------------------------------------ \n")
}
residual.checks(M1)   #apply the function to our model
# Result: residuals OK (5 cases >2 = 4.9%, 1 case >2.5 = 0.98%, none >3). Cook's D, leverage,
# DFBeta/DFFit 'fail' for several cases, but these strict cutoffs flag many cases in any
# sample this size: treat as screening rules, and rely on the plots and largest values.
# Max Cook's D = 0.28 (<1), so no single case is dominating the model.

### 3. Fixing the problem: log-transforming income
# Income shows curvature (diminishing returns), so we model log(income) instead

M2 <- lm(prestige ~ education + log(income) + women, data = Prestige)
summary(M2)
vif(M2)
residualPlots(M2)   #curvature test for log(income) is no longer significant
# Result: R2 rises from 0.80 to 0.84, and the curvature for income is gone (p = .56; Tukey test p = .45).
# VIFs stay low, residuals are still normal (Shapiro p = .98) and homoscedastic (ncvTest p = .87).
# The women variable now shows a small curvature test (p = .02), a minor issue not pursued here.
# general.managers is no longer the most influential case, but still has high leverage (hat = 0.29).
# Note: with log(income), the coefficient is no longer "per dollar" but "per unit of log-income".

### 4. Optional: Identifying and viewing specific cases

# Identifying largest cases and displaying them
which.max(abs(residuals(M1)))         #observation with largest residual
which.max(abs(cooks.distance(M1)))    #observation with max Cook's distance
Prestige["general.managers",]         #display specific case by row name (all variables)
# Result: general.managers has the largest Cook's D (0.28); newsboys has the largest residual

# Identifying cases by threshold and displaying them (safe option)
Prestige$large.residual.3 <- abs(rstandard(M1)) > 3
Prestige[Prestige$large.residual.3, ]
Prestige$large.hat.value <- hatvalues(M1) == max(hatvalues(M1))
Prestige[Prestige$large.hat.value, ]
# -> general.managers: highest leverage (hat = 0.34) due to extreme income (~25,900)

### 5. Exercises
# (1) Re-run the diagnostics (plots, vif, residual.checks) for M2. What has changed compared to M1?
# (2) Which occupations are flagged as influential in M2? Look at them with Prestige[ "name", ]
# (3) Add the predictor 'type' (occupation type) to the model. Do the assumptions still hold?
