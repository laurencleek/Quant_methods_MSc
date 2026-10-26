##############################################################################
# Week 4 Demo: Analyzing an Interaction Effect (OLS Regression)
#
# Data: Prestige (from the carData package), so no external data files are needed.
# How to use: run the script line by line (Ctrl/Cmd + Enter) and look at the
# output and plots after each step. Lines starting with "# Result:" describe
# what we find in this example.
##############################################################################

# Note: The procedures below work in principle similar for logistic regression models

# Example: Does the effect of income on occupational prestige depend on education?
# DV: Prestige score of the occupation (Prestige)
# Two continuous IVs: Income (in 1000 dollars) and Education (average years of education).
# Goal: Conditional income effects at different levels of education.
# Theoretical idea: income and education both 'buy' prestige, but they may be
#                   partly substitutes: income may matter less where education is already high.

# Variables (see Week 2 demo for the recoding):
# Prestige = Pineo-Porter prestige score of the occupation (0-100)
# Income = Average income of the occupation (in 1000 Canadian dollars)
# Education = Average years of education in the occupation (6.4-16.0)
# Women = Percentage of women in the occupation (0-100)
# WC, Prof = Occupation type indicators (white collar, professional; blue collar = reference)

### 0. Setup

# Start with a clean workspace, so objects from earlier sessions cannot get mixed up
rm(list = ls())

# Packages needed for this script
pkgs <- c("carData", "tidyverse", "psych", "marginaleffects", "clarify")

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


########## 1. Model Estimation (OLS Regression) ##########

## Load and describe data (recoding as in the Week 2 demo)
prestige.data <- Prestige |>
  mutate(Income = income/1000,                    # income in 1000 dollars
         WC = if_else(type == "wc", 1, 0),        # occupation type dummies (blue collar = reference)
         Prof = if_else(type == "prof", 1, 0) ) |>
  rename(Prestige = prestige, Education = education, Women = women) |>
  dplyr::select(Prestige, Income, Education, Women, WC, Prof) |>
  na.omit()                                       # removes the 4 occupations without a type
summary(prestige.data)
psych::describe(prestige.data)
hist(prestige.data$Prestige)

## Estimate model
# Note: 'Income*Education' would be a shortcut for Income + Education + Income:Education
M1 <- lm(Prestige ~ Women + WC + Prof + Income + Education + Income:Education,
         data=prestige.data)
summary(M1)
round(vcov(M1), 3)
plot(M1)
# Result: the interaction coefficient is negative (-0.24, p = .001): the income effect gets smaller
#         at higher education levels. Because of the interaction, the 'Income' coefficient (4.38)
#         is only the effect of income for occupations with Education = 0 (not observed!),
#         and the 'Education' coefficient (5.25) only the effect of education at Income = 0.
#         Neither can be interpreted on its own: we need conditional effects (see below)


########## 2. Interpretation/Plotting of Interaction Effect ##########

##### Option 1: 'marginaleffects' library

## Conditional Coefficients: table and plot

# Conditional coefficient for income at different levels of education
slopes(M1,
       variables="Income",
       newdata=datagrid(Education=seq(from=6, to=16, by=1)))
# Note: If you multiply the conditional coefficient by a change in income (e.g. 10),
#       you would obtain the conditional predicted effects.
#       For an easier option, see 'predicted values' below.
# Result: an extra 1000 dollars of income goes with about +2.9 prestige points at 6 years
#         of education, but only about +0.5 points at 16 years: always positive, but shrinking

slopes(M1,
       variables="Income",
       newdata=datagrid(Education=seq(from=6, to=16, by=1))) |>
  ggplot(aes(x=Education, y=estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha = 0.2) +
  scale_x_continuous(limits=c(6,16), breaks=seq(6,16,by=2)) +
  labs(y = "Conditional Income Coefficient (with 95% CI)",
       x = "Education (years)") +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  theme_bw()
# Result: the CI excludes zero up to 15 years of education; at 16 years it includes zero (p = .08),
#         so at the very top of the education range we cannot rule out that income adds nothing.
#         (Observed education runs from 6.4 to 16.0 years, so the ends of the plot are at the edge of the data)

## Predicted Values (-> usually the better or easier to understand option)

# Effect of low (3,000 dollars) vs. high (12,000 dollars) income at different levels of education
predictions(M1,
  newdata=datagrid(Education=seq(from=6,to=16, by=1),
                   Income=c(3,12))) |>
  mutate(Income=factor(Income,  # metric variable must be converted to factor variable
                       levels=c(3, 12),
                       labels=c("Low (3k)", "High (12k)"))) |>
  ggplot(aes(x=Education, y=estimate, linetype=Income)) +
  geom_line() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha = 0.2) +
  labs(y="Predicted Prestige",
       x="Education (years)",
       linetype="Income") +
  scale_x_continuous(breaks=seq(6,16,by=2)) +
  theme_bw()
# Result: at 6 years of education, high-income occupations are predicted about 26 points higher
#         in prestige (46 vs. 20); at 16 years the gap is only about 5 points (70 vs. 65) and the
#         CIs overlap: the lines converge

##### Option 2: 'clarify' library (formerly known as 'Zelig')

# Note: 'clarify' works best with metric variables (in/for estimation and plots).
#       If used, don't transform dummies to factor variables

library(clarify)

## Predicted Values

# Simulate a set of model coefficients (1000 by default) for estimated model
# Note: the simulation is random, so results differ slightly every time (use set.seed(123) to fix them)
M1.sim <- clarify::sim(M1)

# Optional: Calculate and plot predicted values for main effects
Income.est <- sim_setx(M1.sim, x=list(Income=seq(2, 14, by=2)), verbose=FALSE)
summary(Income.est)
plot(Income.est) + labs(y="Predicted Prestige",
                        x="Income (1000 dollars)")

Educ.est <- sim_setx(M1.sim, x=list(Education=c(6:16)), verbose=FALSE)
summary(Educ.est)
plot(Educ.est) + scale_x_continuous(breaks=seq(6,16,by=2)) +
                 labs(y="Predicted Prestige",
                      x="Education (years)")

# Calculate and plot predicted values for interaction effect
IncEduc.est <- sim_setx(M1.sim, x=list(Education=6:16, Income=c(3,12)), verbose=FALSE)
# Note: Other IVs are set to mean or modal value, but can be specified as well, e.g. Prof=0.
summary(IncEduc.est)
plot(IncEduc.est) + scale_x_continuous(breaks=seq(6,16,by=2)) +
                    labs(y="Predicted Prestige",
                         x="Education (years)")
# Alternative: Remove legend and add text to plot area
plot(IncEduc.est) + scale_x_continuous(breaks=seq(6,16,by=2)) +
                    labs(y="Predicted Prestige",
                         x="Education (years)") +
                    theme(legend.position="none") +
                    annotate(geom="text", x=8, y=58, label="High Income (12k)", color="blue") +
                    annotate(geom="text", x=12, y=35, label="Low Income (3k)", color="red")

# Optional: Check the simulated data object,
#           e.g. the values for the other IVs (for means/modal values)
str(IncEduc.est)


################ Appendix: Plotting Conditional Coefficients with base R ###################

# Note: The following commands create the conditional coefficient plots discussed
#       in the video lecture (using the model M1 estimated above).

### Create new data matrix with conditional income coefficient for plotting

# Create data set with values that capture the full range of the education variable:
# seq function creates sequence of numbers from 6 to 16, increasing by 1 at each step
educ <- seq(6,16, by=1); educ

# Optional: store number of occupations at different education levels in new variable
# (education is rounded to whole years, because it is an average and not a whole number)
educ.n <- table(factor(round(prestige.data$Education), levels=educ)); educ.n

# Compute the conditional income coefficient (using income & interaction coefficients).
b.incX <- M1$coef["Income"] + (M1$coef["Income:Education"] * educ); b.incX

# Compute the conditional standard errors (using coefficient variances & covariances).
se.incX <- sqrt( vcov(M1)["Income","Income"] +
                (educ^2 * vcov(M1)["Income:Education","Income:Education"]) +
                (2 * educ * vcov(M1)["Income","Income:Education"]) ); se.incX

# Compute the conditional upper & lower CIs (95% CI; critical t value = 1.96).
lci.incX <- b.incX - (1.96 * se.incX); lci.incX
uci.incX <- b.incX + (1.96 * se.incX); uci.incX

# combine data in matrix
incX <- cbind(educ,b.incX,se.incX,lci.incX,uci.incX,educ.n)
head(incX)

### Plot conditional income coefficient with CIs

par(las=1)
plot(educ, b.incX, ylim=c(min(lci.incX),max(uci.incX)), type="l", lwd=2,
     xlab="Education (years)", ylab="Conditional Income Coefficient (with 95% CI)")
abline(h=0, lty=1)
lines(educ, lci.incX, type="l", lty=3)
lines(educ, uci.incX, type="l", lty=3)

### Plot conditional income coefficient with CIs plus number of occupations
# Note: this requires two overlayed plots

opar <- par(no.readonly=TRUE)
par(mar=c(5,4,4,4), las=1) #changing the margins of the plot (more space on right side)
plot(educ, b.incX, ylim=c(min(lci.incX),max(uci.incX)), type="l", lwd=2, axes=FALSE,
     xlab="Education (years)", ylab="Conditional Income Coefficient (with 95% CI)")
box()
axis(side=1)
axis(side=2)
abline(h=0, lty=1)
lines(educ, lci.incX, type="l", lty=3)
lines(educ, uci.incX, type="l", lty=3)
par(new=TRUE)
plot(educ, educ.n, type="s", col="Grey", axes=F, xlab="", ylab="")
axis(side=4)
par(las=0)
mtext("Number of Occupations", side=4, line=2)
par(opar)


##### Exercises #####
# (1) Swap the roles: plot the conditional effect of Education at different levels of Income.
#     (Hint: in slopes(), use variables="Education" and datagrid(Income=...)).
#     Is the interpretation the same?
# (2) Re-estimate the model with Women as the moderator (Education:Women). Is there an interaction?
# (3) Why is it risky to read the 'Income' coefficient of M1 on its own?
