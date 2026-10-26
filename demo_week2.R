##############################################################################
# Week 2 Demo: Preparing Data, Descriptives, and Linear Regression
#
# Data: Prestige (from the carData package), so no external data files are needed.
# How to use: run the script line by line (Ctrl/Cmd + Enter) and look at the
# output and plots after each step. Lines starting with "# Result:" describe
# what we find in this example.
#
# Tips:
# - Run the code from this editor window (not by pasting blocks into the console),
#   otherwise lines can get garbled and later steps fail with "object not found".
# - Always start from the top: later sections depend on objects created earlier
#   (e.g. prestige.data in Section 1).
# - To run everything at once: source("demo_week2.R", echo = TRUE)
# - If you get strange errors, restart R (RStudio: Session > Restart R) and start again.
##############################################################################

### 0. Setup

# Start with a clean workspace, so objects from earlier sessions (e.g. an old M1)
# cannot get mixed up with the ones created here
rm(list = ls())

# Packages needed for this script
pkgs <- c("carData", "tidyverse", "psych", "arm", "vioplot","httpgd","texreg", "Hmisc")  # Hmisc: needed by mean_cl_normal() in ggplot

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

# Copy/save default plot parameters (in case they get messed up later)
par(las=1)
opar <- par(no.readonly=TRUE)

##### 1. Preparing Data Set #####

# Note: Variable coding is usually reported in a codebook

## Prestige data description (after recoding):
# Prestige = Pineo-Porter prestige score of the occupation (0-100: low to high prestige)
# Education = Average years of education of people in the occupation
# Income = Average income of people in the occupation (in 1000 Canadian dollars)
# Women = Percentage of women in the occupation (0-100)
# BC, WC, Prof = Dichotomous occupation type indicators (1=occupation is of this type, 0=is not)
#                (blue collar, white collar, professional/managerial/technical)
# Type = Categorical occupation type variable (factor)
# Source: Canadian census of 1971; 102 occupations

# Loading the data (it comes with the carData package)
prestige.all <- Prestige   # work on a copy, so the original stays untouched
summary(prestige.all)
glimpse(prestige.all)
head(prestige.all)
# Note: the occupation names are stored as row names, not as a variable
# Result: 102 occupations; 'type' has 4 missing values (NA), all other variables are complete

## Recoding variables (using tidyverse library)

# DV (Prestige): check values; no recoding needed, just give it a capitalised name
summary(prestige.all$prestige)
prestige.all <- prestige.all |>
  mutate(Prestige = prestige)

# IV (Occupation type): check values, create dummies & categorical variable
table(prestige.all$type, useNA="ifany")
# Note: dummies are created with if_else(); NAs stay NAs (a comparison with NA returns NA)
prestige.all <- prestige.all |>
  mutate(BC = if_else(type == "bc", 1, 0),
         WC = if_else(type == "wc", 1, 0),
         Prof = if_else(type == "prof", 1, 0) )
summary(prestige.all)
table(prestige.all$type, prestige.all$Prof, useNA="ifany")   #check: does the dummy match?

# Convert type to a factor variable with readable labels
# Note: When recoding factor variables, put old and new (!) values in quotations
prestige.all <- prestige.all |>
  mutate(Type = recode(type, "bc"="Blue collar", "wc"="White collar", "prof"="Professional"))
summary(prestige.all$Type)

# Other IVs: recode variables (if necessary)
prestige.all <- prestige.all |>
  mutate(Education = education,
         Income = income/1000,     #income in 1000 dollars: coefficients are easier to read
         Women = women )
summary(prestige.all)

## Keep only the recoded variables for further analysis
# Note: Because 'select' is used by different libraries, it is necessary
#       here to indicate the correct library before the function
prestige.data <- prestige.all |>
  dplyr::select(Prestige:Women)
head(prestige.data)

## Remove occupations with missing values (while keeping a count)
# (but caution with big data sets: might remove too many cases)
N1 <- length(prestige.data$Prestige); N1
prestige.data <- na.omit(prestige.data)
N2 <- length(prestige.data$Prestige); N2
# Calculate/format/show statement with loss of cases
cat("Cases with missing values:", N1-N2, "(", round(((N1-N2)/N1)*100, 1), "% )\n")
rm(N1); rm(N2)
describe(prestige.data)

## Optional: Rescaling IVs into 0-1 range (only for plotting/comparison purposes)
# The general logic:  x - min(x) * ( 1 / (max(x) - min(x) ) )
# Important note: This works only if all NAs have been removed first

prestige.data <- prestige.data |>
  mutate(Education.s = (Education-min(Education)) * (1/(max(Education)-min(Education))),
         Income.s = (Income-min(Income)) * (1/(max(Income)-min(Income))),
         Women.s = (Women-min(Women)) * (1/(max(Women)-min(Women))) )
summary(prestige.data)
describe(prestige.data)


##### 2. Visual Diagnostics / Descriptives #####

# Note: these are done for diagnostic checks and are usually NOT reported!

# violin plot for continuous variables: DV and IVs
vioplot(prestige.data$Prestige, h=5, col="grey", names=c("Prestige"))
vioplot(prestige.data$Education, prestige.data$Income, prestige.data$Women, col="grey",
        names=c("Education","Income","Women"))
# the variables have very different scales, so the rescaled (0-1) versions are easier to compare
vioplot(prestige.data$Education.s, prestige.data$Income.s, prestige.data$Women.s, h=0.05, col="grey",
        names=c("Education","Income","Women"))
# Result: Income is strongly right-skewed (a few very high-income occupations)

# basic scatter plot of DV with 'education' (with linear and lowess regression lines)
plot(prestige.data$Education, prestige.data$Prestige)
abline(lm(Prestige ~ Education, data=prestige.data), col="blue", lwd=4)
lines(loess.smooth(prestige.data$Education, prestige.data$Prestige, span=0.7), col="red", lwd=4)
# Note: with discrete variables or many ties, use 'jittering' to randomly scatter points
#       around the same/shared location to make them 'visible'. Not needed here.
# Result: strong, roughly linear positive relationship

# more sophisticated scatter plots with lowess regression lines and CIs (ggplot2 library)
plot.Education <- ggplot(prestige.data, aes(Education, Prestige)) +
    theme_bw() +
    theme(
       plot.background = element_blank()
      ,panel.border = element_rect(colour="black")
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()  ) +
    geom_point(size=1.5) +
    geom_smooth(method="loess", linewidth=1, alpha = I(0.8)) +
    labs(x="Education (years)", y="Prestige")
plot.Education

plot.Income <- ggplot(prestige.data, aes(Income, Prestige)) +
    theme_bw() +
    theme(
       plot.background = element_blank()
      ,panel.border = element_rect(colour="black")
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()  ) +
    geom_point(size=1.5) +
    geom_smooth(method="loess", linewidth=1, alpha = I(0.8)) +
    labs(x="Income (1000 dollars)", y="Prestige")
plot.Income
# Result: the relationship is NOT linear: prestige rises steeply at low incomes and then
#         flattens out. We keep the linear model for now; Week 3 shows how to detect and fix this

plot.Women <- ggplot(prestige.data, aes(Women, Prestige)) +
    theme_bw() +
    theme(
       plot.background = element_blank()
      ,panel.border = element_rect(colour="black")
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()  ) +
    geom_point(size=1.5) +
    geom_smooth(method="loess", linewidth=1, alpha = I(0.8)) +
    labs(x="Women (%)", y="Prestige")
plot.Women

# Note: Type dummies are mutually exclusive, thus it is more convenient to use the
# factor variable Type for plotting
plot.Type <- ggplot(prestige.data, aes(Type, Prestige)) +
			stat_summary(fun="mean", geom="bar", fill="Grey", color="Black") +
			stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2) +
			labs(x="Occupation type", y="Prestige") +
			theme_bw() +
			theme(plot.background = element_blank()
				,panel.border = element_rect(colour="black")
				,panel.grid.major = element_blank()
				,panel.grid.minor = element_blank()  )
plot.Type
# Result: professional occupations have the highest average prestige, blue collar the lowest


##### 3. Statistical Analysis #####

# Estimating a linear regression model (with original variables & next with rescaled variables)
# Note: blue collar (BC) is the reference category, so it is left out of the model
M1 <- lm(Prestige ~ WC + Prof + Education + Income + Women, data=prestige.data)
summary(M1)
M1.s <- lm(Prestige ~ WC + Prof + Education.s + Income.s + Women.s, data=prestige.data)
summary(M1.s)
# Result: each additional year of education is associated with about 3.7 more prestige points,
#         and each additional 1000 dollars of income with about 1.0 more point (holding the rest constant)
#         Occupation type and Women are not clearly different from zero once education and income are included

# Present results in table format ('texreg' library)
# Note: Alternative libraries are 'stargazer' (see Holbrook 2023) or 'modelsummary' (see Brightspace)
screenreg(M1, digits=3)
screenreg(list(M1, M1.s), digits=3)
# Result: the rescaled model gives coefficients for the full range of each variable (min to max),
#         which makes the effect sizes of the IVs easier to compare; fit (R^2) is identical

# Back to M1: other useful functions and statistics for regression model
display(M1, digits=3, detail=TRUE)  #same regression table in an easier-to-read format
confint(M1)                         #confidence intervals for the coefficients

# Short Detour: same model, but using Type as factor variable instead of the dummies
# Note: R creates the dummies automatically, using the first factor level as reference
summary(lm(Prestige ~ Type + Education + Income + Women, data=prestige.data))
contrasts(prestige.data$Type)   #show (automatic) factor coding
# Note: the reference category here is 'Blue collar' only because it comes first alphabetically.
#       Use relevel() to choose another one:
prestige.data$Type <- relevel(prestige.data$Type, ref="Professional")
summary(lm(Prestige ~ Type + Education + Income + Women, data=prestige.data))
prestige.data$Type <- relevel(prestige.data$Type, ref="Blue collar")   #and back again


##### 4. Visual Display of Model: Coefficient Plot (Part 1 - use existing function) #####

# Visual display of regression coefficients with CIs
# Simple solution: coefplot() function from 'arm' library
coefplot(M1)
coefplot(M1, xlim=c(-10,70), mar=c(1,5,5.1,2), cex.var=1, cex.pts=1, main="Linear Regression Estimates")
coefplot(M1.s, xlim=c(-10,70), add=TRUE, col.pts="red", mar=c(1,5,5.1,2), cex.var=1, cex.pts=1, main="Linear Regression Estimates", offset=0.1)
# Note: coefficients of the original variables (black) and rescaled variables (red) are on different scales


##### 5. Visual Display of Model: Coefficient Plot (Part 2 - create own function) #####

# Note:
# - First step-by-step construction of coefficient plot with comments (using base R)
# - Second, defined as function that can be copied and used (without warranty...)
# - Important detail: Variable names are used as coefficient labels

# Create matrix with coefficients and confidence intervals
M1.coef <- cbind(coef(M1), confint(M1)); M1.coef
# Reverse the variable order and trim (remove intercept) the matrix for plotting
M1.coef <- M1.coef[nrow(M1.coef):2,]; M1.coef
summary(M1.coef)

# Set plot parameters
par(oma=c(0,0,0,0))  #outer margins set to zero
par(mar=c(5,7,2,2))  #inner margins (bottom, left, top, right): left required for coefficient labels
# Draw plot
plot(NULL,                                                  # create an empty plot
     xlim=c(min(M1.coef[,2]),max(M1.coef[,3])),             # length of x axis: smallest lower CI & highest upper CI
     ylim=c(.3, nrow(M1.coef) + .3),                        # length of y axis: number of coefficients plus small buffer margin
     xlab=c("Linear Regression Coefficients with 95% CIs"), # add label to horizontal axis
     ylab=NA,                                               # no label on vertical axis
     axes=FALSE)                                            # draw no axis (yet)
box()                                                       # draw box around plot area
abline(v=0, lty=3)                                          # add vertical line at 0 (zero effect)
axis(1)                                                     # add horizontal axis
axis(2, at=c(1:nrow(M1.coef)), labels=rownames(M1.coef))    # add vertical axis using variable names as point labels
for (i in 1:nrow(M1.coef)) {                                # loop over all coefficients
     points(M1.coef[i,1], i, pch=19, cex=1)                 # add coefficients (points)
     lines(c(M1.coef[i,2], M1.coef[i,3]), c(i, i), lwd=1.5) # add CIs (lines)
}
# Add adj. R^2 and N in lower right corner
Nvalue <- nrow(M1$model)
Rvalue <- round(summary(M1)$adj.r.squared, 2)
text(max(M1.coef[,3]), 0.3, sprintf("Adj. R^2=%.2f, N=%d", Rvalue, Nvalue), pos=2, cex=0.8)

# Optional: dotted horizontal lines (better added to plot before coefficients)
#abline(h=1:nrow(M1.coef), col="Grey", lty = "dotted")

# And once more: Define steps as function for simple coefficient plot (for LM object)
coefpointplot=function(model){
	model.coef <- cbind(coef(model),confint(model))
	model.coef <- model.coef[nrow(model.coef):2,]
	par(oma=c(0,0,0,0))
	par(mar=c(5,7,2,2))
	plot(NULL,
	     xlim=c(min(model.coef[,2]),max(model.coef[,3])),
	     ylim=c(.3, nrow(model.coef) + .3),
	     xlab=c("Linear Regression Coefficients with 95% CIs"),
	     ylab=NA,
	     axes=FALSE)
	box()
	abline(v=0, lty=3)
	axis(1)
	axis(2, at=c(1:nrow(model.coef)), labels=rownames(model.coef))
	for (i in 1:nrow(model.coef)) {
	     points(model.coef[i,1], i, pch=19, cex=1)
	     lines(c(model.coef[i,2], model.coef[i,3]), c(i, i), lwd=1.5)
	}
	Nvalue <- nrow(model$model)
	Rvalue <- round(summary(model)$adj.r.squared, 2)
	text(max(model.coef[,3]), 0.3, sprintf("Adj. R^2=%.2f, N=%d", Rvalue, Nvalue), pos=2, cex=0.8)
}

# Create plot for M1
coefpointplot(M1)
display(M1, digits=3, detail=TRUE)  #display coefficient table again for comparison
coefpointplot(M1.s)

# Restore original plot parameters (if default setting were saved before)
par(opar)


##### 6. Demonstration/Extension: Sample size effects on CIs and robustness check of model #####

# Display estimated model again using coefplot() ('arm' library)
coefplot(M1, xlim=c(-25,75), mar=c(1,5,5.1,2), cex.var=1, cex.pts=1, main="Linear Regression Estimates")

## Re-estimating model with two random subsets of occupations ##
# Note: the samples are random, so results differ slightly every time (use set.seed(123) to fix them)

# Take first random sample (w/out replacement) of size 40 from the dataset prestige.data
prestige.sub1 <- prestige.data[sample(1:nrow(prestige.data), 40, replace=FALSE),]
M2 <- lm(Prestige ~ WC + Prof + Education + Income + Women, data=prestige.sub1)
summary(M2)
coefplot(M2, add=TRUE, col.pts="red", cex.pts=1, offset=0.1)
rm(prestige.sub1)

# Take second random sample (w/out replacement) of size 40 from the dataset prestige.data
prestige.sub2 <- prestige.data[sample(1:nrow(prestige.data), 40, replace=FALSE),]
M3 <- lm(Prestige ~ WC + Prof + Education + Income + Women, data=prestige.sub2)
summary(M3)
coefplot(M3, add=TRUE, col.pts="blue", cex.pts=1, offset=-0.1)
rm(prestige.sub2)
# Result: with only 40 occupations the CIs are much wider and the estimates jump around
#         between samples: smaller samples mean less precise estimates


##### 7. Exercises #####
# (1) Add the variable 'Women' as a squared term or recode it into a dummy (more than 50% women).
#     Does the model fit improve?
# (2) Change the reference category of Type to "White collar" and interpret the new coefficients.
# (3) Re-run the sample size demonstration with 20 and with 80 occupations. What happens to the CIs?
