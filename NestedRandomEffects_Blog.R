# Created by Andrew Bibian
# 02/18/16
set.seed(100)

# Install any required packages that are not currently installed 
#-----------------------------------------
# List required packages
adm.req <-c("ggplot2", "lme4", "plyr", "grid", "gridExtra")

# Load currently installed, required packages
tmp <- lapply(adm.req, require, character.only = T)

# Find the required packages that still need to be installed
adm.need <- adm.req[!(paste0("package:",adm.req) %in% search())]

# Install required packages that are not currently installed
if(length(adm.need)>0){ install.packages(adm.need,dependencies=T) }

# Now, make sure all packages are loaded
tmp <- lapply(adm.req, require, character.only = T)

tmp

setwd("/Users/bibsian/Dropbox/GraphiteAnalytics/Blog")
#setwd("C:/Users/MillerLab/Dropbox/GraphiteAnalytics/Blog")

source("FxnNestREBlog.R")

# Designation groups sizes 
# and individuals within groups
#--------------------------
# Number of unique populations
n.pop = 5

# Number of samples from one unique individual within each population
n.rep_ind = 10

# Number of unique individuals sampled within each population
n.ind = 20

# Populaiton level variance and noise
#-----------------------------------
# variance among populations
pop.var= 1.5

# Random noise to be added to observations
# from each popultion. There are ten different 
# populations and every observation from that
# population will have this noise added to it
pop.noise = rnorm(n.pop, 0, pop.var)

# Individual:Population level variance and noise
#------------------------------------------------
# Variance among individuals with each population 
# Note, there are 10 variances that we're assigning here.
# Each variance will add noise for all individuals 
# sampled with a population
ind_pop.var = runif(n.pop, 2, 7)


# This noise is little more complicated to generate.
# First, lets think about what we need. For every replicate
# sample from 1 individual, we want to add the same noise
# to each replicate sample via
# the variance that corresponds to said individual
# Hence, we should take our 40 variances above, and for each
# of them, genearte some noise for the number individuals we 
# sample in each population. 
ind_pop.noise = lapply(ind_pop.var, function(x) rnorm(n.ind, 0, x))

# If you look at the list we generated each element
# has 4 values. Each value is the noise that will be added
# to replicate samples from an individual with a population 
# (elements of the list are populations)

# Collapsing list into a vector
ind_pop.noise = do.call('c', ind_pop.noise)

# Creating regression exquation (predictor variable and slope)
#--------------------------------------------
# Slope for predictor variable
beta = 0.15

# Predictor variable numbers
x = as.numeric(runif(n.pop*n.ind*n.rep_ind, 0, 100))


# Creating data labels and dataframe 
#------------------------------------
# Population Labels
pop = rep(letters[1:n.pop], each = n.ind*n.rep_ind)

# Individuals within population id's
ind_id = rep(rep(1:n.ind, each = n.rep_ind), n.pop)

# Population/individual id's
pop_ind_id = paste0(pop,"_" ,ind_id)

# Combining into dataframe
d <- as.data.frame(cbind(pop,ind_id,pop_ind_id))
d$x <- x


# Generating model matrices
#-------------------------
# This is our model matrix that holds the predictor 
# information. We surpressed the intercept ('-1')
MM <- as.matrix(model.matrix(~x-1, data=d))

# Model matrix for the random effect of population
MM_REpop <- as.matrix(model.matrix(~ pop -1, data=d))

# Model matrix for the random effect of individuals within populations
MM_REind_pop <- as.matrix(model.matrix(~ pop:ind_id-1, data=d))

# Setting observation level variance and generating data
#---------------------------------------------------
# Setting observatin level variance
obs.var = 0.75

# Calculating the mean of the 'response' distribution (which is a normal)
# and factoring in population and individual random effect noise
y_mean = MM %*% as.matrix(beta) + MM_REpop %*% 
  as.matrix(pop.noise) + MM_REind_pop %*% as.matrix(ind_pop.noise)

# Generating response data given the mean
# above and the observation level variance
y = rnorm(nrow(d), y_mean, obs.var)

# Adding response to dataframe
d$y <- y

#Setting colors for the plots we will make
#----------------------------------------
# Lets start with a base color...'blue'
basecolor = "blue1"

# Were going to find out where that lives in 
# R's color list
color_startsequence= which(colors()==basecolor)

# It you look at the color chart for R,
# http://research.stowers-institute.org/efg/R/Color/Chart/
# starting with blue (26), going every 5 numbers over
# seems to get you a different shade so lets make that
# sequence to index the colors

# Creating an empty vector to fill with indeces of colors
color_pop= rep(NA, n.pop)

# Starting the vector 
color_pop[1]= color_startsequence

# Rolling over the vector and generating
# our sequence
for(i in 2:n.pop) {
  color_pop[i]<- color_pop[i-1] + 5
}

# Now turning our colors into hexadecimal form
c_pop_final <- apply(
  t(col2rgb(colors()[color_pop])), 1, function(x) {
    rgb(x[3], x[2], x[1], maxColorValue=255)})


# Inspecting the data
#-------------------
# Plotting the response against the predictor.
# Theres an obvious relationship here because 
# thats how we simulated the data. For every increase
# in x there was a beta (=0.2) increase in our response (y)
base<- ggplot(data=d, aes(x,y, color=pop))+geom_point(size=4)+
  scale_color_manual(values = c(c_pop_final))+
  ggtitle("Response Vs Predictor")
base



# Model 1: No Random Effects i.e. No Poolings
#-------------------------------------------
# Fitting a model with no random effects 
# is as easy as using the lm() function.
m1 <- lm(y~x, data=d)
summary(m1)
# Lets add the residuals to our dataframe
d$m1resid <- resid(m1)


# Model 1: Plots of fit and residuals
#------------------------------------
# Plugging our model output into our m.line function
# to throw on top of our base plot
m1_fit<-base+ 
  stat_function(
    fun= function(x){m1$coef['(Intercept)'] + (m1$coef['x'])*x}, color='black')+
  ggtitle("Response Vs Predictor - No Random Effects")
m1_fit

# M1 Residuals vs Index
#-------------------
# Plotting the residuals by themselves
m1_resid<- fxn_plot_resid(d$m1resid)+ggtitle("M1 Residuals")
m1_resid
# What do you notice?

# M1 Residuals vs X
#------------------
# Now plotting the residuals against the predictor variable
m1_resid_x<- fxn_plot_resid_x(d$m1resid)+ggtitle("M1 Residuals Vs Predictors")
m1_resid_x
# How do you think this fits?

# Model 2: Random Effect of Population
#----------------------------
# Lets look at how we do with a population random effect
m2 <- lmer(y ~ x + (1|pop), data=d)
summary(m2)
# Lets take a look at those residuals now
# Here were going to add them to our original dataset
d$m2resid <- resid(m2)


# Plot the model fits
#-------------------
m2_fit<- base + 
  multi_int(ranef(m2)$pop, fixef(m2)) + 
  ggtitle("Response Vs Predictors- Random Effect of Population")
m2_fit

# M2 Residuals vs Index
#-------------------
# Plotting the residuals by themselves
m2_resid<- fxn_plot_resid(d$m2resid)+ggtitle("M2 Residuals")
m2_resid
# What do you notice?

# M2 Residuals vs X
#------------------
# Now plotting the residuals against the predictor variable
m2_resid_x<- fxn_plot_resid_x(d$m2resid)+ggtitle("M2 Residuals Vs Predictors")
m2_resid_x
# How do you think this fits?

# PLOT Comparisons
#---------------
grid.newpage()
top.vp<- viewport(layout=grid.layout(nrow=2, ncol= 3))

p1_fit <-  viewport(layout.pos.col = 1, layout.pos.row = 1, name ="m1_fit")
p1_resid <- viewport(layout.pos.col = 2, layout.pos.row = 1, name ="m1_resid")
p1_resid_x <- viewport(layout.pos.col = 3, layout.pos.row = 1, name = "m1_resid_x")

p2_fit <- viewport(layout.pos.col = 1, layout.pos.row = 2, name ="m2_fit")
p2_resid <- viewport(layout.pos.col = 2, layout.pos.row = 2, name ="m2_resid")
p2_resid_x <- viewport(layout.pos.col = 3, layout.pos.row = 2, name ="m2_resid_x")

m1.2plot<- vpTree(
  top.vp, vpList(
    p1_fit, p1_resid, p1_resid_x, p2_fit, p2_resid, p2_resid_x))
pushViewport(m1.2plot)

print(m1_fit, vp = "m1_fit")
print(m1_resid, vp= "m1_resid")
print(m1_resid_x, vp= "m1_resid_x")
print(m2_fit, vp = "m2_fit")
print(m2_resid, vp= "m2_resid")
print(m2_resid_x, vp= "m2_resid_x")


# Model 3: Random effect of population and 
# Individuals within populations
#---------------------------------------
m3<- lmer(y ~ x + (1|pop/ind_id), data=d)
summary(m3)
d$m3resid<-resid(m3)

cbind(ind_pop.noise, ranef(m3)$'ind_id:pop')

        

# M3 Residuals vs Index
#-------------------
# Plotting the residuals by themselves
m3_resid<- fxn_plot_resid(d$m3resid)+ggtitle("M3 Residuals")
m3_resid
# What do you notice?

# M3 Residuals vs X
#------------------
# Now plotting the residuals against the predictor variable
m3_resid_x<- fxn_plot_resid_x(d$m3resid)+ggtitle("M3 Residuals Vs Predictors")
m3_resid_x
# How do you think this fits?

# PLOT Comparisons
#---------------
grid.newpage()
newtop.vp<- viewport(layout=grid.layout(nrow=3, ncol= 3))

p3_fit <- viewport(layout.pos.col = 1, layout.pos.row = 3, name ="m3_fit")
p3_resid <- viewport(layout.pos.col = 2, layout.pos.row = 3, name ="m3_resid")
p3_resid_x <- viewport(layout.pos.col = 3, layout.pos.row = 3, name ="m3_resid_x")


mallplot<- vpTree(
  newtop.vp, vpList(
    p1_fit, p1_resid, p1_resid_x, p2_fit, p2_resid, p2_resid_x,
    p3_fit, p3_resid, p3_resid_x))
pushViewport(mallplot)

print(m1_fit, vp = "m1_fit")
print(m1_resid, vp= "m1_resid")
print(m1_resid_x, vp= "m1_resid_x")
print(m2_fit, vp = "m2_fit")
print(m2_resid, vp= "m2_resid")
print(m2_resid_x, vp= "m2_resid_x")
print(m3_resid, vp= "m3_resid")
print(m3_resid_x, vp= "m3_resid_x")

AIC(m1,m2,m3)

m_coef_list <- lapply(list(m1,m2,m3), function(x) {coef(summary(x))})

