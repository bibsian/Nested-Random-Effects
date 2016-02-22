# Nested-Random-Effects
This is an R script that performs roughly three functions. Note,
it is assumed you already know what generalized linear mixed models are,
as well as have an understanding of what random effects are.

This scripts does:

1) Show you how to simulated data with nested random effects

2) How to model those data with lmer (glmm)

3) Looks at how models perform with difference variance strucutres (i.e. no randome effects,
a single random effect, and nested random effects.)

4) Looks at these results with ggplot2

- to do
add how you can recover these paraemeter estimates with
bayes (either using jags or stan) and compare with maximum likelihood (or REML)
