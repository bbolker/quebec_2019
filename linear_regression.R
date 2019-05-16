library(TMB)

#Input data
x = -5:6
y = c(8.8, 8.7, 9.4, 10.0, 8.8, 9.7, 10.7, 10.6, 9.6, 11.4, 10.7, 11.1)

#Organize data for TMB
dat = list(x=x, y=y)

#Set initial values for parameters to estimate
par = list(a=8, b=1, log_sigma=0)

#Compile the model
compile("linear_regression.cpp")
dyn.load(dynlib("linear_regression"))
  
#Combine data, parameters, and model to make objective function (i.e. nll)
obj = MakeADFun(dat, par, DLL="linear_regression")

#Optimize the objective function (i.e. minimize the nll)
opt = nlminb(obj$par, obj$fn, obj$gr)

#Output results
summary(sdreport(obj))








