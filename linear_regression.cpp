#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
 //Input section
  DATA_VECTOR(x);
  DATA_VECTOR(y);
    
  PARAMETER(a); //intercept
  PARAMETER(b); //slope
  PARAMETER(log_sigma); //log std dev of residuals
  
  Type nll = 0; //negative log likelihood
  
  Type sigma = exp(log_sigma); //back transform
  int n = y.size();
  vector<Type> yfit(n); //allocate storage
  
  //Calculation section
  yfit = a + b * x;
  nll = -sum(dnorm(y, yfit, sigma, true));  //vectorized
  
  //Output section
  ADREPORT(sigma); //use this for derived parameters
  
  return nll; //always return the objective function
}
