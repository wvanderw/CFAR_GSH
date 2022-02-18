//
// This Stan program defines 'Stan model 1 -- Log10CFAR ~ Log10MeanGSH_centered + Log10MaxSize_Centered
//

// The input data are vectors 'x' & 'y' of length 'N'.
data {
  int <lower=1> N;
  vector[N] x;
  vector[N] y;
  vector[N] z;
  
}

// The parameters accepted by the model. 
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  y ~ normal(alpha + x * beta , sigma);
}

// need to redefine to keep for comaprison
generated quantities {
  
}
