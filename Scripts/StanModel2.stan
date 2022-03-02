//
// This Stan program defines '## Stan Model 2 -- 
// Log10CFAR ~ Log10MeanGSH_centered * Log10MaxSize_centered
//

// The input data are vectors 'x' & 'y' of length 'N'.
data {
  int <lower=0> N; // number of data points
  int <lower=0> K; // number of predictors 
  matrix[N,K] x; // predictor matrix
  vector[N] y; // outcome (CFAR)
 
  
}

// The parameters accepted by the model. 
parameters {
  real alpha;
  vector[K] beta;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  sigma ~ student_t(3, 0, 10);
  
  y ~ normal(alpha + x * beta , sigma);
}

// need to redefine to keep for comaprison
generated quantities {
  
}
