//
// This Stan program defines '## Stan Model 3 -- 
// Log10CFAR ~ Log10MeanGSH_centered + phylo
//

data {
  int <lower=0> N; // number of data points
  int <lower=0> K; // number of predictors 
  matrix[N,K] x; // predictor matrix
  vector[N] y; // outcome (CFAR)
  matrix[N, N] d_mat; // sigma matrix
  matrix[N, N] A; // vcov matrix
      }

// The parameters accepted by the model. 

      parameters {
        vector[K] beta; // coefficients
        real<lower=0> sigma; // error
        real<lower=0,upper=1> lambda; // phylogenetic signal
      }
      
      
      transformed parameters {
        
        matrix[N, N] sigma_mat;
        matrix[N, N] sigma_total;
        
        

        sigma_mat = (1-lambda)*d_mat + lambda*A;
        sigma_total = sigma*sigma_mat;
        
        

      }

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  beta ~ student_t(3, 0, 10);
  lambda ~ uniform(0,1);
  sigma ~ student_t(3, 0, 2.5);
  
  y ~ multi_normal(x * beta, sigma_total);
}

// need to redefine to keep for comaprison
generated quantities {
 
        real log_lik;
        
        log_lik = multi_normal_lpdf(y | x * beta, sigma_total);
      
}
