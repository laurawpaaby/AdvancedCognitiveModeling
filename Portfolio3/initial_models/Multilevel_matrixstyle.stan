
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////         MULTILEVEL REINFORCEMENT LEARNING MODEL            ////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//when used, change priors to all be target sampled:) 

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> trials; // Integer variable with some properties (here we specify the lower bound to be 0 - it cannot be 0 or lower) - if we want continuous, we write 'real' instead of int
  int<lower=0> agents;// when multilevel,we need it on agent base as well
  array[trials, agents] int choice; // NB: calculalte previous choice from this
  vector[trials, agents] feedback; // Input
  vector[2] initialValue; // Expected value before updating (first trial)
}
}

transformed data{
  array[trials,agents] dummy_pos
  array[trials,agents] dummy_neg
  
  for (a in 1:agents){
    for (t in 1:trials){
     if (feedback[t,a]==-1){
       dummy_pos[t,a] = 0;
       dummy_neg[t,a] = 1;
     }
      // now the other way around:
      if (feedback[t,a]==1){
        dummy_pos[t,a] = 1;
        dummy_neg[t,a] = 0;
    }
    }
}
}

// The parameters accepted by the model.
parameters {
  //////////// ALPHA (NOTE THAT WE NOW REMOVE:<lower=0><upper=1>,AND INSTEAD DO LOGIT SCALES)
  real logit_learningRatePos_mu;
  real logit_learningRateNeg_mu;
  
  //real logit_learningRatePos_sigma;
  //real logit_learningRateNeg_sigma;
  
  vector[agents] logit_learningRatePos_ID_Z;
  vector[agents] logit_learningRateNeg_ID_Z;
  
  /////////TAU
  real logInvTemperature_mu;
  //real logInvTemperature_sigma;
  
  vector[agents] logInvTemperature_ID_Z;
  
  
  //// ID MATRIX (AGENTS X NO OF PARAMS) (we have the sigmas here,so wecomment it out above)
  vector<lower=0>[3]sigmas;
  matrix[agents,3] ID_Z;
  
  //correlation checks: 
  cholesky_factor_corr[3] L_u;
  
}

transformed parameters{
  // TRANSFORMING BBACK FROM LGOIT and logods (tau): 
   vector[agents] learningRatePos_ID;
   vector[agents] learningRateNeg_ID;
   vector[agents] InvTemperature_ID
  
   learningRatePos_ID = logit_learningRatePos_ID_Z * logit_learningRatePos_sigma
   learningRateNeg_ID = logit_learningRateNeg_ID_Z * logit_learningRateNeg_sigma
   InvTemperature_ID = logInvTemperature_ID_Z *logInvTemperature_sigma; // We do it in the model instead
   
  // getting matrix for correlations: 
  matrix[agents,3] IDs;
  IDs = (diag_pre_multiply(sigmas, L_u)*ID_z); // considering 
}


// The model to be estimated. In this case, the outcome is a binomial distribution
model {
  
  //// now we are in logit space for alpha and log for tau
  
  vector[2] Value; // This is the vector where we keep the internal values for each hand
  vector[2] rate; // this is the vector where we keep the rates which are converted to probabilities in the softmax
  vector[2] PredError;
  
  // Priors MU
  logit_learningRatePos_mu ~ normal(0,1);  
  logit_learningRateNeg_mu ~ normal(0,1);  
  logInvTemperature_mu ~ normal(0,1);
  
  // Priors SIGMA (truncated by substracting normal_lccdf)
  logit_learningRatePos_sigma ~ normal(sigmas[1],.3) - normal_lccdf(0 | 0,.3); // the minus normal is the  because sigma can only have pos values and only normal dist have long tails,so we truncate it by minus the normal, so no long tails of short differences
  logit_learningRateNeg_sigma ~ normal(sigmas[2],.3) - normal_lccdf(0 | 0,.3); 
  logInvTemperature_sigma ~normal(sigmas[3],.3) - normal_lccdf(0 | 0,.3);
  
  
   // BEACUSE WE DO Z-SCORE THEY SHOULD LOOK LIKE THIS
  //target += normal_lpdf(logit_learningRatePos_ID |0,1)
  //target += normal_lpdf(logit_learningRateNeg_ID |0,1) 
  //target += normal_lpdf(logInvTemperature_ID |0,1) 
  
  // BUT THE ABOVE CAN NOW BE DONE LIKE THIS,CAUSE WE STORED THE MATRIX:
  target += lkj_corr_cholesky_lpdf(ID_z)
  target += std_normal_lpdf(ID_z)
  
  // priors ID based on priors and sigma: 
  //target += normal_lpdf(logit_learningRatePos_ID |logit_learningRatePos_mu,logit_learningRatePos_sigma)
  //target += normal_lpdf(logit_learningRateNeg_ID |logit_learningRateNeg_mu,logit_learningRateNeg_sigma) 
  //target += normal_lpdf(logInvTemperature_ID |logInvTemperature_mu,logInvTemperature_sigma) 
  
 
  

  
  for (agent in 1:agents){
    
    Value = initialValue;
    
    
   for (t in 1:trials){
    
    // the + of mu and ID log inv tau is to z-scoring them
    rate = softmax(logInvTemperature_mu + logInvTemperature_ID[agent]*to_vector(Value));
    target += categorical_lpmf(choice[t,agent]|rate); // A bernoulli which takes a vector of rates and rescales them
    
    predError = feedback[t,agent] - to_vector(Value);
    Value = Value + inv_logit(logit_learningRateNeg_mu + IDs[agent,1] * predError*dummy_neg[t]) + inv_logit(logit_learningRatePos_mu + IDs[agent,1]*predError*dummy_pos[t]);

    
    }
  }
  
}

generated quantities{
  array[trials] real log_lik;
  array[trials] real expValuePrior;
  array[trials] real expValuePost;
  
  real<lower=0><upper=1>learningRatePos_prior;
  real<lower=0><upper=1>learningRateNeg_prior;
  
  real loginvTemperature_prior;
  real invTemperature_prior;
    
  real rate_prior;
  real rate_post; 
  
  real predError_prior;
  real predError_post; 
  
  
  learningRatePos_prior ~ beta(2,2);  
  learningRateNeg_prior ~ beta(2,2);  
  loginvTemperature_prior ~ normal(0,1);
  invTemperature_prior = exp(loginvTemperature_prior)
  
  //setting first trial exp val:
  expValuePrior[1] = InitalValues
  expValuePost[1] = InitalValues

  
  // mirroring the model:
  for (t in 1:trials){
    
    rate_prior = softmax(invTemperature_prior*expValuePrior);
    rate_post = softmax(invTemperature*expValuePost);
     
    log_lik[t] = categorical_lpmf(choice[t]|rate);  // we only care for post log like
    
    predError_prior = feedback[t] - expValuePrior;
    predError_post = feedback[t] - expValuePost;
    
    expValuePrior[t+1] = expValuePrior[t] + learningRateNeg * predError_prior*dummy_neg[t] + learningRatePos*predError_prior*dummy_pos[t];
    expValuePost[t+1] = expValuePost[t] + learningRateNeg * predError_post*dummy_neg[t] + learningRatePos*predError_post*dummy_pos[t];
    
    
  }
  
}

