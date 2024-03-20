
// The input data is a vector 'y' of length 'N'.
data {
   int<lower=0> trials; // Integer variable with some properties (here we specify the lower bound to be 0 - it cannot be 0 or lower) - if we want continuous, we write 'real' instead of int
  array[trials] int choice; // NB: calculalte previous choice from this
  // vector[trials] prevChoice;  We don't need this here
  vector[trials] feedback; // Input
  vector[2] initialValue; // Expected value before updating (first trial)
}
}

transformed data{
  vector[trials] dummy_pos
  vector[trials] dummy_neg
  
  for (t in 1:trials){
    if (feedback[t]==-1)
      dummy_pos[t] = 0;
      dummy_neg[t] = 1;
      // now the other way around:
    if (feedback[t]==1)
      dummy_pos[t] = 1;
      dummy_neg[t] = 0;
  }
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real logInvTemperature;
  real<lower=0><upper=1>learningRatePos;
  real<lower=0><upper=1>learningRateNeg;
}

transformed parameters{
  real<lower=0.001> invTemperature; // Inverse temperature
  invTemperature = exp(logInvTemperature); // We transform the parameter to ensure a boundary at 0
}


// The model to be estimated. In this case, the outcome is a binomial distribution
model {
  
  vector[2] Value; // This is the vector where we keep the internal values for each hand
  vector[2] rate; // this is the vector where we keep the rates which are converted to probabilities in the softmax
  vector[2] PredError;
  
  // Priors
  learningRatePos ~ beta(2,2);  // could also use a logit tranform of a normal distribution
  learningRateneg ~ beta(2,2);  // could also use a logit tranform of a normal distribution
  loginvTemperature ~ normal(0,1);
  
  Value = initialValue;
  
  for (t in 1:trials){
    
    rate = softmax(invTemperature*Value);
    choice[t] ~ categorical(rate); // A bernoulli which takes a vector of rates and rescales them
    
    predError = feedback - Value;
    Value = Value + learningRateNeg * predError*dummy_neg[t] + learningRatePos*predError*dummy_pos[t];
    
    // another solution (no feedback dummy):
    //if (predError < 0)
      //Value = Value + learningRateNeg * predError;
    //if (predError > 0)
      //Value = Value + learningRatePos * PredError;
    
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

