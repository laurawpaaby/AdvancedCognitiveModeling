
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> trials;
  //vector[trials] choiceREL; 
  array[trials] int<lower=1,upper=2> choiceREL;
  //vector[trials] feedback; 
  array[trials] int<lower=0,upper=1> feedback;
  vector[2] initialValue; //the initial values for choosing left and rigth 
}

// The parameters accepted by the model. Our model
// accepts two parameters 'alpha' = learning rate and 'tau' = temperature.
parameters {
  real<lower=0, upper=1>alpha;
  real<lower=0>logTau;
}

transformed parameters {
  real<lower=0, upper=1>tau;
  tau = exp(logTau); // transforming the parameter 
}


// The model:
model {
  vector[2] Values;
  vector[2] probabilites;

  
 // Prior for logTau, assuming it's normally distributed around 0 with a standard deviation of 1 I HAVE NO FREAKING CLUE IF THIS IS A GOOD IDEA???
  target += uniform_lpdf(logTau | 0, 20); 
  //tau ~ normal(0,.5);  
  
  // Prior for alpha, assuming a beta distribution which is appropriate for parameters bounded between 0 and 1
  target += beta_lpdf(alpha | 2, 1.5); 
  //alpha ~ beta(2,1.5);
  
  
// Initializing the Values vector with the initial values provided in the data
  Values = initialValue;
  
  
  for (t in 1:trials) {
    real pred_error; // Variable to store the prediction error, declaration moved here for clarity
    
    /// for the rest of the trials 
      // Compute probabilities for the next choice by applying the softmax function to the tau-scaled Values
    probabilites = softmax(logTau * Values);
    target += categorical_lpmf(choiceREL[t] | probabilites); 
      
      // Calculate prediction error based on the choice made in the previous trial
    pred_error = feedback[t] - Values[choiceREL[t]];
      
      // Update the value for the chosen option based on the prediction error
    Values[choiceREL[t]] = Values[choiceREL[t]] + alpha * pred_error; // plus 1 cause choice 1 and 2 then
      
      
      // EVT +1 PÅ CHOICES[T] I PRED OG VAL UPDATES, KOMMER AN PÅ HVORDAN DER INDEXES
      
    
    
  }
}


//VI SKAL FITTE PÅ CHOICE_REL HER !!! DET ER CONNECTION 


// the     
generated quantities{
  // specifying the priors we want to assess: 
  real alpha_prior;
  real tau_prior; 
  
  // FOR ALPHA::::
  int<lower=0, upper=1> alpha_prior_preds03;
  int<lower=0, upper=1> alpha_prior_preds07;
  int<lower=0, upper=1> alpha_prior_preds09;
  
  int<lower=0, upper=1> alpha_post_preds03;
  int<lower=0, upper=1> alpha_post_preds07;
  int<lower=0, upper=1> alpha_post_preds09;
  
  
  // computing the prior and posterior prediction values to explore the nature of the different priors:
  // it is sampling from a binomial distribution that makes it into a choice of either 0 or 1. 
  alpha_prior = beta_rng(.5,.5);
  alpha_prior_preds03 = binomial_rng(1,inv_logit(alpha_prior*logit(.3))); // remember that inv_logit(alpha_prior) = the pior 
  alpha_prior_preds07 = binomial_rng(1,inv_logit(alpha_prior*logit(.7)));
  alpha_prior_preds09 = binomial_rng(1,inv_logit(alpha_prior*logit(.9)));
  
  alpha_post_preds03 = binomial_rng(1, inv_logit(alpha*logit(.3))); // remember that inv_logit(alpha) = the posterior 
  alpha_post_preds07 = binomial_rng(1, inv_logit(alpha*logit(.7)));
  alpha_post_preds09 = binomial_rng(1, inv_logit(alpha*logit(.9)));
  
  
   // FOR TAU::::
  int<lower=0, upper=1> tau_prior_preds03;
  int<lower=0, upper=1> tau_prior_preds07;
  int<lower=0, upper=1> tau_prior_preds09;
  
  int<lower=0, upper=1> tau_post_preds03;
  int<lower=0, upper=1> tau_post_preds07;
  int<lower=0, upper=1> tau_post_preds09;
  
  
  // computing the prior and posterior prediction values to explore the nature of the different priors:
  // it is sampling from a binomial distribution that makes it into a choice of either 0 or 1. 
  tau_prior = normal_rng(.5,.5);
  tau_prior_preds03 = binomial_rng(1,inv_logit(alpha_prior*logit(.3))); // remember that inv_logit(alpha_prior) = the pior 
  tau_prior_preds07 = binomial_rng(1,inv_logit(alpha_prior*logit(.7)));
  tau_prior_preds09 = binomial_rng(1,inv_logit(alpha_prior*logit(.9)));
  
  tau_post_preds03 = binomial_rng(1, inv_logit(alpha*logit(.3))); // remember that inv_logit(alpha) = the posterior 
  tau_post_preds07 = binomial_rng(1, inv_logit(alpha*logit(.7)));
  tau_post_preds09 = binomial_rng(1, inv_logit(alpha*logit(.9)));
  
  
  
}









//// THE OLD TAKE: 
// Choice = 1// specyfying the choices of the first trial 
  
  // for (t in 2:trials){
    // pred_error = feedback[t-1]-Values; 
    
    // updating the value for left and right hand: 
//     Values[1] = Values[1]+alpha*(1-choiceREL[t-1])*pred_error;
  //   Values[2] = Values[2]+alpha*(choiceREL[t-1])*pred_error;
    
   //  probabilites = softmax(tau*Values); // computing probabilities by softmaxing the values
//     choice ~ categorical(probabilities);
    
   //  Values = initialValue; // Set the initial values for left and right from the provided data;