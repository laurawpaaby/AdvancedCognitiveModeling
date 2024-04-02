/////////////////////////////////////
// Stan model for Portfolio 3////////
////////////////////////////////////


data {
  int<lower=1> trials;  
  array[trials] int<lower=1, upper=8> first_rating; // Observed initial ratings by participants
  array[trials] int<lower=1, upper=8> group_rating; // Observed group ratings
  array[trials] int<lower=1, upper=8> second_rating; // Observed follow-up ratings by participants
}

transformed data {
  // Scaled ratings to be within 0-1 for compatibility with logit and beta functions
  vector[trials] first_rating_scaled;
  vector[trials] group_rating_scaled;
  vector[trials] second_rating_scaled;
  
  for (i in 1:trials) {
    first_rating_scaled[i] = (first_rating[i] + 1) / 10.0;
    group_rating_scaled[i] = (group_rating[i] + 1) / 10.0;
    second_rating_scaled[i] = ((second_rating[i] + 1) / 10.0) - 1; // lav om til 0,00001 til 6,999999 (rykker bounds)
  }
}

parameters {
  real<lower=0, upper=1> bias; // Participant's bias
}


model {
  // Define a uniform prior for bias
  bias ~ beta(2, 2); // Equivalent to a uniform distribution due to the shape parameters
  
  // Declare updated_belief as a vector to hold the updated beliefs for each trial
  vector[trials] updated_belief;
  
  // Looping over each trial to model the updated belief and its effect on the second rating
  for (t in 1:trials) {
    real temp_belief = bias + logit(first_rating_scaled[t]) + logit(group_rating_scaled[t]);    // Temporarily calculate the updated belief based on the bias and scaled ratings
    updated_belief[t] = inv_logit(temp_belief);   // Store the inv_logit-transformed belief in the updated_belief vector
    
    // Model the second rating based on the updated belief
    target += binomial_lpmf(second_rating[t]-1 | 7, updated_belief[t]); // Likelihood of the second rating, where 7 equals upper_bound - lower_bound
  }
}

generated quantities {
  // Vector to store the log likelihood for each observation, necessary for model comparison
  vector[trials] log_lik;
  
  for (t in 1:trials) {
    real belief = bias + logit(first_rating_scaled[t]) + logit(group_rating_scaled[t]);
    belief = inv_logit(belief); 
    log_lik[t] = binomial_lpmf(second_rating[t]-1 | 7, belief);  //this is not scaled 
  }
}

