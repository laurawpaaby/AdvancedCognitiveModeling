
// This model infers a random bias from a sequences of 1s and 0s (right and left hand choices)

// The input (data) for the model. n of trials and the sequence of choices (right as 1, left as 0)
data {
  int<lower=1> n; // Number of trials
  int<lower=0, upper=1> h[n]; // Sequence of choices (right as 1, left as 0)
  int<lower=0, upper=1> wins[n-1]; // Outcome of each trial (1 for win, 0 for loss), starting from the second trial
}

parameters {
  real<lower=0, upper=1> theta[n]; // Probability of choosing right, for each trial, allowing it to change over time
}

model {
  // Initial prior for theta[1]: Uniform distribution as a non-informative prior
  theta[1] ~ uniform(0, 1);
  
  for (i in 2:n) {
    // Update rule for theta: If the previous trial was a win, theta is likely to stay the same;
    // if it was a loss, theta might shift towards the opposite choice
    // This approach tries to capture the essence of WinStay LoseShift 
    // Here we use a beta distribution to update theta based on wins and losses,
    // assuming a win reinforces the current choice (theta[i-1]) and a loss leads to reconsideration (adjusting theta away from the previous choice)
    if (wins[i-1] == 1)
      theta[i] ~ beta(10 * theta[i-1], 10 * (1 - theta[i-1])); // Reinforce choice after win
    else
      theta[i] ~ beta(10 * (1 - theta[i-1]), 10 * theta[i-1]); // Consider opposite choice after loss
    
    // Likelihood for choices, given the updated theta
    h[i] ~ bernoulli(theta[i]);
  }

generative quantaties {
real.....
}

