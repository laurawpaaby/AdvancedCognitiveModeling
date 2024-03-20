### implement this: https://fusaroli.github.io/AdvancedCognitiveModeling2023/practical-exercise-4---model-quality-checks.html 
### DE NØDVENDIGE PAKKER BURDE LIGGE I RENV, ELLERS SKAL DE P_LOADES FRA EN MARKDOWN CHUNK 

stan_model <- "
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
  

generative quantaties {
real.....
}
"
write_stan_file(
  stan_model,
  dir = "Stan_Models/",
  basename = "W3_SimpleBernoulli.stan")
