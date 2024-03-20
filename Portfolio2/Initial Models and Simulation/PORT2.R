### implement this: https://fusaroli.github.io/AdvancedCognitiveModeling2023/practical-exercise-4---model-quality-checks.html 
## this script should run the stan model and do the following assignment: 

# 1. Describe the model you are working on (N.B. it doesn't need to be one from Assignment 1)
# 2. showcase a commented version of the stan model (what does each line do?)
# 3. discuss model quality of the model (predictive prior and posterior checks, prior-posterior update checks, etc.)
# 4. describe a process of parameter recovery (why are you doing it?, how are you doing it?)
# 5. discuss the results: can you recover the parameter values? How many trials should be used at least to properly recover the parameters? What’s the role of priors? Add relevant plot(s).

### This means that we are to do the following: 

# - Simulate data of matching pennies game agents - plot the behavior 
#     - what strategy could be interesting to implement? Model the behavior based on that 
#     - wsls, REL, tom, noisy bias ???? 
# - create a stan model that theoretically captures the simulated data 
#     - plot the predictive priors of the model (what does these mean)
#     - prior-posterior update checks <- plot and discuss (not sure these are plotable)
#     - plot the posterior predictive checks 
# - fit the stan model to the data - see how well our parameter recovers 
#     - do it for different numbers of trials and potentially agents 


#################### SIMULATE DATA #################### 



#####------------- soft max function-------------###### 
### first we make the softmax function, that converts values into probabilities.
### it takes tau, which is inverse heat and the value x. In this case it is negative, as it resembles temperature (exploitation vs. exploration)
softmax_func <- function(x,tau){
  outcome = 1/(1+exp(-tau*x)) 
  prob = outcome/sum(outcome)
  return(prob)
}


##### another take LETS USE THIS!!
softmax_1 <- function(x, tau) { # x is the expected value for each choice, tau is the temperature parameter, the higher the temperature, the more random the choice (more exploration)
  exp_values <- exp(x / (1/tau))
  prob <- exp_values / sum(exp_values)
  return(prob)
}




#####------------- updating value function: -------------###### 
Value_update_func <- function(value, # the baseline preference for left or right (between 0-1) - this updates as we play - the higher the value the smaller will the update be as the prediction error decreases
                              alpha, # the learning rate (between 0-1)
                              choice, # the choice of left (0) or right (1) hand
                              feedback){ 
  #predicted_error <- feedback - value 
  
  #### updating the preference for choosing each of the hand
  #####(if left hand is chosen, then choice = 0, and 1 is multiplied in the left_v1, in this case the choice in right_v2 value would be 0, and the value isn't updated. If it is the other way around, and we choose right (choice=1), the left_v1 value isn't updated, but right_v2 is.)
  left_v1 <- value[1]+alpha*(1-choice)*(feedback-value[1]) # where (feedback-value[1]) is predicted error for choosing left. if the feedback is smaller than the value, the preference for picking this hand in the next round and vise versa if positive.  
  right_v2 <- value[2]+alpha*(choice)*(feedback-value[2]) 

  ## storing the updated values for right and left hand
  updated_Value <- c(left_v1, right_v2)
  return(updated_Value)
  
}




#####------------- Setting up agents:: -------------###### 
# the player the REL agent plays against (this is the hider): 
random_agent_function <- function(rate){
  choice <- rbinom(1, 1, rate)
  return(choice)
}


# The REL agent: (this is the matcher picking a hand)
REL_agent_function <- function(tau, value, alpha, choice, feedback){
  #### updating value: 
  up_val <- Value_update_func(value, alpha, choice, feedback)
  
  #### softmaxing on the updated values: 
  prob_new_choice <- softmax_func(up_val, tau)
  
  ### computing the new choice in a probabilistic fashion based on the probability found in the softmax  
  new_choice  <- rbinom(1, 1, prob_new_choice[2])
  
  REL_play_ouctome <- tibble(new_choice, "value_left"=value[1],"value_right"=value[2], feedback)
  return(REL_play_ouctome)
  
}



#####------------- Let the agents play against one another: -------------###### 
trials = 100

df_results <- tibble(trial = rep(NA, trials),
            agent_bias_choice = rep(NA, trials),
            agent_REL_choice = rep(NA, trials), 
            left_v1 = rep(NA, trials), 
            right_v2 = rep(NA, trials), 
            feedback = rep(NA, trials))

# initiation of agents
df_results$agent_bias_choice[1] <- random_agent_function(.5)
df_results$agent_REL_choice[1] <- rbinom(1,1,.5)

# initiation of feedback and values:
df_results$feedback[1] <- ifelse(df_results$agent_bias_choice[1] == df_results$agent_REL_choice[1], 1, 0)

# we assume for the player to be completely unbiased in the initial choice between left and right: 
df_results$left_v1[1] <- .5
df_results$right_v2[1] <- .5

df_results$trial[1] <- 1


########## defining parameters: 
tau_param <- .5 
alpha_param <- .8
rate_param <- .7


####### now we move from the first round and let them play by the functions:
for (i in 2:trials){
  
  ### the feedback is given the previous trial:
  feedback <- ifelse(df_results$agent_bias_choice[i-1] == df_results$agent_REL_choice[i-1], 1, 0) 

  agent_bias <- random_agent_function(rate = rate_param)
  agent_REL <- REL_agent_function(tau=tau_param,
                                  value=c(df_results$left_v1[i-1], df_results$right_v2[i-1]),
                                  alpha=alpha_param,
                                  choice=df_results$agent_REL_choice[i-1],
                                  feedback=feedback) 
  print(agent_REL)
  
  ### saving resutls: 
  df_results$trial[i] <- i
  df_results$agent_bias_choice[i] <- agent_bias
  df_results$agent_REL_choice[i] <- agent_REL$new_choice
  df_results$left_v1[i] <- agent_REL$value_left
  df_results$right_v2[i] <- agent_REL$value_right
  df_results$feedback[i] <- agent_REL$feedback
}





# The REL agent: (this is the matcher picking a hand)
REL_agent_function <- function(tau, value, alpha, choice, feedback){
  #### updating value: 
  up_val <- Value_update_func(value, alpha, choice, feedback)
  
  #### softmaxing on the updated values: 
  prob_new_choice <- softmax_func(up_val, tau)
  
  ### computing the new choice in a probabilistic fashion based on the probability found in the softmax  
  new_choice  <- rbinom(1, 1, prob_new_choice[2])
  
  REL_play_ouctome <- c(new_choice, up_val)
  return(REL_play_ouctome)
  
}



#####------------- Let the agents play against one another: -------------###### 
trials = 100

df_results <- tibble(trial = rep(NA, trials),
                     agent_bias_choice = rep(NA, trials),
                     agent_REL_choice = rep(NA, trials), 
                     left_v1 = rep(NA, trials), 
                     right_v2 = rep(NA, trials), 
                     feedback = rep(NA, trials))

# initiation of agents
df_results$agent_bias_choice[1] <- random_agent_function(.5)
df_results$agent_REL_choice[1] <- rbinom(1,1,.5)

# initiation of feedback and values:
df_results$feedback[1] <- ifelse(df_results$agent_bias_choice[1] == df_results$agent_REL_choice[1], 1, 0)

# we assume for the player to be completely unbiased in the initial choice between left and right: 
df_results$left_v1[1] <- .5
df_results$right_v2[1] <- .5

df_results$trial[1] <- 1


########## defining parameters: 
tau_param <- .5 
alpha_param <- .8
rate_param <- .7


####### now we move from the first round and let them play by the functions:
for (i in 2:trials){
  
  ### the feedback is given the previous trial:
  feedback <- ifelse(df_results$agent_bias_choice[i-1] == df_results$agent_REL_choice[i-1], 1, 0) 
  
  agent_bias <- random_agent_function(rate = rate_param)
  agent_REL <- REL_agent_function(tau=tau_param,
                                  value=c(df_results$left_v1[i-1], df_results$right_v2[i-1]),
                                  alpha=alpha_param,
                                  choice=df_results$agent_REL_choice[i-1],
                                  feedback=feedback) 
  print(agent_REL)
  
  ### saving resutls: 
 

# The REL agent: (this is the matcher picking a hand)
REL_agent_function <- function(tau, value, alpha, choice, feedback){
  #### updating value: 
  up_val <- Value_update_func(value, alpha, choice, feedback)
  
  #### softmaxing on the updated values: 
  prob_new_choice <- softmax_func(up_val, tau)
  
  ### computing the new choice in a probabilistic fashion based on the probability found in the softmax  
  new_choice  <- rbinom(1, 1, prob_new_choice[2])
  
  REL_play_ouctome <- tibble(new_choice, "value_left"=value[1],"value_right"=value[2], feedback)
  return(REL_play_ouctome)
  
}



#####------------- Let the agents play against one another: -------------###### 
trials = 100

df_results <- tibble(trial = rep(NA, trials),
            agent_bias_choice = rep(NA, trials),
            agent_REL_choice = rep(NA, trials), 
            left_v1 = rep(NA, trials), 
            right_v2 = rep(NA, trials), 
            feedback = rep(NA, trials))

# initiation of agents
df_results$agent_bias_choice[1] <- random_agent_function(.5)
df_results$agent_REL_choice[1] <- rbinom(1,1,.5)

# initiation of feedback and values:
df_results$feedback[1] <- ifelse(df_results$agent_bias_choice[1] == df_results$agent_REL_choice[1], 1, 0)

# we assume for the player to be completely unbiased in the initial choice between left and right: 
df_results$left_v1[1] <- .5
df_results$right_v2[1] <- .5

df_results$trial[1] <- 1


########## defining parameters: 
tau_param <- .5 
alpha_param <- .8
rate_param <- .7


####### now we move from the first round and let them play by the functions:
for (i in 2:trials){
  
  ### the feedback is given the previous trial:
  feedback <- ifelse(df_results$agent_bias_choice[i-1] == df_results$agent_REL_choice[i-1], 1, 0) 

  agent_bias <- random_agent_function(rate = rate_param)
  agent_REL <- REL_agent_function(tau=tau_param,
                                  value=c(df_results$left_v1[i-1], df_results$right_v2[i-1]),
                                  alpha=alpha_param,
                                  choice=df_results$agent_REL_choice[i-1],
                                  feedback=feedback) 
  print(agent_REL)
  
  ### saving resutls: 
  df_results$feedback[i-1] <- feedback
  
  df_results$agent_REL_choice[i] <- agent_REL[1]
  df_results$agent_bias_choice[i] <- agent_bias[]
  
  df_results$left_v1[i] <- agent_REL[2]
  df_results$right_v2[i] <- agent_REL[3]
}

  
  




  
  