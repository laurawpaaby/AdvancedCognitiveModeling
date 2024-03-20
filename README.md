# Advanced Cognitive Modeling Portfolio
## Cognitive Science Masters at Aarhus University
This repository encompasses the collection of portfolio assignments leading to the final examination of the Advanced Cognitive Modeling course conducted by Riccardo Fusaroli. The repository is structured to sequentially present the following assignments:

##### Portfolio 1: Simulating a Win-Stay-Lose-Shift Agent in R
This section showcases simulations of agents engaged in the Matching Pennies game. The simulation features a "matcher" agent that adopts a Win-Stay-Lose-Shift strategy against an opponent, termed "the capitalist," who employs a random yet biased approach to gameplay.

##### Portfolio 2: Fitting a Reinforcement Learning Agent in STAN
The focus here shifts to simulating two players in the Matching Pennies game, with the first player now utilizing a reinforcement learning strategy instead of Win-Stay-Lose-Shift. The opponent's strategy remains unchanged, playing randomly but with bias. The simulation is conducted through RL_sim.R. Subsequently, the player's behavior is modeled in stan_RL.stan, complete with comments for clarity. Diagnostic analyses, including prior and posterior evaluations alongside prediction checks, are performed in diagnostic_plots.R. The process concludes by assessing the recoverability of the reinforcement learning model's parameters within model.R.

##### Portfolio 3: Fitting Two Multilevel Models and Conducting Model Comparisons in STAN
This segment is dedicated to fitting two multilevel models and undertaking comparative analysis between them using STAN, illustrating advanced applications of Bayesian modeling techniques to cognitive science research.
