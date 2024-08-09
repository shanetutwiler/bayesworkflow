# bayesworkflow

This workflow for Bayesian quantitative data analysis of educational data was based on the following work:

Gelman, A., Vehtari, A., Simpson, D., Margossian, C. C., Carpenter, B., Yao, Y., ... & Modrák, M. (2020). Bayesian workflow. arXiv preprint arXiv:2011.01808.

Gelman, A., Hill, J., & Vehtari, A. (2021). Regression and other stories. Cambridge University Press.

McElreath, R. (2020). Statistical rethinking: A Bayesian course with examples in R and Stan. Chapman and Hall/CRC.

1. Specify Scientific Models (DAGs)
   
  Content: The script uses the dagitty package to specify and plot DAGs for the relationships between attention, self-efficacy, score, and gender. Adjustment sets for specific pathways (e.g., attention to score) are also identified.

2. Fit Model to Toy Data to Debug

  Content: The script simulates data for 3,000 students clustered in 100 classrooms with an ICC of 0.2. It generates gender, self-efficacy, and attention variables, and models their effects on scores. It then checks whether a multilevel model can recover the expected relationship between attention and score.

3. Determine Sample Size via Simulation

  Content: The script runs simulations with different numbers of classrooms (ranging from 4 to 12) to determine the required sample size for reliable effect estimation. It calculates average estimates and standard errors for the effect of attention on score across multiple simulated datasets. The results are visualized to identify the minimum number of classrooms needed to minimize Type M error.

4. Determine Appropriate Priors

  Content: The script conducts prior predictive simulations to compare "ultra-wide" priors with more "informative" priors. It plots the resulting predicted scores to determine the most realistic priors for the Bayesian model, focusing on intercepts and slopes.

5. Fit Model to Data

  Content: The script uses the prior information determined in the previous steps to fit a Bayesian multilevel model to the imported data. The actual fitting process is implemented using the chosen priors.

6. Scrutinize the Model

  Content: The script involves scrutinizing the Bayesian model by running and interpreting prior predictive checks. Specifically, it simulates data based on ultra-wide and informative priors, plotting the expected distributions of the predictor's effect on the outcome (score). The plots help in assessing whether the chosen priors are realistic.

7. Do Other Things! (Model Stacking, etc.)

  Content: In this section, the script generates visualizations to explore the effects of the chosen priors further. It uses ggplot2 to plot the results from the prior predictive checks, visualizing how the number of classrooms affects the estimates and standard errors for the attention effect.
