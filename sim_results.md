sim\_results
================

The code bellow summarizes and presents the result of Monte Carlo power
simulations for the main model presented in our preregistration.

## Load results

Loads result and reformats simulation output

``` r
load("simulation1.RData")

sim_results <- simulation1 %>%
               t %>%
               as.data.frame(stringsAsFactors = FALSE)
sim_results[[1]] <-as.numeric(sim_results[[1]])
sim_results[[2]] <-as.numeric(sim_results[[2]])
sim_results[[3]] <-as.numeric(sim_results[[3]])
sim_results[[4]] <-as.numeric(sim_results[[4]])
sim_results[[5]] <-as.numeric(sim_results[[5]])
sim_results[[6]] <-as.numeric(sim_results[[6]])
sim_results[[7]] <-as.numeric(sim_results[[7]])
```

## Present results

The following code summarizes the result of a power simulation conducted
for the full model:

y\_simulated \~ effort \* social\_inequality + (social\_inequality \|\|
subject) + (effort \* social\_inequality \|\| species) +
(social\_inequality \|\| paper)

Covariates were taken from a preliminary list of 18 eligible papers
found in test\_probabilities.csv.

Fixed effects were simulated based on a priori rejection probabilities
found in the file test\_probabilities.csv.

Random effects were simulated based on the intercept and slope structure
of the maximal model, according to SD estimates found in
simulation\_code\_savio.R.

P-value of model comparison is based on this restricted model:

y\_simulated \~ effort + (social\_inequality \|\| subject) + (effort \*
social\_inequality \|\| species) + (social\_inequality \|\| paper)

``` r
print(paste("Number of iterations: ", nrow(sim_results), sep = ""))
```

    ## [1] "Number of iterations: 1000"

``` r
percent_converged = sum(nchar(sim_results$warnings) == 0) / length(sim_results$warnings) * 100
  
print(paste("Percent of models that converged without errors: ", percent_converged, "%", sep = ""))
```

    ## [1] "Percent of models that converged without errors: 99.8%"

``` r
significant_interaction = mean(as.numeric(sim_results$`p(effort:social_inequality)` < 0.05 & nchar(sim_results$warnings) == 0)) * 100
  
print(paste("Models that converge and had a significant interaction of effort and social_inequality: ", significant_interaction, "%", sep = ""))
```

    ## [1] "Models that converge and had a significant interaction of effort and social_inequality: 99.8%"

``` r
significant_comparison = mean(as.numeric(sim_results$`p(model comparison)`) < 0.05 & nchar(sim_results$warnings) == 0) * 100
  
print(paste("Models that converged and with a significant model comparison: ", significant_comparison, "%", sep = ""))
```

    ## [1] "Models that converged and with a significant model comparison: 99.8%"

``` r
print(paste("Estimated coefficient for intercept: M = ", round(mean(sim_results$b_intercept), digits = 3), ", SD = ", round(sd(sim_results$b_intercept), digits = 3), sep = ""))
```

    ## [1] "Estimated coefficient for intercept: M = 0.708, SD = 0.158"

``` r
print(paste("Estimated coefficient for social_inequality: M = ", round(mean(sim_results$b_social_inequality), digits = 3), ", SD = ", round(sd(sim_results$b_social_inequality), digits = 3), sep = ""))
```

    ## [1] "Estimated coefficient for social_inequality: M = 0.734, SD = 0.161"

``` r
print(paste("Estimated coefficient for effort: M = ", round(mean(sim_results$b_effort), digits = 3), ", SD = ", round(sd(sim_results$b_effort), digits = 3), sep = ""))
```

    ## [1] "Estimated coefficient for effort: M = 0.701, SD = 0.124"

``` r
print(paste("Estimated coefficient for interaction of effort and social inequality: M = ", round(mean(sim_results$b_effortXsocial_inequality), digits = 3), ", SD = ", round(sd(sim_results$b_effortXsocial_inequality), digits = 3), sep = ""))
```

    ## [1] "Estimated coefficient for interaction of effort and social inequality: M = 0.655, SD = 0.198"

``` r
print("All warnings:")
```

    ## [1] "All warnings:"

``` r
print(sim_results$warnings[which(nchar(sim_results$warnings) > 0)])
```

    ## [1] "unable to evaluate scaled gradient | checkConv(attr(opt, \"derivs\"), opt$par, ctrl = control$checkConv, lbound = environment(devfun)$lower) | Model failed to converge: degenerate  Hessian with 1 negative eigenvalues | checkConv(attr(opt, \"derivs\"), opt$par, ctrl = control$checkConv, lbound = environment(devfun)$lower)"                                              
    ## [2] "Model failed to converge with max|grad| = 0.167147 (tol = 0.002, component 1) | checkConv(attr(opt, \"derivs\"), opt$par, ctrl = control$checkConv, lbound = environment(devfun)$lower) | Model is nearly unidentifiable: very large eigenvalue\n - Rescale variables? | checkConv(attr(opt, \"derivs\"), opt$par, ctrl = control$checkConv, lbound = environment(devfun)$lower)"
