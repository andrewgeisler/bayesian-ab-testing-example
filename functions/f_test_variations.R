f_test_variations <- function(success_a, failure_a, alpha, beta,  control_samples, lift = 2) {
  
  a <- c(rep.int(0, failure_a), rep.int(1, success_a))
  
  results <- bayesTest(a,
                       control_samples,
                       priors = c('alpha' = alpha, 'beta' = beta),
                       n_samples = 1e5,
                       distribution = 'bernoulli')
  
  results <- summary(results, percentLift = lift)
  
  results <- tibble(sample_mean_a = mean(a),
                    sample_mean_b = mean(control_samples),
                    probability = results$probability$Probability,
                    percent_lift = results$percentLift,
                    expected_loss = results$posteriorExpectedLoss[[1]]
  )

  return(results)
}  