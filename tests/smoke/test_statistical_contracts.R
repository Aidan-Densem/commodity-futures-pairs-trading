x <- c(-.02, 0, .01, .015, -.005, 0, .02, .01)
metrics <- daily_performance_metrics(x, annualisation = 252, hac_lag = 2)
smoke_equal(metrics$compounded_return, prod(1 + x) - 1,
            message = "Performance: compounded return formula failed")
smoke_equal(metrics$annualised_arithmetic_mean, 252 * mean(x),
            message = "Performance: annualisation failed")
smoke_expect(metrics$historical_var_05 <= 0 &&
               metrics$historical_es_05 <= metrics$historical_var_05,
             "Performance: ES/VaR ordering failed")
nw <- newey_west_mean(x, 2); smoke_expect(all(is.finite(nw)), "Performance: NW inference failed")
matrix <- cbind(A = x, B = x / 2 + .001)
boot <- stationary_bootstrap_model_inference(matrix, replications = 50L,
                                              mean_block_length = 3, seed = 123)
boot_repeat <- stationary_bootstrap_model_inference(matrix, replications = 50L,
                                                     mean_block_length = 3, seed = 123)
smoke_expect(nrow(boot$model_intervals) == 2L && nrow(boot$pairwise_sharpe_contrasts) == 1L,
             "Performance: aligned bootstrap contract failed")
smoke_expect(is.finite(boot$pairwise_sharpe_contrasts$holm_p),
             "Performance: Holm-adjusted inference missing")
smoke_expect(identical(boot$common_indices, boot_repeat$common_indices) &&
               identical(boot$pairwise_sharpe_contrasts,
                         boot_repeat$pairwise_sharpe_contrasts),
             "Performance: stationary bootstrap seed is not deterministic")
cat("STATISTICAL_CONTRACTS_PASS\n")
