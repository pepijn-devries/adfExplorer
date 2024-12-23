load("data-raw/benchresult.rdata")

requireNamespace("ggplot2") {
  library(ggplot2)
  ggplot(bench_result, aes(x = test, y = bench_mean)) +
    geom_col() +
    geom_errorbar(aes(ymin = bench_mean - bench_sd, ymax = bench_mean + bench_sd),
                  width = 0.2) +
    labs(title = "Benchmark results",
         x = "Package version",
         y = "Execution time (s)") +
    theme_minimal()
  ggsave("man/figures/benchmark.png", width = 3, height = 6, dpi = 300)
}