## requirements ################################################################
library(tidyverse)
library(testsim)
library(stats)
library(devtools)
library(clinfun)

## config params ###############################################################
offset <- seq(-1,1, len = 20)
alpha <- 0.05
k <- 3
n_sim <- 1000
var <- 1
n_sample <- 40
n_sample_pow <- seq(9,50, len = 20)


## params generation functions #################################################
gen_rnorm_params <- function(offset) {
  force(offset)
  mean <- c(offset, 0, 0)
  var <- c(1,1,1)
  return(data.frame(mean = mean,
                    var = var))
}

gen_runif_params <- function(offset) {
  force(offset)
  min <- c(offset, 0, 0)
  max <- c(offset + 1, 1, 1)
  return(data.frame(min = min,
                    max = max))
}

gen_cauchy_params <- function(offset) {
  force(offset)
  if (offset < 0) {
    location <- c(0, -offset, -offset)
  } else {
    location <- c(offset, 0, 0)
  }
  scale <- c(1, 1, 1)
  return(data.frame(location = location,
                    scale = scale))
}



## szenarios ###################################################################
gen_szenario <- function(params, gen_destribution = gen_rnorm_vector, n_s = n_sample, order = c(1,2,3)) {
  force(params)
  force(n_s)
  force(gen_destribution)
  force(order)
  return(function() {
    # generate data
    k <- length(params[,1])
    order_j <- "increasing"
    if (order[1] == 1) {
      order_j <- "decreasing"
    }
    data <- params %>%
      gen_destribution() %>%
      generate_ksamples(size = rep(n_s, k))

    test_results <- as.factor(c("anova_h0",
                                "anvova_h1",
                                "kruskal_h0",
                                "kruskal_h1",
                                "mood_h0",
                                "mood_h1",
                                "j_t_h0",
                                "j_t_h1"))

    tests <- c(anova,
               function(data, alpha) {
                 result <- kruskal.test(data$value,
                                           as.numeric(data$s_cat))
                 return(result$p.value > alpha)
               },
               mood_test,
               function(data, alpha) {
                 result <- jonckheere.test(data$value,
                                           as.numeric(data$s_cat),
                                           order_j)
                 return(result$p.value > alpha)

               })

    results <- c()
    i <- 1
    ind <- 1
    for (test in tests) {
      curr_result <- test(data, alpha)
      if (curr_result) {
        ind <- 2*i - 1
      } else {
        ind <- 2*i
      }
      results = append(results, test_results[ind])
      i <- i + 1
    }

    return(results)
  })
}



## main data generation ########################################################
# generate the data
gen_data_beta <- function(gen_destribution, gen_params) {
  force(gen_destribution)
  force(gen_params)
  data <- data.frame(value = c(),
                     n = c(),
                     relative = c(),
                     offset=c())

  process <- txtProgressBar(0, length(offset), style = 3)
  i <- 1
  for (off in offset) {
    params <- gen_params(off)
    order <- c(1, 2, 3)
    if (off < 0) {
      order <- c(3, 2, 1)
    }
    expr <- gen_szenario(params, gen_destribution,
                         order = order)
    data_t <- monte_carlo(expr, n_sim)
    offset_values <- data.frame(offset = rep(off,
                                             length(data_t$value)))

    data_t <- cbind(data_t, offset_values)
    data <- rbind(data, data_t)
    setTxtProgressBar(process, i)
    i <- i + 1
  }
  return(data)
}

gen_data_power <- function(gen_destribution, gen_params, off) {
  force(gen_destribution)
  data <- data.frame(value = c(),
                     n = c(),
                     relative = c(),
                     offset=c())
  params <- gen_params(off)
  process <- txtProgressBar(0, length(offset), style = 3)
  i <- 1
  for (n in n_sample_pow) {
    order <- c(1, 2, 3)
    if (off < 0) {
      order <- c(3, 2, 1)
    }
    expr <- gen_szenario(params, gen_destribution,
                         order = order,
                         n_s = n)

    data_t <- monte_carlo(expr, n_sim)
    n_vals <- data.frame(offset = rep(n,
                                      length(data_t$value)))

    data_t <- cbind(data_t, n_vals)
    data <- rbind(data, data_t)
    setTxtProgressBar(process, i)
    i <- i + 1
  }
  return(data)
}


data_norm_b <- gen_data_beta(gen_rnorm_vector, gen_rnorm_params)
data_unif_b <- gen_data_beta(gen_runif_vector, gen_runif_params)

data_norm_p <- gen_data_power(gen_rnorm_vector, gen_rnorm_params, offset[17])
data_unif_p <- gen_data_power(gen_runif_vector, gen_runif_params, offset[12])
## enhancing data ##############################################################

# include method
include_method_beta <- function(data) {
  methods <- as.factor(c("Kruskal-Wallis", "Mediantest", "ANOVA", "Jonckheere Terpstra"))

  mood_data <- data %>%
    filter(value == "mood_h1")

  kruskal_data <- data %>%
    filter(value == "kruskal_h1")

  anova_data <- data %>%
    filter(value == "anvova_h1")

  jonckheere_data <- data %>%
    filter(value == "j_t_h1")

  mood_data <- cbind(mood_data, data.frame(method=rep(methods[2],
                                                      length(mood_data$value))))
  anova_data <- cbind(anova_data, data.frame(method = rep(methods[3],
                                                          length(anova_data$value))))
  kruskal_data <- cbind(kruskal_data, data.frame(method = rep(methods[1],
                                                              length(kruskal_data$value))))
  jonckheere_data <- cbind(jonckheere_data, data.frame(method = rep(methods[4],
                                                                    length(jonckheere_data$value))))

  plot_data <- rbind(anova_data, kruskal_data)
  plot_data <- rbind(plot_data, mood_data)
  plot_data <- rbind(plot_data, jonckheere_data)
}

data_norm_b_plot <- include_method(data_norm_b)
data_unif_b_plot <- include_method(data_unif_b)

data_norm_p_plot <- include_method(data_norm_p)
data_unif_p_plot <- include_method(data_unif_p)

## plotting the data ###########################################################
theme_set(theme_minimal())

p <- data_cauchy_plot %>%
  ggplot(aes(x = offset,
             y = relative,
             group = method,
             color = method)) +
  theme(text = element_text(size = 13,
                            family= "Lato"))

p + geom_line(size = 1) +
  geom_hline(yintercept = 0.05,
             linetype = "dashed",
             color = "grey37") +
  labs(title = "Beta Simulation",
       subtitle = "Cauchy Verteilung") +
  xlab(expression(tau[1])) +
  ylab("Güte")



