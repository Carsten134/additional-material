## required ####################################################################
library(tidyverse)

## config params ###############################################################
sample_size <- 10000
bootstrap_size <- 1000
estimate_size <- 100

## normal routine ##############################################################
gen_data <- function(sd) {
  data <- rnorm(sample_size,
                sd = sd)

  estimates <- c()

  for (i in 1:bootstrap_size) {
    estimate_data <- sample(data, size = estimate_size)
    estimates <- c(estimates, mean(estimate_data))
  }

  type <- as.factor(c("value", "estimate"))

  type_vals <- c(rep(type[1], sample_size),
                 rep(type[2], bootstrap_size))

  data <- c(data, estimates)

  data <- data.frame(val = data,
                     type = type_vals)

  data <- cbind(data, data.frame(sd = rep(sd, length(type_vals))))

  return(data)
}

data_1 <- gen_data(1)
data_4 <- gen_data(4)

data <- rbind(data_1, data_4)

## plotting ####################################################################
theme_set(theme_minimal())s

values <- data %>%
  filter(type == "value")

estimates <- data %>%
  filter(type == "estimate")

p <- data %>%
  ggplot(aes(x = val,
             group = type,
             color = type,
             fill = type)) +
  geom_area(size = 1,
            stat = "density",
            alpha = 0.3) +
  facet_wrap(vars(sd)) +
  labs(title = "Normalverteilung und Mittelwertschätzungen",
       subtitle = "Vergleich der Dichten") +
  ylab("Wahrscheinlichkeit") +
  xlab("Werte") +
  theme(text = element_text(family = "Lato")) +



p
