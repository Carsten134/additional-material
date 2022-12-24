## required ####################################################################
library(tidyverse)

## config params ###############################################################
sample_size <- 10000
bootstrap_size <- 1000
estimate_size <- 100

## normal routine ##############################################################
data <- rnorm(sample_size,
              sd = 3)

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

## plotting ####################################################################
theme_set(theme_minimal())

values <- data %>%
  filter(type == "value")

estimates <- data %>%
  filter(type == "estimate")

p <- values %>%
  ggplot(aes(x = val,
             group = type)) +
  geom_area(size = 1,
            stat = "density",
            alpha = 0.3,
            color = "deepskyblue",
            fill = "deepskyblue") +
  geom_area(mapping = aes(x = val,
                group = type),
            data = estimates,
            stat = "density",
            size = 1,
            color = "midnightblue",
            fill = "midnightblue",
            alpha = 0.6) +
  labs(title = "Normalverteilung und Mittelwertschätzungen",
       subtitle = "Vergleich der Dichten") +
  ylab("Dichte") +
  xlab("Werte") +
  theme(text = element_text(family = "Lato"))


p
