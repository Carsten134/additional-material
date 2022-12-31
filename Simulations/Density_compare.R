
data_cauchy  <- rcauchy(1000,
                        scale = 0.5)

data_norm <- rnorm(1000,
                   sd = 0.7)

data_cauchy <- data_cauchy[data_cauchy < 4]
data_cauchy <- data_cauchy[data_cauchy > -4]

hist(data_cauchy,
     breaks = seq(-4,4,len = 15),
     include.lowest = F,
     warn.unused = F)


hist(data_norm,
     breaks = seq(-4, 4,
                  len = 15))
data_frame_norm <- data.frame(vals = data_norm,
                              dist = rep("Verteilung 1", length(data_norm)))
data_frame_cauchy <- data.frame(vals = data_cauchy,
                                dist = rep("Verteilung 2", length(data_cauchy)))

data <- rbind(data_frame_norm,
              data_frame_cauchy)

theme_set(theme_minimal())
p_plot <-  data %>%
  ggplot(aes(x = vals)) +
  geom_density(size = 1) +
  facet_wrap(vars(dist)) +
  xlab("Werte") +
  ylab("Wahrscheinlichkeit") +
  labs(title = "Vergleich von Verteilungen") +
  theme(text = element_text(family = "Lato"))

p_plot


