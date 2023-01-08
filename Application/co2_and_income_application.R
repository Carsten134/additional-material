## requirements ################################################################
library(testsim)
library(clinfun)
library(tidyverse)
data_co2 <- read.csv("C:/Users/test/OneDrive - uni-bonn.de/Desktop/Studium/Bachelor Arbeit/Data/FAOSTAT_data_en_12-28-2022.csv")
data_gni <- read.csv("C:/Users/test/OneDrive - uni-bonn.de/Desktop/Studium/Bachelor Arbeit/Data/FAOSTAT_data_gni_per_capita.csv")

data <- inner_join(data_co2, data_gni, by = c("Area" = "Area"))
## config params ###############################################################
# thresholds from world bank 2021
thresholds <- c(1085, 4255, 13205)

## pre processing ##############################################################
income_levels <- factor(c("low", "lower-middle", "upper-middle", "high"),
                        levels = c("low", "lower-middle", "upper-middle", "high"))
lower_income <- data %>%
  filter(Value.y <= thresholds[1]) %>%
  filter(Element.x == "Emissions (CO2)") %>%
  summarize(log_gni_per_capita = log(Value.y),
            log_CO2 = log(Value.x),
            Co2 = Value.x,
            gni = Value.y,
            level = income_levels[1])

lower_middle_income <- data %>%
  filter(Value.y <= thresholds[2]) %>%
  filter(Value.y > thresholds[1]) %>%
  filter(Element.x == "Emissions (CO2)") %>%
  summarize(log_gni_per_capita = log(Value.y),
            log_CO2 = log(Value.x),
            Co2 = Value.x,
            gni = Value.y,
            level = income_levels[2])

upper_middle_income <- data %>%
  filter(Value.y <= thresholds[3]) %>%
  filter(Value.y > thresholds[2]) %>%
  filter(Element.x == "Emissions (CO2)") %>%
  summarize(log_gni_per_capita = log(Value.y),
            log_CO2 = log(Value.x),
            Co2 = Value.x,
            gni = Value.y,
            level = income_levels[3])

high_income <- data %>%
  filter(thresholds[3] < Value.y) %>%
  filter(Element.x == "Emissions (CO2)") %>%
  summarize(log_gni_per_capita = log(Value.y),
            log_CO2 = log(Value.x),
            Co2 = Value.x,
            gni = Value.y,
            level = income_levels[4])

test_data <- rbind(lower_income,
                   lower_middle_income,
                   upper_middle_income,
                   high_income)

test_data <- test_data %>%
  filter(log_CO2 > 0)


## explorative data analysis ###################################################

# test for normal-destribution --> p-val = 0.9413
test <- ks.test(test_data$log_CO2,
        "pnorm",
        mean(test_data$log_CO2),
        sqrt(var(test_data$log_CO2)))

# plotting data
theme_set(theme_minimal())
p <-  test_data %>%
  ggplot(aes(x = log_CO2,
             group = level,
             color = level,
             fill = level)) +
  labs(title = "Vergleich der Einkommensgruppen") +
  geom_density(size = 1,
               alpha = 0.35) +
  ylab("Wahrscheinlichkeit") +
  xlab("Co2-Emissionen (Kilotonnen, logarithmiert)") +
  theme(text = element_text(family = "Lato"))
p

# scatter plot
linear_regression <- lm(log_CO2 ~ log_gni_per_capita + log_gni_per_capita^2, data = test_data)
p_s <- test_data %>%
  ggplot(aes(y = log_CO2,
             x = log_gni_per_capita)) +
  geom_point(size = 1) +
  geom_line(data = data.frame(est_co2 = linear_regression$fitted.values,
                              gni = test_data$log_gni_per_capita),
            mapping = aes(x = gni,
                          y = est_co2),
            color = "midnightblue",
            size = 1) +
  labs(title = "Lineare Regression") +
  xlab("Einkommen pro Kopf(logarithmiert)") +
  ylab("Co2-Emissionen (logarithmiert)") +
  theme(text = element_text(family = "Lato"))

p_s

residuals <- data.frame(values = linear_regression$residuals,
                        gni = test_data$log_gni_per_capita)

p_r <- residuals %>%
  ggplot(aes(y = values,
             x = gni)) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_smooth() +
  labs(title = "Residuen der linearen Regression") +
  xlab("Einkommen pro Kopf(logarithmiert)") +
  ylab("Co2-Emissionen (logarithmiert)") +
  theme(text = element_text(family = "Lato"))

p_r

t_factor <- factor(c("transformed", "native"))

log_transform <- data.frame(co2 = c(test_data$Co2, test_data$log_CO2),
                            gni = c(test_data$gni, test_data$log_gni_per_capita),
                            method = c(rep(t_factor[2], length(test_data$Co2)),
                                       rep(t_factor[1], length(test_data$log_CO2))))

p_t <- log_transform %>%
  ggplot() +
  geom_density(aes(x = co2)) +
  facet_wrap(vars(method),
             scales = "free") +
  labs(title = "Log-Transformation") +
  ylab("Wahrscheinlichkeit") +
  ylab("Co2-Emissionen") +
  theme(text = element_text(family = "Lato"))

p_t
# variances of the groups
g_variances <- c(var(test_data$log_CO2[test_data$level == income_levels[1]]),
                 var(test_data$log_CO2[test_data$level == income_levels[2]]),
                 var(test_data$log_CO2[test_data$level == income_levels[3]]),
                 var(test_data$log_CO2[test_data$level == income_levels[4]]))

# testing for balanced samples
test_data %>% count(level)

# testing for normality amongst the samples
test_results <- c(0,0,0,0)
for (i in 1:4) {
  curr_level <- income_levels[i]
  curr_data <- test_data %>%
    filter(level == curr_level)
  curr_test_result <- ks.test(curr_data$log_CO2,
          "pnorm",
          mean(curr_data$log_CO2),
          sqrt(var(curr_data$log_CO2)))$p.value
  print(curr_test_result)
}

ks.test(test_data$log_CO2,
        "pnorm",
        mean(test_data$log_CO2),
        sqrt(var(test_data$log_CO2)))



## testing the data ############################################################
kruskal.test(test_data$log_CO2,
             test_data$level)

jonckheere.test(test_data$log_CO2,
                as.numeric(test_data$level),
                "increasing")

oneway.test(log_CO2 ~ level, test_data)

mood_test(log_CO2 ~ level, test_data)
