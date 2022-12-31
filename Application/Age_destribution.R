## requirements ################################################################
library(tidyverse)
library(coin)
library()

age_dis <- read.csv("C:/Users/test/OneDrive - uni-bonn.de/Desktop/Studium/Bachelor Arbeit/Data/Altersverteilung_nach_Berufsstellung.csv",
                    sep = ";")

## config ######################################################################

borders <- c(20,25,30,35,40,45,50,55,60,65,70)

## data processing #############################################################
# get rid of additional rows
age_dis <- age_dis %>%
  select(Stellung, Altersgruppe, Anzahl_in_tausend)

## data generation ##############################################################
data <- data.frame(Stellung = c(),
                   Alter = c())

for (i in 0:(length(age_dis$Anzahl_in_tausend) - 1)) {
  len <- age_dis$Anzahl_in_tausend[i + 1]
  work_type <- age_dis$Stellung[i + 1]
  min <- borders[(i%%9) + 1]
  max <- borders[(i%%9) + 2]
  data <- rbind(data, data.frame(Stellung = rep(work_type, len),
                                 Alter = seq(min, max, length.out = len)))
}

data$Stellung <- factor(data$Stellung)

## data transformation #########################################################

unif_data <- data%>%
  summarise(unif_alter = (Alter - 19.5)/46,
            stellung = Stellung)

transformed_data <- unif_data %>%
  summarise(norm_alter = qnorm(unif_alter),
            stellung = stellung)


## plotting data ###############################################################
theme_set(theme_minimal())

p <- data %>%
  ggplot(aes(x = Alter)) +
  geom_density(size = 1,
               fill = "blue",
               alpha = 0.5,
               color = "blue") +
  labs(title = "Altersverteilung über alle Stellungen") +
  ylab("Dichte") +
  xlab("Alter") +
  theme(text = element_text(family = "Lato"))

p

p_d <- data %>%
  ggplot(aes(x = Alter,
             group = Stellung,
             color = Stellung,
             fill = Stellung)) +
  geom_density(alpha = 0.5) +
  labs(title = "Vergleich der Altersverteilungen",
       subtitle = "Nach Stellung im Beruf") +
  ylab("Wahrscheinlichkeit") +
  xlab("Alter") +
  theme(text = element_text(family = "Lato"))

p_d

p_v <- data %>%
  ggplot(aes(y = Alter,
             x = Stellung,
             group = Stellung,
             color = Stellung,
             fill = Stellung)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Boxplot der Altersverteilung",
       subtitle = "Nach Stellung im Beruf") +
  ylab("Alter") +
  theme(text = element_text(family = "Lato"))


p_v

p_g <- transformed_data %>%
  ggplot(aes(x = norm_alter)) +
  geom_density(size = 1,
               fill = "midnightblue",
               alpha = 0.5) +
  geom_density(aes(x = data),
               data = data.frame(data = rnorm(40000)),
               color = "red",
               size = 1,
               fill = "red",
               alpha = 0.5) +
  xlab("normiertes Alter") +
  labs(title = "Verteilung des normierten Alters") +
  theme(text = element_text(family = "Lato"))

p_g

p_n_d <- transformed_data %>%
  ggplot(aes(x = norm_alter,
             group = stellung,
             color = stellung,
             fill = stellung)) +
  geom_density(size = 1,
               alpha = 0.5) +
  labs(title = "Vergleich der normierten Altersverteilungen",
       subtitle = "Nach Stellung im Beruf") +
  ylab("Wahrscheinlichkeit") +
  xlab("normiertes Alter") +
  theme(text = element_text(family = "Lato"))

p_n_d
## checking assumptions ########################################################

ks.test(transformed_data$norm_alter,
        "pnorm",
        mean(transformed_data$norm_alter),
        sqrt(var(transformed_data$norm_alter)))


## applying the tests ##########################################################

median_test(Alter ~ Stellung, data)

oneway.test(norm_alter ~ stellung, transformed_data)


