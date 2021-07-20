# Linear models

library(readr)
library(dplyr)

setwd("~/data")
pums <- read_csv(
  file = 'census_pums/sample.csv',
  col_types = cols_only(
    AGEP = 'i',  # Age
    WAGP = 'd',  # Wages or salary income past 12 months
    SCHL = 'i',  # Educational attainment
    SEX = 'f',   # Sex
    MAR = 'f', # Marital status
    HINS1 = 'f', # has insurance through employer
    WKHP = 'i')) # Usual hours worked per week past 12 months

pums <- within(pums, {
  SCHL <- factor(SCHL)
  levels(SCHL) <- list(
    'Incomplete' = c(1:15),
    'High School' = 16:17,
    'College Credit' = 18:20,
    'Bachelor\'s' = 21,
    'Master\'s' = 22:23,
    'Doctorate' = 24)}) %>%
  filter(
    WAGP > 0,
    WAGP < max(WAGP, na.rm = TRUE))

# Linear regression

fit.schl <- lm(
  formula = WAGP ~ SCHL,
  data = pums)

library(ggplot2)

ggplot(pums,
       aes(x = SCHL, y = WAGP)) +
  geom_boxplot()

fit.schl <- lm(
  log(WAGP) ~ SCHL,
  data = pums)

# Predictor class

fit.agep <- lm(
  log(WAGP) ~ AGEP,
  data = pums)

summary(fit.agep)

# GLM families

fit <- glm(log(WAGP) ~ SCHL,
  family = gaussian,
  data = pums)

summary(fit)

# Logistic Regression

fit <- glm(HINS1 ~ WAGP,
  family = binomial,
  data = pums)

levels(pums$HINS1)

pums$HINS1 <- factor(pums$HINS1, levels = c("2", "1"))
levels(pums$HINS1)

fit <- glm(HINS1 ~ WAGP,
           family = binomial,
           data = pums)

summary(fit)

#use chi square test to see if deciance in your model is lower than the null model
anova(fit, update(fit, HINS1~1), test = 'Chisq')

# Random Intercept

library(lme4)
fit <- lmer(
  log(WAGP) ~ (1|MAR) + SCHL,
  data = pums)

summary(fit)

null.model <- lmer(
  log(WAGP) ~ (1|MAR),
  data = pums)

anova(null.model, fit)

# Random Slope

fit <- lmer(
  log(WAGP) ~ (WKHP|SCHL),
  data = pums) #error

fit <- lmer(
  log(WAGP) ~ (WKHP | SCHL),
  data = pums,
  control = lmerControl(optimizer = "bobyqa"))

summary(fit)

ggplot(pums,
  aes(x = WKHP, y = log(WAGP), color = SCHL)) +
  geom_point() +
  geom_line(aes(y=predict(fit))) +
  labs(title = 'Random intercept and slope with lmer')



##Exercise 1
fit <- lm(
  WKHP ~ AGEP + I(AGEP^2),
  pums)

ggplot(pums,
       aes(x = AGEP, y = WKHP)) +
  geom_point(shape = 'o') +
  geom_line(aes(y = predict(fit)))

##2
fit <- lm(WAGP ~ SCHL, pums)
summary(fit)
anova(fit, update(fit, WAGP ~ SCHL + SEX))
summary(update(fit, WAGP ~ SCHL + WKHP))

##3
df <- pums
levels(df$SCHL) <- c(0, 0, 0, 0, 1, 1)
fit <- glmer(
  SCHL ~ (1 | OCCP) + AGEP,
  family = binomial,
  data = df)
anova(fit, update(fit, . ~ . + SEX), test = 'Chisq')

##4
WAGP ~ (1 | SCHL) + SEX
