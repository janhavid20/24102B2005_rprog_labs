# Palmer Penguins Statistical Analysis
# install.packages(c("palmerpenguins","dplyr","ggplot2","moments","car","effsize"))

library(palmerpenguins)
library(dplyr)
library(ggplot2)
library(moments)
library(car)
library(effsize)

data("penguins")

penguins <- na.omit(penguins)

# 1. Descriptive Statistics
summary_stats <- penguins %>%
  group_by(species) %>%
  summarise(
    Mean = mean(body_mass_g),
    Median = median(body_mass_g),
    Min = min(body_mass_g),
    Max = max(body_mass_g),
    Variance = var(body_mass_g),
    SD = sd(body_mass_g),
    Q1 = quantile(body_mass_g,0.25),
    Q3 = quantile(body_mass_g,0.75),
    IQR = IQR(body_mass_g),
    Skewness = skewness(body_mass_g),
    Kurtosis = kurtosis(body_mass_g)
  )

print(summary_stats)

# Visualizations
ggplot(penguins,aes(body_mass_g))+geom_histogram(bins=20)
ggplot(penguins,aes(species,body_mass_g))+geom_boxplot()
ggplot(penguins,aes(body_mass_g,color=species))+geom_density()

# 2. Hypothesis Testing
male <- penguins$body_mass_g[penguins$sex=="male"]
female <- penguins$body_mass_g[penguins$sex=="female"]

shapiro.test(male)
shapiro.test(female)

t_test <- t.test(male,female,var.equal=FALSE)
print(t_test)

cohen.d(body_mass_g ~ sex,data=penguins)

# 3. One Way ANOVA
leveneTest(body_mass_g ~ species,data=penguins)

anova1 <- aov(body_mass_g ~ species,data=penguins)
summary(anova1)

TukeyHSD(anova1)

# 4. Kruskal Wallis
kruskal.test(body_mass_g ~ species,data=penguins)

# 5. Two Way ANOVA
anova2 <- aov(body_mass_g ~ species*sex,data=penguins)
summary(anova2)

# 6. Flipper Length Analysis
anova_flipper <- aov(flipper_length_mm ~ species,data=penguins)
summary(anova_flipper)

ggplot(penguins,aes(species,flipper_length_mm,fill=species))+
geom_boxplot()
