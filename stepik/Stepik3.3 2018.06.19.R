



rw(swiss)
swiss <- data.frame(swiss)

fit_full <- lm(Fertility ~. , data = swiss)
summary(fit_full)

fit_reduced1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, data = swiss )
summary(fit_reduced1)
anova(fit_full, fit_reduced1)

fit_reduced2 <- lm(Fertility ~ Infant.Mortality  + Catholic + Education + Agriculture, data = swiss )
summary(fit_reduced2)
anova(fit_full, fit_reduced2)

# ----------------------------------------------------------

# model selection

optimal_fit <- step(fit_full, direction = 'backward')
summary(optimal_fit)

# ----------------------------------------------------------

model_full <- lm(rating ~ ., data = attitude) 
model_null <- lm(rating ~ 1, data = attitude)

scope = list(lower = model_null, upper = model_full) # Пространство моделей

ideal_model <- step(model_full, scope = scope, direction = 'backward')

anova(model_full, ideal_model)

# -----------------------------------------------------------------


l <- lm(sr ~(pop15+pop75+dpi+ddpi)^2, data = LifeCycleSavings)


