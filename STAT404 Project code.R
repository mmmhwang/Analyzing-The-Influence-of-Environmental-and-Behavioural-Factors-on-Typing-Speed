library(MASS)
library(emmeans)
#Blocking and Main Effects
b <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
       3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3)
t <- c(1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,
       -1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1)
m <- c(1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,
       1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1)
d <- c(1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,
       -1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1)
p <- c(1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,
       -1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1)
y <- c(54, 60, 93, 98, 45, 49, 104, 91, 53, 66, 108, 99, 56, 60, 105, 102,
       62, 60, 67, 68, 61, 59, 69, 67, 69, 66, 75, 62, 68, 67, 72, 70, 70, 
       65, 83, 77, 65, 58, 78, 76, 60, 64, 79, 85, 57, 56, 73, 86)

b <- factor(b)
t <- factor(t)
m <- factor(m)
d <- factor(d)
p <- factor(p)
#Interaction Effects
t_m <- as.numeric(as.character(t)) * as.numeric(as.character(m))
t_d <- as.numeric(as.character(t)) * as.numeric(as.character(d))
t_p <- as.numeric(as.character(t)) * as.numeric(as.character(p))
m_d <- as.numeric(as.character(m)) * as.numeric(as.character(d))
m_p <- as.numeric(as.character(m)) * as.numeric(as.character(p))
d_p <- as.numeric(as.character(d)) * as.numeric(as.character(p))
t_m_d <- as.numeric(as.character(t)) * as.numeric(as.character(m)) * 
  as.numeric(as.character(d))
t_m_p <- as.numeric(as.character(t)) * as.numeric(as.character(m)) * 
  as.numeric(as.character(p))
t_d_p <- as.numeric(as.character(t)) * as.numeric(as.character(d)) * 
  as.numeric(as.character(p))
m_d_p <- as.numeric(as.character(m)) * as.numeric(as.character(d)) * 
  as.numeric(as.character(p))
t_m_d_p <- as.numeric(as.character(t)) * as.numeric(as.character(m)) * 
  as.numeric(as.character(d)) * as.numeric(as.character(p))

b_t <- as.numeric(as.character(b)) * as.numeric(as.character(t))
b_m <- as.numeric(as.character(b)) * as.numeric(as.character(m))
b_d <- as.numeric(as.character(b)) * as.numeric(as.character(d))
b_p <- as.numeric(as.character(b)) * as.numeric(as.character(p))
b_t_m <- as.numeric(as.character(b)) * t_m
b_t_d <- as.numeric(as.character(b)) * t_d
b_t_p <- as.numeric(as.character(b)) * t_p
b_m_d <- as.numeric(as.character(b)) * m_d
b_m_p <- as.numeric(as.character(b)) * m_p
b_d_p <- as.numeric(as.character(b)) * d_p
b_t_m_d <- as.numeric(as.character(b)) * t_m_d
b_t_m_p <- as.numeric(as.character(b)) * t_m_p
b_t_d_p <- as.numeric(as.character(b)) * t_d_p
b_m_d_p <- as.numeric(as.character(b)) * m_d_p
b_t_m_d_p <- as.numeric(as.character(b)) * t_m_d_p
#excluded  b_t_m_d_p, t_m_d_p, b_t_m_d, b_t_m_p, b_t_d_p, b_m_d_p,
#b_t_m, b_t_d, b_t_p, b_m_d, b_m_p, b_d_p, t_m_d, t_m_p, t_d_p, m_d_p
df <- data.frame(
  b, t, m, d, p, 
  t_m, t_d, t_p, m_d, m_p, d_p,
  b_t, b_m, b_d, b_p
)

boxcox(y ~ ., data = df,
      lambda = seq(-1, 2, len = 21),
      ylab = "Log likelihood" )

# #Apply sqrt transformation
# y_transformed <- sqrt(y)

# boxcox(y_transformed ~ ., data = df,
#        lambda = seq(-1, 2, len = 21),
#        ylab = "Log likelihood" )


# #Construct Model
# model <- lm(y_transformed ~ ., data = df)

# qqnorm(residuals(model))
# qqline(residuals(model), col = "red")

# plot(fitted.values(model), residuals(model), xlab = "Fitted Values", 
#      ylab = "Residuals", main = "Residual vs. Fitted Plot")

# #ANOVA
# anova <- aov(y_transformed ~ ., data = df)
# summary(anova)

# means <- aggregate(y_transformed ~ d, data = df, mean)
# print(means)

# summary(model)


#Apply log transformation
y_transformed_log <- log(y)

boxcox(y_transformed_log ~ ., data = df,
       lambda = seq(-1, 2, len = 21),
       ylab = "Log likelihood" )


#Construct Model
model <- lm(y_transformed_log ~ ., data = df)

qqnorm(residuals(model))
qqline(residuals(model), col = "red")

plot(fitted.values(model), residuals(model), xlab = "Fitted Values", 
     ylab = "Residuals", main = "Residual vs. Fitted Plot")

#ANOVA
anova <- aov(y_transformed_log ~ ., data = df)
summary(anova)

#Performing contrast
means <- aggregate(y_transformed_log ~ d, data = df, mean)
print(means)

summary(model)

png(filename = "interactionPlot.png", width = 350, height = 350)
interaction.plot(df$d, df$b, response = y)
dev.off()

treat_means <- model.tables(anova, type = "mean", se = TRUE, 
             cterms = c("d"))

treatment_means <- data.frame(treat_means$tables[2])


kd_hat <- treatment_means[1,1] - treatment_means[2,1]
kd_hat


MSE <- mean(summary(model)$residuals^2)
MSE
se_kd <- (2 * sqrt(MSE / 48))

exp(-0.649575810)
0.1235^2
t_stat <- kd_hat / se_kd
t_stat
pt(t_stat, df = 31, lower.tail = FALSE)
sqrt(MSE / 1)
qt(0.025, 31, lower.tail = FALSE)

exp(kd_hat - (qt(0.025, 31, lower.tail = FALSE))*se_kd)
exp(kd_hat + (qt(0.025, 31, lower.tail = FALSE))*se_kd)

summary(model)

exp(predict(model, newdata = data.frame(b,t,m,d,p)))

#1 -1 -1 -1 -1 block 1

#2 -1 -1 -1  1 block 2

#3  1  1 -1  1 block 3