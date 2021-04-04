# Binomial Regression

n <- nrow(mtcars)
train <- sample(1:n,ceiling(0.8*n),replace = F)
model <- glm(am ~ disp + hp, family=binomial, data=mtcars[train,])
new_data <- mtcars[-train,c('disp','hp')]

cbind(pred = round(predict(model,new_data, type='response')), actual=mtcars[-train,'am'])

# Poisson Regression

model1 <- glm(cyl ~ drat + wt, family=poisson,data=mtcars[train,])
new_data1 <- mtcars[-train,c('drat','wt')]
cbind(pred = round(predict(model1,new_data1,type='response')), actual = mtcars[-train,'cyl'])

# Poisson Regression 2
y <- sample(1:100, size=100, replace = T)
x1 <- rnorm(100,60,20)
x2 <- rpois(100,30)
dat <- data.frame(y,x1,x2)
model2 <- glm(y ~ x1 + x2, family=poisson, data = dat)
predict(model2,newdata = data.frame(x1=75.354,x2=47),type='response')
