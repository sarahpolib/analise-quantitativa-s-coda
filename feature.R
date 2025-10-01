library(glmnet)

x <- model.matrix(target ~ ., dataset)[, -1]
y <- dataset$target

lasso_model <- cv.glmnet(x, y, alpha = 1)
coef(lasso_model, s = "lambda.min")