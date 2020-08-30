Core scripts Logistic Lasso regression

form <- as.formula(paste(target, paste(vars, collapse="+"), sep="~"))
form
xfactors <- model.matrix(form, data)[, -1]
x <- as.matrix(data.frame(xfactors))
bad_flag <- data$bad_flag
```

```{r, message=FALSE, warning=FALSE, error=FALSE}
## Train lasso logistic regression model.
# Note alpha=1 for lasso only and can blend with ridge penalty down to alpha=0 ridge only.
model_glmnet <- glmnet(x[train, ], y=bad_flag[train], alpha=1, family="binomial")
# Plot variable coefficients vs. shrinkage parameter lambda.
# plot(model_glmnet, xvar="lambda")
# Obtain optimal lambda using cross validation
model_glmnet_cv <- cv.glmnet(x[train, ], y=bad_flag[train], alpha=1, family="binomial")
# plot(model_glmnet_cv)
best_lambda <- model_glmnet_cv$lambda.min %T>% print()
# Obtain the coefficient of each variable in the optimal fitted model, which shows variable importance as well.
coef(model_glmnet_cv, s=best_lambda)
```

Once a (lasso logistic regression) model has been built based on a
training data set, next the validity of the model needs to be assessed
in a testing data set.

```{r, message=FALSE, warning=FALSE, error=FALSE}
# Evaluate model performance on the validation dataset. 
# Obtain the probabilities of credit default for the fitted optimal glmnet() model.
probs <- predict(model_glmnet_cv, newx=x[test, ], type="response", s=best_lambda)