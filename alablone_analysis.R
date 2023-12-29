library(readr)
library(visdat)
library(ggplot2)

library(GGally)
library(ggfortify)

library(emmeans)

library(pillar)


data <- read.table("abalone/abalone.data", header=FALSE, sep=",") 

col_names <- c("sex","length", "diameter", "height", "whole_weight", "shucked_weight","viscera_weight","shell_weight","rings")
colnames(data) <- col_names

data <- data[data$height != 0, ]
data$rings=data$rings+1.5



model <- lm(rings ~ 
              length + 
              diameter + 
              height + 
              shucked_weight +
              viscera_weight + 
              shell_weight, data=data)

# plot(model$fitted.values, model$residuals, xlab="Fitted values", ylab="Residuals")
autoplot(model,which=1:2)
abline(h=0, col="red")

step_model <- step(model, direction="both")
summary(step_model)

ggplot(data, aes(x=rings)) + 
  geom_histogram(,fill="skyblue", color="black", binwidth=1) + 
  labs(title="Distribution of Rings(+1.5)", x="Rings(+1.5)", y="Count") + 
  theme_minimal()




transform_var <- function(data, var, trans) {
  if (trans == "log") return(log(data[[var]] + 1))
  if (trans == "sqrt") return(sqrt(data[[var]]))
  if (trans == "square") return(data[[var]]^2)
  return(data[[var]])
}


vars <- c("rings", "length", "diameter", "height", "shucked_weight", "viscera_weight", "shell_weight")
transformations <- c("original", "log", "sqrt", "square")


results <- list()

ggplot(data, aes(x = length)) +
  geom_histogram(fill = "skyblue", color = "black", binwidth = 0.1) +
  labs(title = "Distribution of Length", x = "Length", y = "Count") +
  theme_minimal()



for (trans_rings in transformations) {
  for (trans_length in transformations) {
    for (trans_diameter in transformations) {
      for (trans_height in transformations) {
        for (trans_shucked in transformations) {
          for (trans_viscera in transformations) {
            for (trans_shell in transformations) {
              
              
              data_trans <- data
              data_trans$rings <- transform_var(data, "rings", trans_rings)
              data_trans$length <- transform_var(data, "length", trans_length)
              data_trans$diameter <- transform_var(data, "diameter", trans_diameter)
              data_trans$height <- transform_var(data, "height", trans_height)
              data_trans$shucked_weight <- transform_var(data, "shucked_weight", trans_shucked)
              data_trans$viscera_weight <- transform_var(data, "viscera_weight", trans_viscera)
              data_trans$shell_weight <- transform_var(data, "shell_weight", trans_shell)
              
              
              model <- lm(rings ~ length + diameter + height + shucked_weight + viscera_weight + shell_weight, data = data_trans)
              
              
              formula_key <- paste(trans_rings, "rings ~", trans_length, "length +", trans_diameter, "diameter +", trans_height, "height +", trans_shucked, "shucked_weight +", trans_viscera, "viscera_weight +", trans_shell, "shell_weight")
              
              aic_model = step(model, direction = "backward", trace = FALSE)
              results[[formula_key]] <- AIC(aic_model)
            }
          }
        }
      }
    }
  }
}

best_model_key <- names(which.min(unlist(results)))
best_model_aic <- results[[best_model_key]]

cat("Best model is:", best_model_key, "with AIC:", best_model_aic, "\n")


library(caret)
# install.packages("caret")
cv_full = train(
  log(rings) ~ (length*length) +sqrt(diameter) + sqrt(height) + sqrt(shucked_weight) +sqrt(viscera_weight) + sqrt(shell_weight), data,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = FALSE
  )
)
cv_full

cv_simple = train(
  rings ~ length + diameter + height + shucked_weight + viscera_weight + shell_weight, data,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = FALSE
  )
)
cv_simple
