library(WeightSVM)

# Generate example data
set.seed(123)
x1 = c(rnorm(50, mean = -1), rnorm(50, mean = 1))
x2 = c(rnorm(50, mean = 1), rnorm(50, mean = -1))
y = c(rep(-1, 50), rep(1, 50))
data = data.frame(x1 = x1, x2 = x2, y = factor(y))

# Fit standard soft-margin SVM
model = wsvm(x = data[, c("x1", "x2")], y = data$y, weight = rep(1, 100), kernel = "linear", cost = 10, scale = FALSE)
cf = coef(model)

# Plot data and separating hyperplane
plot(x2 ~ x1, data = data[which(data[, 3] == 1), ], col = y, pch = 1, main = "Standard SVM")
points(x2 ~ x1, data = data[which(data[, 3] == -1), ], col = y, pch = 2)
abline(-cf[1] / cf[3], -cf[2] / cf[3], col = "red")
abline(-(cf[1] + 1) / cf[3], -cf[2] / cf[3], col = "blue", lty = "dashed")
abline(-(cf[1] - 1) / cf[3], -cf[2] / cf[3], col = "blue", lty = "dashed")
sv_data = data[rownames(model$SV), ]
points(x2 ~ x1, data = sv_data[which(sv_data[, 3] == -1), ], col = y, pch = 17)
points(x2 ~ x1, data = sv_data[which(sv_data[, 3] == 1), ], col = y, pch = 16)

# Define different weights for the positive and negative samples
weights = ifelse(y == 1, 3, 1)

# Fit weighted soft-margin SVM
modelw = wsvm(x = data[, c("x1", "x2")], y = data$y, weight = weights, kernel = "linear", cost = 10, scale = FALSE)
cf = coef(modelw)

# Plot data and separating hyperplane
plot(x2 ~ x1, data = data[which(data[, 3] == 1), ], col = y, pch = 1, main = "Weighted SVM")
points(x2 ~ x1, data = data[which(data[, 3] == -1), ], col = y, pch = 2)
abline(-cf[1] / cf[3], -cf[2] / cf[3], col = "red")
abline(-(cf[1] + 1) / cf[3], -cf[2] / cf[3], col = "blue", lty = "dashed")
abline(-(cf[1] - 1) / cf[3], -cf[2] / cf[3], col = "blue", lty = "dashed")
sv_data = data[rownames(modelw$SV), ]
points(x2 ~ x1, data = sv_data[which(sv_data[, 3] == -1), ], col = y, pch = 17)
points(x2 ~ x1, data = sv_data[which(sv_data[, 3] == 1), ], col = y, pch = 16)
