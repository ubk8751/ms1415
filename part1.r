# Load necessary libraries
suppressPackageStartupMessages({
  library(gamlss, quietly = TRUE)
  library(VGAM, quietly = TRUE)
  library(caret, quietly = TRUE)
})

sink(file = "out.log")
# Load provided functions
source("funcs/rreg.fit.R")


# Load data
data <- read.csv("data/image_SF.csv")

# Print some metadata to m_ake sure file was read
dims <- dim(data)
print(paste("Image dimensions:", paste(dims, collapse = "x")))

itm <- data[1, 1]
print(paste("Value at [1,1]:  ", itm))

print("")

# Sub matrices are defined by [rows, cols]

# 1.  Deﬁne the tested regions;
m_a <- data[20:40, 20:40] # Should be water
colnames(m_a) <- 20:40
vectorized_a <- as.vector(m_a)

m_b <- data[20:40, 320:340] # Should be forest
colnames(m_b) <- 320:340
vectorized_b <- as.vector(m_b)

m_c <- data[160:180, 320:340] # Should be city
colnames(m_c) <- 320:340
vectorized_c <- as.vector(m_c)

#sink(file = "Matrix_content.log")
#print(vectorized_a)
#sink(file = "out.log")

# Step 2: Create the observed signal
observed_signal <- c(
  vectorized_a,
  vectorized_b,
  vectorized_c
)

sink(file = NULL)

# Step 3: Check data behavior
combined_signal <- unlist(observed_signal)

#sink(file = "combined_signal.log")
#print(combined_signal)
#sink(file = "out.log")

plot(combined_signal, type = "l", main = "Combined Regions")

sink(file = "out.log", append = TRUE)

# Step 4: Create dummy covariates
x2 <- c(
  rep(0, length(vectorized_a)),
  rep(1, length(vectorized_b)),
  rep(0, length(vectorized_c))
)
x3 <- c(
  rep(0, length(vectorized_a)),
  rep(0, length(vectorized_b)),
  rep(1, length(vectorized_c))
)

names(observed_signal) <- NULL
observed_signal <- unlist(observed_signal)

model_data <- data.frame(x2 = x2, x3 = x3, observed_signal = observed_signal)

# Model 1: GLM with Gamma distribution
gamma <- glm(observed_signal ~ x2 + x3,
  data = model_data,
  family = Gamma(link = "log")
)

# Model 2: GLM with Rayleigh distribution
rayleigh <- rr.fit(x = model_data[, c("x2", "x3")],
                       y = model_data$observed_signal, diag = 0)

# Model 3: GLM with norm_al distribution
norm_al <- glm(observed_signal ~ x2 + x3,
  data = model_data,
  family = gaussian(link = "identity")
)

# 6.  Perform the detection theory. Are the covariates signifiant to the
#     model? Are they introducing information about variations in y?
detect_ground <- function(beta) {
  if (beta[2] != 0 || beta[3] != 0) {
    cat("Ground type is detected.\n")
  } else {
    cat("Ground type is not detected.\n")
  }
}

# 7.  Test the residuals. Is the model correctly speciﬁed? (Consider a
#     residual vs index plot and check evidence of normality with a
#     histogram, for example).
test_residuals <- function(residuals, model_name) {
  # Residuals vs Index Plot
  plot(residuals ~ seq_along(residuals),
    main = paste("Residuals vs Index Plot for", model_name),
    xlab = "Index",
    ylab = "Residuals"
  )

  # Normality Check
  hist(residuals,
    main = paste("Histogram of Residuals for", model_name),
    xlab = "Residuals"
  )

  cat("Residual info for", model_name, "\n")
  cat("Standard Deviation:", sd(residuals), "\n")
  cat("Mean:", mean(residuals), "\n")
  cat("Median:", median(residuals), "\n")
  cat("Min:", min(residuals), "\n")
  cat("Max:", max(residuals), "\n")
}

# 8.  Check the relationship between the mean of y and the dummy covariates.
check_relationship <- function(y, x, covariate_name) {
  cat("Relationship between the observed signal and", covariate_name, "\n")
  # Scatterplot
  plot(x, y,
    xlab = covariate_name, ylab = "Mean of y",
    main = paste("Relationship between Mean of y and", covariate_name)
  )

  # Regression Analysis
  print(paste("lm_result", covariate_name))
  lm_result <- lm(y ~ x)
  print(summary(lm_result))
  cat("\n")
}

# Covariate x2
check_relationship(
  model_data$observed_signal,
  model_data$x2,
  "x2"
)

# Covariate x3
check_relationship(
  model_data$observed_signal,
  model_data$x3,
  "x3"
)

# 9.  Verify the determination coeﬃcient for the ﬁtted models.
verify_r_squared <- function(model) {
  model_summary <- summary(model)
  null_deviance <- model_summary$null.deviance
  residual_deviance <- model_summary$deviance
  r_squared <- 1 - (residual_deviance / null_deviance)
  cat("R-squared:", r_squared, "\n")
}

# 10. In conclusion: what is the most accurate model for such data,
#     considering detection and modeling evaluation?

# Results:
cat("Model 1 (GLM with Gamma distribution):\n")
summary(gamma)
detect_ground(coef(gamma))
test_residuals(residuals(gamma), "Gamma distribution")
verify_r_squared(gamma)

cat("\nModel 2 (GLM with Rayleigh distribution):\n")
print(rayleigh$model)
detect_ground(rayleigh$coef)
test_residuals(rayleigh$resid1, "Reyleigh distribution, quantile residuals")
test_residuals(rayleigh$resid2, "Reyleigh distribution, standardized residuals")
test_residuals(rayleigh$resid3, "Reyleigh distribution, deviance residuals")
cat(paste("R-squared:", rayleigh$R2, "\n"))


cat("\nModel 3 (GLM with normal distribution):\n")
summary(norm_al)
detect_ground(coef(norm_al))
test_residuals(residuals(norm_al), "Normal distribution")
verify_r_squared(norm_al)

sink(file = NULL)