# Load necessary libraries
library(gamlss)
library(caret)

# Load provided functions
source("funcs/rreg.fit.R")


# Load data
data <- read.csv("data/image_SF.csv")

# Print some metadata to m_ake sure file was read
dims <- dim(data)
print(dims)

itm <- data[1, 1]
print(itm)

# 1.  Deﬁne the tested regions;
m_a <- data[9:17, 14:21]
colnames(m_a) <- 14:21
vectorized_a <- as.vector(m_a)

m_b <- data[8:16, 170:177]
colnames(m_b) <- 170:177
vectorized_b <- as.vector(m_b)

m_c <- data[62:70, 112:119]
colnames(m_c) <- 112:119
vectorized_c <- as.vector(m_c)

# Step 2: Create the observed signal
observed_signal <- c(vectorized_a,
                     vectorized_b,
                     vectorized_c)

# Step 3: Check data behavior
# Plot individual "regions"
for (region in names(observed_signal)) {
  plot(observed_signal[[region]], type = "l", main = paste("Region", region))
}

# Plot the combined signal
combined_signal <- unlist(observed_signal)
plot(combined_signal, type = "l", main = "Combined Regions")

# Step 4: Create dummy covariates
x2 <- c(rep(0, length(vectorized_a) + length(vectorized_c)),
        rep(1, length(vectorized_b)))
x3 <- c(rep(0, length(vectorized_a) + length(vectorized_b)),
        rep(1, length(vectorized_c)))

names(observed_signal) <- NULL
observed_signal <- unlist(observed_signal)

model_data <- data.frame(x2 = x2, x3 = x3, observed_signal = observed_signal)

# Model 1: GLM with Gamma distribution
glm_gamma <- glm(observed_signal ~ x2 + x3,
                 data = model_data,
                 family = Gamma(link = "log"))

# Model 2: GLM with Rayleigh distribution
glm_rayleigh <- rr.fit(x = model_data[, c("x2", "x3")],
                       y = model_data$observed_signal)

# Model 3: GLM with norm_al distribution
glm_norm_al <- glm(observed_signal ~ x2 + x3,
                   data = model_data,
                   family = gaussian(link = "identity"))

# 6.  Perform the detection theory. Are the covariates signifiant to the
#     model? Are they introducing information about variations in y?
detect_ground <- function(model) {
  beta <- coef(model)
  if (beta[2] != 0 | beta[3] != 0) {
    cat("Ground type is detected.\n")
  } else {
    cat("Ground type is not detected.\n")
  }
}

cat("Model 1 (GLM with Gamma distribution):\n")
detect_ground(glm_gamma)

cat("\nModel 2 (GLM with Rayleigh distribution):\n")
detect_ground(glm_rayleigh)

cat("\nModel 3 (GLM with normal distribution):\n")
detect_ground(glm_norm_al)


# 7.  Test the residuals. Is the model correctly speciﬁed? (Consider a
#     residual vs index plot and check evidence of normality with a
#     histogram, for example).

# 8.  Check the relationship between the mean of y and the dummy covariates.

# 9.  Verify the determination coeﬃcient for the ﬁtted models.

# 10. In conclusion: what is the most accurate model for such data,
#     considering detection and modeling evaluation?
