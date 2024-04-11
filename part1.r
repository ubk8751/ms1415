# Load data
data <- read.csv("data/image_SF.csv")

# Print some metadata to make sure file was read
dims <- dim(data)
print(dims)

itm <- data[1, 1]
print(itm)


# 1.  Deﬁne the tested regions;
mA <- data[9:17, 14:21]
colnames(mA) <- 14:21
# print("A:")
# print(mA)

mB <- data[8:16, 170:177]
colnames(mB) <- 170:177
# print("B:")
# print(mB)

mC <- data[62:70, 112:119]
colnames(mC) <- 112:119
# print("C:")
# print(mC)

# 2.  Create the observed signal with the vectorized pixels of
#     these three regions using windows of 20 × 20 pixels;

# 3.  Check the data behavior to verify if the considered regression
#     models are suitable approaches to ﬁt such data;

# 4.  Create two dummy covariates;

# 5.  Fit the selected regression models. You can use the functions
#     available on Canvas.

# 6.  Perform the detection theory. Are the covariates signiﬁcant to the
#     model? Are they introducing information about variations in y?

# 7.  Test the residuals. Is the model correctly speciﬁed? (Consider a
#     residual vs index plot and check evidence of normality with a
#     histogram, for example).

# 8.  Check the relationship between the mean of y and the dummy covariates.

# 9.  Verify the determination coeﬃcient for the ﬁtted models.

# 10. In conclusion: what is the most accurate model for such data,
#     considering detection and modeling evaluation?
