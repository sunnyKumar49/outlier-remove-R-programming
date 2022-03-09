# Arun Anirudhan

# https://statsandr.com/blog/outliers-detection-in-r/

# An outlier is a value or an observation that is distant from other observations, 
# that is to say, a data point that differs significantly from other data points.

# For instance, a human weighting 786 kg (1733 pounds) is clearly an error when encoding the weight of the subject.

library("ggplot2")
dat <- ggplot2::mpg
summary(dat$hwy)

hist(dat$hwy,
     xlab = "hwy",
     main = "Histogram of hwy",
     breaks = sqrt(nrow(dat))
) # set number of bins

ggplot(dat) +
  aes(x = hwy) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()

boxplot(dat$hwy,
        ylab = "hwy"
)

ggplot(dat) +
  aes(x = "", y = hwy) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

# I=[q0.25???1.5???IQR;q0.75+1.5???IQR]

boxplot.stats(dat$hwy)$out

# Thanks to the which() function it is possible to extract the row number corresponding to these outliers:
out <- boxplot.stats(dat$hwy)$out
out_ind <- which(dat$hwy %in% c(out))
out_ind

dat[out_ind, ]


boxplot(dat$hwy,
        ylab = "hwy",
        main = "Boxplot of highway miles per gallon"
)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

lower_bound <- quantile(dat$hwy, 0.025)
lower_bound

upper_bound <- quantile(dat$hwy, 0.975)
upper_bound

outlier_ind <- which(dat$hwy < lower_bound | dat$hwy > upper_bound)
outlier_ind

dat[outlier_ind, "hwy"]

dat[outlier_ind, ]


# Setting the percentiles to 1 and 99 gives the same potential outliers as with the IQR criterion.

lower_bound <- quantile(dat$hwy, 0.01)
upper_bound <- quantile(dat$hwy, 0.99)

outlier_ind <- which(dat$hwy < lower_bound | dat$hwy > upper_bound)

dat[outlier_ind, ]

subset(dat, hwy != 44)


iqr <- IQR(dat$hwy)
up <-  quantile(dat$hwy, 0.75) + 1.5*iqr # Upper Range  
low<- quantile(dat$hwy, 0.25) - 1.5*iqr # Lower Range???


eliminated <- subset(dat, dat$hwy > low & dat$hwy < up)
