# Are Oelsner
# Simulations Lab 10

N = 100            # Number of CIs to generate
set.seed(28545965) # Student ID
n = 25              # Sample size
k = 8              # Shape
theta = 0.5        # Scale
m = k * theta      # True mean of distribution
confLevel = 0.99# Confidence Level

#sampleSize <- c(25, 49, 100, 1000)
#for(i in 1:4) {
#  n = sampleSize[i]
pdfFilename = paste("plotN:", n, "CL:", confLevel, ".pdf", sep = "")
cairo_pdf(pdfFilename, width = 8, height = 4)
par(mfrow = c(1,1))

plot(NULL, type = "l", xlab = "", ylab = "", xlim = c(1, 100), ylim = c(1, 8), main = paste(N, (confLevel*100),"% CIs: rgamma(", n, "shape = 8, scale = 0.5)"), sub = paste("initial seed = 28545965"), axes = FALSE)
axis(2, at = seq(1, 8, by = 1), las = 1)
lines(x = c(1, 100), y = c(m, m), lwd = 2)
for(i in 1:N) {
  x <- rgamma(n, shape = k, scale = theta)  # 95% confidence interval generator
  t <- t.test(x, mu = m, conf.level = confLevel)
  if(t$conf.int[1] > m || t$conf.int[2] < m) {
    points(x = i, y = t$estimate, type = "p", col = "red", cex = c(0.5, 0.5))
    lines(x = c(i, i), y = t$conf.int, col = "red")
  }
  else {
    points(x = i, y = t$estimate, type = "p", cex = c(0.5, 0.5))
    lines(x = c(i, i), y = t$conf.int)
  }
}

dev.off()
#}

