# Are Oelsner
# Simulations Lab 10b

N = 100            # Number of CIs to generate
initialSeed = 5612127090  # Student ID
numJobs = 65536    # number of jobs
#numJobs = 500
warmup = 1024      # length of warmup period to be removed
lambda = 1         # lambda
nu = 10/9          # nu
m = 1/(nu - lambda)# theoretical mean sojourn time for ssq
# b = batch size
# n = number of batches (numJobs/b)
ymin = 6
ymax = 14
yby = 2
confLevel = 0.95# Confidence Level

batchSizes <- c(64, 128, 256, 512, 1024, 2048)

for(i in 1:6) {
  set.seed(561212709) # Student ID
  b = batchSizes[i]
  n = numJobs/b
  
  pdfFilename = paste("sojournPlotB:", b, "N:", n, ".pdf", sep = "")
  cairo_pdf(pdfFilename, width = 8, height = 4)
  par(mfrow = c(1,1))
 
  plot(NULL, type = "l", xlab = "", ylab = "", xlim = c(1, 100), ylim = c(ymin, ymax), main = paste(N, (confLevel*100),"% CIs SSQ Sojourn Times"), axes = FALSE)
  axis(2, at = seq(ymin, ymax, by = yby), las = 1)
  lines(x = c(1, 100), y = c(m, m), lwd = 2)
  misses = 0
  for(i in 1:N) {
    output <- ssq(numJobs + warmup, showOutput = FALSE, saveSojournTimes = TRUE)
    sojourns <- output$sojournTimes[-(1:warmup)] #removes warmup
    batchMeans <- numeric(n)
    for(j in 1:n) {
      batch <- sojourns[(1:b) + (b * (j-1))]
      batchMeans[j] <- mean(batch)
    }
    t <- t.test(batchMeans, mu = m, conf.level = confLevel)
    if(t$conf.int[1] > m || t$conf.int[2] < m) {
      lines(x = c(i, i), y = t$conf.int, col = "red")
      misses = misses + 1
    }
    else {
      lines(x = c(i, i), y = t$conf.int)
    }
  }
  legend("topright", legend = c(paste("batch size = ", b), paste("misses = ", misses), paste("seed=", seed)))#, cex = c(.5, .5))
  dev.off()
}

