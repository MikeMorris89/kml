# Creating an animation using R
# September 12, 2016
# By Rene Essomba
# 
# inShare
# 1
# (This article was first published on DataScience+, and kindly contributed to R-bloggers)
# 179
# SHARES
# Share
# Tweet
# In this post, I will show you how to create an animation using R and ffmpeg. The idea to do so is pretty simple:
#   
#   Generate a number of snapshots
# Combine them in a video file using ffmpeg
# The best way to learn about the art of animation is by doing it ourselves, so our work example is the infamous Broken Stick Problem.
# 
# Here is the scenario:
#   Let there be a stick of length 1. Pick two points uniformly at random along the stick and break the stick at those points. What is the probability of the three resulting pieces being able to form a triangle? In the first part of this post, I will provide my analytical approach to calculate the probability. Then in the second part, I will use Monte Carlo simulation to reach to the answer.
# 
# Analytical Solution
# The triangle inequality states that for any triangle, the sum of the lengths of any two sides must be greater than the length of the remaining side. Let’s suppose that \(X_2 > X_1\), the vertices of our triangle could have the following lengths: \(X_1\text{; } X_2 – X_1 \text{ and } 1 – X_2\). For the three pieces to form a triangle, none of them should have a length less than half. In order words the following conditions must be satisfied: (a) \(X_1 < \dfrac{1}{2}\) (b) \(X_2 – X_1 < \dfrac{1}{2}\) (c) \(1 – X_2 < \dfrac{1}{2}\) (d) Sum of the 3 vertices is equal to 1.
# 
# The above conditions could also be established for the case where \(X_1 > X_2\). The diagram below displays the above conditions. Our areas of interest are derived by finding the intersection where conditions (a)-(d) are valid, hence giving us the probability of obtaining a triangle under such conditions.
# 
# Solution Triangle
# 
# Looking at the two areas (red and blue) above, the probability of obtaining a triangle is:
#   
#   \(P(triangle) = Area(X_1 > X_2) + Area(X_2 > X_1) = 1/8 + 1/8 = 1/4\)
# 
# Simulated Solution
# The estimation of the probability is done using a Monte Carlo algorithm. Here is my proposed approach to solve this problem:
#   
#   Generate 2 random points from a uniform distribution between 0 & 1
# Check whether the 2 two values obtained satisfy conditions (a)-(d); if yes give 1 to that pair otherwise give 0
# Compute the cumulative empirical probability
# Repeat the above steps multiple times (in this case 1000 times)
# The R-code is as follows:
#   
# suppressPackageStartupMessages(require(ggplot2))
# set.seed(2016)
# index_Score <- function(){
#   # Picking 2 points randomly on the stick at the same time
#   x <- runif(n = 2, min = 0, max = 1) 
#   a <- min(x) # first point
#   b <- max(x) # second point
#   # pieces of the stick with their respective length
#   pieces <- c(a, b-a, 1-b)
#   cond1 <- sum(pieces[c(1,2)]) > pieces[3] # condition # 1
#   cond2 <- sum(pieces[c(1,3)]) > pieces[2] # condition # 2
#   cond3 <- sum(pieces[c(3,2)]) > pieces[1] # condition # 3
#   combine_conds <- ifelse(cond1 & cond2 & cond3, 1, 0) # if all 3 conditions are satisfied
#   return(combine_conds)
# }
# 
# cnt <- c()
# total <- 100
# for(k in 1:total) cnt = c(cnt, index_Score())
# df <- setNames(data.frame(1:total, rep(0, total)), c("Incrmt","Probs"))
# for (i in 1:total)  df$Probs[i] <- sum(cnt[1:i])/i

#runGist(8608815)

#The final step consists of generating the snapshots (1000 of them) depicting the iterations and the cumulative probabilities and encoding these images into a video format.
if(1==0){
  for(i in 1:total) {
    sub_df <- subset(df, df$Incrmt <= i)
    simul_plot <- iteratemc(i)
    ggsave(plot = simul_plot, filename = paste(sprintf("images/brokenstick_%02d",i),".png", sep = ""), limitsize = FALSE)
    rm(sub_df)
    dev.off()
  }
}


iteratemc<-function(i=100){
  sub_df <- subset(montecarlodf, montecarlodf$Incrmt <= i)
  sub_df$i<-sub_df$Incrmt
  sub_df$set<-1
  sub_df$mean<-cumsum(sub_df$Probs)/cummax(sub_df$i)
  sub_df$sd<-sapply(sub_df$i,function(x){
    probs<-sub_df[sub_df$i<=x,]$Probs
    my.mean<-sub_df[sub_df$i==x,]$mean
    msq<-(probs-my.mean)^2
    my.mean.msq<-base::mean(msq)
    sqrt(my.mean.msq)
  })
  #ds <- plyr::ddply(sub_df, "set", plyr::summarise, mean = cummean(Probs), sd = cumsd(Probs))
  df <- data.frame(
    gp = sub_df$i
    ,y =as.numeric(sub_df$Probs)
  )
  dfsd <- data.frame(
    gp = sub_df$i
    ,y =as.numeric(sub_df$Probs)
    ,mean = sub_df$mean
    ,sd = sub_df$sd
  )
  
  p<-ggplot() +
    geom_point(data = dfsd, aes(gp, y)) +
    geom_point(data = dfsd, aes(gp, mean), colour = 'red', size = 3) + 
    labs(x = "iterations", y = "Probabilities", title = "Monte Carlo Simulation")
    # geom_errorbar(
    #   data = ds,
    #   aes(gp, mean, ymin = mean - sd, ymax = mean + sd),
    #   colour = 'red',
    #   width = 0.4
    # )
  
  return(p)
}

# for(i in 1:total) {
#   sub_df <- subset(df, df$Incrmt <= i)
#   simul_plot <- iteratemc(i)
#   plot(simul_plot)
# }
# ffmpeg is quite a nice tool which can be installed in Linux, Windows and Mac OS. The following command line in a terminal shell produces a video file in the mpeg format
# 
# ffmpeg -r 10 -i broken_stick_%02d.png -b:v 20M BrokenStick_video.mp4
# -r 10 controls the rate of frames per seconds (10 fps here) and -b:v 20M sets the bitrate in the output file.
# 
# The result is the following video
# 
# And voila! Done.
# 
# When it comes to rendering great animations either to understand the dynamic behind data or just to impress your manager and /or colleagues, R provides multiples options to do so. The best R-package for animations that I would recommend is animation which provides functions to save animations in Flash, GIF, HTML pages, PDFs and videos.
# If you have enjoyed this post, please drop me some comments/suggestions.