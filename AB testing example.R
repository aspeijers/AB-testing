# load packages
library(ggplot2)

# default values
p1    <- 0.4
p2    <- 0.45
signif <- 0.05
beta_err <- 0.2
#n     <- 10^4

# calculate missing parameter: sample size, alpha or beta
# (take difference in CR to always be defined)
if (!exists("beta_err")) {
    # calculate beta/power
    cat("calculating beta...", "\n")
    beta_err <- pnorm( (p1 - p2 + qnorm(1-signif)*sqrt(p1*(1-p1)/n)) * sqrt(n/(p2*(1-p2))) )
} else if (!exists("signif")) {
    # calculate alpha
    cat("calculating alpha...", "\n")
    signif <- 1 - pnorm( (p2 - p1 + qnorm(beta)*sqrt(p2*(1-p2)/n)) * sqrt(n/(p1*(1-p1))) )
} else if (!exists("n")) {
    # calculate sample size
    cat("calculating sample size...", "\n")
    n <- ( qnorm(1-signif)*sqrt(p1*(1-p1)) - qnorm(beta_err)*sqrt(p2*(1-p2)))^2 / (p2-p1)^2
} else {
    cat("all parameters defined")
} 

# calculate distribution parameters
mu_A <- n*p1/n
mu_B <- n*p2/n

sd_A <- sqrt( n*p1*(1-p1) )/n
sd_B <- sqrt( n*p2*(1-p2) )/n




# generate data from distributions and estimate kernel densities
x1 <- rnorm( n=n, mean=mu_A, sd=sd_A )
x2 <- rnorm( n=n, mean=mu_B, sd=sd_B )

x1_dens <- density(x1)
x2_dens <- density(x2)

dist <- as.data.frame( cbind(x1_dens$x, x1_dens$y/n, x2_dens$x, x2_dens$y/n) )

# calc intersections for alpha
int_x <- qnorm(p=(1-signif), mean=mu_A, sd=sd_A)
int_y <- dnorm(x=int_x, mean=mu_A, sd=sd_A)/n
int_y_B <- dnorm(x=int_x, mean=mu_B, sd=sd_B)/n

# Calculate beta
#beta_err <- pnorm(q=int_x, mean=mu_B, sd=sd_B)


# find area to shade
shade_A <- rbind( c(int_x, 0),
                  subset(dist[,1:2], V1 > int_x),
                  c(dist[nrow(dist),1],0) )
shade_B <- rbind( c(dist[1,3],0),
                  subset(dist[,3:4], V3 < int_x),
                  c(int_x, 0) )
#names(shade_B) <- c("V3", "V4")

# plot distributions together
ggplot(dist) +
    geom_line(aes(x=V1, y=V2, col="version_a")) +
    geom_line(aes(x=V3, y=V4, col='version_b')) +
    geom_vline(xintercept=int_x) +
    #geom_segment(aes(x = int_x, y = 0, xend = int_x, yend = int_y), col="black") +
    geom_polygon(data=shade_A, aes(x=V1, y=V2, fill="alpha"), alpha=0.3) +
    geom_polygon(data=shade_B, aes(x=V3, y=V4, fill="beta"), alpha=0.3) +
    labs(x="Conversion Rate (CR)", y="Density") +
    #annotate("text", x=mu_B, y=max(int_y, int_y_B)/3, label = paste(round(signif,2)), col="red") +
    #annotate("text", x=mu_A, y=max(int_y, int_y_B)/3, label = paste(round(beta_err,2)), col="blue") +
    annotate("text", x=Inf, y=Inf, hjust=1.2, vjust=1.2, label=paste(round(n,0), "samples"), size=8) +
    scale_colour_manual(name = "Distributions",
                        breaks = c("version_a", "version_b"),
                        values = c('red', 'blue'),
                        labels = c("Control (A)", "Treatment (B)")) +
    scale_fill_manual(name = "",
                      breaks = c("alpha", "beta"),
                      values = c('red', 'blue'),
                      labels = c("alpha", "beta"))

