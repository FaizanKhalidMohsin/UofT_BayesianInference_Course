



z = rbeta(10000, 6, 16)
hist(z)

plot(density(z)) # This is the prior

# Want a point estimate: then what about the mean.
 mean(z)

sd(z)

quantile(z, c(0.25, .975)) # From the sample of 10000 rbetas. 

qbeta( c(0.25, 0.975),  6, 16) # Theoretical values. 

xxx = seq(0.01, 0.9, 0.01)

plot(xxx, dbeta(xxx, 6, 16)) # The prior

zz = rbeta( 10000, 0.5, 0.5) # a=b=1/2 gives Jeffery's Prior. So beta can 
                             # give Jeffery's Prior
hist(zz)

zzz = rbeta(10000, asin((0.5)^0.5), asin((0.5)^0.5))
hist(zzz)


