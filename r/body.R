# Title     : TODO
# Objective : TODO
# Created by: Cygnus
# Created on: 1/27/2020
x <- read.csv('body.csv', header = F, col.names=c("Height", "Hand_Length", "Foot_Size"))
print(x)

means <- apply(x, 2, mean)
print('means')
print(means)

stds <- apply(x, 2, sd)
print('stds')
print(stds)
