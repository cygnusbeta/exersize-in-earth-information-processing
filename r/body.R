x <- read.csv('body.csv', header = F, col.names=c("Height", "Hand_Length", "Foot_Size"))
print(x)

means <- apply(x, 2, mean)
print('means')
print(means)

stds <- apply(x, 2, sd)
print('stds')
print(stds)

plot(x$Height, x$Hand_Length)
plot(x$Height, x$Foot_Size)
plot(x$Hand_Length, x$Foot_Size)
