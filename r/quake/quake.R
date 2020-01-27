x <- read.csv('quake/query.csv')
x <- data.frame(x$longitude, x$depth, x$mag)
colnames(x) <- c('longitude', 'depth', 'mag')
# print(x)
means <- apply(x, 2, mean)
print('means')
print(means)

stds <- apply(x, 2, sd)
print('stds')
print(stds)

plot(x$longitude, x$depth)
plot(x$longitude, x$mag)
plot(x$depth, x$mag)

print('correlation matrix')
cor(cbind(x))

print('--- pca ---')
pca <- prcomp(x, scale=TRUE)
# print(pca)

print('固有値(の正の平方根)')
print(pca$sdev)

print('固有ベクトル')
print(pca$rotation)

print('主成分得点')
print(pca$x)

biplot(pca)