load("~/income_elec_state.Rdata")
ds <- income_elec_state
ds <- ds[order(ds$income), ]
#
kmres <- kmeans(ds, center=10)
plot(ds, col = kmres$cluster)
points(kmres$centers, col = 1:3, pch = 8)
wss <- numeric(15) 
for (i in 1:15) wss[i] <- sum(kmeans(ds, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
kmres <- kmeans(ds, center=4)
plot(ds, col = kmres$cluster)
points(kmres$centers, col = 1:3, pch = 8)
#
log_ds <- log10(ds)
kmres <- kmeans(log_ds, center=4)
plot(log_ds, col = kmres$cluster)
points(kmres$centers, col = 1:3, pch = 8)
wss <- numeric(15) 
for (i in 1:15) wss[i] <- sum(kmeans(log_ds, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
#
print(subset(ds, ds$income < 40000))
print(subset(ds, ds$income > 65000))
clear_ds <- subset(ds, ds$elec > 650 & ds$elec < 1300 & ds$income > 40000 & ds$income < 65000)
kmres <- kmeans(clear_ds, center=1)
plot(clear_ds, col = kmres$cluster)
wss <- numeric(15) 
for (i in 1:15) wss[i] <- sum(kmeans(clear_ds, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
#
map_order <- c('AL', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'DC', 'FL',
               'GA', 'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME',
               'MD', 'MA', 'MA', 'MA', 'MI', 'MI', 'MN', 'MS', 'MO',
               'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NY', 'NY',
               'NY', 'NC', 'NC', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA',
               'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'VA',
               'VA', 'WA', 'WA', 'WA', 'WA', 'WA', 'WV', 'WI', 'WY')
map_color <- kmres$cluster
map('state', col = map_color, fill=T)
#
# Hierarchical 
#
distances <- dist(clear_ds, method="euclidean")
reshc <- hclust(distances, method="complete")
plot(reshc, cex = 0.7, hang= -1)
rect.hclust(reshc, k = 4, border = 2:5)
#
distances <- dist(clear_ds, method="euclidean")
reshc <- hclust(distances, method="single")
plot(reshc, cex = 0.7, hang= -1)
rect.hclust(reshc, k = 4, border = 2:5)
#
distances <- dist(clear_ds, method="euclidean")
reshc <- hclust(distances, method="average")
plot(reshc, cex = 0.7, hang= -1)
rect.hclust(reshc, k = 4, border = 2:5)
#
distances <- dist(clear_ds, method="euclidean")
reshc <- hclust(distances, method="centroid")
plot(reshc, cex = 0.7, hang= -1)
rect.hclust(reshc, k = 4, border = 2:5)
