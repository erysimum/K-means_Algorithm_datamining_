install.packages("klaR")
library(klaR)
mss= read.csv("/Users/RRChhabra/Downloads/Datawarehousefiles/mushrooms.csv", stringsAsFactors = TRUE)
dim(mss)
str(mss)

# checking for na/infinity values in the data
indexes <- apply(mss, 2, function(x) any(is.na(x) | is.infinite(x)))
colnames(mss)[indexes]

# Data count
table(mss$type)

#Remove the type class (we don't need the responding class), and veil_type attribute or column because it has zero variance.
x<- subset(mss, select = -c(type, veil_type))
str(x)


x.ohe <- model.matrix(~.-1, data=x)
x.ohe
str(x.ohe)



set.seed(20) #for reproducibility
#nstart = 50, indicates R will run 50 different random starting assignments and selects the lowest within cluster variation
result.kmean = kmeans(x.ohe, 2, nstart = 50, iter.max = 50) 

result.kmean.mm <- table(mss$type, result.kmean$cluster)
result.kmean.mm

#Calculate purity
purity.kmean <- sum(apply(result.kmean.mm, 2, max)) / nrow(x)
purity.kmean   # Accuracy is 89.83%

#p 1756 2160


#Kmodes:
result.kmode <- kmodes(x, 2, iter.max = 100, weighted = FALSE)
result.kmode.mm <- table(mss$type, result.kmode$cluster)
result.kmode.mm
purity.kmode <- sum(apply(result.kmode.mm, 2, max)) / nrow(x)
purity.kmode
