k <- 0
for (i in 1:50){
    #print(paste("k is", k, ", i is", i, sep = " "))
    k = k + i
}
print(k)

circumference <- function(radius=1) {
    2*pi*radius
}

for (i in 1:5) {
    print(circumference(i))
}