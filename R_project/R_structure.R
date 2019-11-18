# Atomic vector
a <- c(1,4,5,2, "a")
class(a)

# List vector
b <- list(1,4,3,5, "a")
class(b)

b[[1]]

# matrix
m <- matrix(1:4, nrow = 2,ncol = 2)
m

# Base types
x <- c(1,2,3,4)
pryr::otype(c)

b <- c(4,5,6,7)

df <- as.vector(rbind(x,b))
pryr::otype(df)
df
