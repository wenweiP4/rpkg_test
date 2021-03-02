# array is a vector or list with 2+ dimensions
# (Matrix is a 2-dimension Array. Array is like stacked matrices)
anVec <- c("twas","brillig","and","the","slithey","toves","did","gyre","and","gimble","in","wabe")

# array(data_vector,dim_vector)
# dimension is expressed as row, column, z-index
anArray <- array(anVec,c(2,3,2))
anArray

# indexing into the array
# anArray[2,3,3] # this fails because there is not a third level
anArray[2,3,2]
anArray[2,3,2] <- "a new value"
anArray[2,,2] # omission returns the entire range. Here - all columns of row 2, table 2


# dimnames
my.row.names <- c("up","down")
my.column.names <- c("left","middle","right")
my.table.names <- c("behind","in front")
dimnames(anArray) <- list(my.row.names,my.column.names,my.table.names)
anArray
anArray["down",3,2]
