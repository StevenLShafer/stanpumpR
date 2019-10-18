# Find the closest entry in vector (vector) to the value x. 
# Return the index of X. If there are ties, return the first
closest <- function(x, vector)
{
  which(abs(x-vector) == min(abs(x-vector)))[1]
}
