# Function to determine if two tables are the same or not
sameTable <- function(A,B)
{
  if (is.null(A) & is.null(B)) return(TRUE)
  if (is.null(A)) return (FALSE)
  if (is.null(B)) return (FALSE)
  if (nrow(A) != nrow(B)) return(FALSE)
  if (nrow(A) == 0 & nrow(B) == 0) return(TRUE)
  if (sum(A != B) == 0) return(TRUE)
  return(FALSE)
}

