## I simply set the input x as a matrix
## and then set the solved value "s" as a null
## then I changed every reference to "mean" to "solve"

make_cache <- function(x = matrix(sample(1:100,9),3,3)) {
  s <- NULL
  set <- function(y) {
    m <<- y
    s <<- NULL
  }
  get <- function() m
  set_solve <- function(solve) s <<- solve
  get_solve <- function() s
  list(set = set, get = get,
       set_solve = set_solve,
       get_solve = get_solve)
}

cache_solve <- function(x, ...) {
  s <- x$get_solve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$set_solve(s)
  s
}

# Example usage
cache <- make_cache()

# Set a matrix
set_m <- function(matrix_value) {
  cache$set(matrix_value)
}

# Get the matrix
get_m <- function() {
  cache$get()
}

# Set the inverse
set_inv <- function(solve_value) {
  cache$set_solve(solve_value)
}

# Get the inverse
get_inv <- function() {
  cache$get_solve()
}

# Example usage
matrix_value <- matrix(c(1, 2, 3, 4), 2, 2)  # Example matrix
set_m(matrix_value)                          # Set the matrix
cached_matrix <- get_m()                     # Retrieve the matrix
inverse_matrix <- solve(cached_matrix)        # Compute the inverse
set_inv(inverse_matrix)                       # Cache the inverse
cached_inverse <- get_inv()                   # Retrieve the cached inverse
