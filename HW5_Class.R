## HW5 Class/Methods

setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)


# Validity
setValidity("sparse_numeric", function(object) {
  errs <- character()
  
  len <- object@length
  pos <- object@pos
  val <- object@value
  
  # length slot
  if (length(len) != 1L || !is.integer(len) || is.na(len) || len < 0L)
    errs <- c(errs, "`length` must be a single non-negative integer.")
  
  # type checks
  if (!is.integer(pos))
    errs <- c(errs, "`pos` must be an integer vector.")
  if (!is.numeric(val))
    errs <- c(errs, "`value` must be a numeric vector.")
  
  # matching lengths
  if (length(pos) != length(val))
    errs <- c(errs, "`pos` and `value` must have the same length.")
  
  # bounds & sorting
  if (length(pos) > 0L) {
    if (any(pos < 1L))
      errs <- c(errs, "`pos` values must be >= 1.")
    if (any(pos > len))
      errs <- c(errs, "`pos` cannot exceed `length`.")
    if (is.unsorted(pos, strictly = TRUE))
      errs <- c(errs, "`pos` must be strictly increasing (sorted, no duplicates).")
  }
  
  # non-zero values
  if (any(val == 0))
    errs <- c(errs, "`value` cannot contain zeros.")
  
  # final
  if (length(errs)) errs else TRUE
})


# Coercion Methods

# numeric → sparse_numeric
setAs("numeric", "sparse_numeric", function(from) {
  # indices of nonzero values
  nz <- which(from != 0)
  vals <- from[nz]
  
  new("sparse_numeric",
      value  = as.numeric(vals),
      pos    = as.integer(nz),
      length = as.integer(length(from)))
})

# sparse_numeric → numeric
setAs("sparse_numeric", "numeric", function(from) {
  dense <- numeric(from@length)
  if (length(from@pos) > 0L)
    dense[from@pos] <- from@value
  dense
})


# Arithmetic Functions

# helper: check length match
.check_same_length <- function(x, y) {
  if (x@length != y@length)
    stop("Sparse vectors must have the same `length`.")
}

if (!isGeneric("sparse_add"))  setGeneric("sparse_add",  function(x, y, ...) standardGeneric("sparse_add"))
if (!isGeneric("sparse_sub"))  setGeneric("sparse_sub",  function(x, y, ...) standardGeneric("sparse_sub"))
if (!isGeneric("sparse_mult")) setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))
if (!isGeneric("sparse_crossprod"))
  setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

# addition
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            .check_same_length(x, y)
            pos_all <- sort(unique(c(x@pos, y@pos)))
            val_x <- setNames(x@value, x@pos)
            val_y <- setNames(y@value, y@pos)
            vals <- sapply(pos_all, function(p)
              (val_x[as.character(p)] %||% 0) + (val_y[as.character(p)] %||% 0))
            nz <- which(vals != 0)
            new("sparse_numeric",
                value  = as.numeric(vals[nz]),
                pos    = as.integer(pos_all[nz]),
                length = x@length)
          })

# subtraction
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            .check_same_length(x, y)
            pos_all <- sort(unique(c(x@pos, y@pos)))
            val_x <- setNames(x@value, x@pos)
            val_y <- setNames(y@value, y@pos)
            vals <- sapply(pos_all, function(p)
              (val_x[as.character(p)] %||% 0) - (val_y[as.character(p)] %||% 0))
            nz <- which(vals != 0)
            new("sparse_numeric",
                value  = as.numeric(vals[nz]),
                pos    = as.integer(pos_all[nz]),
                length = x@length)
          })

# multiplication
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            .check_same_length(x, y)
            # only overlapping positions matter
            common <- intersect(x@pos, y@pos)
            if (length(common) == 0L)
              return(new("sparse_numeric", value = numeric(), pos = integer(), length = x@length))
            val_x <- setNames(x@value, x@pos)
            val_y <- setNames(y@value, y@pos)
            vals <- as.numeric(val_x[as.character(common)] * val_y[as.character(common)])
            new("sparse_numeric",
                value  = vals,
                pos    = as.integer(common),
                length = x@length)
          })

# cross product (dot product)
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            .check_same_length(x, y)
            common <- intersect(x@pos, y@pos)
            if (length(common) == 0L) return(0)
            val_x <- setNames(x@value, x@pos)
            val_y <- setNames(y@value, y@pos)
            sum(val_x[as.character(common)] * val_y[as.character(common)])
          })


setMethod("+", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))
setMethod("-", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))
setMethod("*", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))


# Display and Plot

# show() method
setMethod("show", "sparse_numeric", function(object) {
  cat("sparse_numeric vector of length", object@length, "\n")
  if (length(object@pos) == 0L) {
    cat("  (all elements are zero)\n")
  } else {
    df <- data.frame(pos = object@pos, value = object@value)
    print(df, row.names = FALSE)
  }
})

# plot() method
setMethod("plot",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_length(x, y)
            par(mfrow = c(1, 1))
            plot(x@pos, x@value,
                 col = "blue", pch = 16, ylim = range(c(x@value, y@value)),
                 xlab = "Index Position", ylab = "Value",
                 main = "Non-zero elements of two sparse vectors")
            points(y@pos, y@value, col = "red", pch = 17)
            legend("topright",
                   legend = c("x", "y"),
                   col = c("blue", "red"),
                   pch = c(16, 17))
          })



# Extra Method

# sum() method
setMethod("sum", "sparse_numeric", function(x, ..., na.rm = FALSE) {
  if (na.rm) sum(x@value, na.rm = TRUE) else sum(x@value)
})