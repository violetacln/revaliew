
library(testit)
library(testthat)

# as seen on:
# https://github.com/yihui/testit/blob/master/man/assert.Rd
# we can try examples like

has_warning(2-3)

has_error(1+"a")
has_error(stop("err"), silent=TRUE)

assert('A non-exported function works', {
  res = utility_foo(x = 'abcd', y = 1:100)
  (is.character(res))
})


try(assert("logical(0) cannot pass", 1 == integer(0)))
stopifnot(1 == integer(0)) # it's OK!

# a compound expression
try(assert("this if statement returns TRUE", if (TRUE) {
  x = 1
  x == 2
}))

# no message
assert(!FALSE, TRUE, is.na(NA))

#test_pkg()

