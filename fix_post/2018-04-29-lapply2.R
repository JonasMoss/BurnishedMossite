x = 0

f = function(y) {
  assign("x", x + 1, envir = parent.frame(n = 1))
  y + x
}

lapply(1:3, f)


f = function(y) {
  envir = parent.frame()
  assign("x", x + 1, envir = envir)
  y + x
}

x = 0
f(1)
list(f(1), f(2), f(3))

x = 0
lapply(1:3, f)


f = function(x) substitute(x)

x = 1
y = 2
z = 3

list(f(x), f(y), f(z))
lapply(alist(x, y, z), f)


lapply2 = function(X, FUN, ...) {
  FUN = substitute(FUN)
  dots = as.list(substitute((...))[-1])
  envir = parent.frame()
  lapply(X, function(x) eval(as.call(c(FUN, x, dots)), envir = envir))
}




x = 0

f = function(y) {
  assign("x", x + 1, envir = parent.frame(n = 1))
  y + x
}

lapply(1:3, f)
lapply2(1:3, f)


g = function(x) substitute(x)
h = function(x) x



f = function(x, ...) paste(list(...), deparse(substitute(x)))

x = 1
y = 2
z = 3

list(f(x), f(y), f(z))
lapply(alist(x, y, z), f, "a")
lapply2(alist(x, y, z), f, "a", "b")

s = function(x, ...) {
  dots = as.list(substitute((...))[-1])
  paste(dots, deparse(substitute(x)))
}

lapply(alist(x, y, z), s, x, y)
lapply2(alist(x, y, z), s, x, y)


t = function(X, FUN, ...) {
  FUN = substitute(FUN)
  dots = as.list(substitute((...))[-1])
  envir = parent.frame()
  lapply(X, function(x) as.call(c(FUN, x, dots)))
}

t(alist(x, y, z), f, x, y)
