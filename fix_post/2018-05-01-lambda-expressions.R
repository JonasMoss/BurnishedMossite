env = new.env()
z = 0
env2 = new.env()
delayedAssign("var", z + 2, assign.env = env)
substitute(var, env = env)

make_promise = function(name,
                        promise, 
                        eval_env = parent.frame(), 
                        assign_env = parent.frame()) {
  
  
  call = as.call(c(delayedAssign, x = name, value = promise,
                   eval.env = eval_env, assign.env = assign_env))
  eval(call)
}

make_promise("x", quote(y^2))


f = function(x, y) match.call()
f(y = x <- 3 -> z)

l = function(expr) {
  call = as.list(sys.call()[-1])[[1]]
  stopifnot(identical(call[[1]], quote(`<-`)))
  f = function() NULL
  eval(call[[3]])
  formals(f) = eval(call[[3]])
  body(f) = call[[2]]
  environment(f) = parent.frame()
  f
}

L = function(expr) {
  call = as.list(sys.call()[-1])[[1]]
  stopifnot(identical(call[[1]], quote(`<-`)))
  f = function() NULL
  formals(f) = alist(x = )
  names(formals(f)) = deparse(call[[3]])
  body(f) = call[[2]]
  environment(f) = parent.frame()
  f
}

`%l%` = function(par, expr) {
  f = function() NULL
  formals(f) = alist(x = )
  names(formals(f)) = deparse(substitute(par))
  body(f) = substitute(expr)
  environment(f) = parent.frame()
  f
}



l(alist(x = , y = ) -> sum(x)*prod(x,y)) -> F

l(alist(. = ) -> sum(.)) 

x = rgamma(100, 2, 4)

nlm(L(p -> -mean(dgamma(x, p[1], p[2], log = TRUE))), 
    p = c(1, 1))

nlm(p %l% -mean(dgamma(x, p[1], p[2], log = TRUE)),
    p = c(1, 1))

nlm(l(alist(. = ) -> -mean(dgamma(x, .[1], .[2], log = TRUE))), 
    p = c(1, 1))

nlm(l(alist(... = ) -> -mean(dgamma(x, ..1, ..2, log = TRUE))),
    p = c(1, 1))


nlm(L(-mean(dgamma(x, ...[[1]], ...[[2]], log = TRUE))), p = c(1, 1))

#L(-mean(dgamma(x, ..1, ..2, log = TRUE)))

L = function(call, quoted = FALSE) {
  call = if(!quoted) substitute(call) else call
  f = function(...) NULL
  body(f) = call
  environment(f) = parent.frame()
  f
}


nlm(L(-mean(dgamma(x, ...[[1]], ...[[2]], log = TRUE))), p = c(1, 1))





nlm(H(-mean(dgamma(x, p[1], p[2], log = TRUE))), p = c(1, 1))

f = function(x, y) {
  x + y
}

g = function(z, w) {
  z + w
}

H = function(call, quoted = FALSE) {
  
  call = if(!quoted) substitute(call) else call
  parent_call = sys.call(which = -1)
  parent_definition =  sys.function(which = -1)
  matched_call = match.call(definition = parent_definition,
                            call = parent_call)
  
  current_call = sys.call()
  
  # Make an environemnt for the call.
  args = Filter(f = function(x) x != current_call,
                x = as.list(matched_call)[-1])
  
  env = as.environment(args)
  print(env)
  parent.env(env) = parent.frame()
  eval(call, envir = env)
}


f(H(g(y, 2*y)), y = 3)



nlm(H(-mean(dgamma(x, p[1], p[2], log = TRUE))), p = c(1, 1))
