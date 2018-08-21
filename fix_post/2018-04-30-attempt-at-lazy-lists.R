print.llist = function(x, ...) {
  
}

`[[.llist` = function(x, i) {
  keep_class = class(x)
  on.exit({class(x) = keep_class})
  
  class(x) = "env"
  `[[`(x, paste0("e", as.character(i)))
}


make_promise = function(name,
                        promise, 
                        eval_env = parent.frame(), 
                        assign_env = parent.frame()) {
  
  
  call = as.call(c(delayedAssign, x = name, value = promise,
                   eval.env = eval_env, assign.env = assign_env))
  eval(call)
}

llapply =  function(X, FUN, ...) {
  FUN = substitute(FUN)
  dots = as.list(substitute((...))[-1])
  envir = parent.frame()
  env = new.env(parent = envir)  
  class(env) = c("llist", "env")
  
  for(i in 1:length(X)) {
    make_promise(name = paste0("e",as.character(i)), 
                 promise =  as.call(c(FUN, X[[i]], dots)),
                 eval_env = env, 
                 assign_env = env)
    print(as.call(c(FUN, X[[i]], dots)))
  }
  
  env
}

lapply(1:3, function(x) x^2)
llapply(alist(x, y, z), function(x) x^2) -> env

f = function(x) substitute(x)







f = function(x, y) x
f(1, y <- 5)

g = function(x, y) x + y
#g(1, y <- 5)

g(1, assign("y", 88))

a = 3

h = function(x = 5, y = a <<- b) {
  b = 6
  y
  x + a
}

h()

(function() {
  a = 1
  h()
  })()

x = 4

f = function(x) {
  environment() = parent.frame()
  x
}

f(3)
