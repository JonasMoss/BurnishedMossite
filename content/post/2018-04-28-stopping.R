#' Find the cumulative maximal streak length in a vector of bools.
#' 
#' @param bools Logical vector.
#' @return An integer vector. The \code{i}th element is the maximal streak
#' length in \code{x[1:i]}.
#' @example
#'     bools1 = c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
#'     streaks(bools1) [1] 0 1 1 1 2 3 3
#'     
#'     bools2 = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE)
#'     streaks(bools2) [1] 0 1 2 3 4 4 4

streaks = function(bools) {
  if(length(bools) == 0 | length(bools) == 1) return(as.integer(bools))
  
  streaks = vector("integer", length(bools))
  counter = as.integer(bools[[1]])
  streaks[[1]] = counter
  
  for(i in 2:length(bools)) {
    if(bools[[i]]) {
      counter = counter + 1
      streaks[[i]] = max(streaks[[i - 1]], counter)
    } else {
      counter = 0
      streaks[[i]] = streaks[[i - 1]]      
    }
  }
  
  streaks
  
}


#' Simulate a compensating sequential design
#' 
#' Finds the probability of falsly rejecting the null-hypothesis for a 
#' compensating sequential design for each \code{k} from 1 to n.
#' 
#' @param n The maximal number of attempts to obtain at a success.
#' @param scenario String; "A" for scenario A, "B" for scenario B.
#' @param N The number of simulations.
#' @param C One-sided cut-off value for the z-statistics. Defaults to ~ 1.64. 
#' @return A n-ary vector of probabilites. The ith value is the probability 
#' of rejecting the null-hypothesis when a streak of length n is demanded.

streak_stopping = function(n, N, C = qnorm(0.95), scenario = "A") {
  
  checked = array(dim = c(N, n))

  for(i in 1:N){
    streak = streaks(cumsum(rnorm(2*n - 1, 0, 1))/sqrt(1:(2*n - 1)) > C)
    if(scenario == "A") {
      for(j in 1:n) {
        checked[i, j] = if(streak[n + j - 1] >= j) 1 else 0
      }
    } else if(scenario == "B") {
      for(j in 1:n) {
        checked[i, j] = if(streak[n] >= j) 1 else 0
      }     
    }
  }
  
  colMeans(checked)
  
}


## =============================================================================
## Graph 1. Relationship between n and k; big.
## =============================================================================

set.seed(313)
N = 10000
ns = 1:20*50

ks_A = sapply(ns, function(n) {
  which(streak_stopping(n, N, scenario = "A") < 0.05)[1]
})

ks_B = sapply(ns, function(n) {
  which(streak_stopping(n, N, scenario = "B") < 0.05)[1]
})

## This blocked is used in the blog post.
plot(ns, ks_A, xlab = "n", ylab = expression(k[n]),
     main = "Least k for attaining level 0.05", bty = "l", pch = 20)
abline(lm(ks_A ~ ns))
points(ns, ks_B, pch = 20, col = "blue")
abline(lm(ks_B ~ ns), col = "blue")
legend("topleft", col = c("black", "blue"), pch = c(20, 20), lty = c(1, 1),
       legend = c(paste0("Scenario A; Slope: ",
                         signif(coef(lm(ks_A ~ ns))[2],2)),
                  paste0("Scenario B; Slope: ",
                         signif(coef(lm(ks_B ~ ns))[2],2))),
       bty = "n")

## =============================================================================
## Graph 2. Relationship between n and k; small.
## =============================================================================

set.seed(313)
N = 100000
ns_small = 1:50

ks_A_small = sapply(ns_small, function(n) {
  which(streak_stopping(n, N, scenario = "A") < 0.05)[1]
})
ks_A_small[1] = 1

ks_B_small = sapply(ns_small, function(n) {
  which(streak_stopping(n, N, scenario = "B") < 0.05)[1]
})
ks_B_small[1] = 1

## This blocked is used in the blog post.
plot(ns_small , ks_A_small, type = "s", xlab = "n", ylab = expression(k[n]),
     main = "Least k for attaining level 0.05", bty = "l")
grid()
points(ns_small , ks_A_small, pch = 20)
lines(ns_small , ks_B_small, type = "s", col = "blue")
points(ns_small , ks_B_small, pch = 20, col = "blue")
abline(lm(ks_A_small ~ ns_small ))
abline(lm(ks_B_small ~ ns_small ), col = "blue")
legend("topleft", col = c("black", "blue"), pch = c(20, 20), lty = c(1, 1),
       legend = c(paste0("Scenario A; Slope: ",
                         signif(coef(lm(ks_A_small~ ns_small ))[2],2)),
                  paste0("Scenario B; Slope: ",
                         signif(coef(lm(ks_B_small ~ ns_small ))[2],2))),
       bty = "n")

## =============================================================================
## Graph 3. Example of probabilities
## =============================================================================

set.seed(313)
n = 100
x = 1:n
N = 100000
y = streak_stopping(n, N, scenario = "A")
z = streak_stopping(n, N, scenario = "B")

## This blocked is used in the blog post.
plot(x, y, xlab = "n", ylab = "Propobability of rejection", type = "l", lwd = 2,
     main = paste0("Probability of rejection, n = ", n), bty = "l",
     pch = 20, cex = 1.5)
lines(x, z, type = "l", lwd = 2, lty = 3)
legend("topright", col = c("black", "black", "blue", "red"), lwd = c(2, 2, 1, 1),
       lty = c(1, 3, 1, 1),
       legend = c("Scenario A", "Scenario B", "0.05 cutoff", "0.01 cutoff"),
       bty = "n")
abline(h = 0.05, lty = 2, col = "blue")
abline(h = 0.01, lty = 2, col = "red")

## =============================================================================
## Post-processing: Store the data in an .rd file.
## =============================================================================

save.image(file = "content/post/optimal_stopping.rds")
