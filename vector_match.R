#' @description  This is a function that implements Vector Matching (VM) for nominal treatment levels
#' CITATION: Lopez, MJ and Gutman, R. 
#' Estimation of Causal Effects with Multiple Treatments: A Review and New Ideas (2017). 
#' Statistical Science; 32(3): 432-454
#' @param W A factor of length n indicating treatment assignment for each unit.
#' @param Z number of interventions (must be greater than 2).
#' @param X covariates (a matrix containing the covariates to match on).
#' @param k.clustnum need description here.

vector.match <- function(W, X, t = levels(W)[1], k.clustnum = 5){
  require(car)
  require(multinom)
  require(Matching)
  require(dplyr)
  
  d.pre.match <- data.frame(W, X)
  
  # Obtain estimated GPS vector using multinomial logistic regression
  var.list <- paste(names(X), collapse = "+")
  var.list <- paste("W", var.list, sep = "~")
  
  Rx.fit <- multinom(as.formula(var.list), data = d.pre.match)
  Rx.set <- fitted(Rx.fit) 
  colnames(Rx.set) <- paste0(rep("p", nlevels(W)), levels(W))
  d.pre.match <- cbind(data.frame(d.pre.match), Rx.set)
  
  # Determine region of common support
  # Remove units outside region
  min.max.Ps <- d.pre.match %>%
    group_by(W) %>%
    summarise(min1 = min(p1), max1 = max(p1), 
              min2 = min(p2), max2 = max(p2), 
              min3 = min(p3), max3 = max(p3),
              min4 = min(p4), max4 = max(p4), 
              min5 = min(p5), max5 = max(p5))
  
  d.pre.match$Eligible <- 
    d.pre.match$p1 >= max(min.max.Ps$min1) & d.pre.match$p1 <= min(min.max.Ps$max1) &
    d.pre.match$p2 >= max(min.max.Ps$min2) & d.pre.match$p2 <= min(min.max.Ps$max2) &
    d.pre.match$p3 >= max(min.max.Ps$min3) & d.pre.match$p3 <= min(min.max.Ps$max3) & 
    d.pre.match$p4 >= max(min.max.Ps$min4) & d.pre.match$p4 <= min(min.max.Ps$max4) & 
    d.pre.match$p5 >= max(min.max.Ps$min5) & d.pre.match$p5 <= min(min.max.Ps$max5)
  
  d.pre.match <- filter(d.pre.match, Eligible)
  
  # Calculate new GPS for eligible units
  Rx.fit.E <- multinom(as.formula(var.list), data = d.pre.match)
  Rx.set.E <- fitted(Rx.fit.E) 
  colnames(Rx.set.E) <- paste0(rep("p", nlevels(W)), levels(W)) 
  d.pre.match.E <- d.pre.match %>% 
    select(
      noquote(
        paste0(rep("p", nlevels(W)), 1:nlevels(W))
      )
    )
  d.pre.match.E <- cbind(d.pre.match.E, Rx.set.E)
  
  tmp1 <- kmeans()
}