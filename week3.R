# loss function

x <- c(4,19,20,23,23,25,25,26,27,28,28,28,29,30,rep(31,5),rep(32,4),rep(33,3),rep(34,6),rep(35,4),
       rep(36,3),rep(37,3),38,38,39,39,46,47,47,47,49)

getmode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x,uniqx)))]
}

mean(x)
median(x)
getmode(x)

g = 30

#L0 : 0/1 loss
L0 <- Vectorize(function(x,g) if (x==g ) { 0 } else {1})

L0(x,g)
sum(L0(x,g))

#L1 : linear loss: sum(abs(x-g))

L1 <- Vectorize(function(x,g) sum(abs(x-g)))

L1(x,g)
sum(L1(x,g)) #I'm getting a total of 310 and slides show 346...so I'm off on my guess of x vector

#L2 : squared loss

L2 <- Vectorize(function(x,g) sum((x-g)^2))

L2(x,g)
sum(L2(x,g)) #I'm getting a total of 3356 and slides show 3732...so I'm off on my guess of x vector

