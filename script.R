data1 <- c(13.1, 19.6, 21.3, 11.6, 15.4, 23.7, 18.6, 16.1, 19.3, 17.4, 21.5, 16.8, 14.9)
data1

hist(data1)

t.test(data1)
?t.test

t.test(data1, conf.level = 0.9)

x <- data1
alpha <- 0.1
n <- length(data1)
m <- mean(data1)
s <- sd(x)
se <- s / sqrt(n)
tval <- qt(0.5*alpha, df=n-1, lower.tail = FALSE)
CI_lwr <- m - tval * se
CI_upr <- m + tval * se
round(c(CI_lwr, CI_upr), 2)

data2<- c(11.5, 16.3, 12.4, 19.5, 14.6, 17.8, 21.5, 15.6, 13.1, 12.6, 16.4,
          17.9, 13.6, 19.5, 15.1)

t.test(data2, mu=12, alternative = "greater")


mean = 15
sd = 4
lb=17
ub=Inf

x <- seq(mean - 4*sd, mean + 4*sd, length=1000)
hx <- dnorm(x, mean, sd)

plot(x, hx, type="1", xlab="X", ylab="", xlim=c(mean-4*sd, mean+4*sd),
     main = "Normal Distribution pdf", axes=FALSE)
Axis(side=1)

i <- x > lb & x <= ub
lines(x, hx)
polygon(c(lb, x[i], ub), c(0, hx[i], 0), col="blue")
abline(h=0)
abline(v=lb, col="blue")


1 - pnorm(16, 15, 4)


# Questions
# Q1
q1d <- c(69, 74, 79, 81, 85, 86, 89, 90, 94, 97, 100, 105)
t.test(q1d, conf.level = 0.99)

# The 'manual' way
alpha <- 0.01
m <- mean(q1d)
n <- length(q1d)
s <- sd(q1d)
se <- s / sqrt(n)
tval <- qt(0.5*alpha, df=n-1, lower.tail = FALSE)
CI_lwr <- m - se*tval
CI_upr <- m + se*tval
CI_lwr
CI_upr



# Q3

n <- 50
mu <- 100
var <- 30
sd <- sqrt(var)

# Probability that Y_1 is greater than 95
1 - pnorm(95, mu, sd)
# verified by hand

# P(Ybar > 102)
1-pnorm(102, 100, sqrt(30/50))
# verified by hand

