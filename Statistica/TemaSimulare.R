# TEMA SIMULARE
#1) F(x) = x^n, 0 < x < 1, n natural fixat
#generarea unei valori
n <- 10
u <- runif(1)
x <- u^(1/n)
hist(x, freq = F)

#pt n valori
n <- 10^6
nr <- 10
u <- runif(n)
x <- u^(1/nr)
f <- function(x){
  nr * x^(nr-1)
}
f(1)
hist(x, freq = F)
t <- seq(min(x),max(x),1) 
lines(t,f(t), col ="pink")

#2) F(x) = (x^2 + x)/2, 0 <= x <=1
#generez o valoare
u <- runif(1)
x <- sqrt(2*u + 1/4) - 1/2

#generez n valori
n <- 10^6
u <- runif(n)
x <- sqrt(2*u + 1/4) - 1/2
f <- function(x){
 (2 *x + 1)/ 2
}
f(0)
hist(x, freq = F)
t <- seq(min(x),max(x),1) 
lines(t,f(t), col ="blue")

#3) F(x) = 1 - e^(-alpha * x^beta)
#generez o valoare
u <- runif(1)
alfa <- 2
beta <- 3
x <- ((-log(u)/alfa)^(1/beta))

#generez n valori
n <- 10^6
alfa <- 2
beta <- 3
u <- runif(n)
x <- (-log(u)/alfa)^(1/beta)
f <- function(x){
  alfa * beta  * exp(1)^(-alfa * x^beta)*x^(beta -1)
}
f(0)
hist(x, freq = F)
t <- seq(min(x),max(x),1) 
lines(t,f(t), col ="blue")

#4)
#pt x avem nevoie de if ca avem doua ramuri
# un if care sa compare pt fiecare u din vect meu cu 1/2

# simulam intai o valore
u <- runif(1)
if (u < 1/2) x <- 2 * u else x <-  4 * u

#varianta cu for(nu e de prefeart)
n <- 100
x <- rep(0,n) # vectori de 0 de n ori
for (i in 1:n) {
  u <- runif(1)
  if (u < 1/2) x[i] <- 2 * u else x[i] <-  4 * u
}

hist(x, freq = F)

#Varianta fara for
n <- 10^6
x <- rep(0,n) # vectori de 0 de n ori
i <- 1
while (i < n){
  i <- i + 1
  u <- runif(1)
  if (u < 1/2) x[i] <- 2 * u else x[i] <-  4 * u
}
hist(x, freq = F)

#5)
#generez o valoare
u <- runif(1)
b <- 3
x <- (-b * log(u))^(1/2)

#generez n valori
n <- 10^6
b <- 3
u <- runif(n)
x <- (-b * log(u))^(1/2)
f <- function(x){
  ((2*x)/b * exp(1)^(-x^2/2))
}
f(0)
hist(x, freq = F)
t <- seq(min(x),max(x),1) 
lines(t,f(t), col ="blue")

# 6)
#generez o valoare
u <- runif(1)
x <- tan(pi*(u-1/2))

#generez n valori
n <- 10^6
u <- runif(n)
x <- tan(pi*(u-1/2))
f <- function(x){
  1/(pi * (x^2 +1))
}
f(0)
hist(x, freq = F)
t <- seq(min(x),max(x),1) 
lines(t,f(t), col ="blue")


#Problema 7)
#Metoda inversa pt F(x) = 2/pi * arctg(e^x)
#pt n valor
n <-  10^6
u <- runif(n)
x <- log(tan(pi/2 * u))
f <- function(x){
  2/pi * (exp(1)^x)/(1 + exp(1)^(2*x))
}
f(0)
1/pi
hist(x, freq = F)
t <- seq(min(x),max(x),1) 
lines(t,f(t), col ="pink")


#8)

u <- runif(1)
x <- tan(pi*(u-1/2))
k <- 2
F <- function(x){
  1 - (k * x^2 + 2* k *x + 2)/2 *exp(1)^(-k*x)
}
fct_cuantila <- function(x){
  F(x) - u
}


#ChatGPT
# Instalare pachet necesar (dacă nu este deja instalat)
if (!require("rootSolve")) install.packages("rootSolve", dependencies = TRUE)

library(rootSolve)

# Definirea funcției de distribuție F(x)
F <- function(x, k) {
  1 - ((k * x^2 + 2 * k * x + 2) / 2) * exp(-k * x)
}

# Funcția inversă g(u)
inverse_F <- function(u, k) {
  # Definirea funcției care trebuie rezolvată
  equation <- function(x) F(x, k) - u
  
  # Verificarea valorilor la capetele intervalului
  F_0 <- F(0, k)
  F_1 <- F(1, k)
  
  # Verificăm dacă u este în intervalul [F(0), F(1)]
  if (F_0 <= u && F_1 >= u) {
    # Rezolvăm ecuația
    root <- uniroot(equation, lower = 0, upper = 1)$root
    return(root)
  } else {
    return(NA)  # Returnăm NA dacă u este în afara intervalului
  }
}

# Parametrul k
k <- 2  # Exemplu de valoare pentru k
n <- 1000  # Numărul de mostre dorit

# Generarea variabilelor aleatoare u
u <- runif(n)

# Calcularea valorilor x folosind funcția inversă
x <- sapply(u, function(u) inverse_F(u, k))

# Eliminăm NA-urile, dacă este cazul
x <- na.omit(x)

# Histograma
hist(x_samples, breaks = 30, main = "Distribuția generată folosind metoda inversă", 
     xlab = "x", ylab = "Frecvență", col = "lightblue", probability = TRUE)


#9)
# simulam intai o valore
lamda <- 2
u <- runif(1)
if (u < 1/2) x <- 2 * u else x <-  4 * u

#10)
#generez o valoare
u <- runif(1)
x <- exp(1)^(2*u -1)

#generez n valori
n <- 10^6
u <- runif(n)
x <- exp(1)^(2*u -1)
f <- function(x){
  1/(2*x)
}
f(1/exp(1))
hist(x, freq = F)
t <- seq(min(x),max(x),1) 
lines(t,f(t), col ="blue")

#11)
#generez o valoare
u <- runif(1)
x <- 2 * u^2 +1
#generez n valori
n <- 10^6
u <- runif(n)
x <- 2 * u^2 +1
f <- function(x){
  1/4 * 1/((x-1)/2)^(1/2)
}
f(2)
hist(x, freq = F)
t <- seq(min(x),max(x),1) 
lines(t,f(t), col ="blue")




#23)
#generez o valoare
u <- runif(1)
x <- log(u*(exp(1) -1)+1)
#generez n valori
n <- 10^6
u <- runif(n)
x <- log(u*(exp(1) -1)+1)
f <- function(x){
  exp(1)^x/(exp(1)-1)
}
f(1/2)
hist(x, freq = F)
t <- seq(min(x),max(x),1) 
lines(t,f(t), col ="blue")

#24)
#???????????????????????????????????????????????????????????//?????????????????/
# simulam intai o valore
u <- runif(1)
if (u < 1/4) x <- 2 +2*u^(1/2) else x <-  6 * (1- ((1-u)/3)^(1/2))


#Varianta fara for
n <- 10^6
x <- rep(0,n) # vectori de 0 de n ori
i <- 1
while (i < n){
  i <- i + 1
  u <- runif(1)
  if (u < 1/4) x[i] <- 2 +2*u^(1/2) else x[i] <-  6 * (1- ((1-u)/3)^(1/2))
}
hist(x, freq = F)
#???????????????????????????????????????????????????????????//?????????????????/

#25

# simulam intai o valore
u <- runif(1)
if (u < 1/2) x <- 1/2*log(2*u) else x <-  -1/2*log(2-2*u)


#Varianta fara for
n <- 10^6
x <- rep(0,n) # vectori de 0 de n ori
i <- 1
while (i < n){
  i <- i + 1
  u <- runif(1)
  if (u < 1/2) x[i] <- 1/2*log(2*u) else x[i] <-  -1/2*log(2-2*u)
}
hist(x, freq = F)

#26 #Metoda respingerii

#mai intai simulam o valoare

ok <- 1  
while(ok==1)
{
  lambda <- 1/2
  u_y <- runif(1)
  y <- -1/lambda * log(u_y)
  
  u <- runif(1) 
  if (u <= (y^2 * exp(1)^((-y+4)/2)/16))
  {
    x <-  y
    ok <- 0
  } 
}

#Simulez n valori
n <- 10^3
x <- c()
for (i in 1:n){
  ok <- 1
  while(ok==1)
  {
    lambda <- 1/2
    u_y <- runif(1)
    y <- -1/lambda * log(u_y)
    
    u <- runif(1) 
    if (u <= (y^2 * exp(1)^((-y+4)/2)/16))
    {
      x <-  c(x,y)
      ok <- 0
    } 
  }
}
hist(x, freq = F)
t <-seq(0,3, 0.001)
lines(t, dnorm(t), col = "magenta")


#contor

n <- 10^3
x <- c()
contor <- rep(0,n) # creez vector de 0 cu n indexi
for (i in 1:n){
  ok <- 1
  contor[i] <- 0
  while(ok==1)
  {
    lambda <- 1/2
    u_y <- runif(1)
    y <- -1/lambda * log(u_y)
    contor[i] <- contor[i] + 1
    u <- runif(1) 
    if (u <= (y^2 * exp(1)^((-y+4)/2)/16))
    {
      x <-  c(x,y)
      ok <- 0
    } 
  }
}
hist(contor, freq = F)
mean(contor) # foarte aproape de c = 16 * e ^ (-2)



#27 METODA RESPINGERII

#mai intai simulam o valoare

ok <- 1  
while(ok==1)
{
  y <-runif(1, min=0.8, max=1)
  u <- runif(1)
  if (u <= (10^4 * y *(1-y)^3)/64)
  {
    x <-  y
    ok <- 0
  } 
}

#Simulez n valori
n <- 10^3
x <- c()
for (i in 1:n){
  ok <- 1
  while(ok==1)
  {
    y <-runif(1, min=0.8, max=1)
    u <- runif(1)
    if (u <= (10^4 * y *(1-y)^3)/64)
    {
      x <-  y
      ok <- 0
    } 
  }
}
hist(x, freq = F)
t <-seq(0.8,1, 0.001)
lines(t, dunif(t), col = "magenta")

#ceva nu e bine, dar nu imi dau seama ce

#28
#mai intai simulam o valoare
ok <- 1  
a <- 2
while(ok==1)
{
  y <-runif(1, min=0, max=a)
  u <- runif(1)
  if (u <= 1- log(y)/log(a))
  {
    x <-  y
    ok <- 0
  } 
}

#Simulez n valori
n <- 10^3
x <- c()
a <- 2
for (i in 1:n){
  ok <- 1
  while(ok==1)
  {
    contor[i] <- contor[i] + 1
    y <-runif(1, min=0, max=a)
    u <- runif(1) 
    if (u <=  1- log(y)/log(a))
    {
      x <-  c(x,y)
      ok <- 0
    } 
  }
}

f <- function(x){
  1/a * log(a/x)
}
f(1)
hist(x, freq = F)
t <- seq(0,a,1) 
lines(t,f(t), col ="magenta")


#contor

n <- 10^3
x <- c()
a <- 3
contor <- rep(0,n) # creez vector de 0 cu n indexi
for (i in 1:n){
  ok <- 1
  contor[i] <- 0
  while(ok==1)
  {
    contor[i] <- contor[i] + 1
    y <-runif(1, min=0, max=a)
    u <- runif(1) 
    if (u <=  1- log(y)/log(a))
    {
      x <-  c(x,y)
      ok <- 0
    }  
  }
}
hist(contor, freq = F)
mean(contor) #eu am ales c = ln a

