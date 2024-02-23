# 
# # EXERCITIUL I - 1.
# set.seed(311)
# # Definim parametrii pentru distributiile de probabilitate
# n <- 10  # Numarul de incercari pentru distributia binomiala si geometrica
# p_binomial <- 0.5  # Probabilitatea de succes pentru distributia binomial
# p_geometric <- 0.3  # probabilitatea de succes pentru distributia geometrica
# lambda_poisson <- 2  # parametrul pentru distributia Poisson
# min_uniform_discrete <- 1  # valoarea minima pentru distributia uniforma discreta
# max_uniform_discrete <- 10  # valoarea maxima pentru distributia uniforma discreta
# min_uniform_continuu <- 0  # valoarea minima pentru distributia uniforma continua
# max_uniform_continuu <- 1  # valoarea maxima pentru distributia uniforma continua
# rate_exponential <- 0.5  # rata pentru distributia exponentiala
# shape_gamma <- 2  # forma pentru distributia gamma
# rate_gamma <- 1  # rata pentru distributia gamma
# shape_beta <- 2  # prima forma pentru distributia beta
# shape2_beta <- 5  # a doua forma pentru distributia beta
# 
# # Generam un vector de valori pentru x
# x_values <- seq(-3, 3, length.out = 1000)
# 
# # calculam P(Z_n <= x) pentru fiecare distributie
# prob_binomial <- pnorm(sqrt(n) * (x_values - n * p_binomial) / sqrt(n * p_binomial * (1 - p_binomial)))
# 
# prob_geometric <- pnorm(sqrt(n) * (x_values - n / p_geometric) / sqrt(n / p_geometric^2))
# 
# prob_poisson <- pnorm(sqrt(n) * (x_values - n * lambda_poisson) / sqrt(n * lambda_poisson))
# 
# prob_uniform_discrete <- pnorm(sqrt(n) * (x_values - n * (min_uniform_discrete + max_uniform_discrete) / 2)
#                                / sqrt(n * ((max_uniform_discrete - min_uniform_discrete + 1)^2 - 1) / 12))
# prob_uniform_continue <- pnorm(sqrt(n) * (x_values - n * (min_uniform_continuu + max_uniform_continuu) / 2)
# 
#                                / sqrt(n * (max_uniform_continuu - min_uniform_continuu)^2 / 12))
# prob_exponential <- pnorm(sqrt(n) * (x_values - n / rate_exponential) / sqrt(n / rate_exponential^2))
# 
# prob_gamma <- pnorm(sqrt(n) * (x_values - n * shape_gamma / rate_gamma) / sqrt(n * shape_gamma / rate_gamma^2))
# 
# prob_beta <- pnorm(sqrt(n) * (x_values - n * shape_beta / (shape_beta + shape2_beta)) / sqrt(n * shape_beta * shape2_beta
#                                                                                              / ((shape_beta + shape2_beta)^2 * (shape_beta + shape2_beta + 1))))
# 
# 
# # EXERCITIUL I - 2.
# # Facem grafic pentru fiecare repartitie calculata anterior
# par(mfrow = c(3, 3))
# 
# plot(x_values, prob_binomial, col="deeppink", type = "l", ylab = "P(Zn <= x)", xlab = "x", main = "Repartitia normalizata Zn pentru distributia Binomiala")
# 
# plot(x_values, prob_geometric, col="deeppink", type = "l", ylab = "P(Zn <= x)", xlab = "x", main = "Repartitia normalizata Zn pentru distributia Geometrica")
# 
# plot(x_values, prob_poisson, col="deeppink", type = "l", ylab = "P(Zn <= x)", xlab = "x", main = "Repartitia normalizata Zn pentru distributia Poisson")
# 
# plot(x_values, prob_uniform_discrete, col="deeppink", type = "l", ylab = "P(Zn <= x)", xlab = "x", main = "Repartitia normalizata Zn pentru distributia uniforma Discreta")
# 
# plot(x_values, prob_uniform_continue,col="deeppink", type = "l", ylab = "P(Zn <= x)", xlab = "x", main = "Repartitia normalizata Zn pentru distributia uniforma Continua")
# 
# plot(x_values, prob_exponential, col="deeppink", type = "l", ylab = "P(Zn <= x)", xlab = "x", main = "Repartitia normalizata Zn pentru distributia Exponentiala")
# 
# plot(x_values, prob_gamma, col="deeppink", type = "l", ylab = "P(Zn <= x)", xlab = "x", main = "Repartitia normalizata Zn pentru distributia Gamma")
# 
# plot(x_values, prob_beta, col="deeppink", type = "l", ylab = "P(Zn <= x)", xlab = "x", main = "Repartitia normalizata Zn pentru distributia Beta")
# 
# 
# # EXERCITIUL I - 3.
# # Definirea functiei de diferente generale
# diferenta <- function(prob) {
#   function(x) {
#     abs(prob(x) - pnorm(x))
#   }
# }
# 
# # Calculul maximei diferente absolute pentru fiecare distributie
# 
# # Binomial
# prob_binomial <- function(x) pbinom(floor(x * 100), size = n, prob = p_binomial)
# max_diff_binomial <- optimize(diferenta(prob_binomial), interval = c(-3, 3), maximum = TRUE)$objective
# c(max_diff_binomial)
# 
# # Geometric
# prob_geometric <- function(x) pgeom(floor(x * 100), prob = p_geometric)
# max_diff_geometric <- optimize(diferenta(prob_geometric), interval = c(-3, 3), maximum = TRUE)$objective
# c(max_diff_geometric)
# 
# # Poisson
# prob_poisson <- function(x) ppois(floor(x * 100), lambda = lambda_poisson)
# max_diff_poisson <- optimize(diferenta(prob_poisson), interval = c(-3, 3), maximum = TRUE)$objective
# c(max_diff_poisson)
# 
# # Uniform Discretă
# prob_uniform_discrete <- function(x) punif(floor(x * 100), min = min_uniform_discrete, max = max_uniform_discrete)
# max_diff_uniform_discrete <- optimize(diferenta(prob_uniform_discrete), interval = c(-3, 3), maximum = TRUE)$objective
# c(max_diff_uniform_discrete)
# 
# # Uniform Continuă
# prob_uniform_continue <- function(x) punif(x, min = min_uniform_continuu, max = max_uniform_continuu)
# max_diff_uniform_continue <- optimize(diferenta(prob_uniform_continue), interval = c(-3, 3), maximum = TRUE)$objective
# c(max_diff_uniform_continue)
# 
# # Exponential
# prob_exponential <- function(x) pexp(x, rate = rate_exponential)
# max_diff_exponential <- optimize(diferenta(prob_exponential), interval = c(-3, 3), maximum = TRUE)$objective
# c(max_diff_exponential)
# 
# # Gamma
# prob_gamma <- function(x) pgamma(x, shape = shape_gamma, rate = rate_gamma)
# max_diff_gamma <- optimize(diferenta(prob_gamma), interval = c(-3, 3), maximum = TRUE)$objective
# c(max_diff_gamma)
# 
# # Beta
# prob_beta <- function(x) pbeta(x, shape1 = shape_beta, shape2 = shape2_beta)
# max_diff_beta <- optimize(diferenta(prob_beta), interval = c(-3, 3), maximum = TRUE)$objective
# c(max_diff_beta)
# 
# # EXERCITIUL I - 4.
# #
# funct_EV <- function(repartitie, n, p, lmbda, beta, norm_mean, norm_sd, unif_a, unif_b, unif_n, functie_masa=NULL, functie_densitate=NULL)
# {
#   if(is.null(functie_masa) && is.null(functie_densitate))
#   {
#     if(repartitie == "binomiala")
#       functie_masa <- function(k){dbinom(k, size=n, prob=p)}
# 
#     else if(repartitie == "geometrica")
#       functie_masa <- function(k){dgeom(k, prob=p)}
# 
#     else if(repartitie == "poisson")
#       functie_masa <- function(k){dpois(k, lambda=lmbda)}
# 
#     else if(repartitie == "uniforma continuu")
#       functie_densitate <- function(x){dunif(x, min=unif_a, max=unif_b)}
# 
#     else if(repartitie == "exponentiala")
#       functie_densitate <- function(x){dexp(x, rate=lmbda)}
# 
#     else if(repartitie == "normala")
#       functie_densitate <- function(x){dnorm(x, mean=norm_mean, sd=norm_sd)}
# 
#     else return("repartitia nu se gaseste in cele enumerate")
#   }
# 
#   if(is.null(functie_masa)) #cazul continuu
#   {
#     E_X <- integrate(function(x){x*functie_densitate(x)}, 0, Inf)$value
#     E_X2 <- integrate(function(x){x^2*functie_densitate(x)}, 0, Inf)$value
#     Var_X <- E_X2 - E_X^2
#   }
#   else #cazul discret
#   {
#     #x <- numeric(n + 1)
#     #for (i in 0:n) {
#     #x[i + 1] <- i}
#     x <- 0:n
#     E_X <- sum(x*functie_masa(x))
#     Var_X <- sum((x - E_X)^2*functie_masa(x))
#   }
# 
#   return(c(E_X, Var_X))
# 
# 
# }
# 
# print("Repartitiile sunt: binomiala, geometrica, poisson, uniforma continuu, exponentiala, normala.")
# repartitie <- readline(prompt="Numele repartitiei: ")
# 
# 
# ni <- as.numeric(readline(prompt="Introduceti valoare pentru n: "))
# 
# pi <- as.numeric(readline(prompt="Introduceti valoare intre 0 si 1 pentru p: "))
# 
# lambdai <- as.numeric(readline(prompt="Introduceti valoare pentru lambda: "))
# 
# betai <- as.numeric(readline(prompt="Introduceti valoare pentru beta: "))
# 
# norm_meani <- as.numeric(readline(prompt="Introduceti valoare pentru norm_mean: "))
# 
# norm_sdi <- as.numeric(readline(prompt="Introduceti valoare pentru norm_sd: "))
# 
# sigmai <- as.numeric(readline(prompt="Introduceti valoare pentru sigma: "))
# 
# mui <- as.numeric(readline(prompt="Introduceti valoare pentru mu: "))
# 
# unif_ai <- as.numeric(readline(prompt="Introduceti valoare pentru unif_a: "))
# 
# unif_bi <- as.numeric(readline(prompt="Introduceti valoare pentru unif_b: "))
# 
# unif_ni <- as.numeric(readline(prompt="Introduceti valoare pentru unif_n: "))
# 
# func_EV_valori <- funct_EV(repartitie, n=ni, p=pi, lmbda=lambdai, beta=betai, norm_mean=norm_meani, norm_sd=norm_sdi, unif_a=unif_ai, unif_b=unif_bi, unif_n=unif_ni)
# E_X <- func_EV_valori[1]
# Var_X <- func_EV_valori[2]
# 
# print("Media si dispersia:")
# print(E_X)
# print(Var_X)
# 
# 
# # EXERCITIUL I - 5.
# # La acest ex vom folosi partea de recunoastere a denumirilor de la 4)
# funct_EV5 <- function(repartitie, n, p, lmbda, beta, norm_mean, norm_sd, unif_a, unif_b, unif_n, functie_masa=NULL, functie_densitate=NULL)
# {
#   if(is.null(functie_masa) && is.null(functie_densitate))
#   {
#     if(repartitie == "binomiala")
#       functie_masa <- function(k){dbinom(k, size=n, prob=p)}
# 
#     else if(repartitie == "geometrica")
#       functie_masa <- function(k){dgeom(k, prob=p)}
# 
#     else if(repartitie == "poisson")
#       functie_masa <- function(k){dpois(k, lambda=lmbda)}
# 
#     else if(repartitie == "uniforma discret")
#       functie_masa <- function(x){dunif(x, n=unif_n)}
# 
#     else if(repartitie == "uniforma continuu")
#       functie_densitate <- function(x){dunif(x, min=unif_a, max=unif_b)}
# 
#     else if(repartitie == "exponentiala")
#       functie_densitate <- function(x){dexp(x, rate=lmbda)}
# 
#     else if(repartitie == "normala")
#       functie_densitate <- function(x){dnorm(x, mean=norm_mean, sd=norm_sd)}
# 
#     else return("repartitia nu se gaseste in cele enumerate")
#   }
# 
#   if(is.null(functie_masa)) # avem functie de densitate, deci caz continuu
#   {
#     E_X <- integrate(function(x){x*functie_densitate(x)}, 0, Inf)$value
#     #Luam numai valoarea integralei
#     E_X_1 <- integrate(function(x){abs(x-E_X)^3*functie_densitate(x)}, 0, Inf)$value
#   }
#   else # avem functie de masa, deci suntem pe cazul discret
#   {
#     x <- 0:n
#     E_X <- sum(x*functie_masa(x))
#     E_X_1 <- sum(abs(x-E_X)^3*functie_masa(x))
#   }
#   return(E_X_1)
# }
# 
# print("Repartitiile sunt: binomiala, geometrica, poisson, uniforma continuu, exponentiala, normala.")
# repartitie <- readline(prompt="Numele repartitiei: ")
# 
# 
# ni <- as.numeric(readline(prompt="Introduceti valoare pentru n: "))
# 
# probi <- as.numeric(readline(prompt="Introduceti valoare intre 0 si 1 pentru p: "))
# 
# lambdai <- as.numeric(readline(prompt="Introduceti valoare pentru lambda: "))
# 
# betai <- as.numeric(readline(prompt="Introduceti valoare pentru beta: "))
# 
# norm_meani <- as.numeric(readline(prompt="Introduceti valoare pentru norm_mean: "))
# 
# norm_sdi <- as.numeric(readline(prompt="Introduceti valoare pentru norm_sd: "))
# 
# sigmai <- as.numeric(readline(prompt="Introduceti valoare pentru sigma: "))
# 
# mui <- as.numeric(readline(prompt="Introduceti valoare pentru mu: "))
# 
# unif_ai <- as.numeric(readline(prompt="Introduceti valoare pentru unif_a: "))
# 
# unif_bi <- as.numeric(readline(prompt="Introduceti valoare pentru unif_b: "))
# 
# unif_ni <- as.numeric(readline(prompt="Introduceti valoare pentru unif_n: "))
# 
# 
# func_EV5_valoare <- funct_EV5(repartitie, n=ni, p=probi, lmbda=lambdai, beta=betai, norm_mean=norm_meani, norm_sd=norm_sdi, unif_a=unif_ai, unif_b=unif_bi, unif_n=unif_ni)
# print(func_EV5_valoare)
# 
# 
# # EXERCITIUL I - 6.
# # Combinam functiile de la 4) si 5)
# 
# func_EV6<- function(repartitie, n, p, lmbda, beta, norm_mean, norm_sd, unif_a, unif_b, unif_n, functie_masa=NULL, functie_densitate=NULL)
# {
#   if(is.null(functie_masa) && is.null(functie_densitate))
#   {
#     if(repartitie == "binomiala")
#       functie_masa <- function(k){dbinom(k, size=n, prob=p)}
# 
#     else if(repartitie == "geometrica")
#       functie_masa <- function(k){dgeom(k, prob=p)}
# 
#     else if(repartitie == "poisson")
#       functie_masa <- function(k){dpois(k, lambda=lmbda)}
# 
# 
#     else if(repartitie == "uniforma continuu")
#       functie_densitate <- function(x){dunif(x, min=unif_a, max=unif_b)}
# 
#     else if(repartitie == "exponentiala")
#       functie_densitate <- function(x){dexp(x, rate=lmbda)}
# 
#     else if(repartitie == "normala")
#       functie_densitate <- function(x){dnorm(x, mean=norm_mean, sd=norm_sd)}
# 
#     else return("repartitia nu se gaseste in cele enumerate")
#   }
# 
#   if(is.null(functie_masa)) # avem functie de densitate, deci caz continuu
#   {
#     E_X <- integrate(function(x){x*functie_densitate(x)}, 0, Inf)$value
#     E_X2 <- integrate(function(x){x^2*functie_densitate(x)}, 0, Inf)$value
#     E_X_1 <- integrate(function(x){abs(x-E_X)^3*functie_densitate(x)}, 0, Inf)$value
#     Var_X <- E_X2-E_X^2
#   }
#   else # avem functie de masa, deci suntem pe cazul discret
#   {
#     x <- 0:n
#     E_X <- sum(x*functie_masa(x))
#     E_X_1 <- sum(abs(x-E_X)^3*functie_masa(x))
#     Var_X <- sum((x - E_X)^2*functie_masa(x))
#   }
#   return(c(E_X, Var_X, E_X_1))
# 
# }
# 
# # Definim functia care calculeaza marginea data de inegalitatea Berry Essen:
# margine_ineg_Berry_Essen <- function(n, Var_X1, E_X_1_u)
# {
#   return(33/4*E_X_1_u/(sqrt(n*Var_X1^3)))
# }
# 
# # Facem un data.frame pe care il completam cu cu marginea fiecarei repartitii si am cream mai
# # multi vectori marg30, marg100, marg1000 punem toate valorile
# # date de functia margine_ineg_Berry_Essen in cazul cu 30, 100 si 1000 de
# # termeni in sirul de variabile Xn
# 
# 
# # binomiala
# val <- func_EV6(n=10, functie_masa = function(k){dbinom(k, size=10, prob=0.7)})
# 
# marg30 <- c(margine_ineg_Berry_Essen(30, val[2], val[3]))
# marg100 <- c(margine_ineg_Berry_Essen(100, val[2], val[3]))
# marg1000 <- c(margine_ineg_Berry_Essen(1000, val[2], val[3]))
# 
# medii <- c(val[1])
# dispersii <- c(val[2])
# 
# # geometrica
# 
# val <- func_EV6(n=10, functie_masa = function(k){dgeom(k, prob = 0.2)})
# 
# marg30 <- c(marg30, margine_ineg_Berry_Essen(30, val[2], val[3]))
# marg100 <- c(marg100, margine_ineg_Berry_Essen(100, val[2], val[3]))
# marg1000 <- c(marg1000, margine_ineg_Berry_Essen(1000, val[2], val[3]))
# 
# medii <- c(medii, val[1])
# dispersii <- c(dispersii, val[2])
# 
# # poisson
# 
# val <- func_EV6(n=10, functie_masa = function(k){dpois(k, lambda = 3)})
# 
# marg30 <- c(marg30, margine_ineg_Berry_Essen(30, val[2], val[3]))
# marg100 <- c(marg100, margine_ineg_Berry_Essen(100, val[2], val[3]))
# marg1000 <- c(marg1000, margine_ineg_Berry_Essen(1000, val[2], val[3]))
# 
# medii <- c(medii, val[1])
# dispersii <- c(dispersii, val[2])
# 
# # uniforma discreta
# 
# val <- func_EV6(n=10, functie_masa = function(k){dunif(k, min=0, max=1)})
# 
# marg30 <- c(marg30, margine_ineg_Berry_Essen(30, val[2], val[3]))
# marg100 <- c(marg100, margine_ineg_Berry_Essen(100, val[2], val[3]))
# marg1000 <- c(marg1000, margine_ineg_Berry_Essen(1000, val[2], val[3]))
# 
# medii <- c(medii, val[1])
# dispersii <- c(dispersii, val[2])
# 
# # uniforma continua
# 
# val <- func_EV6(functie_densitate = function(x){dunif(x, min=0, max=1)})
# 
# marg30 <- c(marg30, margine_ineg_Berry_Essen(30, val[2], val[3]))
# marg100 <- c(marg100, margine_ineg_Berry_Essen(100, val[2], val[3]))
# marg1000 <- c(marg1000, margine_ineg_Berry_Essen(1000, val[2], val[3]))
# 
# medii <- c(medii, val[1])
# dispersii <- c(dispersii, val[2])
# 
# # exponentiala
# 
# val <- func_EV6(functie_densitate = function(x){dexp(x, rate=7)})
# 
# marg30 <- c(marg30, margine_ineg_Berry_Essen(30, val[2], val[3]))
# marg100 <- c(marg100, margine_ineg_Berry_Essen(100, val[2], val[3]))
# marg1000 <- c(marg1000, margine_ineg_Berry_Essen(1000, val[2], val[3]))
# 
# medii <- c(medii, val[1])
# dispersii <- c(dispersii, val[2])
# 
# # gamma
# 
# val <- func_EV6(functie_densitate = function(x){dgamma(x, shape = 6, rate = 2)})
# 
# marg30 <- c(marg30, margine_ineg_Berry_Essen(30, val[2], val[3]))
# marg100 <- c(marg100, margine_ineg_Berry_Essen(100, val[2], val[3]))
# marg1000 <- c(marg1000, margine_ineg_Berry_Essen(1000, val[2], val[3]))
# 
# medii <- c(medii, val[1])
# dispersii <- c(dispersii, val[2])
# 
# # beta
# 
# val <- func_EV6(functie_densitate = function(x){dbeta(x, shape1=1, shape2=3)})
# 
# marg30 <- c(marg30, margine_ineg_Berry_Essen(30, val[2], val[3]))
# marg100 <- c(marg100, margine_ineg_Berry_Essen(100, val[2], val[3]))
# marg1000 <- c(marg1000, margine_ineg_Berry_Essen(1000, val[2], val[3]))
# 
# medii <- c(medii, val[1])
# dispersii <- c(dispersii, val[2])
# 
# 
# valori_inegalitatea_Berry_Essen.data <- data.frame(
# 
#   distributia = c("Binomiala","Geometrica","Poisson","Uniforma discreta","Uniforma continua", "Exponentiala", "Gamma", "Beta"),
#   n30 = marg30,
#   n100 = marg100,
#   n1000 = marg1000,
#   stringsAsFactors = FALSE
# )
# 
# # afisam data.frame
# print(valori_inegalitatea_Berry_Essen.data)
# 
# 
# 
# # EXERCITIUL I - 7.
# 
# repartitii <- valori_inegalitatea_Berry_Essen.data$distributia
# 
# # Creare vectori cu valorile pentru n = 30
# valori_n30 <- valori_inegalitatea_Berry_Essen.data$n30
# 
# # Creare vectori cu valorile pentru n = 100
# valori_n100 <- valori_inegalitatea_Berry_Essen.data$n100
# 
# # Creare vectori cu valorile pentru n = 1000
# valori_n1000 <- valori_inegalitatea_Berry_Essen.data$n1000
# 
# # Creare grafic pentru repartitii
# par(mfrow=c(3,1))  # Aseaza graficele pe 3 randuri si 1 coloana
# 
# # n = 30
# barplot(valori_n30, names.arg = repartitii, col = "blue", border = "black",
#         main = "Evolutia diferentei P(Z_n <= x) - phi(x) - n = 30",
#         xlab = "Distributia",
#         ylab = "Diferenta")
# legend("topright", legend = "n = 30", fill = "blue", border = "black")
# 
# # n = 100
# barplot(valori_n100, names.arg = repartitii, col = "red", border = "black",
#         main = "Evolutia diferentei P(Z_n <= x) - phi(x) - n = 100",
#         xlab = "Distributia",
#         ylab = "Diferenta")
# legend("topright", legend = "n = 100", fill = "red", border = "black")
# 
# # n = 1000
# barplot(valori_n1000, names.arg = repartitii, col = "green", border = "black",
#         main = "Evolutia diferentei P(Z_n <= x) - phi(x) - n = 1000",
#         xlab = "Distributia",
#         ylab = "Diferenta")
# legend("topright", legend = "n = 1000", fill = "green", border = "black")
# 
# 
# 
# 
# 
# # EXERCITIUL I - 8.
# # Functie pentru calcularea marginii data de inegalitatea Berry-Essen
# 
# margine_inegalitatea_Berry_Essen <- function(n, Var_X1, E_X_1_u) {
#   return(33 / 4 * E_X_1_u / (sqrt(n * Var_X1^3)))
# }
# 
# # distributia binomiala
# n_binomial <- 10
# p_binomial <- 0.5
# 
# functie_masa_binomiala <- function(k) {
#   dbinom(k, size = n_binomial, prob = p_binomial)
# }
# 
# E_X_1_u_binomiala <- funct_EV5("binomiala", n_binomial, p_binomial, 0, 0, 0, 0, 0, 0, 0, functie_masa_binomiala)
# margine_binomiala <- margine_inegalitatea_Berry_Essen(n_binomial, func_EV6("binomiala", n_binomial, p_binomial, 0, 0, 0, 0, 0, 0, 0,
#                                                                            functie_masa_binomiala)[2], E_X_1_u_binomiala)
# 
# # distributia geometrica
# n_geometric <- 10
# p_geometric <- 0.3
# 
# functie_masa_geometrica <- function(k) {
#   dgeom(k, prob = p_geometric)
# }
# 
# E_X_1_u_geometrica <- funct_EV5("geometrica", n_geometric, p_geometric, 0, 0, 0, 0, 0, 0, 0, functie_masa_geometrica)
# margine_geometrica <- margine_inegalitatea_Berry_Essen(n_geometric, func_EV6("geometrica", n_geometric, p_geometric, 0, 0, 0, 0, 0, 0, 0,
#                                                                              functie_masa_geometrica)[2], E_X_1_u_geometrica)
# 
# # distributia Poisson
# n_poisson <- 10
# lambda_poisson <- 2
# 
# functie_masa_poisson <- function(k) {
#   dpois(k, lambda = lambda_poisson)
# }
# 
# E_X_1_u_poisson <- funct_EV5("poisson", n_poisson, 0, lambda_poisson, 0, 0, 0, 0, 0, 0, functie_masa_poisson)
# margine_poisson <- margine_inegalitatea_Berry_Essen(n_poisson, func_EV6("poisson", n_poisson, 0, lambda_poisson, 0, 0, 0, 0, 0, 0,
#                                                                         functie_masa_poisson)[2], E_X_1_u_poisson)
# 
# # distributia uniforma discreta
# n_uniform_discreta <- 10
# min_uniform_discreta <- 1
# max_uniform_discreta <- 10
# 
# functie_masa_uniform_discreta <- function(x) {
#   dunif(x, min = min_uniform_discreta, max = max_uniform_discreta)
# }
# 
# E_X_1_u_uniform_discreta <- funct_EV5("uniforma discret", n_uniform_discreta, 0, 0, 0, 0, 0, 0, 0, 0, functie_masa_uniform_discreta)
# margine_uniform_discreta <- margine_inegalitatea_Berry_Essen(n_uniform_discreta, func_EV6("uniforma discret", n_uniform_discreta, 0, 0, 0, 0, 0, 0, 0, 0,
#                                                                                           functie_masa_uniform_discreta)[2], E_X_1_u_uniform_discreta)
# 
# # distributia uniforma continua
# n_uniform_continua <- 10
# min_uniform_continua <- 0
# max_uniform_continua <- 1
# 
# functie_densitate_uniform_continua <- function(x) {
#   dunif(x, min = min_uniform_continua, max = max_uniform_continua)
# }
# 
# E_X_1_u_uniform_continua <- funct_EV5("uniforma continuu", n_uniform_continua, 0, 0, 0, 0, 0, 0, 0, 0, functie_densitate_uniform_continua)
# margine_uniform_continua <- margine_inegalitatea_Berry_Essen(n_uniform_continua, func_EV6("uniforma continuu", n_uniform_continua, 0, 0, 0, 0, 0, 0, 0, 0,
#                                                                                           functie_densitate_uniform_continua)[2], E_X_1_u_uniform_continua)
# 
# # distributia exponențiala
# n_exponentiala <- 10
# rate_exponentiala <- 0.5
# 
# functie_densitate_exponentiala <- function(x) {
#   dexp(x, rate = rate_exponentiala)
# }
# 
# E_X_1_u_exponentiala <- funct_EV5("exponentiala", n_exponentiala, 0, 0, 0, 0, 0, 0, 0, 0, functie_densitate_exponentiala)
# margine_exponentiala <- margine_inegalitatea_Berry_Essen(n_exponentiala, func_EV6("exponentiala", n_exponentiala, 0, 0, 0, 0, 0, 0, 0, 0,
#                                                                                   functie_densitate_exponentiala)[2], E_X_1_u_exponentiala)
# 
# # distributia gamma
# n_gamma <- 10
# shape_gamma <- 2
# rate_gamma <- 1
# 
# functie_densitate_gamma <- function(x) {
#   dgamma(x, shape = shape_gamma, rate = rate_gamma)
# }
# 
# E_X_1_u_gamma <- funct_EV5("gamma", n_gamma, 0, 0, 0, 0, 0, 0, 0, 0, functie_densitate_gamma)
# margine_gamma <- margine_inegalitatea_Berry_Essen(n_gamma, func_EV6("gamma", n_gamma, 0, 0, 0, 0, 0, 0, 0, 0, functie_densitate_gamma)[2], E_X_1_u_gamma)
# 
# # distributia beta
# n_beta <- 10
# shape_beta <- 2
# shape2_beta <- 5
# 
# functie_densitate_beta <- function(x) {
#   dbeta(x, shape1 = shape_beta, shape2 = shape2_beta)
# }
# 
# E_X_1_u_beta <- funct_EV5("beta", n_beta, 0, 0, 0, 0, 0, 0, 0, 0, functie_densitate_beta)
# margine_beta <- margine_inegalitatea_Berry_Essen(n_beta, func_EV6("beta", n_beta, 0, 0, 0, 0, 0, 0, 0, 0, functie_densitate_beta)[2], E_X_1_u_beta)
# 
# # Afisarea rezultatelor
# cat("Marginea data de inegalitatea Berry-Essen pentru distributia binomiala:", margine_binomiala, "\n")
# cat("Marginea data de inegalitatea Berry-Essen pentru distributia geometrica:", margine_geometrica, "\n")
# cat("Marginea data de inegalitatea Berry-Essen pentru distributia Poisson:", margine_poisson, "\n")
# cat("Marginea data de inegalitatea Berry-Essen pentru distributia uniforma discreta:", margine_uniform_discreta, "\n")
# cat("Marginea data de inegalitatea Berry-Essen pentru distributia uniforma continua:", margine_uniform_continua, "\n")
# cat("Marginea data de inegalitatea Berry-Essen pentru distributia exponentiala:", margine_exponentiala, "\n")
# cat("Marginea data de inegalitatea Berry-Essen pentru distributia gamma:", margine_gamma, "\n")
# cat("Marginea data de inegalitatea Berry-Essen pentru distributia beta:", margine_beta, "\n")
# 
# 
# 
# # EXERCITIUL II - a.
# # Definirea densitatii de probabilitate
# f <- function(x) {
#   exp(-x^2/2) * (sin(6*x)^2 + 3*cos(x)^2*sin(4*x)^2 + 1)
# }
# 
# # Densitatea de probabilitate a repartitiei normale standard
# g <- function(x) {
#   dnorm(x, mean = 0, sd = 1)
# }
# 
# # Reprezentarea grafica si gasirea constantei de margine M
# x <- seq(-5, 5, length.out = 1000)
# y <- f(x)
# yg <- g(x)
# 
# plot(x, y, type = "l", col = "blue", lwd = 2, ylab = "f(x)", xlab = "x")
# lines(x, yg, col = "red", lty = 2, lwd = 2)
# 
# # Gasirea constantei de margine (M)
# 
# M_values <- f(x) / g(x)
# M <- max(M_values)
# cat("Valoarea constantei de margine M:", M, "\n")
# 
# 
# 
# # EXERCITIUL II - b.
# # Functia de respingere
# 
# simulare <- function(n) {
#   x <- rep(0, n) # Initializarea unui vector pentru stocarea observatiilor generate
#   total <- 0
#   for (i in 1:n) {
#     x[i] <- rnorm(1, 0, 1)  # Generare a unei propuneri din distributia g(x)
#     total <- total + 1
#     u <- runif(1)
#     while (u > (f(x[i]) / (M * g(x[i])))) {  # Verificarea conditiei de acceptare/respingere
#       x[i] <- rnorm(1, 0, 1)  # Generarea unei noi propuneri in cazul respingerii
#       u <- runif(1)
#       total <- total + 1
#     }
#   }
#   return(list(x, total))
# }
# 
# # Generarea a 25000 de observatii
# observatii <- unlist(simulare(25000)[1])
# length(observatii)
# unlist(simulare(25000)[2])
# # Afisarea histogramei
# hist(observatii, freq = FALSE, breaks = 50, col = "deeppink", main = "Histograma observatiilor generate")
# 
# 
# # EXERCITIUL II - c.
# # Calculul ratei de acceptare
# #nr de puncte acceptate/ nr de iteratii efectuate(atat pucntele acceptate cat si cele respinse)
# A <-  length(observatii) / unlist(simulare(25000)[2])
# A
# 
# # Aproximarea constantei de normalizare
# M_aprox <- 1 / A
# 
# # Normalizarea functiei f(x)
# f_normalized <- function(x) {
#   f(x) / M_aprox
# }
# 
# # Reprezentarea grafica a functiei normalizate
# hist(observatii, breaks = 100, freq=FALSE, col = "deeppink")
# curve(f_normalized, from = -5, to = 5, add = TRUE,col = "blue", lwd = 2, ylab = "f(x)", xlab = "x",
#       main = "Functia de densitate de probabilitate normalizata")
# 
# 
# # EXERCITIUL III - 1.
# library(ConvergenceConcepts)
# # Setam nr de observatii
# n<-1000
# 
# # Generam n observatii din distributia Beta(1/n, 1/n)
# x <- runif(n, 0, 1)
# 
# #Beta este definita pe (0,1)
# #1
# # Xi~Beta(1/n,1/n)
# # Functia de densitate pentru variabilele Beta(1/n, 1/n)
# beta <- function(n){
#   dbeta(n, 1/n, 1/n)
# }
# 
# # Functia de densitate pentru distributia binomiala
# densitate_binomX <- function(x){
#   dbinom(x, 1, 1/2)
# }
# # Functia pentru distributia binomial
# distributie_binomX <- function(x){
#   pbinom(x, 1, 1/2)
# }
# 
# # Verificam convergenta in Lege (distributie)
# check.convergence(nmax = n, M = n/2, genXn = beta, mode = "L", density = F,
#                   densfunc = densitate_binomX,
#                   probfunc = distributie_binomX,
#                   tinf = 0,tsup = 1)
# # Se observa ca Fn-F -> 0 => converge in lege
# 
# #Xi~Beta(a/n,b/n)
# 
# # Functia pentru variabilele Beta(a/n, b/n)
# beta2 <- function(n, a = 33, b = 44){
#   dbeta(n, a/n, b/n)
# }
# 
# # Verificam convergenta in Lege (distributie)
# check.convergence(nmax = n, M = n/2, genXn = beta2, mode = "L", density = F,
#                   densfunc = densitate_binomX,
#                   probfunc = distributie_binomX,
#                   tinf = 0, tsup = 1)
# 
# # EXERCITIUL III - 2.
# n <- 1000
# x <- 1:n
# 
# # Xi~Random(1/n,1)
# # Generam n observatii uniform distribuite pe multimea {1/n, 2/n, ..., 1}
# UnifXi <- function(n){
#   runif(n, 1/n, 1)
# }
# 
# 
# # Functia de densitate pentru distributia uniforma pe intervalul (0, 1)
# densitate_unifX <- function(x){
#   dunif(x, 0, 1)
# }
# 
# 
# # Functia pentru distributia X~Unif(0,1)
# distributie_unifX <- function(x){
#   punif(x, 0, 1)
# }
# 
# # Verificam convergenta in Lege (distributie)
# check.convergence(nmax = n, M = n/2, genXn = UnifXi, mode = "L", density = FALSE,
#                   densfunc = densitate_unifX,
#                   probfunc = distributie_unifX,
#                   tinf = 0, tsup = 1)
# 
# # Nu converge in Lege pt ca nu tinde la 0
# 
# 
# # Verificam convergenta in Probabilitate (distributie)
# check.convergence(nmax = n, M = n/2, genXn = UnifXi, mode="p", density=TRUE,
#                   densfunc = densitate_unifX,
#                   probfunc = distributie_unifX,
#                   tinf = 0, tsup = 1)
# 
# # Nu converge in probabilitate pt ca p nu tinde la 0
# 
# # EXERCITIUL III - 3.
# n <- 1000
# x <- runif(n, 0, 1)
# m = min(x)
# M = max(x)
# 
# # Functia pentru minimul din n observatii
# UnifXi <- function(n){
#   min(x)
# }
# 
# # Functia de densitate pentru X~Unif(m,m)
# densitate_unifX <- function(x, ep = 0.005){
#   dunif(x, 0, 1)
# }
# 
# # Functia pentru distribuția X~Unif(0,1)
# distributie_unifX <- function(x, ep = 0.005){
#   punif(x, 0, 1)
# }
# 
# 
# # Verificam convergenta Aproape Sigura
# check.convergence(nmax = n, M = n/2, genXn = UnifXi, mode = "as", density = FALSE,
#                   densfunc = densitate_unifX,
#                   probfunc = distributie_unifX,
#                   tinf = m, tsup = M)
# 
# # Converge aproape sigur, pentru ca diferenta dintre valori tinde spre 0/ are valori mici
# 
# # Functia pentru maximul din n observatii
# UnifXi2 <- function(n){
#   max(x)
# }
# 
# # Functia de densitate pentru X~Unif(0, 1)
# densitate_unifX2 <- function(x, ep = 0.005){
#   dunif(x, 0, 1)
# }
# 
# # Functia pentru distributia pentru X~Unif(0,1)
# distributie_unifX2 <- function(x, ep = 0.005){
#   punif(x, 0, 1)
# }
# 
# 
# # Verificam convergenta Aproape Sigura
# check.convergence(nmax = n, M = n/2, genXn = UnifXi2, mode = "as", density = FALSE,
#                   densfunc = densitate_unifX2,
#                   probfunc = distributie_unifX2,
#                   tinf = m, tsup = M)
# 
# # Nu converge aproape sigur, pentru ca diferenta dintre valori nu tinde spre 0/ are valori mici
# 
# # EXERCITIUL IV - 1.a.
# set.seed(311)
# # Functie pentru a genera un bilet simplu de loto cu distributia uniforma discreta
# genereaza_bilet <- function() {
#   sample(1:49, 6, replace = FALSE)
# }
# 
# # Functie care verifica cate numere din biletul participantului se potrivesc cu numerele castigatoare
# verifica_potrivire <- function(bilet_jucat, bilet_castigator) {
#   intersectie <- intersect(bilet_jucat, bilet_castigator)
#   lungime_intersectie <- length(intersectie)
#   return(lungime_intersectie)
# }
# 
# # Functie pentru a estima probabilitatile de castig din prima incercare pentru fiecare categorie
# functie_estimare <- function(nr_simulari, bilet_castigator) {
#   # Vector care memoreaza numarul de castiguri pentru fiecare categorie
#   castiguri <- rep(0, 4)
# 
#   for (i in 1:nr_simulari) {
#     # Generam biletul participantului
#     bilet_jucat <- genereaza_bilet()
#     # Verificam daca este castigator
#     potriviri <- verifica_potrivire(bilet_jucat, bilet_castigator)
#     # Daca sunt cel putin 3 potriviri, actualizam numarul de castiguri pentru categoria respectiva
#     if (potriviri >= 3) {
#       castiguri[potriviri - 2] <- castiguri[potriviri - 2] + 1
#     }
#   }
# 
#   # Estimam probabilitatile de castig pentru fiecare categorie
#   probabilitati <- castiguri / nr_simulari
#   return(probabilitati)
# }
# 
# nr_simulari <- 10^6
# 
# # Numere castigatoare
# bilet_castigator <- genereaza_bilet()
# 
# # Probabilitatile de castig din prima incercare pentru fiecare categorie
# probabilitati_a <- functie_estimare(nr_simulari, bilet_castigator)
# probabilitati_a
# 
# # EXERCITIUL IV - 1.b.
# set.seed(311)
# # Functie pentru a genera un bilet simplu de loto cu distributia uniforma discreta
# genereaza_bilet <- function() {
#   sample(1:49, 6, replace = FALSE)
# }
# 
# # Functie care verifica cate numere din biletul participantului se potrivesc cu numerele castigatoare
# verifica_potrivire <- function(bilet_jucat, bilet_castigator) {
#   intersectie <- intersect(bilet_jucat, bilet_castigator)
#   lungime_intersectie <- length(intersectie)
#   return(lungime_intersectie)
# }
# 
# # Functie pentru a estima probabilitatea de castig pentru fiecare categorie dupa k incercari
# functie_estimare <- function(nr_simulari, bilet_castigator, k) {
#   # Vector care memoreaza numarul de castiguri pentru fiecare categorie
#   castiguri_total <- rep(0, 4)
# 
#   # Realizam simularile
#   for (i in 1:nr_simulari) {
#     # Vector care memoreaza numarul de castiguri pentru fiecare categorie in cadrul unei incercari
#     castiguri <- rep(0, 4)
#     # Realizam k incercari
#     for (j in 1:k) {
#       # Generam biletul participantului
#       bilet_jucat <- genereaza_bilet()
#       # Verificam daca este castigator
#       potriviri <- verifica_potrivire(bilet_jucat, bilet_castigator)
#       # Daca sunt cel putin 3 potriviri, actualizam numarul de castiguri pentru categoria respectiva
#       if (potriviri >= 3) {
#         castiguri[potriviri - 2] <- 1
#       }
#     }
#     # Adaugam rezultatele in totalul de castiguri
#     castiguri_total <- castiguri_total + castiguri
#   }
# 
#   # Estimam probabilitatile de castig pentru fiecare categorie
#   probabilitati <- castiguri_total / nr_simulari
#   return(probabilitati)
# }
# 
# # Numarul de incercari
# k <- 5
# nr_simulari <- 10^6
# 
# # Numerele castigatoare
# bilet_castigator <- genereaza_bilet()
# # Probabilitatea de a castiga dupa k incercari
# probabilitati_b <- functie_estimare(nr_simulari, bilet_castigator, k)
# probabilitati_b
# 
# 
# # EXERCITIUL IV - 1.c.
# set.seed(311)
# # Functie pentru a genera un bilet simplu de loto cu distributia uniforma discreta
# genereaza_bilet <- function() {
#   sample(1:49, 6, replace = FALSE)
# }
# 
# # Functie care verifica cate numere din biletul participantului se potrivesc cu numerele castigatoare
# verifica_potrivire <- function(bilet_jucat, bilet_castigator) {
#   intersectie <- intersect(bilet_jucat, bilet_castigator)
#   lungime_intersectie <- length(intersectie)
#   return(lungime_intersectie)
# }
# 
# # Functie pentru a estima probabilitatea de castig de r ori pentru fiecare categorie dupa k incercari
# functie_estimare <- function(nr_simulari, bilet_castigator, k , r) {
#   # Vector care memoreaza numarul de castiguri pentru fiecare categorie
#   castiguri_total <- rep(0, 4)
# 
#   # Realizam simularile
#   for (i in 1:nr_simulari) {
#     # Vector care memoreaza numarul de castiguri pentru fiecare categorie in cadrul unei incercari
#     castiguri <- rep(0, 4)
#     # Realizam k incercari
#     for (j in 1:k) {
#       # Generam biletul participantului
#       bilet_jucat <- genereaza_bilet()
#       # Verificam daca este castigator
#       potriviri <- verifica_potrivire(bilet_jucat, bilet_castigator)
#       # Daca sunt cel putin 3 potriviri, actualizam numarul de castiguri pentru categoria respectiva si iesim din bucla daca sunt cel putin r reusite
#       if (potriviri >= 3) {
#         castiguri[potriviri - 2] <- 1
#         if (sum(castiguri) >= r) {
#           break
#         }
#       }
#     }
#     # Adaugam rezultatele in totalul de castiguri
#     castiguri_total <- castiguri_total + castiguri
#   }
# 
#   # Estimam probabilitatile de castig pentru fiecare categorie
#   probabilitati <- castiguri_total / nr_simulari
#   return(probabilitati)
# }
# 
# # Numarul de incercari
# k <- 5
# r <- 2
# nr_simulari <- 10^6
# 
# # Numerele castigatoare
# bilet_castigator <- genereaza_bilet()
# # Probabilitatea de a castiga de r ori dupa k incercari
# probabilitati_c <- functie_estimare(nr_simulari, bilet_castigator, k , r)
# probabilitati_c
# 
# # EXERCITIUL IV - 1.d.
# set.seed(311)
# # Functie pentru a genera un bilet simplu de loto cu distributia uniforma discreta
# genereaza_bilet <- function() {
#   sample(1:49, 6, replace = FALSE)
# }
# 
# # Functie care verifica cate numere din biletul participantului se potrivesc cu numerele castigatoare
# verifica_potrivire <- function(bilet_jucat, bilet_castigator) {
#   intersectie <- intersect(bilet_jucat, bilet_castigator)
#   lungime_intersectie <- length(intersectie)
#   return(lungime_intersectie)
# }
# #  Functie pentru a estima probabilitatea de castig de r ori pentru fiecare categorie dupa k esecuri
# functie_estimare <- function(nr_simulari, bilet_castigator, r, k) {
#   # Vector care memoreaza numarul de castiguri pentru fiecare categorie
#   castiguri_total <- rep(0, 4)
# 
#   for (i in 1:nr_simulari) {
#     # Vector care memoreaza numarul de castiguri pentru fiecare categorie in cadrul unei incercari
#     castiguri <- rep(0, 4)
#     # Realizam k esecuri
#     nr_esecuri <- 0
#     for (j in 1:k) {
#       # Generam biletul participantului
#       bilet_jucat <- genereaza_bilet()
#       # Verificam daca este castigator
#       potriviri <- verifica_potrivire(bilet_jucat, bilet_castigator)
#       # Daca sunt cel putin 3 potriviri, actualizam numarul de castiguri pentru categoria respectiva si iesim din bucla daca sunt cel putin r reusite
#       if (potriviri >= 3) {
#         castiguri[potriviri - 2] <- 1
#         if (sum(castiguri) >= r) {
#           break
#         }
#       } else {
#         nr_esecuri <- nr_esecuri + 1
#         if (nr_esecuri >= k) {
#           break
#         }
#       }
#     }
#     # Adaugam rezultatele in totalul de castiguri
#     castiguri_total <- castiguri_total + castiguri
#   }
# 
#   # Estimam probabilitatile de castig pentru fiecare categorie
#   probabilitati <- castiguri_total / nr_simulari
#   return(probabilitati)
# }
# 
# r <- 2
# k <- 5
# 
# # Numerele castigatoare
# bilet_castigator <- genereaza_bilet()
# # Probabilitatea de a castiga de r ori dupa k esecuri
# probabilitati_d <- functie_estimare(nr_simulari, bilet_castigator, r, k)
# probabilitati_d
# 
# # EXERCITIUL IV - 1.e.
# set.seed(311)
# # Functie pentru a genera un bilet simplu de loto cu distributia uniforma discreta
# genereaza_bilet <- function() {
#   sample(1:49, 6, replace = FALSE)
# }
# 
# # Functie care verifica cate numere din biletul participantului se potrivesc cu numerele castigatoare
# verifica_potrivire <- function(bilet_jucat, bilet_castigator) {
#   intersectie <- intersect(bilet_jucat, bilet_castigator)
#   lungime_intersectie <- length(intersectie)
#   return(lungime_intersectie)
# }
# 
# # Functie care estimeaza probabilitatea de a castiga cel putin o data pe an in 30 de ani
# functie_estimare <- function(nr_simulari, nr_ani) {
#   # Contor pentru numarul de ani castigatori
#   ani_castigatori <- 0
# 
#   for (i in 1:nr_simulari) {
#     # Contor pentru castigurile dintr un an
#     castig_anual <- 0
#     for (k in 1:nr_ani * 52){
#     # Extragerile pentru fiecare saptamana din an
#     for (j in 1:2) {
#       # Numerele castigatoare
#       bilet_castigator <- genereaza_bilet()
#       # Generam biletul participantului
#       bilet_jucat <- genereaza_bilet()
#       # Verificam daca este castigator
#       potriviri <- verifica_potrivire(bilet_jucat, bilet_castigator)
#       # Daca sunt cel putin 3 potriviri, adaugam 1 la castigul anual si iesim din bucla
#       if (potriviri >= 3) {
#         castig_anual <- 1
#         break
#       }
#     }
#     }
#     # Daca am castigat macar o data in acest an, incrementam contorul ani_castigatori
#     if (castig_anual == 1) {
#       ani_castigatori <- ani_castigatori + 1
#     }
#   }
# 
#   # Estimam probabilitatea de a castiga cel putin o data pe an timp de 30 de ani
#   probabilitate <- ani_castigatori / nr_simulari
#   return(probabilitate)
# }
# nr_simulari <- 10^6
# nr_ani <- 30
# 
# # Probabilitatea de a castiga cel putin o data pe an in 30 de ani
# probabilitate_e <- functie_estimare(nr_simulari, nr_ani)
# probabilitate_e
# 
# 
# # EXERCITIUL IV - 1.f.
# set.seed(311)
# # Functie pentru a genera un bilet simplu de loto cu distributia uniforma discreta
# genereaza_bilet <- function() {
#   sample(1:49, 6, replace = FALSE)
# }
# 
# # Functie care verifica cate numere din biletul participantului se potrivesc cu numerele castigatoare
# verifica_potrivire <- function(bilet_jucat, bilet_castigator) {
#   intersectie <- intersect(bilet_jucat, bilet_castigator)
#   lungime_intersectie <- length(intersectie)
#   return(lungime_intersectie)
# }
# # Functie pentru a estima probabilitatea de a juca saptamanal, timp de un an, cate un bilet simplu si a nu castiga niciodata
# functie_estimare <- function(nr_simulari) {
#   an <- 0
#   for (i in 1:nr_simulari) {
#     # Contor pentru anii castigatori
#     an_castigator <- 0
#     # Extragerile pentru fiecare saptamana din an
#     for (j in 1:52) {
#       for (k in 1:2){
#       # Numerele castigatoare
#       bilet_castigator <- genereaza_bilet()
#       # Generam biletul participantului
#       bilet_jucat <- genereaza_bilet()
#       # Verificam daca este castigator
#       potriviri <- verifica_potrivire(bilet_jucat, bilet_castigator)
#       # Daca sunt cel putin 3 potriviri, trecem la urmatoarea simulare
#       if (potriviri >= 3 && an_castigator == 0) {
#         an_castigator <- an_castigator + 1
#       }
#       }
#     }
#     an <- an + an_castigator
#   }
# 
#   # Estimam probabilitatea de a juca saptamanal, timp de un an, cate un bilet simplu ai a nu castiga niciodata
#   probabilitate <- 1 - (an / nr_simulari)
#   return(probabilitate)
# }
# 
# nr_simulari <- 10^6
# 
# # Probabilitatea de a juca saptamanal, timp de un an, cate un bilet simplu si a nu castiga niciodata
# probabilitate_f <- functie_estimare(nr_simulari)
# probabilitate_f
# 
# # EXERCITIUL IV - 1.g.
# #############################################################
# 
# # Ne vom folosi de numarul mediu de castigatori de la 2.d.
# set.seed(311)
# # Functie pentru a genera un bilet simplu de loto cu distributia uniforma discreta
# genereaza_bilet <- function() {
#   sample(1:49, 6, replace = FALSE)
# }
# 
# # Functie care verifica cate numere din biletul participantului se potrivesc cu numerele castigatoare
# verifica_potrivire <- function(bilet_jucat, bilet_castigator, nr_participanti) {
#   intersectie <- intersect(bilet_jucat, bilet_castigator)
#   lungime_intersectie <- length(intersectie)
#   return(lungime_intersectie)
# }
# 
# # Functie pentru a estima numarul mediu de castigatori pentru fiecare categorie de la o extragere
# functie_estimare <- function(nr_simulari, bilet_castigator, nr_participanti) {
#   # Vector care memoreaza numarul de castiguri pentru fiecare categorie
#   castiguri <- rep(0, 4)
# 
#   for (i in 1:nr_simulari) {
#     # Generam biletul participantului
#     bilet_jucat <- genereaza_bilet()
#     # Verificam daca este castigator
#     potriviri <- verifica_potrivire(bilet_jucat, bilet_castigator)
#     # Daca sunt cel putin 3 potriviri, actualizam numarul de castiguri pentru categoria respectiva
#     if (potriviri >= 3) {
#       castiguri[potriviri - 2] <- castiguri[potriviri - 2] + 1
#     }
#   }
# 
#   # Estimam probabilitatile de castig pentru fiecare categorie
#   castigatori <- castiguri / nr_simulari * nr_participanti
#   return(castigatori)
# }
# 
# nr_simulari <- 10^6
# # Presupunem ca la o extragere participa 10^7 persoane
# nr_participanti <- 10^7
# 
# # Numere castigatoare
# bilet_castigator <- genereaza_bilet()
# 
# # Probabilitatile de castig din prima incercare pentru fiecare categorie
# nr_mediu_castigatori <- functie_estimare(nr_simulari, bilet_castigator, nr_participanti)
# nr_mediu_castigatori
# 
# ###########################################################################
# set.seed(311)
# # Functie pentru a genera un bilet simplu de loto cu distributia uniforma discreta
# genereaza_bilet <- function() {
#   sample(1:49, 6, replace = FALSE)
# }
# 
# # Functie care verifica cate numere din biletul participantului se potrivesc cu numerele castigatoare
# verifica_potrivire <- function(bilet_jucat, bilet_castigator) {
#   intersectie <- intersect(bilet_jucat, bilet_castigator)
#   lungime_intersectie <- length(intersectie)
#   return(lungime_intersectie)
# }
# # Functie pentru a calcula castigul corespunzator numarului de potriviri
# castig_potriviri <- function(potriviri) {
#   castig <- 0
#   if (potriviri == 3) {
#     castig <- 30
#   } else if (potriviri == 4) {if(nr_mediu_castigatori[1] !=0){
#     castig <- 363350 / nr_mediu_castigatori[1]
#   } else castig <-0
#    } else if (potriviri == 5) {if(nr_mediu_castigatori[2] !=0){
#     castig <- 390000 / nr_mediu_castigatori[2]
#    }  else castig <-0
#    } else if (potriviri == 6) { if(nr_mediu_castigatori[3] !=0){
#     castig <- 1090000 / nr_mediu_castigatori[3]
#   }  else castig <-0}
#   return(castig)
# }
# 
# # Functie pentru a estima probabilitatea de a juca saptamanal, timp de un an, cate un bilet simplu ai de a castiga, cumulat, o suma mai mare decat costul total al biletelor jucate
# functie_estimare <- function(nr_simulari, cost_bilet) {
#   # Contor sume castigate cumulat
#   sume_castigate_cumulat <- 0
# 
#     for (i in 1:nr_simulari) {
#     # Contor suma castigan un an
#     suma_castigata_an <- 0
# 
#     for (j in 1:52) {
#       for(k in 1:2){
#       # Numerele castigatoare
#       bilet_castigator <- genereaza_bilet()
#       # Generam biletul participantului
#       bilet_jucat <- genereaza_bilet()
#       # Verificam daca este castigator
#       potriviri <- verifica_potrivire(bilet_jucat, bilet_castigator)
#       # Actualizam suma castigata in an in functie de numarul de potriviri
#       suma_castigata_an <- suma_castigata_an + castig_potriviri(potriviri)
#       }
#     }
# 
#     # Daca suma castigata in acest an este mai mare decat costul total al biletelor, adaugam 1 la contorul sume_castigate_cumulat
#     if (suma_castigata_an > (52 * 2 * cost_bilet)) {
#       sume_castigate_cumulat <- sume_castigate_cumulat + 1
#     }
#   }
# 
#   # Estimam probabilitatea de a juca saptamanal, timp de un an, cate un bilet simplu ai de a castiga, cumulat, o suma mai mare decat costul total al biletelor jucate
#   probabilitate <- sume_castigate_cumulat / nr_simulari
#   return(probabilitate)
# }
# 
# cost_bilet <- 7
# nr_simulari <- 10^6
# probabilitate_g <- functie_estimare(nr_simulari, cost_bilet)
# probabilitate_g
# 
# 
# # EXERCITIUL IV - 2.a.
# set.seed(311)
# # Functie pentru a genera un bilet simplu de loto cu distributia uniforma discreta
# genereaza_bilet <- function() {
#   sample(1:49, 6, replace = FALSE)
# }
# 
# # Functie care verifica cate numere din biletul participantului se potrivesc cu numerele castigatoare
# verifica_potrivire <- function(bilet_jucat, bilet_castigator) {
#   intersectie <- intersect(bilet_jucat, bilet_castigator)
#   lungime_intersectie <- length(intersectie)
#   return(lungime_intersectie)
# }
# 
# # Functie pentru a estima castigul mediu pe un an al Loteriei
# functie_estimare <- function(nr_simulari) {
#   castig <- 0
#   for (j in 1:52)
#   {for(k in 1:2){
#       # Acestea tin cont de castigurile participantilor si numarul celor care au castigat la o singura extragere
#       castig_3_numere <- 0
#       castig_4_numere <- 0
#       castig_5_numere <- 0
#       castig_6_numere <- 0
#       oameni_4 <- 0
#       oameni_5 <- 0
#       oameni_6 <- 0
#       # Numere castigatoare
#       bilet_castigator <- genereaza_bilet()
#       # Extragerile pentru fiecare saptamana din an
#       for (i in 1:nr_simulari) {
#         # Generam biletul participantului
#         bilet_jucat <- genereaza_bilet()
#         # Verificam daca este castigator
#         num_potrivite <- verifica_potrivire(bilet_jucat, bilet_castigator)
#         # Daca sunt cel putin 3 potriviri, actualizam numarul de castiguri pentru categoria respectiva
#         if (num_potrivite == 3) {
#           castig_3_numere <- castig_3_numere + 30
#         } else if (num_potrivite == 4) {
#           castig_4_numere <- castig_4_numere + 363350
#           oameni_4 <- oameni_4 + 1
#         } else if (num_potrivite == 5) {
#           castig_5_numere <- castig_5_numere + 390000
#           oameni_5 <- oameni_5 + 1
#         } else if (num_potrivite == 6) {
#           castig_6_numere <- castig_6_numere +  1090000
#           oameni_6 <- oameni_6 + 1
#         }
# 
#       }
#       if(oameni_4 != 0){
#         castig_4_numere <- castig_4_numere / oameni_4
#       }
#       if(oameni_5 != 0){
#         castig_5_numere <- castig_5_numere / oameni_5
#       }
#       if(oameni_6 != 0){
#         castig_6_numere <- castig_6_numere / oameni_6
#       }
#       castig <- castig + castig_3_numere + castig_4_numere +  castig_5_numere + castig_6_numere
#     }}
#   suma <- castig
#   suma_bilete_vandute <- 7 * 2 * 52 * nr_simulari
#   castig_loterie <- suma_bilete_vandute - suma
#   return(castig_loterie)
# }
# 
# # Numarul presupus de participanti
# nr_simulari <- 10^6
# 
# # Estimarea castigul mediu pe un an al Loteriei
# castig_mediu <- functie_estimare(nr_simulari)
# castig_mediu
# 
# 
# # EXERCITIUL IV - 2.b.
# set.seed(311)
# # Functie pentru a genera un bilet simplu de loto cu distributia uniforma discreta
# genereaza_bilet <- function() {
#   sample(1:49, 6, replace = FALSE)
# }
# 
# # Functie care verifica cate numere din biletul participantului se potrivesc cu numerele castigatoare
# verifica_potrivire <- function(bilet_jucat, bilet_castigator) {
#   intersectie <- intersect(bilet_jucat, bilet_castigator)
#   lungime_intersectie <- length(intersectie)
#   return(lungime_intersectie)
# }
# 
# # Functie pentru a estima numarul minim de bilete vandute pentru ca Loteria sa nu iasa in pierdere
# functie_estimare <- function(nr_simulari) {
#   castig <- 0
#   for (j in 1:52)
#   {for(k in 1:2){
#       # Acestea tin cont de castigurile participantilor si numarul celor care au castigat la o singura extragere
#       castig_3_numere <- 0
#       castig_4_numere <- 0
#       castig_5_numere <- 0
#       castig_6_numere <- 0
#       oameni_4 <- 0
#       oameni_5 <- 0
#       oameni_6 <- 0
#       # Numere castigatoare
#       bilet_castigator <- genereaza_bilet()
#       # Extragerile pentru fiecare saptamana din an
#       for (i in 1:nr_simulari) {
#         # Generam biletul participantului
#         bilet_jucat <- genereaza_bilet()
#         # Verificam daca este castigator
#         num_potrivite <- verifica_potrivire(bilet_jucat, bilet_castigator)
#         # Daca sunt cel putin 3 potriviri, actualizam numarul de castiguri pentru categoria respectiva
#         if (num_potrivite == 3) {
#           castig_3_numere <- castig_3_numere + 30
#         } else if (num_potrivite == 4) {
#           castig_4_numere <- castig_4_numere + 363350
#           oameni_4 <- oameni_4 + 1
#         } else if (num_potrivite == 5) {
#           castig_5_numere <- castig_5_numere + 390000
#           oameni_5 <- oameni_5 + 1
#         } else if (num_potrivite == 6) {
#           castig_6_numere <- castig_6_numere +  1090000
#           oameni_6 <- oameni_6 + 1
#         }
# 
#       }
#       if(oameni_4 != 0){
#         castig_4_numere <- castig_4_numere / oameni_4
#       }
#       if(oameni_5 != 0){
#         castig_5_numere <- castig_5_numere / oameni_5
#       }
#       if(oameni_6 != 0){
#         castig_6_numere <- castig_6_numere / oameni_6
#       }
#       castig <- castig + castig_3_numere + castig_4_numere +  castig_5_numere + castig_6_numere
#     }}
#   suma <- castig
#   bilete_vandute <- suma / 7
#   return(bilete_vandute)
# }
# 
# # Numarul presupus de participanti
# nr_simulari <- 10^6
# 
# # Estimarea numarului minim de bilete vandute pentru ca Loteria sa nu iasa in pierdere
# bilete_vandute <- functie_estimare(nr_simulari)
# bilete_vandute
# 
# 
# # EXERCITIUL IV - 2.d.
# set.seed(311)
# # Functie pentru a genera un bilet simplu de loto cu distributia uniforma discreta
# genereaza_bilet <- function() {
#   sample(1:49, 6, replace = FALSE)
# }
# 
# # Functie care verifica cate numere din biletul participantului se potrivesc cu numerele castigatoare
# verifica_potrivire <- function(bilet_jucat, bilet_castigator, nr_participanti) {
#   intersectie <- intersect(bilet_jucat, bilet_castigator)
#   lungime_intersectie <- length(intersectie)
#   return(lungime_intersectie)
# }
# 
# # Functie pentru a estima numarul mediu de castigatori pentru fiecare categorie de la o extragere
# functie_estimare <- function(nr_simulari, bilet_castigator, nr_participanti) {
#   # Vector care memoreaza numarul de castiguri pentru fiecare categorie
#   castiguri <- rep(0, 4)
# 
#   for (i in 1:nr_simulari) {
#     # Generam biletul participantului
#     bilet_jucat <- genereaza_bilet()
#     # Verificam daca este castigator
#     potriviri <- verifica_potrivire(bilet_jucat, bilet_castigator)
#     # Daca sunt cel putin 3 potriviri, actualizam numarul de castiguri pentru categoria respectiva
#     if (potriviri >= 3) {
#       castiguri[potriviri - 2] <- castiguri[potriviri - 2] + 1
#     }
#   }
# 
#   # Estimam probabilitatile de castig pentru fiecare categorie
#   castigatori <- castiguri / nr_simulari * nr_participanti
#   return(castigatori)
# }
# 
# nr_simulari <- 10^6
# # Presupunem ca la o extragere participa 10^7 persoane
# nr_participanti <- 10^7
# 
# # Numere castigatoare
# bilet_castigator <- genereaza_bilet()
# 
# # Probabilitatile de castig din prima incercare pentru fiecare categorie
# nr_mediu_castigatori <- functie_estimare(nr_simulari, bilet_castigator, nr_participanti)
# nr_mediu_castigatori
# 
# # EXERCITIUL IV - 4.
# 
# # Alegem din histograma cele mai frecvente 6 numere care au fost extrase pentru castig
# 
# generare_bilet <- function() {
#   return(sample(1:49, 104*6, replace = TRUE))
# }
# 
# # Cream un cadru de date initial cu toate valorile initiale pe zero
# df <- data.frame(matrix(0, nrow = 20, ncol = 104 * 6))
# 
# # Iteram prin fiecare an si generam 104 extrageri de 6 numere aleatorii pentru fiecare an
# for (i in 1:20) {
#   df[i, ] <- generare_bilet()
# }
# 
# # Reshape la dataframe pentru a avea 6 coloane pentru fiecare extragere
# df <- data.frame(matrix(unlist(df), nrow = 20, byrow = TRUE))
# 
# # Atribuim nume coloanelor
# colnames(df) <- paste0("Extragere_", 1:(104 * 6))
# 
# # Adaugam o coloana cu anii
# df$An <- seq(2001, 2020)
# 
# 
# # Calculam frecvenaa fiecarui numar
# frecventa_numerelor <- table(as.matrix(df[, 1:(104*6)]))
# 
# # Cream un grafic de bare pentru frecventa numerelor
# barplot(frecventa_numerelor,
#         main = "Frecventa Numerelor in Extrageri de Loterie",
#         xlab = "Numarul Extragerii",
#         ylab = "Frecventa",
#         col = "deeppink",
#         las = 1)
