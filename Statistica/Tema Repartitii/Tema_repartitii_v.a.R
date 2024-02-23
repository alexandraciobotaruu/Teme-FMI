#1. Durata necesară(exprimată ȋn ore) pentru reparaţia unei maşini este o variabilă aleatoare
#repartizată exponenţial de parametru λ=1/3. Determinaţi:

#X -Exp(1/3)
lambda <- 1/3
# P(X>3) = 1 - P(X<=3)= 1- F(3)

# a) Probabilitatea ca reparaţia să dureze mai mult de 3 ore

1-pexp(3, lambda)

#b) Probabilitatea ca reparaţia să dureze 12 ore ştiind că reparaţia durează mai mult de 9 ore

#Probabilitatea intr-un singur punct e 0, gen daca zice ceva fix, cum avem 12 ore, ne da 0
#Asa nu

#P(X=12|X>9)=P(X=12)/P(X>9)
epsilon<-1/10 #facem cu un interval in jurul lui epsilon
#Reimaginam'
#P(12-epsilon<x<12+epsilon|x>9)=P(12-epsilon<x<12+epsilon)/P(X>9)
(pexp(12+epsilon,1/3)-pexp(12-epsilon,1/3))/(1-pexp(9,1/3))


#2. Presupunem că numărul de viroze respiratorii pe care le suferă o persoană ȋntr-un an este o
#variabilă aleatoare repartizată Poisson de parametru λ=5. Un vaccin minune apare pe piaţă care
#se dovedeşte eficient ȋn cazul a 75% din persoanele cărora acesta le-a fost administrat, reducând
#parametrul repartiţiei Poisson la λ=3(pentru restul populaţiei se presupune că nu are un efect
#considerabil). Cu ce probabilitate o persoana căreia i s-a administrat vaccinul şi care s-a
#ȋmbolnăvit de 2 ori ȋntr-un an(de viroze respiratorii!) se află printre cei 75% din populaţie
#pentru care vaccinul a fost eficient?

# SOLUTIE:

# Th. lui Bayes : P(A|B)= [ P(B|A) * P(A)]/ P(B)
# A = persoana care primeste vaccinul si e eficient (75%)
# B = persoana se imbolnaveste de 2 ori pe an
# P(A) = 0.75
p_a <- 0.75

# P(B|A) = prob ca pers cu vaccin s-a imbolnavit (lambda = 3, vaccin eficient)
ppois(2, 3, lower.tail = TRUE, log.p = FALSE) 

# P(B) = P(A|B) * P(A) + P(B|A^c)* P(A^c)
#  P(B|A^c) = 0.25
# P(A^c) = 0.25
p_ac <- 0.25
# P(X = k) =( e^(-lambda)* lambda^k)/ k!  -----> repartitia poisson

# P(B)
ppois(2, 3, lower.tail = TRUE, log.p = FALSE) * p_a + ppois(2, 5, lower.tail = TRUE, log.p = FALSE) * p_ac 

((ppois(2, 3, lower.tail = TRUE, log.p = FALSE))* p_a)/(ppois(2, 3, lower.tail = TRUE, log.p = FALSE) * p_a + ppois(2, 5, lower.tail = TRUE, log.p = FALSE) * p_ac) 

#3. Un lot format din 100 de produse este supus controlului de calitate. Se extrag cinci produse
#din lot, fără revenire. Dacă se găseşte un produs defect atunci lotul se respinge. Ştiind că 5%
#din produse sunt defecte determinaţi probabilităţile următoarelor evenimente:
 # a) Lotul este acceptat
 # b) Lotul este respins
 # c)Lotul este respins după a treia verificare

p_d <- 0.05 # d = evenimentul ca un produs este defect
p_n <- 0.95 # n = evenimentul ca un produs este nedefect

# a)
p_a <- p_n * p_n *p_n *p_n *p_n # P(A), A= acceptat
# b)
p_r <- 1 - p_a ## P(R), R= respins
# c)
p_n * p_n * p_n * p_d * (p_d * p_n)

# 4. Se ştie că ȋnalţimea(măsurată ȋn cm) bărbaţilor de 65 de ani este o variabilă aleatoare
# repartizată normal de medie m=173 cm şi dispersie 16. Determinaţi ce procent din aceşti barbaţi
# au ȋnălţimea mai mare de 176 cm.
# Datele
m <- 173
deviatie <- 4
inaltime_cautata <- 176

# Calculul probabilității folosind distribuția normală standard
z <- (inaltime_cautata - m) / deviatie
probabilitate <- 1 - pnorm(z)

# Afișarea rezultatului sub formă de procent
procent_probabilitate <- probabilitate * 100
cat("Procentul de bărbați de 65 de ani cu înălțimea mai mare de 176 cm este:", round(procent_probabilitate, 2), "%\n")

#5. Dintr-o urnă ce conţine 150 de bile roz şi 100 de bile mov se extrag, cu revenire, cinci
#bile. Fie X variabila aleatoare ce indică numărul bilelor roz obţinute, ȋn total, ȋn urma celor
#cinci extrageri.
#Determinaţi:

 # a) Repartiţia v.a. X

N <- 250
n <- 5
m <- 150

# Valori posibile pentru k (0 până la 5)
k <- 0:5

# Calcularea probabilităților pentru fiecare valoare posibilă a lui k
probabilitati <- dhyper(k, m, N - m, n)

# Afișarea probabilităților
for (i in 1:length(k)) {
  cat("P(X =", k[i], ") =", probabilitati[i], "\n")
}

# b) P(X = 4 ), P(X ≥ 1/2), P(X < π/3), P(X ≤ 2 și X > 0.2)


# P(X ≥ 1/2)
probabilitate_X_ge_1_2 <- sum(dhyper(0:5, m, N - m, n))

# P(X < π/3)
probabilitate_X_lt_pi_3 <- sum(dhyper(0:2, m, N - m, n))

# P(X ≤ 2 și X > 0.2)
probabilitate_X_le_2_and_gt_0_2 <- sum(dhyper(1:2, m, N - m, n))

cat("P(X = 4) =", probabilitate_X4, "\n")
cat("P(X ≥ 1/2) =", probabilitate_X_ge_1_2, "\n")
cat("P(X < π/3) =", probabilitate_X_lt_pi_3, "\n")
cat("P(X ≤ 2 și X > 0.2) =", probabilitate_X_le_2_and_gt_0_2, "\n")

# c) Calculul funcției de repartiție F(7/2)

x <- 7/2
probabilitate_F_7_2 <- sum(dhyper(0:floor(x), m, N - m, n))

cat("F(7/2) =", probabilitate_F_7_2, "\n")

#d) E(X), (Var(X)

# Calculul mediei (E(X))
media_X <- n * (m / N)

# Calculul varianței (Var(X))
varianta_X <- n * (m / N) * (1 - m / N) * (N - n) / (N - 1)

cat("Media (E(X)) =", media_X, "\n")
cat("Varianța (Var(X)) =", varianta_X, "\n")

#6. O monedă nemăsluită (echilibrată) este aruncată până când capul apare de 10 ori. Fie X o
# v.a. ce numără de câte ori apare pajura ȋn cadrul acestor aruncări. Determinaţi funcţia de masă
# a v.a. X.


# Numărul de aruncări necesare (de la 1 la 10)
k <- 1:10

# Probabilitatea de a obține un cap într-o singură aruncare (1/2 pentru o monedă echilibrată)
p <- 1/2

# Calculul probabilităților folosind dgeom()
probabilitati <- dgeom(k, prob = p)

# Afișarea probabilităților
for (i in 1:length(k)) {
  cat("P(X =", k[i], ") =", probabilitati[i], "\n")
}

#7. Fie X o v.a. repartizată normal cu media 3 şi dispersia 49. Determinaţi valoarea
# parametrului c pentru care P(X>c)=0.15

media <- 3
dispersia <- 49
probabilitate <- 0.15

# Calculul valorii corespunzătoare lui c
c <- qnorm(1 - probabilitate, mean = media, sd = sqrt(dispersia))

cat("Valoarea lui c pentru P(X > c) = 0.15 este:", c, "\n")

#8. Profitul anual al unui agent economic, exprimat ȋn milioane de unităţi monetare este o v.a.
#continuă X având densitatea de probabilitate
#f :R → R, f(x) = k*x^5*(1-x)^7
#Determinaţi:
  #a) Valoarea parametrului k

densitate <- function(x) {
  k <- 1 / integrate(function(x) x^5 * (1 - x)^7, lower = 0, upper = 1)$value
  return(k * x^5 * (1 - x)^7)
}

k <- 1 / (integrate(function(x) x^5 * (1 - x)^7, lower = 0, upper = 1)$value)

cat("Valoarea parametrului k este:", k, "\n")

# b) Profitul mediu şi dispersia v.a. profit.


# Calculul profitului mediu (media) al lui X
media_X <- integrate(function(x) x * densitate(x), lower = 0, upper = 1)$value

cat("Profitul mediu (E(X)) este:", media_X, "\n")

# Calculul profitului mediu la pătrat (E(X^2))
media_X2 <- integrate(function(x) x^2 * densitate(x), lower = 0, upper = 1)$value
cat("Media (E(X^2)) este:", media_X2, "\n")
# Calculul dispersiei lui X
variance_X <- media_X2 - media_X^2

cat("Dispersia (Var(X)) este:", variance_X, "\n")

#9. Ȋntr-un cazino intră ȋn medie o persoană la 10 minute. Determinaţi:

#a) Probabilitatea ca nicio persoană să nu intre ȋn cazino ȋn intervalul 12:00-12:30.

lambda <- 3

# Probabilitatea ca nicio persoană să nu intre în intervalul de 30 de minute
probabilitate_a <- dpois(0, lambda)

cat("Probabilitatea ca nicio persoană să nu intre în intervalul 12:00-12:30 este:", probabilitate_a, "\n")


#b) Probabilitatea ca cel puţin 4 persoane să intre ȋn cazino ȋn intervalul 12:00-12:30.

# Probabilitatea că mai puțin de 4 persoane intră în intervalul de 30 de minute
probabilitate_mai_putin_de_4 <- ppois(3, lambda)
cat("Probabilitatea ca mai puțin de 4 persoane să intre în intervalul 12:00-12:30 este:", probabilitate_mai_putin_de_4, "\n")

probabilitate_b <- 1 - probabilitate_mai_putin_de_4

cat("Probabilitatea ca cel puțin 4 persoane să intre în intervalul 12:00-12:30 este:", probabilitate_b, "\n")

#10. Dintr-o urnă ce conţine 50 de bile roz şi 200 de bile mov se extrag, fără revenire, cinci
# bile. Fie X variabila aleatoare ce indică numărul bilelor roz obţinute, ȋn total, ȋn urma celor
# cinci extrageri.Determinaţi:
  #a) Repartiţia v.a. X

N <- 250
n <- 5
m <- 50

# Valori posibile pentru k (0 până la 5)
k <- 0:5

# Calcularea probabilităților pentru fiecare valoare posibilă a lui k
probabilitati <- dhyper(k, m, N - m, n)

# Afișarea probabilităților
for (i in 1:length(k)) {
  cat("P(X =", k[i], ") =", probabilitati[i], "\n")
}


#b) P(X = 1), P (X ≥ 5/2), P (X < π/3) şi P(X ≤ 2 / X > 0.8)

# P(X ≥ 5/2 )
probabilitate_X_ge_5_2 <- sum(dhyper(0:5, m, N - m, n))

# P(X < π/3)
probabilitate_X_lt_pi_3 <- sum(dhyper(0:2, m, N - m, n))

# P(X ≤ 2 și X > 0.8)
probabilitate_X_le_2_and_gt_0_8 <- sum(dhyper(1:2, m, N - m, n))


cat("P(X ≥ 5/2) =", probabilitate_X_ge_5_2, "\n")
cat("P(X < π/3) =", probabilitate_X_lt_pi_3, "\n")
cat("P(X ≤ 2 și X > 0.8) =", probabilitate_X_le_2_and_gt_0_8, "\n")

#c) F (11/5), unde F este funcţia de repartiţie a v.a. X

x <- 11/5
probabilitate_F_11_5 <- sum(dhyper(0:floor(x), m, N - m, n))

cat("F(11/5) =", probabilitate_F_11_5, "\n")

#d) E(X),Var(X)

media_X <- n * (m / N)

varianta_X <- n * (m / N) * (1 - m / N) * (N - n) / (N - 1)

cat("Media (E(X)) =", media_X, "\n")
cat("Varianța (Var(X)) =", varianta_X, "\n")

#11. Un test folosit pentru diagnosticarea sindromului obsesivo-compulsiv are o acurateţe de
# 90%(i.e. dacă persoana are sindromul atunci rezultatul testului va fi pozitiv cu o probabilitate
# de 0.9, iar dacă persoana nu are sindromul atunci rezultatul testului va fi negativ cu o
# probabilitate de 0.9). Ştiind că sindromul apare ȋn medie la 1% din populaţie, determinaţi:
 
# a) Care este probabilitatea ca persoana să aibă acest sindrom dacă rezultatul testului este
#pozitiv ȋn cazul unei persoane luate la ȋntâmplare

prob_sindrom <- 0.01  # Probabilitatea de a avea sindromul
prob_test_pozitiv_sindrom <- 0.9  # Probabilitatea ca testul să fie pozitiv dacă are sindromul
prob_test_pozitiv_nu_are_sindrom <- 0.1  # Probabilitatea ca testul să fie pozitiv dacă nu are sindromul

# Probabilitatea că testul este pozitiv
prob_test_pozitiv <- (prob_test_pozitiv_sindrom * prob_sindrom) + (prob_test_pozitiv_nu_are_sindrom * (1 - prob_sindrom))

# Probabilitatea că are sindromul dacă testul este pozitiv (Teorema lui Bayes)
prob_are_sindrom <- (prob_test_pozitiv_sindrom * prob_sindrom) / prob_test_pozitiv

cat("Probabilitatea că persoana are sindromul dacă testul este pozitiv:", prob_are_sindrom, "\n")

#b) Care este probabilitatea ca persoana să nu aibă acest sindrom dacă rezultatul testului
#este negativ

prob_nu_are_sindrom <- 1 - prob_sindrom  # Probabilitatea de a nu avea sindromul
prob_test_negativ_sindrom <- 0.1  # Probabilitatea ca testul să fie negativ dacă are sindromul
prob_test_negativ_nu_are_sindrom <- 0.9  # Probabilitatea ca testul să fie negativ dacă nu are sindromul

# Probabilitatea că testul este negativ
prob_test_negativ <- (prob_test_negativ_sindrom * prob_sindrom) + (prob_test_negativ_nu_are_sindrom * prob_nu_are_sindrom)

# Probabilitatea că nu are sindromul dacă testul este negativ (Teorema lui Bayes)
prob_nu_are_sindrom <- (prob_test_negativ_nu_are_sindrom * prob_nu_are_sindrom) / prob_test_negativ

cat("Probabilitatea că persoana nu are sindromul dacă testul este negativ:", prob_nu_are_sindrom, "\n")

