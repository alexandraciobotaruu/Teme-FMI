###########. Ex 1 ############

# Construiti functii ȋn R care sa implementeze algoritmi de simulare pentru urmatoarele
#situatii descrise:

#### a)
# X ~ Cauchy (x0,y)
# F(x) = 1/2 + 1/pi(arctg((x-x0)/y))


simulare_cauchy <- function(n,x0,y){
  u <- runif(n)
  x <- x0 + y * tan(pi * (u - 0.5))
  return(x)
}

set.seed(576)
valori_cauchy <- simulare_cauchy(n = 100, x0 = 0, y = 1)
print(valori_cauchy)

#histograma
hist(valori_cauchy, freq = F)
t <- seq(min(valori_cauchy),max(valori_cauchy),1) 
lines(t,dcauchy(t, location = 0, scale = 1), col = "magenta")
#observam ca obtinem si numere negative.

## b)

simulare_discreta <- function(n,val,p){
  x <- rep(0,n) # vectori de 0 de n ori
  i <- 1
  while (i <= n){
    u <- runif(1)
    if (u < p[1]){
      x[i] <- val[1]
    } 
    else if ((p[1]+p[2])< u ){
      x[i] <-  val[2]
    }
    else x[i] <- val[3]
    i <- i + 1
  }
  return(x)
}

set.seed(576)
valori_discreta <- simulare_discreta(n = 100, val= c(1,2,3), p = c(1/2,1/3,1/6))
print(valori_discreta)
hist(valori_discreta, freq = F, breaks = c(0.5, 1.5, 2.5, 3.5))


# c)

simulare_c <- function(n, x1, x2, p) {
  x <- rep(0,n) # vectori de 0 de n ori
  i <- 1
  while (i <= n){
    u <- runif(1)
    if (u < p ){
      x[i] <- x2
    } 
    else x[i] <- x1
    i <- i + 1
  }
  return(x)
}

set.seed(576)
valori_c<- simulare_c(n = 100, x1 = 3, x2 = 7, p = 0.4)
print(valori_c)
hist(valori_c, freq = F,  breaks = c(2.5, 3.5, 6.5, 7.5))

#d)
#extragerea unui nr intreg cuprins intre a si b 

simulare_perturbata_aleator <- function(t,a, b) {
  n <- b - a + 1 # Numarul total de valori din interval
  k_m <- (t %% n) + a  # Pozitia corecta in interval
  
  # Calculam ponderile exponentiale
  k <- seq(a, b)
  #f <- exp(-abs(k - k_m)) #am observat cu aceasta modalitate, valorile sunt aproape numai de a
  f <- 1 / (1 + abs(k - k_m)) # Penalizare liniara in loc de exponentiala
  # Normalizam ponderile pentru a obtine probabilitati
  probabilitatile <- f / sum(f)
  
  # Alegem un numar pe baza probabilitatilor calculate
  x <- sample(k, size = 1, prob = probabilitatile)
  #prob_x <- exp(-abs(x - k_m))/sum(f)
  #x_prob <- c(x,t,k_m,prob_x)
  return(x) #return(x_prob)
}

set.seed(576)
# Obtinem ora curenta
ora <- as.numeric(format(Sys.time(), "%H"))
valori_sim_pert<- simulare_perturbata_aleator(t= ora,a=2, b= 40)
print(valori_sim_pert)


#linie STB
#ipoteza

set.seed(576)
xmin <- simulare_perturbata_aleator(t= ora,a= 100, b=350)
xmax <- simulare_perturbata_aleator(t= ora,a= 671, b=1000)
y <- simulare_perturbata_aleator(t= ora,a=xmin, b=xmax)


tipuri_zile <- simulare_discreta(n = 30, val= c(1,2,3), p = c(1/2,1/3,1/6))
zile_lejere <- sum(tipuri_zile == 1)
zile_normale <- sum(tipuri_zile == 2)
zile_aglomerate <- sum(tipuri_zile == 3)


pret_bilet <- 3 
pret_abonament <-  70

set.seed(576)
pasageri_x <- function(x){
  v <- simulare_cauchy(n = 1, x0 = 0, y = 1)
  pr <- abs(v - floor(v)) #partea fractionara a numarului v, atribuita lui p de la c)
  
  # x1,x2 extrase uniform 
  x1 <- sample(1:99, 1) 
  x2 <- sample(1:99, 1) 
  x_bilet_vector <- simulare_c(n = 1, x1 = x1, x2 = x2, p = pr) #x% din cei care nu platesc abonament au bilet
  x_bilet <- x_bilet_vector[1]
  
  z <- abs(floor(simulare_cauchy(n = 1, x0 = 5, y = 2))) 
  x_abonament <- simulare_perturbata_aleator(t = z, a=1, b= 99) # x% din total achizitioneava ab
  
  x_procent <- c(x_bilet,x_abonament)
  return (x_procent)
}

valori_pasageri_x <- pasageri_x()
#cu datele de mai sus, incepem sa rezolvam subpunctele:


#Rezolvare 
#################### a) Simulam numarul de calatori pentru fiecare zi din decembrie 2024

set.seed(576)
simulare_calatori<- function(n){
  i <- 1
  x <- rep(0,n)
  while (i<=n){
    xmin <- simulare_perturbata_aleator(t= ora,a= 100, b=350) #am observat ca valorile sunt mai apropiate de a, asa ca am pus un a mai mare
    xmax <- simulare_perturbata_aleator(t= ora,a= 671, b=1500)
    x[i] <- simulare_perturbata_aleator(t= ora,a=xmin, b=xmax)
    i <- i + 1
  }
  return (x)
}
calatori_dec <- simulare_calatori(31)
print(calatori_dec)

# Constructia histogramei
hist(calatori_dec, freq = F, 
     main = "Distributia numarului de calatori in decembrie 2024",
     xlab = "Numar de calatori", ylab = "Densitate")
lines(density(calatori_dec), col = "red", lwd = 2)



#################### b) simulare toate lunile:

# Functie pentru simularea datelor unei luni
simulare_luna <- function(n) {
  # Generam numarul de calatori pentru o luna
  calatori <- simulare_calatori(n)
  print(calatori)
  
  # Calculam statistici
  media <- round(sum(calatori)/n,2)
  minim <- min(calatori)
  maxim <- max(calatori)
  
  lejere <- sum(calatori < 350)
  normale <- sum(calatori>= 351 & calatori <= 670)
  aglomerate <- sum(calatori > 670)
  
  procent_lejere <- lejere/n *100
  procent_normale<- normale/n *100
  procent_aglomerate <- aglomerate/n *100
  
  
  # Returnam un vector cu rezultatele
  return(c(Media = media, Min = minim, Max = maxim, 
           Lejere = procent_lejere, Normale = procent_normale, Aglomerate = procent_aglomerate))
}

# Numarul de zile din fiecare luna din 2024 (an bisect)
zile_luni <- c(
  ianuarie = 31, februarie = 29, martie = 31, aprilie = 30, mai = 31,
  iunie = 30, iulie = 31, august = 31, septembrie = 30,
  octombrie = 31, noiembrie = 30, decembrie = 31
)

# Dataframe pentru stocarea rezultatelor
rezultate <- data.frame(
  Luna = names(zile_luni),
  Media = numeric(length(zile_luni)),
  Min = numeric(length(zile_luni)),
  Max = numeric(length(zile_luni)),
  Lejere = numeric(length(zile_luni)),
  Normale = numeric(length(zile_luni)),
  Aglomerate = numeric(length(zile_luni))
)

# Iteram prin fiecare luna si simulam datele
for (i in seq_along(zile_luni)) {
  rezultate[i, 2:7] <- simulare_luna(zile_luni[i])
}

# Afisam rezultatele
print(rezultate)

#################### c)
# Avem functia simulare_calatori si stim ca x_abonament % din calatori achizitioneana abonament si x_bilet% cumpara bilet 
# pe noi ne intereseaza: nr pasageri cu abonament, nr pasageri cu bilet si nr pasageri care nu platesc bilet
# sa determinam pret total din abonamenete si bilete in fiecare luna si veniturile care ar fi trebuit sa fie de pe bilete, 
#dar nu sunt
# copiez b si fac modificari


set.seed(2025)
simulare_luna_cu_venituri <- function(n) {
  calatori <- simulare_calatori(n)
  
  # Calculam statistici
  media <- round(sum(calatori)/n)
  minim <- min(calatori)
  maxim <- max(calatori)
  
  tipuri_zile <- simulare_discreta(n, val= c(1,2,3), p = c(1/2,1/3,1/6))
  zile_lejere <- sum(tipuri_zile == 1)
  zile_normale <- sum(tipuri_zile == 2)
  zile_aglomerate <- sum(tipuri_zile == 3)
  lejere <- sum(calatori < 350)
  normale <- sum(calatori>= 351 & calatori <= 670)
  aglomerate <- sum(calatori > 670)
  
  procent_lejere <- round(lejere/n *100,2)
  procent_normale<- round(normale/n *100,2)
  procent_aglomerate <- round(aglomerate/n *100,2)
  
  # Calculam veniturile si pierderile
  x_procent <- pasageri_x()  # Apelam functia care genereaza procentele
  x_bilet <- x_procent[1]  # Procentul celor care cumpara bilete
  x_abonament <- x_procent[2]  # Procentul celor care cumpara abonamente
  
  # Calculam numarul de pasageri pentru fiecare categorie
  total_calatori <- sum(calatori)
  #pt ca sunt pasageri, nr.nu poate fi cu virgula, deci rotunjim/aproximam
  achizitioneaza_abonament <- round(x_abonament / 100 * total_calatori )
  nu_achizitioneaza_abonament <- round(total_calatori - achizitioneaza_abonament)
  
  platesc_bilet <-round( x_bilet / 100 * nu_achizitioneaza_abonament)
  nu_platesc_bilet <- round(nu_achizitioneaza_abonament - platesc_bilet)
  
  # Calculam veniturile
  venituri_abonament <- achizitioneaza_abonament * pret_abonament
  venituri_bilet <- platesc_bilet * pret_bilet
  venituri_nerealizate <- nu_platesc_bilet * pret_bilet
  
  
  # Returnam un vector cu rezultatele
  return(c(Media = media, Min = minim, Max = maxim, 
           Lejere = procent_lejere,
           Normale = procent_normale, 
           Aglomerate = procent_aglomerate,
           Venit_Ab = venituri_abonament, 
           Venit_Bilet = venituri_bilet,
           Venit_Nerealizat = venituri_nerealizate))
}

# Numarul de zile din fiecare luna din 2024 (an bisect)
zile_luni <- c(
  ianuarie = 31, februarie = 29, martie = 31, aprilie = 30, mai = 31,
  iunie = 30, iulie = 31, august = 31, septembrie = 30,
  octombrie = 31, noiembrie = 30, decembrie = 31
)

# Dataframe pentru stocarea rezultatelor
rezultate_cu_venituri <- data.frame(
  Luna = names(zile_luni),
  Media = numeric(length(zile_luni)),
  Min = numeric(length(zile_luni)),
  Max = numeric(length(zile_luni)),
  Lejere = numeric(length(zile_luni)),
  Normale = numeric(length(zile_luni)),
  Aglomerate = numeric(length(zile_luni)),
  Venit_Ab = numeric(length(zile_luni)),
  Venit_B = numeric(length(zile_luni)),
  Venit_Nerealizat = numeric(length(zile_luni))
)

# Iteram prin fiecare luna si simulam datele
for (i in seq_along(zile_luni)) {
  rezultate_cu_venituri[i, 2:10] <- simulare_luna_cu_venituri(zile_luni[i])
}

# Afisam rezultatele
print(rezultate_cu_venituri)

#################### d) 

#mai intai simulam amenzile pentru o zi
# cei posibil sa primeasca amenzi sunt cei care nu platit bilet sau abonament
simulare_amenzi <- function(calatori,tip_zi){
  amenda <- 50
  plata_controlor <- 214
  nr_controale <- 2
  
  # Stabilim cate calatori pe zi nu platesc bilet
  x_procent <- pasageri_x()  # Apelam functia care genereaza procentele
  x_bilet <- x_procent[1]  # Procentul celor care cumpara bilete
  x_abonament <- x_procent[2]  # Procentul celor care cumpara abonamente
  
  achizitioneaza_abonament <- round(x_abonament / 100 * calatori )
  nu_achizitioneaza_abonament <- round(calatori - achizitioneaza_abonament)
  
  platesc_bilet <-round( x_bilet / 100 * nu_achizitioneaza_abonament)
  nu_platesc_bilet <- round(nu_achizitioneaza_abonament - platesc_bilet)
  
  # Generam nr de amenzi
  amenzi <- 0
  
  for (nr in 1:nr_controale){
    nr_verificari <- 0
    # geneream amenzile in functie de tipul zilei
    if (tip_zi == 1){
      nr_pasageri_verif <- sample(2:11,1) #nr aleator intre 2 si 11
      i <- 1
      while (i <= nr_pasageri_verif){
        if (runif(1) < 0.5){
          amenzi <- amenzi + amenda
          nr_verificari <- nr_verificari + 1
          if (nr_verificari == 3) break # se opeste dupa 3 amenzi
        }
      }
    } else if(tip_zi == 2){
      nr_pasageri_verif <- calatori
      i <- 1
      while(i <= nr_pasageri_verif){
        if (runif(1) < 0.5){
          amenzi <- amenzi + amenda
          nr_verificari <- nr_verificari + 1
          if (nr_verificari == 5) break # se opeste dupa 5 amenzi
        }
      }
    } else if(tip_zi == 3){
      nr_pasageri_verif <- sample(3:5,1)
      i <- 1
      while (i<= nr_pasageri_verif){
        if (runif(1) < 0.5){
          amenzi <- amenzi + amenda
          nr_verificari <- nr_verificari + 1
          if (nr_verificari == 1) break # se opeste dupa 5 amenzi
        }
      }
    }
  }
  
  # pierderea pe neplata biletelor pe zi
  venituri_pierdute <- nu_platesc_bilet * pret_bilet  # Pretul biletului este 3 lei
  print(venituri_pierdute)
  
  #costuri controlor
  plata_control_zi <- plata_controlor * nr_controale
  
  # profitul din amenzi vs pierderi din neplata biletelor
  profit_amenzi_zi <- amenzi - venituri_pierdute - plata_control_zi
  
  
  # Returnam rezultatele
  return(c(amenzi,venituri_pierdute,plata_control_zi,profit_amenzi_zi))
}
#exemplu
simulare_zi_lejera <- simulare_amenzi(calatori =243,tip_zi = 1)

# simulam amenzile pe luna:

simulare_amenzi_luna <- function(n) {
  calatori <- simulare_calatori(n)
  
  tipuri_zile <- simulare_discreta(n, val= c(1,2,3), p = c(1/2,1/3,1/6))
  
  # simulare amenzi pe luna
  i <- 1
  venituri_amenzi_pe_luna <- 0
  venituri_pierdute_pe_zi <- 0
  plata_control_pe_luna <- 0
  profit_amenzi_pe_zi <- 0
  zile_profit <- 0
  while (i <= n){
    simulare_amenzi_pe_zi <- simulare_amenzi(calatori = calatori[i],tip_zi =tipuri_zile[i])
    venituri_amenzi_pe_luna <- venituri_amenzi_pe_luna + simulare_amenzi_pe_zi[1]
    venituri_pierdute_pe_zi <- venituri_pierdute_pe_zi + simulare_amenzi_pe_zi[2]
    plata_control_pe_luna <- plata_control_pe_luna + simulare_amenzi_pe_zi[3]
    profit_amenzi_pe_zi <- profit_amenzi_pe_zi + simulare_amenzi_pe_zi[4]
    if (profit_amenzi_pe_zi > 0){
      zile_profit <-  zile_profit + 1
    }
    i <- i +1
  }
  
  profit_amenzi_pe_luna <- venituri_amenzi_pe_luna - venituri_pierdute_pe_zi - plata_control_pe_luna
  
  
  
  # Returnam un vector cu rezultatele
  return(c(Venit_Amenzi = venituri_amenzi_pe_luna,
           Profit = profit_amenzi_pe_luna,
           Zile_Cu_Profit = zile_profit))
}

# Numarul de zile din fiecare luna din 2024 (an bisect)
zile_luni_2025 <- c(
  ianuarie = 31, februarie = 28, martie = 31, aprilie = 30, mai = 31,
  iunie = 30, iulie = 31, august = 31, septembrie = 30,
  octombrie = 31, noiembrie = 30, decembrie = 31
)

# Dataframe pentru stocarea rezultatelor
rezultate_amenzi <- data.frame(
  Luna = names(zile_luni_2025),
  Venit_Amenzi = numeric(length(zile_luni_2025)),
  Profit = numeric(length(zile_luni_2025)),
  Zile_Cu_Profit = numeric(length(zile_luni_2025))
)

# Iteram prin fiecare luna si simulam datele
for (i in seq_along(zile_luni_2025)) {
  rezultate_amenzi[i, 2:4] <- simulare_amenzi_luna(zile_luni_2025[i])
}

# Afisam rezultatele
print(rezultate_amenzi)


#################### e) 
#mai departe folosesc functia anterioara
simulare_amenzi_fraudat <- function(calatori,tip_zi){
  amenda <- 50
  plata_controlor <- 214
  nr_controale <- 3
  
  
  # Stabilim cate calatori pe zi nu platesc bilet
  x_procent <- pasageri_x()  # Apelam functia care genereaza procentele
  x_bilet <- x_procent[1]  # Procentul celor care cumpara bilete
  x_abonament <- x_procent[2]  # Procentul celor care cumpara abonamente
  
  achizitioneaza_abonament <- round(x_abonament / 100 * calatori )
  nu_achizitioneaza_abonament <- round(calatori - achizitioneaza_abonament)
  
  platesc_bilet <-round( x_bilet / 100 * nu_achizitioneaza_abonament)
  nu_platesc_bilet <- round(nu_achizitioneaza_abonament - platesc_bilet)
  
  # Generam nr de amenzi
  amenzi <- 0
  venit_pastrat <- 0
  
  for (nr in 1:nr_controale){
    nr_verificari <- 0
    medie_pastrat <- 0
    # geneream amenzile in functie de tipul zilei
    if (tip_zi == 1){
      nr_pasageri_verif <- sample(2:11,1) #nr aleator intre 2 si 11
      i <- 1
      while (i <= nr_pasageri_verif){
        if (runif(1) < 0.5){
          amenzi <- amenzi + amenda
          nr_verificari <- nr_verificari + 1
          if (nr_verificari == 3) break # se opeste dupa 3 amenzi
        }
      }
    } else if(tip_zi == 2){
      nr_pasageri_verif <- calatori
      i <- 1
      while(i <= nr_pasageri_verif){
        if (runif(1) < 0.5){
          amenzi <- amenzi + amenda
          nr_verificari <- nr_verificari + 1
          if (nr_verificari == 5) break # se opeste dupa 5 amenzi
        }
      }
    } else if(tip_zi == 3){
      nr_pasageri_verif <- sample(3:5,1)
      i <- 1
      while (i<= nr_pasageri_verif){
        if (runif(1) < 0.5){
          amenzi <- amenzi + amenda
          nr_verificari <- nr_verificari + 1
          if (nr_verificari == 1) break # se opeste dupa 5 amenzi
        }
      }
    }
    medie_pastrat <- 30/100 * amenzi
    venit_pastrat <- venit_pastrat + medie_pastrat
  }
  
  # pierderea pe neplata biletelor pe zi
  venituri_pierdute <- nu_platesc_bilet * pret_bilet  # Pretul biletului este 3 lei
  print(venituri_pierdute)
  
  #costuri controlor
  plata_control_zi <- plata_controlor * nr_controale
  
  # profitul din amenzi vs pierderi din neplata biletelor
  profit_amenzi_zi <- amenzi - venituri_pierdute - plata_control_zi - venit_pastrat
  
  
  # Returnam rezultatele
  return(c(amenzi,venituri_pierdute,plata_control_zi,profit_amenzi_zi,venit_pastrat))
}

simulare_amenzi_luna_fraudat <- function(n) {
  calatori <- simulare_calatori(n)
  
  tipuri_zile <- simulare_discreta(n, val= c(1,2,3), p = c(1/2,1/3,1/6))
  
  # simulare amenzi pe luna
  i <- 1
  venituri_amenzi_pe_luna <- 0
  venituri_pierdute_pe_zi <- 0
  plata_control_pe_luna <- 0
  profit_amenzi_pe_zi <- 0
  zile_profit <- 0
  venit_pastrat_pe_zi <- 0
  while (i <= n){
    simulare_amenzi_pe_zi <- simulare_amenzi_fraudat(calatori = calatori[i],tip_zi =tipuri_zile[i])
    venituri_amenzi_pe_luna <- venituri_amenzi_pe_luna + simulare_amenzi_pe_zi[1]
    venituri_pierdute_pe_zi <- venituri_pierdute_pe_zi + simulare_amenzi_pe_zi[2]
    plata_control_pe_luna <- plata_control_pe_luna + simulare_amenzi_pe_zi[3]
    profit_amenzi_pe_zi <- profit_amenzi_pe_zi + simulare_amenzi_pe_zi[4]
    venit_pastrat_pe_zi <- venit_pastrat_pe_zi + simulare_amenzi_pe_zi[5]
    
    if (profit_amenzi_pe_zi > 0){
      zile_profit <-  zile_profit + 1
    }
    i <- i +1
  }
  
  profit_amenzi_pe_luna <- venituri_amenzi_pe_luna - venituri_pierdute_pe_zi - plata_control_pe_luna - venit_pastrat_pe_zi
  
  
  
  # Returnam un vector cu rezultatele
  return(c(Venit_Amenzi = venituri_amenzi_pe_luna,
           Profit = profit_amenzi_pe_luna,
           Zile_Cu_Profit = zile_profit,
           Venit_neraportat = venit_pastrat_pe_zi))
}


# Numarul de zile din fiecare luna din 2024 (an bisect)
zile_luni_2025 <- c(
  ianuarie = 31, februarie = 28, martie = 31, aprilie = 30, mai = 31,
  iunie = 30, iulie = 31, august = 31, septembrie = 30,
  octombrie = 31, noiembrie = 30, decembrie = 31
)

# Dataframe pentru stocarea rezultatelor
rezultate_frauda <- data.frame(
  Luna = names(zile_luni_2025),
  Venit_Amenzi = numeric(length(zile_luni_2025)),
  Profit = numeric(length(zile_luni_2025)),
  Zile_Cu_Profit = numeric(length(zile_luni_2025)),
  Venit_neraportat = numeric(length(zile_luni_2025))
)

# Iteram prin fiecare luna si simulam datele
for (i in seq_along(zile_luni_2025)) {
  rezultate_frauda[i, 2:5] <- simulare_amenzi_luna_fraudat(zile_luni_2025[i])
}

# Afisam rezultatele
print(rezultate_frauda)

#Observam ca desi venitul pe amenzi este mai mare cu 3 controlori, faptul ca pentru fiecare control nu se 
#declara 30%, profitul este mult mai mic


#Ex 3
#1) Calculati P(Zn <= x) pentru repartiile urmatoare stiind ca Zn = sqrt(n)*(sum(Xi)-media)/sigma:
#X1,X2,...Xn i.i.d.
# media = E[X1]
# sigma = Var(X1)
# X_bar = 1/n *sum(Xi)

#Aplicand TLC pe Zn avem ca converge in distributie la N(0,1), i.e Zn -> N(0,1)(in distr),
# deci folosim pnorm() pentru a calcula P(Zn <= x) in cadrul unei distributii normale standard


##### Repartitia binomiala Xi ~ Bin(n,p)
# media binomialei = n*p
#varianta binomialei = n*p*(1-p)

# Functie pentru simularea unei repartitii binomiale si calculul P(Zn <= x)
simuleaza_binomiala <- function(n, p) {
  media <-  n * p
  sigma <- sqrt(n*p*(1-p))
  
  # Generarea de variabile aleatoare geometrice
  X<- rbinom(n, size = n, prob = p)
  cat("V.a binomiala X:", X, "\n\n")

  X_bar <- mean(X)
  cat("X_bar_geom:", X_bar, "\n\n")
  
  # Calculul lui Zi (pt fiecare x)
  Zi <- sqrt(n) * (X - media) / sigma
  cat("Zi_binom:", Zi, "\n\n")
  
  Zn <- sqrt(n) * (X_bar - media) / sigma
  # Calculul probabilitatii P(Zn <= x)
  
  prob_zn <- pnorm(Zn)
  cat("P(Zn <= x):", prob_zn, "\n\n")
  prob_zi <- pnorm(Zi)
  cat("P(Zi <= x):", prob_zi, "\n\n")
  
  # Returnare
  return(Zi)
}

# Exemplu de apel al functiei
prob_binomiala <- simuleaza_binomiala(n = 100, p = 0.5)


##### Repartitia geometrica: Xi ~ Geom(p)
# media  = 1/p
#varianta  = (1-p)/p^2
simuleaza_geometrica <- function(n, p) {
  media <- 1 / p
  sigma <- sqrt((1 - p) / p^2)
  
  # Generarea de variabile aleatoare geometrice
  X <- rgeom(n, prob = p)
  cat("V.a geometrica X:", X, "\n\n")
  
  X_bar <- mean(X)
  cat("X_bar_geom:", X_bar, "\n\n")
  
  # Calculul lui Zi (pt fiecare x)
  Zi <- sqrt(n) * (X - media) / sigma
  cat("Zi_geometrica:", Zi, "\n\n")
  
  Zn <- sqrt(n) * (X_bar - media) / sigma
  # Calculul probabilitatii P(Zn <= x)
  
  prob_zn <- pnorm(Zn)
  cat("P(Zn <= x):", prob_zn, "\n\n")
  prob_zi <- pnorm(Zi)
  cat("P(Zi <= x):", prob_zi, "\n\n")
  
  # Returnare
  return(Zi)
}

prob_geometrica <- simuleaza_geometrica(n = 100, p = 0.3)

##### Repartitia poisson: Xi ~ Poisson(lambda)
# media  = lambda
#varianta  = lambda
simuleaza_poisson <- function(n, lambda) {
  media <- lambda
  sigma <- sqrt(lambda)
  
  # Generarea de variabile aleatoare Poisson
  X <- rpois(n, lambda = lambda)
  cat("V.a poisson X:", X, "\n\n")
  
  X_bar <- mean(X)
  cat("X_bar_pois:", X_bar, "\n\n")
  
  # Calculul lui Zi (pt fiecare x)
  Zi <- sqrt(n) * (X - media) / sigma
  cat("Zi_poisson:", Zi, "\n\n")
  
  Zn <- sqrt(n) * (X_bar - media) / sigma
  # Calculul probabilitatii P(Zn <= x)
  
  prob_zn <- pnorm(Zn)
  cat("P(Zn <= x):", prob_zn, "\n\n")
  prob_zi <- pnorm(Zi)
  cat("P(Zi <= x):", prob_zi, "\n\n")
  
  # Returnare
  return(Zi)
}

prob_poisson <- simuleaza_poisson(n = 100, lambda = 1)

  
##### Repartitia uniforma pe caz discret: Xi ~ Unif{a,....,b}
# media  = a+b/2
#varianta  = ((b-a+1)^2 -1)/12
simuleaza_uniforma_discreta <- function(n, a, b) {
  media <- (a + b) / 2
  sigma <- sqrt(((b - a + 1)^2 - 1) / 12)
  
  # Generarea de variabile aleatoare uniforme discrete
  X <- sample(a:b, n, replace = TRUE)
  cat("V.a uniforme discrete X:", X, "\n\n")
  
  X_bar <- mean(X)
  cat("X_bar_unif:", X_bar, "\n\n")
  
  # Calculul lui Zi (pt fiecare x)
  Zi <- sqrt(n) * (X - media) / sigma
  cat("Zi_uniforma_discreta:", Zi, "\n\n")
  
  Zn <- sqrt(n) * (X_bar - media) / sigma
  # Calculul probabilitatii P(Zn <= x)
  
  prob_zn <- pnorm(Zn)
  cat("P(Zn <= x):", prob_zn, "\n\n")
  prob_zi <- pnorm(Zi)
  cat("P(Zi <= x):", prob_zi, "\n\n")
  
  # Returnare
  return(Zi)
}

prob_unif_discreta <- simuleaza_uniforma_discreta(n = 100, a = 1, b = 6)

##### Repartitia uniforma pe caz continuu: Xi ~ Unif((a,b))
# media  = a+b/2
#varianta  = (b-a)^2/12
simuleaza_uniforma_continua <- function(n, a, b) {
  media <- (a + b) / 2
  sigma <- sqrt((b-a)^2/12)
  
  # Generarea de variabile aleatoare uniforme continue
  X <- runif(n,min=a,max=b)
  cat("V.a uniforme continue X:", X, "\n\n")
  
  X_bar <- mean(X)
  cat("X_bar_unif:", X_bar, "\n\n")
  
  # Calculul lui Zi (pt fiecare x)
  Zi <- sqrt(n) * (X - media) / sigma
  cat("Zi_uniforma_continua:", Zi, "\n\n")
  
  Zn <- sqrt(n) * (X_bar - media) / sigma
  # Calculul probabilitatii P(Zn <= x)
  
  prob_zn <- pnorm(Zn)
  cat("P(Zn <= x):", prob_zn, "\n\n")
  prob_zi <- pnorm(Zi)
  cat("P(Zi <= x):", prob_zi, "\n\n")
  
  # Returnare
  return(Zi)
}

prob_unif_cont <- simuleaza_uniforma_continua(n = 100, a = 1, b = 6)

##### Repartitia exponentiala: Xi ~ Exp(lambda)
# media  = 1/lambda
#varianta  = 1/lambda^2
simuleaza_exponentiala<- function(n, lambda) {
  media <- 1/lambda
  sigma <- sqrt(1/lambda^2)
  
  # Generarea de variabile aleatoare exponentiale
  X <- rexp(n,rate = lambda)
  cat("V.a exponentiale X:", X, "\n\n")
  
  X_bar <- mean(X)
  cat("X_bar_exp:", X_bar, "\n\n")
  
  # Calculul lui Zi (pt fiecare x)
  Zi <- sqrt(n) * (X - media) / sigma
  cat("Zi_exponentiala:", Zi, "\n\n")
  
  Zn <- sqrt(n) * (X_bar - media) / sigma
  # Calculul probabilitatii P(Zn <= x)
  
  prob_zn <- pnorm(Zn)
  cat("P(Zn <= x):", prob_zn, "\n\n")
  prob_zi <- pnorm(Zi)
  cat("P(Zi <= x):", prob_zi, "\n\n")
  
  # Returnare
  return(Zi)
}

prob_exponentiala<- simuleaza_exponentiala(n = 100, lambda = 0.4)


##### Repartitia gamma: Xi ~ Gamma(alpha,beta)
# media  = alpha*beta
#varianta  = alpha*beta^2
simuleaza_gamma<- function(n, alpha,beta) {
  media <- alpha*beta
  sigma <- sqrt(alpha*beta^2)
  
  # Generarea de variabile aleatoare gamma
  X <- rgamma(n,shape = alpha, rate = beta)
  cat("V.a gamma X:", X, "\n\n")
  
  X_bar <- mean(X)
  cat("X_bar_gamma:", X_bar, "\n\n")
  
  ## Calculul lui Zi (pt fiecare x)
  Zi <- sqrt(n) * (X - media) / sigma
  cat("Zi_gamma:", Zi, "\n\n")
  
  Zn <- sqrt(n) * (X_bar - media) / sigma
  # Calculul probabilitatii P(Zn <= x)
  
  prob_zn <- pnorm(Zn)
  cat("P(Zn <= x):", prob_zn, "\n\n")
  prob_zi <- pnorm(Zi)
  cat("P(Zi <= x):", prob_zi, "\n\n")
  
  # Returnare
  return(Zi)
}

prob_Gamma<- simuleaza_gamma(n = 100, alpha = 2, beta =1)


##### Repartitia beta: Xi ~ Beta(alpha,beta)
# media  = alpha/(alpha + beta)
#varianta  = alpha*beta/((alpha+beta)^2*(alpha+beta+1))
simuleaza_beta<- function(n, alpha,beta) {
  media <- alpha/(alpha + beta)
  sigma <- sqrt(alpha*beta/((alpha+beta)^2*(alpha+beta+1)))
  
  # Generarea de variabile aleatoare Beta
  X <- rbeta(n,shape1 = alpha, shape2 = beta)
  cat("V.a beta X:", X, "\n\n")
  
  X_bar <- mean(X)
  cat("X_bar_beta:", X_bar, "\n\n")
  
  # Calculul lui Zi (pt fiecare x)
  Zi <- sqrt(n) * (X - media) / sigma
  cat("Zi_beta:", Zi, "\n\n")
  
  Zn <- sqrt(n) * (X_bar - media) / sigma
  # Calculul probabilitatii P(Zn <= x)
  
  prob_zn <- pnorm(Zn)
  cat("P(Zn <= x):", prob_zn, "\n\n")
  prob_zi <- pnorm(Zi)
  cat("P(Zi <= x):", prob_zi, "\n\n")
  
  # Returnare
  return(Zi)
}

prob_beta<- simuleaza_beta(n = 100, alpha = 2, beta =5)


########## 2) Reprezentati grafic functiile obtinute la 1).
Zi_binomiala <- prob_binomiala
hist(Zi_binomiala, main = "Distributie Binomiala Standarizata", 
     xlab = "Zi", probability = TRUE)
lines(density(Zi_binomiala), col = "magenta", lwd = 2)


Zi_geometrica <- prob_geometrica
hist(Zi_geometrica, main = "Distributie Geometrica Standarizata", 
     xlab = "Zi", probability = TRUE)
lines(density(Zi_geometrica), col = "magenta", lwd = 2)


Zi_poisson<- prob_poisson
hist(Zi_poisson, main = "Distributie Poisson Standarizata", breaks = "Scott",
     xlab = "Zi", probability = TRUE)
lines(density(Zi_poisson), col = "magenta", lwd = 2)


Zi_unif_discret <- prob_unif_discreta
hist(Zi_unif_discret, main = "Distributie Uniforma Discreta Standarizata",
     xlab = "Zi", probability = TRUE)
lines(density(Zi_unif_discret), col = "magenta", lwd = 2)


Zi_unif_continua<- prob_unif_cont
hist(Zi_unif_continua, main = "Distributie Uniforma Continua Standarizata",
     xlab = "Zi", probability = TRUE)
lines(density(Zi_unif_continua), col = "magenta", lwd = 2)


Zi_exponentiala <- prob_exponentiala
hist(Zi_exponentiala, main = "Distributie Exponentiala Standarizata",
     xlab = "Zi", probability = TRUE)
lines(density(Zi_exponentiala), col = "magenta", lwd = 2)


Zi_gamma <- prob_Gamma
hist(Zi_gamma, main = "Distributie Gamma Standarizata",
     xlab = "Zi", probability = TRUE)
lines(density(Zi_gamma), col = "magenta", lwd = 2)

Zi_beta <- prob_beta
hist(Zi_beta, main = "Distributie Beta Standarizata",
     xlab = "Zi", probability = TRUE)
lines(density(Zi_beta), col = "magenta", lwd = 2)


#3)
#momentul de ordinul trei este legat de modul in care valorile unei distributii sunt distribuite in jurul mediei sale.
# Pt o v.a X, momentul de ordin 3 este definit ca: μ3 = E[(X-μ)^3]
#Simetria distributiei: Daca momentul de ordinul trei este 0, distributia este simetrica in jurul mediei (de exemplu, distributia normala).
#Asimetria: Daca momentul de ordinul trei este pozitiv (adica  μ3>0), distributia are o coada mai lunga pe partea dreapta (distributie asimetrica pozitiv sau dreapta).
#           Daca momentul de ordinul trei este negativ (adica μ3<0), distributia are o coada mai lunga pe partea stanga (distributie asimetrica negativa sau stanga).
# Functie pentru calculul diferentei absolute intre P(Zn <= x) si functia de repartitie a normalei standard

# Functia supremului Berry-Esseen
berry_esseen_sup <- function(n, dist_fun, medie_x, var_x) {
  x <- dist_fun(n)  # Generam n valori din distributie
  x_bar <- mean(x)
  sigma_x <- sqrt(var_x)  # Deviatia standard teoretica
  
  Z_n <- (x_bar - medie_x) / (sigma_x / sqrt(n))  # Standardizare
  F_n <- ecdf(Z_n)  # Functia de repartitie empirica
  
  supremum_func <- function(x) {
    abs(F_n(x) - pnorm(x))
  }
  
  rezultat_sup <- optimize(supremum_func, interval = c(-3, 3), maximum = TRUE)
  return(rezultat_sup$objective)
}

# Lista de distributii discrete si continue cu parametrii lor
distributii <- list(
  list(name = "Binomiala(10, 0.5)", dist_fun = function(n) rbinom(n, size = 10, prob = 0.5),
       param = list(X = "binomiala", n = 10, p = 0.5)),
  
  list(name = "Geometrica(0.3)", dist_fun = function(n) rgeom(n, prob = 0.3),
       param = list(X = "geometrica", p = 0.3)),
  
  list(name = "Poisson(λ=4)", dist_fun = function(n) rpois(n, lambda = 4),
       param = list(X = "poisson", lambda = 4)),
  
  list(name = "Uniform Discreta(0,10)", dist_fun = function(n) sample(0:10, n, replace = TRUE),
       param = list(X = "uniforma", caz = "discret", a = 0, b = 10)),
  
  list(name = "Uniform(0,1)", dist_fun = function(n) runif(n, 0, 1),
       param = list(X = "uniforma", caz = "continuu", a = 0, b = 1)),
  
  list(name = "Exponentiala(1)", dist_fun = function(n) rexp(n, rate = 1),
       param = list(X = "exponentiala", lambda = 1)),
  
  list(name = "Gamma(2,1)", dist_fun = function(n) rgamma(n, shape = 2, rate = 1),
       param = list(X = "gamma", alpha = 2, beta = 1)),
  
  list(name = "Beta(2,5)", dist_fun = function(n) rbeta(n, shape1 = 2, shape2 = 5),
       param = list(X = "beta", alpha = 2, beta = 5))
)

n <- 100 
rezultate_sup <- data.frame(Distributii = character(), Supremum = numeric(), stringsAsFactors = FALSE)  

# Calculam supremul pentru fiecare distributie
for (dist in distributii) {
  params <- dist$param
  medie_var <- do.call(calcul_medie, params)  # Calculam media si varianta
  
  
  supremum <- berry_esseen_sup(n, dist$dist_fun, medie_var["media"], medie_var["varianta"])
  
  rezultate_sup <- rbind(rezultate_sup, data.frame(Distributii = dist$name, Supremum = supremum))  
}

# Afisare rezultate
print(rezultate_sup)


#4 Functie care calculeaza media si varianta unei distributii. Tipul repartitiei este 
#trimis prin denumire sau functie de masa/densitate
#Facem doua functii, una in care sa calcul media si varianta repartitiei dupa denumire 
#si alta in care calculam dupa functie pt caz cont, si discret


###################################################
# Functia calcul_medie 
calcul_medie <- function(X, caz = NULL, n = NULL, p = NULL, 
                         a = NULL, b = NULL, lambda = NULL, 
                         alpha = NULL, beta = NULL, miu = NULL, sigma2 = NULL, 
                         x = NULL, p_val = NULL) {
  # Verificam daca X este un string (denumirea distributiei)
  if (is.character(X)) {
    if (X == "binomiala") {
      media <- n * p
      varianta <- n * p * (1 - p)  
    } else if (X == "geometrica") {
      media <- 1 / p
      varianta <- (1 - p) / p^2
    } else if (X == "poisson") {
      media <- lambda
      varianta <- lambda
    } else if (X == "uniforma" && caz == "discret") {
      media <- (a + b) / 2
      varianta <- ((b - a + 1)^2 - 1) / 12
    } else if (X == "uniforma" && caz == "continuu") {
      media <- (a + b) / 2
      varianta <- (b - a)^2 / 12
    } else if (X == "normala") {
      media <- miu
      varianta <- sigma2
    } else if (X == "exponentiala") {
      media <- 1 / lambda
      varianta <- 1 / lambda^2
    } else if (X == "gamma") {
      media <- alpha * beta
      varianta <- alpha * beta^2
    } else if (X == "beta") {
      media <- alpha / (alpha + beta)
      varianta <- alpha * beta / ((alpha + beta)^2 * (alpha + beta + 1))
    } else {
      stop("Distributia specificata nu este recunoscuta.")
    }
  }
  # Verificam daca X este o functie
  else if (is.function(X)) {
    if (caz == "continuu") {
      media <- integrate(function(x) x * X(x), lower = -Inf, upper = Inf)$value
      varianta <- integrate(function(x) (x - media)^2 * X(x), lower = -Inf, upper = Inf)$value
    } else if (caz == "discret") {
      # Calculeaza media si varianta pentru cazuri discrete
      if (is.null(x) || is.null(p_val)) {
        stop("Pentru cazul discret, trebuie sa oferiti vectorii x si p_val.")
      }
      media <- sum(x * p_val)
      varianta <- sum((x - media)^2 * p_val)
    } else {
      stop("Trebuie specificat daca cazul este continuu sau discret.")
    }
  } else {
    stop("Trebuie specificata fie denumirea distributiei, fie o functie X.")
  }
  
  # Returnam media si varianta
  return(c(media = media, varianta = varianta))
}


# Exemple de utilizare:

# 1. **Pentru denumire - Distributie binomiala:**
# Parametrii pentru distributia binomiala
n <- 10  # Numarul de incercari
p <- 0.3  # Probabilitatea de succes

# Calculam media si varianta
rezultat_binomial <- calcul_medie("binomiala", n = n, p = p)
print("Rezultate pentru distributia binomiala (prin denumire):")
print(rezultat_binomial)



##2. Parametrii pentru distributia exponentiala
# Parametrii pentru distributia exponentiala
lambda <- 1  # Rata

# Functia de densitate pentru distributia exponentiala
X_exponential <- function(x) {
  ifelse(x >= 0, lambda * exp(-lambda * x), 0)
}

# Calculam media si varianta
rezultat_exponential <- calcul_medie(X_exponential, caz = "continuu")
print("Rezultate pentru distributia exponentiala (prin functie, caz continuu):")
print(rezultat_exponential)

