library(ggplot2)
b <- back.electron(N.e=1000, E.0 = 1, theta = pi/4, A=28.1, Rho=2.33, Z.a=14, J=0.173, E.c = 0.1, E.bot = 0.01, m = 0.8)

v <- c(0, 0, 0)
for(i in 1:300){
     b <- back.electron(N.e=1000, E.0 = 1, theta = pi/4, A=28.1, Rho=2.33, Z.a=14, J=0.173, E.c = 0.1, E.bot = 0.01, m = 0.8)
     NN <- sum(b[,1])
     EBS <- sum(b[,2])
     RE <- sum(b[,3])
     v <- rbind(NN, EBS, RE)
}

# строим графики
b.df <- as.data.frame(b)
# плотность распределения энергии обратнорассеянных электронов
ggplot(b.df[-1,], aes(x=b.df[-1, 2])) + 
     geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                    binwidth=.01,
                    colour="black", fill="white") +
     geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
     xlab("Энергия обратнорассеянных электронов, [КэВ]") +
     ylab("Плотность распределения") +
     ggtitle("Плотность распределения энергии обратнорассеянных электронов")

# плотность распределения энергии рентгеновского излучения
ggplot(b.df[-1,], aes(x=b.df[-1, 3])) + 
     geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                    binwidth=50,
                    colour="black", fill="white") +
     geom_density(alpha=.2, fill="#FF6666") +
     coord_cartesian(xlim = c(0, 5000)) +
     xlab("Энергия рентгеновского излучения, [КэВ]") +
     ylab("Плотность распределения") +
     ggtitle("Плотность распределения энергии рентгеновского излучения")


# функция энергии обратнорассеянных электронов и рентгеновского излучения
back.electron <- function(N.e = 1000, E.0, theta, A, Rho, Z.a, J, E.c, E.bot, m = 0.8){
     result <- c(0, 0, 0)
     names(result) <- c('BackScattered', 'EnergyBS', 'Roentgen')
     for(iN in 1:N.e){
          cc <- one.electron( E.0, theta, A, Rho, Z.a, J, E.c, E.bot, m)
          result <- rbind(result, cc)
     }
     return(result)
}

one.electron <- function(E.0, theta, A, Rho, Z.a, J, E.c, E.bot, m){
     N <- 0 #отразился ли электрон обратно
     W <- 0 #если отразился то с какой энергией
     I.1 <- 0 #выход рентгеновского излучения
     E <- E.0 # текущая энергия электрона
     X <- 0 # начальная координата
     Y <- 0 # начальная координата
     Z <- 0 # начальная координата
     alpha.x <- pi / 2 # угол до столкновения между направлением движения электрона и осью x
     alpha.y <- pi / 2 - theta # угол до столкновения между направлением движения электрона и осью y
     alpha.z <- theta # угол до столкновения между направлением движения электрона и осью z
     alpha <- 3.4 * 0.001 * Z.a^0.67 / E
     stop <- 0
#     for(i in 1:1){    
     while(stop == 0){
          lambda <- lambda(A, E, alpha, Rho, Z.a) # средняя длина пробега
          rnd.1 <- runif(1) # первое сл.число
          rnd.2 <- runif(1) # второе сл.число
          rnd.3 <- runif(1) # третье сл.число
          S <- (-1) * lambda * log(rnd.1) # длина шага
          cos.phi <- 1 - 2 * alpha * rnd.2 / (1 + alpha - rnd.2) # угол рассеяния
          psi <- 2 * pi * rnd.3 # азимутальный угол
          AM <- (-1) * cos(alpha.x) / cos(alpha.z) 
          AN <- 1 / sqrt(1 + AM^2)
          V.1 <- AN * sqrt(1 - cos.phi^2)
          V.2 <- AN * AM * sqrt(1 - cos.phi^2)
          V.3 <- cos(psi)
          V.4 <- sin(psi)
          cos.beta.x <- cos(alpha.x) * cos.phi + V.1 * V.2 + cos(alpha.y) * V.2 * V.4
          cos.beta.y <- cos(alpha.y) * cos.phi + V.4 * (cos(alpha.z) * V.1 - cos(alpha.x) * V.2)
          cos.beta.z <- cos(alpha.z) * cos.phi + V.2 * V.3 - cos(alpha.y) * V.1 * V.4
          
          X.n <- X + S * cos.beta.x
          Y.n <- X + S * cos.beta.y
          Z.n <- X + S * cos.beta.z 
          
          E.n <- E - 7.85 * 0.001 * Z * Rho * S / A / E * log(1.166 * (E + 0.85 * J) / J)
          
          X <- X.n
          Y <- Y.n
          Z <- Z.n
          E <- E.n
          
          alpha.x <- acos(cos.beta.x)
          alpha.y <- acos(cos.beta.y)
          if(cos.beta.z == 0){
               alpha.z <- acos(0.00001)          
          }else{
               alpha.z <- acos(cos.beta.z)
          }
          
          I <- log(E.0 / E.c) / E.0^m / cos(theta) + abs( log(E / E.c) / E^m / cos.beta.z)
          I.1 <- I.1 + I
          if(Z <= 0){
               stop <- 1
          }
          if(E < E.bot){
               stop <- 1
          }
     }
          
     if(Z <= 0){
          return(c(1, E, I.1))
     }
     if(E < E.bot){
          return(c(0, 0, I.1))
     }
}

lambda <- function(A, E, alpha, Rho, Z.a){
     
     lmbd <- 0.863 * A * E * (1 + alpha) / Rho / Z.a^1.33 * ((E + 1024) / (E + 511))^2
     return(lmbd)
}
