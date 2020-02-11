library(Metrics)

newLine <- function () { # afiseaza o linie libera
  blank <- matrix()
  rownames(blank) <- ""
  colnames(blank) <- ""
  print(blank, na.print = "")
}

justTheString <- function (str) { # afiseaza un string fara ""
  blank <- matrix()
  rownames(blank) <- str
  colnames(blank) <- ""
  print(blank, na.print = "")
}

plotModel <- function (m) { # afiseaza 4 toate cele 4 ploturi ale unui model intr+un singur plot
  print(summary(m))
  op<-par(mfrow=c(2,2))
  for(i in c(1:3,5)) {plot(m, which=i)}
  par(op)
}

solve2 <- function() {
  print(names(infert))
  print(head(infert))
  attach(infert)
  
  set.seed(123) # putea fi orice numar
  index <- sample(nrow(infert), 0.8 * nrow(infert))
  train <- infert[index, ]
  test  <- infert[-index, ]
  
  #regresie simpla
  justTheString("Regresie simpla: plotul 1 (primul)")
  print(density(age))
  par(mar=c(2,1,2,1))
  op<-par(mfrow=c(2,1))
  plot(density(age), main="Age density", xlab="AGE", las=1, ask=FALSE)
  hist(age, main="Age density", xlab="AGE", ylab="Density", freq=FALSE)
  lines(density(age), lwd=5, col="red")
  par(op)
  
  #regresie multipla
  newLine()
  justTheString("Regresie multipla: 2 modele - ploturile 2 si 3")
  m1 <- lm(infert$age~infert$parity, data = train)
  m2 <- lm(infert$age~infert$parity+infert$induced+infert$case+infert$spontaneous, data = train)
  print(cor(age, parity))
  print(confint(m1, conf.level=0.95))
  plotModel (m1)
  plotModel (m2)
  
  #adaugam noi variabile: successful births & failed births
  newLine()
  justTheString("Se adauga noi variabile: Successful_births si Failed_births")
  s_births<-sample(x=c(0,1,2,3,4),size=length(infert[[1]]), replace=TRUE)
  f_births<-sample(x=c(0,1),      size=length(infert[[1]]), replace=TRUE)
  justTheString(paste("Cor(age, s_births) =", cor(age,s_births)))
  justTheString(paste("Cor(age, f_births) =", cor(age,f_births)))
  
  #regresie simpla
  newLine()
  justTheString("Regresie simpla: plotul 4")
  scatter.smooth(x=infert$age, y=s_births, main="age ~ successful births")
  
  #regresie multipla
  newLine()
  justTheString("Regresie multipla: plotul 5 (ultimul)")
  m3 <- lm(infert$age~infert$parity+infert$induced+s_births+f_births, data = train)
  plotModel (m3)
  
  tab <- matrix (rep(0, 2*3), nrow = 2, ncol = 3)
  rownames(tab) <- c("MSE", "RMSE")
  colnames(tab) <- c("M1", "M2", "M3")
  tab[1, 1] <- mse (infert$age, predict(m1, test))
  tab[2, 1] <- rmse(infert$age, predict(m1, test))
  tab[1, 2] <- mse (infert$age, predict(m2, test))
  tab[2, 2] <- rmse(infert$age, predict(m2, test))
  tab[1, 3] <- mse (infert$age, predict(m3, test))
  tab[2, 3] <- rmse(infert$age, predict(m3, test))
  print (tab)
  newLine()
}

solve2()
