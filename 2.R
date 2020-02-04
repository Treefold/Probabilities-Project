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
  
  #regresie simpla
  justTheString("Regresie simpla: plotul 1 (primul)")
  print(density(infert$age))
  par(mar=c(2,1,2,1))
  op<-par(mfrow=c(2,1))
  plot(density(infert$age), main="Age density", xlab="AGE", las=1, ask=FALSE)
  hist(infert$age, main="Age density", xlab="AGE", ylab="Density", freq=FALSE)
  lines(density(infert$age), lwd=5, col="red")
  par(op)
  
  #regresie multipla
  newLine()
  justTheString("Regresie multipla: 2 modele - ploturile 2 si 3")
  plotModel (lm(infert$age~infert$parity))
  plotModel (lm(infert$age~infert$parity+infert$induced+infert$case+infert$spontaneous))
  
  #adaugam noi variabile: successful births & failed births
  newLine()
  justTheString("Se adauga noi variabile: Successful_births si Failed_births")
  s_births<-sample(x=c(0,1,2,3,4),size=length(infert[[1]]), replace=TRUE)
  f_births<-sample(x=c(0,1),      size=length(infert[[1]]), replace=TRUE)
  
  #regresie simpla
  newLine()
  justTheString("Regresie simpla: plotul 4")
  scatter.smooth(x=infert$age, y=s_births, main="age ~ successful births")
  
  #regresie multipla
  newLine()
  justTheString("Regresie multipla: plotul 5 (ultimul)")
  plotModel (lm(infert$age~infert$parity+infert$induced+s_births+f_births))
}

solve2()
