newLine <- function () {
  blank <- matrix()
  rownames(blank) <- ""
  colnames(blank) <- ""
  print(blank, na.print = "")
}
justTheString <- function (str) {
  blank <- matrix()
  rownames(blank) <- str
  colnames(blank) <- ""
  print(blank, na.print = "")
}

solve1 <- function () { 
  
  justTheString ("Datasetul: infert")
  newLine()
  
  # Media si Varianta
  tab <- matrix (rep(0, 2*7), nrow = 2, ncol = 7)
  rownames(tab) <- c("Media", "Varianta")
  colnames(tab) <- names(infert)[2:8]
  for (i in 1:7) {
    tab[1, i] <- floor(mean(infert[,i+1]) * 100) / 100
    tab[2, i] <- floor(var (infert[,i+1]) * 100) / 100
  }
  print (tab)
  newLine()
  
  justTheString ("Quantile")
  
  tab <- matrix (rep(0, 7*11), nrow = 7, ncol = 11)
  rownames(tab) <- names(infert)[2:8]
  colnames(tab) <- strsplit(paste (strsplit(toString(0:11 * 10), ", ")[[1]], collapse = "% "), " ")[[1]][1:11]

  
  for (i in 1:7) {
    tab[i,] <- quantile(infert[,i+1], probs = (0:10 / 10))
  }
  print (tab)
  newLine()
  
  #different boxplots for comparing
  boxplot(infert, main="Complete boxplot", ylab="Value", ylim=c(0,100), las=1) #un boxplot cu toate datele din infert
  boxplot(infert$age ~ infert$education, main="Age by education years", las=1) #un boxplot care afiseaza varsta persoanelor in functie de educatie
  boxplot(infert$age[infert$education=="12+ yrs"], main="Age of those with more than 12 years of education", ylab="Age") #boxplotul afiseaza varsta celor cu 12+ ani de educatie
  
  #interpretari grafice
  cor(infert$age, infert$parity)
  plot(infert$age, infert$parity, main = "Age scattered by parity", xlab = "AGE", ylab= "STR",las=1, xlim=c(25,35), ylim = c(0,6), cex=2, pch=8, col=2)
  abline(lm(infert$parity~infert$age), col=3)
  
  count<-table(infert$age)
  percent<-table(infert$parity)/248
  barplot(count, main="Varsta", ylab="count",xlab="age",las=1)
  pie(percent, main = "Parity distribution")
  box()
}

solve1()
