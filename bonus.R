newLine <- function () {
  blank <- matrix()
  rownames(blank) <- ""
  colnames(blank) <- ""
  print(blank, na.print = "")
}

rand <- function (n) {return (floor(runif(1, min=1, max=n+1)))}

GenerateCompliteTable <- function (n, m) {
  tab <- matrix(runif((n+1) * (m+1)), nrow = n+1, ncol = m+1)
  tab[, m+1] <- rep(0, times=n+1)
  tab[n+1, ] <- rep(0, times=m+1)
  s <- sum (tab)
  for (i in 1:n) {
    for (j in 1:m) {
      tab[i,j] <- tab[i,j] / s
    }
    tab[i, m+1] <- sum(tab[i,])
  }
  for (j in 1:(m+1)) {
    tab[n+1, j] <- sum(tab[1:n,j])
  }
  return (tab)
}

posValid <- function (i, j, n, m, tab) {
  linie   = length (which(is.na(tab[1:n,j])))
  coloana = length (which(is.na(tab[i,1:m])))
  return (linie == 0 || coloana == 0)
}

frepcomgen <- function (n, m) {
  tab <- GenerateCompliteTable(n, m)
  # print (tab) # uncomment pentru a putea vedea matricea originala si a verifica raspunsul de la b
  
  n <- n+1
  m <- m+1
  for (x in 1:(n*m)) {
    i <- rand (n)
    j <- rand (m)
    if (is.na(tab[i, j]) == FALSE && posValid(i, j, n, m, tab)) {
     
       tab[i,j] <- NA
    }
  }
  return (tab)
}

fcomplrepcom <- function (n, m, tab) {
  tab[n+1,m+1] <- 1
  again <- 0
  updated  <- 0
  
  # incearca sa completezi ultima coloana (p[i])
  tofind <- which(is.na(tab[1:n,m+1]))
  if (length (tofind) > 0) { # elemente lipsa pe ultima coloana
    if (length (tofind) == 1) { # un element 
      tab[tofind[1], m+1] <- 0
      tab[tofind[1], m+1] <- 1 - sum(tab[1:n,m+1])
      updated  <- 1
    }
    else {
      for (i in tofind) {
        l <- tab[i,1:m]
        l <- length(l[is.na(l)])
        if (l == 0) {
          s <- sum(tab[i,1:m])
          tab[i, m+1] <- s
          updated <- 1
        }
        else {again <- 1}
      }
    }
  }
  
  # incearca sa completezi ultima linie (q[i])
  tofind <- which(is.na(tab[n+1,1:m]))
  if (length (tofind) > 0) { # elemente lipsa pe ultima coloana
    if (length (tofind) == 1) { # un element 
      tab[n+1, tofind[1]] <- 0
      tab[n+1, tofind[1]] <- 1 - sum(tab[n+1,1:m])
      updated  <- 1
    }
    else {
      for (i in tofind) {
        l <- tab[1:n,i]
        l <- length(l[is.na(l)])
        if (l == 0) {
          s <- sum(tab[1:n,i])
          tab[n+1, i] <- s
          updated <- 1
        }
        else {again <- 1}
      }
    }
  }
  
  # incearca sa completezi interiorul tabelului (pi[i])
  tofind <- which(is.na(tab[1:n,1:m]))
  if (length (tofind) > 0) { # elemente lipsa in interior
    for (x in tofind) {
      # determinam pozitia din matrice
      i <- (x - 1) %% n + 1 
      j <- (x - 1) %/% n + 1 
      
      if (is.na(tab[i, m+1]) == FALSE) { # totalul liniei e cunoscut
        if (length(which(is.na(tab[i,1:m]))) == 1) { 
          # e singurul element necunoscut de pe linia asta
          tab[i,j] <- 0
          tab[i,j] <- tab[i,m+1] - sum(tab[i,1:m])
          updated  <- 1
          next # try the next element
        }
        else {again <- 1}
      }
      
      if (is.na(tab[n+1, j]) == FALSE) { # totalul coloanei e cunoscut
        if (length(which(is.na(tab[1:n,j]))) == 1) { 
          # e singurul element necunoscut de pe coloana asta
          tab[i,j] <- 0
          tab[i,j] <- tab[n+1,j] - sum(tab[1:n,j])
          updated  <- 1
          next # try the next element
        }
        else {again <- 1}
      }
    }
  }
  
  
  if (again == 1 && updated == 1) 
       {return (fcomplrepcom(n, m, tab))}
  else {return (tab)}
}

prod <- function (xv, xp, yv, yp) {
  xyv <- c()
  xyp <- c()
  
  for (i in 1:length(xv)) {
    for (j in 1:length(yv)) {
      v <- xv[i] * yv[j]
      p <- xp[i] * yp[j]
      #xyv <- c(xyv, v)
      #xyp <- c(xyp, p)
      k <- which (xyv == v)
      if (length (k) > 0) { # este deja in lista
        k <- k[1]  
        xyp[k] <- xyp[k] + p 
      }
      else {
        k <- length (xyv[xyv < v])
        if (k == 0) {
          xyv <- c(v, xyv)
          xyp <- c(p, xyp)
        }
        else {
          xyv <- c(c(xyv[1:k], v), xyv[-(1:k)])
          xyp <- c(c(xyp[1:k], p), xyp[-(1:k)])
        }
      }
    }
  }
  return (list(xyv,xyp))
}

afiseaza <- function (name, v, p) {
  
  print (paste0("Repartitia lui ", name, ":"))
  va <- matrix(p, ncol = length(p))
  colnames(va) <- v
  rownames(va) <- ""
  print(va, na.print = "")
  
  #af <- matrix(rep(0, times=2*length(v)), nrow = 2, ncol = length(v))
  #af[1, ] <- v
  #af[2, ] <- p
  #print (af)
}

media <- function (v, p) {
  s <- 0
  for (i in 1:length(v)) {
    s <- s + v[i] * p[i]
  }
  return (s)
}

cov <- function (xv, xp, yv, yp) {
  
  xy  <- prod(xv, xp, yv, yp)
  xyv <- as.vector(unlist(xy[1]))
  xyp <- as.vector(unlist(xy[2]))
  afiseaza ("XY", xyv, xyp)
  mxy <- media (xyv, xyp)
  mx  <- media (xv, xp)
  my  <- media (yv, yp)
  return (mxy - mx * my)
}

fverind <- function (n, m, tab, xycov) {
  if (xycov != 0.0) {return (FALSE)}
  for (i in 1:n) {
    for (j in 1:m) {
      if (tab[i, j] != tab[i, m+1] * tab[n+1, j]) {
        return (FALSE)
      }
    }
  }
  return (TRUE)
}

fvernecor <- function (xycor, xyind) {
  return (xycor == 0 && xyind == FALSE) 
  # corelatia = 0 fara sa fie independente
}

solveBonus <- function  (n, m) {
  xv <- rep (0, times = n)
  yv <- rep (0, times = m)
  
  xv[1] <- rand(2*n) - n
  for (i in 2:n) {
    xv[i] <- xv[i-1] + rand (n)
  }
  yv[1] <- rand(2*m) - m
  for (i in 2:m) {
    yv[i] <- yv[i-1] + rand (m)
  }
  
  print ("Subpunctul a: Repartitie comuna lui X si Y incompleta")
  tab <- frepcomgen(n, m)
  colnames (tab) <- c(yv, "P")
  rownames (tab) <- c(xv, "Q")
  print (tab, na.print = "      ")
  newLine()
  print ("Subpunctul b: Repartitie comuna lui X si Y completa")
  tab <- fcomplrepcom(n, m, tab)
  print (tab)
  newLine()
  
  print ("Subpunctul c:")
  xp <- tab [1:n, m+1]
  yp <- tab [n+1, 1:m]
  
  afiseaza ("X",  xv,  xp)
  newLine()
  afiseaza ("Y",  yv,  yp)
  newLine()
  xycov <- cov(xv, xp, yv, yp)
  newLine()
  
  print (paste ("Subpunctul c) 1) cov(5X, -3Y) =", -15 * xycov))
  xc <- xv[0 < xv & xv < 3]
  yc <- yv[yv > 2]
  pc <- 0
  if (length (xc) > 0 && length (yc) > 0) {
    xst <- which (xv == xc[1])[1]
    xdr <- which (xv == xc[length(xc)])[1]
    yst <- which (yv == yc[1])[1]
    ydr <- which (yv == yc[length(yc)])
    pc  <- sum (tab[xst:xdr, yst:ydr]) / sum (yp[yst:ydr])
  }
  print (paste ("Subpunctul c) 2) P (0 < X < 3 | Y > 2) =", pc))
  
  xc <- xv[xv > 6]
  yc <- yv[yv < 7]
  pc <- 0
  if (length (xc) > 0 && length (yc) > 0) {
    xst <- which (xv == xc[1])[1]
    xdr <- which (xv == xc[length(xc)])[1]
    yst <- which (yv == yc[1])[1]
    ydr <- which (yv == yc[length(yc)])
    pc  <- sum (tab[xst:xdr, yst:ydr])
  }
  print (paste ("Subpunctul c) 3) P (X > 6 , Y < 7) =", pc))
  
  newLine()
  xyind <- fverind(n, m, tab, xycov)
  if (xyind) {msg <- "Independete"}
  else       {msg <- "Dependente"}
  print (paste ("Subpunctul d) 1) Dependenta:", msg))
  if (fvernecor(xycov, xyind))  
       {msg <- "Necorelate"}
  else {msg <- "Corelate"}                       
  print (paste ("Subpunctul d) 2) Corelanta:", msg))
}

solveBonus (5, 4)
