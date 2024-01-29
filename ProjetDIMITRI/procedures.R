library(DT)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(shiny)
library(plotly)
library(ChainLadder)

# L'importation de la matrice de triangle
import <- function(path){
  #identification du type de fichier
  var <- strsplit(path,"")[[1]]
  a <- which(var==".")
  pos <- a[length(a)]
  type <- substr(path,pos+1,nchar(path))
  #importation selon les cas CSV ou xlsx
  if (type %in% c("xlsx","xls")){
    require(readxl)
    df <- read_excel(path)
  }else if (type %in% c("txt","csv")){
    require(data.table)
    df <- as.data.frame(fread(path, header = TRUE))
  }
  df[is.na(df)] <- 0
  return(as.matrix(df))
}
# OK

# L'evolution des paiments

evolution <- function(mat){
  pays = sapply(1:ncol(mat), function(x){
    indc = seq(from = x,to = 1000, by = ncol(mat)-1)[1:x] # to = 100 is a maximum value for the number of columns
    return(sum(as.vector(t(mat))[indc]))})
  
  return(pays)
}
#OK

evolutionGraph <- function(mat){
  pays <- evolution(mat)
  names(pays) <- colnames(mat)
  dfEvl = data.frame(Year = names(pays),Evolution = pays)
  dfEvl %>%
    ggplot(aes(x = as.factor(Year), y =Evolution )) +
    geom_bar(stat = "identity", fill = "steelblue", width = 0.6)+
    coord_cartesian(ylim = c(max(min(pays)-sd(pays),0), max(pays)+sd(pays)))+
    labs(
      y= "Payments",
      x= "Years",
      title= paste("Evolution des paiement par année")
    )+ theme_grey()
}
#OK

# Cumul du triangle

# Cette fonction a encore un pb !!
cummul <- function(data, kind = 0){
    data %>%
    apply(1,cumsum) %>%
    t() -> res
  if(kind == 0){
    return(res)
  }
  if(kind == 1){ 
    for(i in 1:ncol(res)){
      nr <- nrow(res)
      res[,i] <- c(res[,i][1:(nr-i+1)],rep(0,(i-1)))
    }
    return(res)}
  if(kind == 2){
    for(i in 1:ncol(res)){
      nr <- nrow(res)
      res[,i] <- c(res[,i][1:(nr-i+1)],rep(NA,(i-1)))
    }
    return(res)
  }

  }
#ok

# Coefficients de developement

f_j <- function(mat){
  f = sapply(1:ncol(mat)-1, function(x){
    f_ = sum(mat[1:(ncol(mat)-x),x+1])/sum(mat[1:(ncol(mat)-x),x])
    return(f_)
  })
  return(f[-1])
}
#OK

fjGraph <- function(data){
  f_k <- data %>% cummul(kind = 1) %>% f_j()
  df_Factor = data.frame(X = (1:length(f_k)), Y = f_k)
  df_Factor %>%
    ggplot(aes(x = X, y = Y))+
    coord_cartesian(ylim = c(min(f_k)-sd(f_k), max(f_k)+sd(f_k)/2))+
    geom_line(colour = "blue")+
    labs(
      y= " ",
      x= "Années de développement",
      title= paste("Evolution des facteurs de développement")
    )+ theme_grey()
}

p_j <- function(f_j){
  sapply(1:length(f_j), function(x){
    return(1/(prod(f_j[x:length(f_j)])))
  })
}
#OK
# Completing the triangle
C_chapeau=function(C,f){
  n=ncol(C)
  for (j in 1:(n-1)){
    C[(n-j+1):n,j+1]=f[j]*C[(n-j+1):n,j]
  }
  return(C)
}
#OK
independanceHyp <- function(df){
  meanChain = apply(df, 2,FUN = function(x)round(mean(x, na.rm = TRUE),2))
  
  sdChain = apply(df, 2,FUN = function(x)round(sd(x, na.rm = TRUE),2))
  
  cvChain = round(sdChain/meanChain,2)
  
  res = rbind(meanChain, sdChain, cvChain)
  
  res[is.na(res)] <- 0
  
  Qte = row.names(res)
  return(cbind(Qte,res))
}

ccplot <- function(df, j = 1){
  df %>% 
    as.data.frame() %>% 
    cummul() %>% 
    as.data.frame() %>%
    select(c(j,j+1)) -> plotData
    
  colnames(plotData) <- c("Y", "X")
  plotData %>%
    ggplot(aes(x=X, y=Y)) +
    labs(
      y= paste("C_",j+1),
      x= paste("C_",j),
      title= paste("CC-plot"))+
    geom_point() +
    stat_smooth(method=lm)
  
}

compareBase <- function(df){
  df %>%
    cummul(kind = 1)%>%
    f_j() -> f
  
  df %>%
    cummul(kind = 1)%>%
    C_chapeau(f) -> Rij
  prov = Rij[,ncol(Rij)]-diag(Rij[, ncol(Rij):1])
  return(prov)
}

compareMack <- function(df){
  df %>%
    cummul(kind = 2)%>%
    MackChainLadder(est.sigma = "Mack") %>%
    summary() -> mack
  
  return(mack$ByOrigin[,3])
}

compareBoot <- function(df){
  df %>%
    cummul(kind = 2)%>%
    BootChainLadder(R=999, process.distr="gamma") %>%
    summary() -> boot
  
  return(boot$ByOrigin[,3])
}

compare <- function(df, method = "Boot"){
  df <- as.data.frame(df)
  if(method == "Boot"){ 
    dfCompare <- data.frame(
      x = as.integer(colnames(df)),
      chL = compareBase(df),
      Boot = compareBoot(df)
    )
    df_long <- pivot_longer(dfCompare, cols = c("chL", "Boot"), names_to = "variable", values_to = "value")
    
    ggplot(df_long, aes(x = x, y = value, color = variable, group = variable)) +
      geom_line() +
      scale_color_manual(name = "ChainLadder vs Bootstrap", labels = c("Chain-Ladder", "Bootstrap"), values = c("red", "blue"))
    
  }
  else{ 
    dfCompare <- data.frame(
      x = as.integer(colnames(df)),
      chL = compareBase(df),
      Mack = compareMack(df)
    )
    
    df_long <- pivot_longer(dfCompare, cols = c("chL", "Mack"), names_to = "variable", values_to = "value")
    
    ggplot(df_long, aes(x = x, y = value, color = variable, group = variable)) +
      geom_line() +
      scale_color_manual(name = "ChainLadder vs Mack", labels = c("Chain-Ladder", "Mack"), values = c("red", "blue"))
    
  }
}
