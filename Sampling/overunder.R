# OverSampling
# UnderSampling

underSampling <- function(df, class) {
  # Cette methode d'echantillonage retourne un dataframe contenant le
  # meme nombres d'individus que la plus petite classe en utilisant
  # l'under sampling. Cette methode peut provoquer du sous apprentissage
  # class est le nom (string) de la variable a equilibrer


  size <- min(table(df[, class])) # on prend la taille de la plus petite classe
  min.class <- unique(as.character(df[,class]))[which.min(table(df[, class]))]

  delta <- function(val, df, size) {
    # fonction d'echantillonage pour une classe
    if (val == min.class) {
      # pour la plus petite classe on garde tous les individus
      return(which(df[, class] == val))
    } else {
      # sinon on prend un echantillon de meme taille que la plus petite classe
      return(sample(which(df[, class] == val), size))
    }
  }

  # application de delta sur toutes les classes (sauf la plus petite)
  lsamp <- unlist(sapply(X=unique(as.character(df[, class])), FUN=delta,
                         df=df, size=size))

  return(df[lsamp, ])
}

overSampling <- function(df, class) {
  # Cette methode d'echantillonage retourne un data frame contenant le meme
  # nombre d'individus pour chaque classe (maximal) par la methode d'over
  # sampling
  # class est le nom (string) de la variable a equilibrer


  size <- max(table(df[, class])) # on prend la taille de la plus petite classe
  max.class <- unique(as.character(df[, class]))[which.max(table(df[, class]))]

  delta <- function(val, df, size) {
    # fonction d'echantillonage pour une classe
    if (val == max.class) {
      # pour la plus grande classe on garde tous les individus
      return(which(df[, class] == val))
    } else {
      # sinon on prend un echantillon de meme taille que la plus petite classe
      return(sample(which(df[, class] == val), size, replace=TRUE))
    }
  }

  # application de delta sur toutes les classes (sauf la plus petite)
  lsamp <- unlist(sapply(X=unique(as.character(df[, class])), FUN=delta,
                         df=df, size=size))

  return(df[lsamp, ])
}
