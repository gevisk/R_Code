# Spectral clustering implementation in R (not optimized)


# Graphe de Voisinage +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
my_nngraph  <- function(X, similarity="gaussian", neighbor="connexe",
                        sigma=NULL, deg=NULL, nbvoisins=NULL, theta=NULL) {
  # Verification des parametres
  if (!neighbor %in% c("connexe", "seuil", "knn"))
    stop("neighbor must be 'connexe', 'seuil' or 'knn'")
  if (!similarity %in% c("gaussian", "linear", "polynomial"))
    stop("similarity must be 'gaussian', 'linear' or 'polynomial'")
  if (neighbor == "seuil" & !(is.numeric(theta) & length(theta) == 1))
    stop("With neighbor == 'seuil', theta must be a numeric value")
  if (neighbor == "knn" & !(is.numeric(nbvoisins) & length(nbvoisins) == 1))
    stop("With neighbor == 'knn', k must be a numeric value")
  if(similarity == "gaussian" & is.null(sigma))
    stop("sigma must be not null for gaussian similarity")
  if(similarity == "polynomial" & is.null(deg))
    stop("deg must be not null for gaussian polynomial")

  # initialisation ------------------------------------------------------------
  # fonction de similarite entre deux individus
  funSim <- list(gaussian=function(xi, xj, sigma, deg)
    exp(-(norm(as.matrix(xi-xj))^2) / (2*sigma^2)),
    linear=function(xi, xj, sigma, deg)
      t(xi) %*% xj + 1,
    polynomial=function(xi, xj, sigma, deg)
      (t(xi) %*% xj + 1)^deg
  )[[similarity]]

  # calcul de similarites -----------------------------------------------------
  d <- matrix(nrow=nrow(X), ncol=nrow(X), dimnames=list(1:nrow(X), 1:nrow(X)))
  d <- apply(X=X, MARGIN=1, function(li) {
    apply(X=X, MARGIN=1, function(lj, li) funSim(li, lj, sigma, deg),
          li=li)
  })

  # graphe de voisinage -------------------------------------------------------
  W <- matrix(data=0, nrow=nrow(d), ncol=ncol(d))

  if (neighbor == "knn") {
    row.names(d) <- 1:nrow(X)
    colnames(d) <- 1:nrow(X)
    lapply(1:nrow(d), function(i) {
      names(sort(d[i, ]))[2:(nbvoisins+1)]
    })
    for (i in 1:nrow(W)) {
      for (j in 1:ncol(W)) {
        if (as.character(i) %in% nn[[j]] &&
            as.character(j) %in% nn[[i]]) {
          W[i, j] <- d[i, j]
        }
      }
    }
  } else if (neighbor == "seuil") {
    W[d>=theta] <- d[d>=theta]
  } else {
    # neighbor == "connexe"
    W[d>=0] <- d[d>=0]
  }
  return(W)
}

# Spectral clustering +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
my_spclust <- function(X, k, iter.max=50, similarity="gaussian",
                       neighbor="connexe", sigma=NULL, deg=NULL, nbvoisins=NULL,
                       theta=NULL, normalized=TRUE) {
  # Matrice de voisinage
  W <- my_nngraph(X, similarity=similarity, neighbor=neighbor,
                  sigma=sigma, deg=deg, nbvoisins=nbvoisins, theta=theta)

  # Matrice diagonale des degres de W
  D <- diag(x=apply(W, 1, sum), nrow=nrow(W))

  # Matrice Laplacienne
  L <- D - W

  if (normalized)
    L <- solve(D^0.5) %*% L %*% solve(D^0.5)
  # F matrice des k premiers vecteurs propres
  F <- eigen(L)$vectors[, (ncol(L)-k+1):ncol(L)]
  if (normalized)
    F <- apply(F, 2, scale)

  return(kmeans(F, centers=k, iter.max=iter.max))
}
