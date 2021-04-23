logit <- function(x){
  1 / (1 + exp(-x))
}

invLogit <- function(x){
  log(x / (1 - x))
}

ExtractCovariatesFromText <- function(covariates_text) {
  
  covariates <- sapply(strsplit(covariates_text, split = ",")[[1]], function(x){
    if(grepl("-",x)){
      as.numeric(seq(strsplit(x, split = "-")[[1]][1], strsplit(x, split = "-")[[1]][2]))
    } else {
      as.numeric(x)
    }
  })
  covariates <- as.vector(unlist(covariates))
  if(covariates[1] == 0) covariates <- c()
  
  covariates
}

# true or false if the covaraites is categorical or numerical
ExtractClassCovariates <- function(ncov, fac_covariates, column_covariate){
  classCovariates <- rep(T, ncov)
  classCovariates[match(fac_covariates, column_covariate)] <- F
  
  classCovariates
}

Extract_IndexesCovariates <- function(X, column_covariate, ncov, classCovariates) {
  
  indexes_covariates <- c()
  indexes_covariates[1] <- 1
  if(any(column_covariate != 0)){
    
    indexes_covariates <- c()
    indexes_covariates[1] <- 1
    k <- 2
    for (i in 1:ncov) {
      if(classCovariates[i]){
        indexes_covariates[k] <- i + 1
        k <- k + 1
      } else {
        num_levels <- length(unique(X[,i]))
        indexes_covariates[k + 0:(num_levels-2)] <- i + 1
        k <- k + num_levels - 1
      }
    }
    
  }
  
  indexes_covariates
}

ExtractInteractions <- function(inter_covariates_text){
  
  interactions <- t(sapply(strsplit(inter_covariates_text, split = ",")[[1]], function(x){
    c(as.numeric(strsplit(x, split = ":")[[1]][1]), as.numeric(strsplit(x, split = ":")[[1]][2]))
  }))
  
  interactions
}

createInteractionMatrix <- function(interactions, column_covariate, X) {
  
  allInteractions <- apply(interactions, 1, function(x){
    paste(c(colnames(X)[match(x[1], column_covariate)],
            colnames(X)[match(x[2], column_covariate)]),
          collapse = "*")
  })
  
  fmla <- as.formula(paste(" ~ ", paste(allInteractions, collapse= "+")))
  
  fullMatrix <- model.matrix(fmla, X)[,-1]
  InterCov <- grep(":",colnames(fullMatrix))
  
  list("matrix" = fullMatrix[,InterCov],
       "namesVariables" = as.character(allInteractions))
}

Extract_IndexesInteractions <- function(interactions, X, num_covariates, fac_covariates,
                                        column_covariate, classCovariates) {
  
  indexes_interactions <- c()
  
  k <- 0
  l <- 0
  for (i in 1:nrow(interactions)) {
    
    if(classCovariates[match(interactions[i,1], column_covariate)]){
      num_levels1 <- 1
    } else {
      num_levels1 <- length(unique(X[,match(interactions[i,1], column_covariate)])) - 1
    }
    
    if(classCovariates[match(interactions[i,2], column_covariate)]){
      num_levels2 <- 1
    } else {
      num_levels2 <- length(unique(X[,match(interactions[i,2], column_covariate)])) - 1
    }
    
    indexes_interactions[k + 1:(num_levels1*num_levels2)] <- l + 1
    
    l <- l + 1  
    k <- k + num_levels1*num_levels2 
    
  }
  
  indexes_interactions
}

# compute subset of X consisting only of the columns (covariates) used
computeXgamma <- function(X, indexes_covariates, gamma){
  
  index_present <- indexes_covariates %in% which(gamma == 1)
  X_gamma <- X[,index_present,drop = F]
  
  X_gamma
}

# compute subset of the coefficient beta consisting only of the covariates used
compute_betagamma <- function(beta, indexes_covariates, gamma){
  
  index_present <- indexes_covariates %in% which(gamma == 1)
  beta_gamma <- beta[index_present]
  
  beta_gamma
}

sample_z <- function(k_s, w_sm, psi, theta11, theta10, p){
  
  S <- nrow(w_sm)
  M <- ncol(w_sm)
  
  # number of sampling occasion per site
  M_site <- apply(w_sm, 1, function(x){
    sum(!is.na(x))  
  })
  
  w_s <- apply(w_sm, 1, function(x) {
    sum(x, na.rm = T)
  })
  
  z <- rep(NA, S)
  
  for (s in 1:S) {
    if(k_s[s] == 1){
      z[s] = 1
    } else {
      
      p_zsequal1 <- ((1 - p) * psi[s] * theta11[s]^w_s[s] * (1 - theta11[s])^(M_site[s] - w_s[s])) / 
        ((1 - p) *psi[s] * theta11[s]^w_s[s] * (1 - theta11[s])^(M_site[s] - w_s[s]) + (1 - psi[s]) * theta10[s]^w_s[s] * (1 - theta10[s])^(M_site[s] - w_s[s]))
      
      z[s] <- rbinom(1, 1, p_zsequal1)
      
    }
  }
  
  z
}

sample_wsm <- function(theta11, z, theta10, p11, p10, y_sm, K){
  
  S <- nrow(y_sm)
  M <- ncol(y_sm)
  w_sm <- matrix(NA, nrow = S, ncol = M)
  
  for (s in 1:S) {
    
    for (m in 1:M) {
      
      if(!is.na(y_sm[s, m])){
        
        p_wsm <- (theta11[s]^z[s] * theta10[s]^(1 - z[s]) * p11[s]^y_sm[s,m] *(1 - p11[s])^(K - y_sm[s,m]) ) / 
          (theta11[s]^z[s] * theta10[s]^(1 - z[s]) * p11[s]^y_sm[s,m] *(1 - p11[s])^(K - y_sm[s,m]) +
             (1 - theta11[s])^z[s] * (1 - theta10[s])^(1 - z[s]) * p10[s]^y_sm[s,m] *(1 - p10[s])^(K - y_sm[s,m])  )
        
        w_sm[s,m] <- rbinom(1, 1, p_wsm)
        
      }
      
    }
    
  }
  
  w_sm
}

compute_pzwsm_old <- function(zi, w_sm_current, psi, p, theta11, theta10, k_s, p11, p10, y_sm, K, M_site, s){
  
  totalProb <- 1
  
  ws <- sum(w_sm_current)
  
  p_zsequal1 <- ((1 - p) * psi[s] * theta11[s]^ws * (1 - theta11[s])^(M_site[s] - ws)) / 
    ((1 - p) *psi[s] * theta11[s]^ws * (1 - theta11[s])^(M_site[s] - ws) + (1 - psi[s]) * theta10[s]^ws * (1 - theta10[s])^(M_site[s] - ws))
  
  if(zi == 1){ # this is actually 0
    
    zs <- 0
    
    totalProb <- totalProb * (1 - p_zsequal1)
    
  } else {
    
    zs <- 1
    
    totalProb <- totalProb * p_zsequal1
    
  }
  
  for (m in 1:M_site[s]) {
    
    p_wsm <- (theta11[s]^zs * theta10[s]^(1 - zs) * p11[s]^y_sm[s,m] * (1 - p11[s])^(K - y_sm[s,m])) / 
      (theta11[s]^zs * theta10[s]^(1 - zs) * p11[s]^y_sm[s,m] *(1 - p11[s])^(K - y_sm[s,m]) +
         (1 - theta11[s])^zs * (1 - theta10[s])^(1 - zs) * p10[s]^y_sm[s,m] *(1 - p10[s])^(K - y_sm[s,m]))
    
    if(w_sm_current[m] == 1){
      totalProb <- totalProb * p_wsm  
    } else {
      totalProb <- totalProb * (1 - p_wsm)
    }
    
  }
  
  totalProb
}

compute_pzwsm <- function(zi, w_sm_current, psi, p, theta11, theta10, k_s, p11, p10, y_sm, K, M_site, s){
  
  firstTerm <- (psi[s] * (1 - p))^zi
  
  prodFirstTerm <- 1
  
  for (m in 1:M_site[s]) {
    prodFirstTerm <- prodFirstTerm * (theta11[s] * dbinom(y_sm[s,m],K,p11[s]))^(w_sm_current[m]) *
      ((1 - theta11[s]) * dbinom(y_sm[s,m],K,p10[s]))^(1 - w_sm_current[m])
  }
  
  firstTerm <- firstTerm * (prodFirstTerm)^(zi)
  
  secondTerm <- (1 - psi[s])^(1 - zi)
  
  prodSecondTerm <- 1
  
  for (m in 1:M_site[s]) {
    prodSecondTerm <- prodSecondTerm * (theta10[s] * dbinom(y_sm[s,m],K,p11[s]))^(w_sm_current[m]) *
      ((1 - theta10[s]) * dbinom(y_sm[s,m],K,p10[s]))^(1 - w_sm_current[m])
  }
  
  secondTerm <- secondTerm * prodSecondTerm^(1 - zi)
  
  firstTerm * secondTerm
}

BinToDec <- function(x) {
  sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1)) 
}

DecToBin <- function(x, M){
  x <- rev(intToBits(x))
  x <- x[length(x) - 0:(M-1)]
  x <- paste(as.integer(x), collapse = "")  
  as.numeric(strsplit(x,"")[[1]]) 
}

sample_z_wsm <- function(psi, theta11, theta10, k_s, p11, p10, y_sm, K, p){
  
  S <- nrow(y_sm)
  M <- ncol(y_sm)
  
  M_site <- apply(y_sm, 1, function(x){
    sum(!is.na(x))  
  })
  
  z <- rep(NA, S)
  w_sm <- matrix(NA, nrow = S, ncol = M)
  
  for (s in 1:S) {
    
    if(k_s[s] == 1){
      
      z[s] = 1
      
      for (m in 1:M_site[s]) {
        
        if(!is.na(y_sm[s, m])){
          
          p_wsm <- (theta11[s]^z[s] * theta10[s]^(1 - z[s]) * p11[s]^y_sm[s,m] *(1 - p11[s])^(K - y_sm[s,m]) ) / 
            (theta11[s]^z[s] * theta10[s]^(1 - z[s]) * p11[s]^y_sm[s,m] *(1 - p11[s])^(K - y_sm[s,m]) +
               (1 - theta11[s])^z[s] * (1 - theta10[s])^(1 - z[s]) * p10[s]^y_sm[s,m] *(1 - p10[s])^(K - y_sm[s,m])  )
          
          w_sm[s,m] <- rbinom(1, 1, p_wsm)
          
        }
        
      }
      
    } else {
      
      # p_zwsm_old <- matrix(NA, nrow = 2, ncol = 2^M_site[s])
      # 
      # for (zi in 1:2) { # 1 is 0 and 2 is 1
      # 
      #   for (l in 1:(2^(M_site[s]))) {
      # 
      #     w_sm_current <- DecToBin(l - 1, M_site[s])
      # 
      #     p_zwsm_old[zi, l] <- compute_pzwsm_old(zi, w_sm_current, psi, p, theta11, theta10, k_s, p11, p10, y_sm, K, M_site, s)
      # 
      #   }
      # 
      # }
      
      p_zwsm <- matrix(NA, nrow = 2, ncol = 2^M_site[s])
      
      for (zi in 1:2) { # 1 is 0 and 2 is 1
        
        for (l in 1:(2^(M_site[s]))) {
          
          w_sm_current <- DecToBin(l - 1, M_site[s])
          
          p_zwsm[zi, l] <- compute_pzwsm(zi - 1, w_sm_current, psi, p, theta11, theta10, k_s, p11, p10, y_sm, K, M_site, s)
          
        }
        
      }
      
      p_zwsm <- p_zwsm / sum(p_zwsm)
      
      sampledCombination <- sample(nrow(p_zwsm) * ncol(p_zwsm), 1, prob = as.vector(p_zwsm))
      
      z[s] <- (1 - sampledCombination %% 2)
      
      index_wsm_combination <- floor((sampledCombination + 1) / 2) 
      w_sm[s,1:M_site[s]] <- DecToBin(index_wsm_combination - 1, M_site[s])
    }
    
  }
  
  list("z" = z,
       "w_sm" = w_sm)
}

update_psi <- function(beta, gamma, z, X, indexes_covariates, b_psi, B_psi, usingC, d_bar){
  
  S <- length(z)
  ncov <- ncol(gamma) - 1
  
  gamma_psi <- NULL
  
  if(usingC){ # if covariates are used
    
    k <- z - .5
    n <- rep(1, S)
    
    list_gammabeta <- sample_gamma_beta_cpp(gamma[1,], beta[1,],  X, b_psi, B_psi, 
                                            ncov, n, k, indexes_covariates, 1, d_bar)
    gamma_psi <- list_gammabeta$gamma
    beta_psi <- list_gammabeta$beta
    
    index_present <- indexes_covariates %in% which(gamma_psi == 1)
    beta_gamma <- beta_psi[index_present]
    
    psi <- as.vector(logit(X[,index_present,drop = F] %*% beta_gamma))
    
  } else { # if no covariates a used
    
    n <- rep(1, S)
    sumn <- sum(n)
    sumz <- sum(z)
    k <- sumz - sumn * (.5)
    
    X_single <- matrix(1)
    
    beta_psi <- sample_beta_nocov_cpp(beta[1,1], X_single, b_psi, B_psi, sumn, k)
    psi <- logit(beta_psi)
    
    psi <- rep(psi, S)
    
  }
  
  list("beta" = beta_psi, "psi" = psi, "gamma" = gamma_psi)
}

update_theta <- function(beta, gamma, w_sm, z, X, indexes_covariates, theta_1or0, b_theta, 
                         B_theta, usingC, d_bar){
  
  M <- ncol(w_sm)
  S <- nrow(w_sm)
  ncov <- ncol(gamma) - 1
  
  M_site <- apply(w_sm, 1, function(x){
    sum(!is.na(x))  
  })
  
  gamma_theta <- NULL
  
  if(usingC){
    
    if(theta_1or0){
      
      k <- as.vector(t(w_sm[z==1,])) - .5
      k <- k[!is.na(k)]
      
      n <- rep(1, length(k))
      
      cov_indexes <- rep(1:S, times = M_site * z) # select only the observation with w == 1
      X_cov <- X[cov_indexes,,drop = FALSE]
      
      list_gammabeta <- sample_gamma_beta_cpp(gamma[2,], beta[2,], 
                                              X_cov, b_theta, B_theta, ncov, n, k, indexes_covariates, 1, d_bar)
      gamma_theta <- list_gammabeta$gamma
      beta_theta <- list_gammabeta$beta
      
    } else {
      
      k <- as.vector(t(w_sm[z==0,])) - .5
      k <- k[!is.na(k)]
      
      n <- rep(1, length(k))
      
      cov_indexes <- rep(1:S, times = M_site * (1-z))
      X_cov <- X[cov_indexes,,drop = FALSE]
      
      list_gammabeta <- sample_gamma_beta_cpp(gamma[3,], beta[3,], 
                                              X_cov, b_theta, B_theta, ncov, n, k, indexes_covariates, 1, d_bar)
      gamma_theta <- list_gammabeta$gamma
      beta_theta <- list_gammabeta$beta
      
    }
    
    index_present <- indexes_covariates %in% which(gamma_theta == 1)
    beta_gamma <- beta_theta[index_present]
    
    theta <- as.vector(logit(X[,index_present,drop = F] %*% beta_gamma))
    
  } else {
    
    if(theta_1or0){
      
      sumn <- sum(z * M_site)
      sumz <- sum(w_sm[z==1,], na.rm = T)
      # sumn <- sum(z) * M
      # sumz <- sum(w_sm[z==1,])
      k <- sumz - sumn * (.5)
      
      X_single <- matrix(1)
      
      beta_theta <- sample_beta_nocov_cpp(beta[2,1], X_single, b_theta, B_theta, sumn, k)
      
    } else {
      
      sumn <- sum((1 - z) * M_site)
      sumz <- sum(w_sm[z==0,], na.rm = T)
      # sumn <- (S - sum(z)) * M
      # sumz <- sum(w_sm[z==0,])
      k <- sumz - sumn * (.5)
      
      X_single <- matrix(1)
      
      beta_theta <- sample_beta_nocov_cpp(beta[3,1], X_single, b_theta, B_theta, sumn, k)
      
    }
    
    theta <- logit(beta_theta)
    theta <- rep(theta, S)
    
  }
  
  list("theta" = theta, "beta" = beta_theta, "gamma" = gamma_theta)
}

update_p <- function(beta, gamma, y_sm, w_sm, z, X, indexes_covariates, p_1or0, K, b_p,
                     B_p, usingC, d_bar){
  
  M <- ncol(w_sm) 
  S <- nrow(w_sm)  
  ncov <- ncol(gamma) - 1
  
  M_site <- apply(w_sm, 1, function(x){
    sum(!is.na(x))  
  })
  
  if(usingC){
    
    if(p_1or0){ # if p11 is updated
      
      # take the subset of y_sm corresponding to the occasion where eDNA was present
      k <- as.vector(t(y_sm))[as.vector(t(w_sm)) == 1] - (K/2)
      k <- k[!is.na(k)]
      n <- rep(K, length(k))
      
      # take the row of the covariance matrix (potentially replicating some rows) corresponding
      # to the cells of y_sm where eDNA was present
      sum_wsm <- apply(w_sm, 1, function(x){
        sum(x, na.rm = T)  
      })
      cov_indexes <- rep(1:S, times = sum_wsm)
      X_cov <- X[cov_indexes,,drop = FALSE]
      
      list_gammabeta <- sample_gamma_beta_cpp(gamma[4,], beta[4,], 
                                              X_cov, b_p, B_p, ncov, n, k, indexes_covariates, 1, d_bar)
      
    } else { # if p10 is updated
      
      # take the subset of y_sm corresponding to the occasion where eDNA was not present
      k <- as.vector(t(y_sm))[as.vector(t(w_sm)) == 0] - (K/2)
      k <- k[!is.na(k)]
      n <- rep(K, length(k))
      
      # take the row of the covariance matrix (potentially replicating some rows) corresponding
      # to the cells of y_sm where eDNA was not present
      sum_wsm <- apply(w_sm, 1, function(x){
        sum(x, na.rm = T)  
      })
      cov_indexes <- rep(1:S, times = M_site - sum_wsm)
      X_cov <- X[cov_indexes,,drop = FALSE]
      
      list_gammabeta <- sample_gamma_beta_cpp(gamma[5,], beta[5,], 
                                              X_cov, b_p, B_p, ncov, n, k, indexes_covariates, 1, d_bar)
      
    }
    
    gamma_p <- list_gammabeta$gamma
    beta_p <- list_gammabeta$beta
    
    X_gamma <- computeXgamma(X, indexes_covariates, gamma_p)
    beta_gamma <- compute_betagamma(beta_p, indexes_covariates, gamma_p)
    
    p <-  as.vector(logit(X_gamma %*% as.matrix(beta_gamma)))
    
  } else {
    
    if(p_1or0){
      
      successes_y_sm_w_smequal1 <- sum(as.vector(y_sm)[as.vector(w_sm)==1], na.rm = T)
      trials_w_smequal1 <- K * sum(w_sm == 1, na.rm = T)
      
      sumn <- trials_w_smequal1
      sumz <-  successes_y_sm_w_smequal1
      k <- sumz - sumn * (.5)
      
      X_single <- matrix(1)
      
      beta_p <- sample_beta_nocov_cpp(beta[4,1], X_single, b_p, B_p, sumn, k)
      
    } else {
      
      successes_y_sm_w_smequal0 <- sum(as.vector(y_sm)[as.vector(w_sm)==0], na.rm = T)
      trials_w_smequal0 <- K * sum(w_sm == 0, na.rm = T)
      
      sumn <- trials_w_smequal0
      sumz <-  successes_y_sm_w_smequal0
      k <- sumz - sumn * (.5)
      
      X_single <- matrix(1)
      
      beta_p <- sample_beta_nocov_cpp(beta[5,1], X_single, b_p, B_p, sumn, k)
      
    }
    
    p <- logit(beta_p)
    p <- rep(p, S)
    gamma_p <- NULL
    
  }
  
  list("p" = p, "beta" = beta_p, "gamma" = gamma_p)
}


createPlots <- function(data_output, beta_data_output, gamma_data_output, indexes_covariates,
                        usingC, S, niter, nchain, nameVariable, VariableText ){
  
  data_output2 <- matrix(NA, nrow = niter * nchain, ncol = S)
  for (chain in 1:nchain) {
    data_output2[(chain - 1)*niter + 1:niter,] <- data_output[chain,,]
  }
  
  data_output_long <- reshape2::melt(data_output2)
  
  CI_data  <- sapply(1:ncol(data_output2), function(i){
    c(quantile(data_output2[,i], probs = c(0.025,0.975)),
      mean(data_output2[,i]))
  })
  
  if(usingC){
    plot1 <- ggplot2::ggplot() + 
      ggplot2::geom_errorbar(data = NULL, ggplot2::aes(x = 1:S, ymax=CI_data[2,], 
                                     ymin=CI_data[1,]),
                    width=0.2, size=1, color="black") + 
      ggplot2::geom_point(data = NULL, ggplot2::aes(x = 1:S, 
                                  y=CI_data[3,]), size=2, shape=21, fill="white") +
      # theme(panel.background = element_rect(fill = "white")) +
      ggplot2::theme_bw() + ggplot2::scale_y_continuous(name = nameVariable) +
      ggplot2::xlab("Sites") 
  } else {
    plot1 <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = "Site", y = data_output2[,1])) + ggplot2::geom_boxplot() +
      ggplot2::theme_bw() + ggplot2::scale_y_continuous(name = nameVariable) +
      ggplot2::xlab("")
  }
  
  data_plot <- plot1
  
  # beta_psi - gamma_psi
  if(usingC){
    
    {
      beta_data_output2 <- matrix(NA, nrow = niter * nchain, ncol = dim(beta_data_output)[3])
      for (chain in 1:nchain) {
        beta_data_output2[(chain - 1)*niter + 1:niter,] <- beta_data_output[chain,,]
      }
      gamma_data_output2 <- matrix(NA, nrow = niter * nchain, ncol = dim(gamma_data_output)[3])
      for (chain in 1:nchain) {
        gamma_data_output2[(chain - 1)*niter + 1:niter,] <- gamma_data_output[chain,,]
      }
      
      beta_data_output2[,1] <- logit(beta_data_output2[,1])
      colnames(beta_data_output2) <- dimnames(beta_data_output)[[3]]
      
      beta0_data_plot <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = beta_data_output2[,1], y = ..density..)) + 
        ggplot2::geom_histogram(fill = "cornsilk", color = "black") + ggplot2::ylab("") + 
        ggplot2::xlab("Probability") + 
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, 
                                                          margin = ggplot2::margin(0,0,5,0), size = 14, face = "bold"),
              panel.background = ggplot2::element_rect(fill = "white"), 
              panel.border = ggplot2::element_rect(fill = NA, colour = "grey20"),
              panel.grid.major = ggplot2::element_line(colour = "grey92"), 
              panel.grid.minor = ggplot2::element_line(colour = "grey92", size = 0.25), 
              strip.background = ggplot2::element_rect(fill = "grey85", colour = "grey20"), 
              legend.key = ggplot2::element_rect(fill = "white", colour = NA)) + 
        ggplot2::ggtitle(VariableText)
      
      CICoefficients_data  <- sapply(1:dim(beta_data_output2)[2], function(i){
        if(i == 1){
          c(quantile(beta_data_output2[,1], probs = c(0.025,0.975)),
            mean(beta_data_output2[,1]))
        } else {
          # print(beta_data_output2[gamma_data_output2[,indexes_covariates[i]-1]!= 0,i])
          c(quantile(beta_data_output2[gamma_data_output2[,indexes_covariates[i]-1]!= 0,i], probs = c(0.025,0.975)),
            mean(beta_data_output2[gamma_data_output2[,indexes_covariates[i]-1]!= 0,i]))
        }
      })
      
      PIP_data <- data.frame(name = dimnames(gamma_data_output)[[3]],
                             prob = apply(gamma_data_output2, 2, mean))
      
      gamma_data_plot <- ggplot2::ggplot(PIP_data, ggplot2::aes(x=reorder(name, prob), y=prob)) +
        ggplot2::geom_point(size=3) + # Use a larger dot
        ggplot2::theme_bw() +
        ggplot2::ylab("PIP") + ggplot2::xlab("Variable") + 
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, 
                                                          margin = ggplot2::margin(0,0,5,0), size = 14, face = "bold"),
              panel.grid.major.x = ggplot2::element_blank(),
              panel.grid.minor.x = ggplot2::element_blank(),
              panel.grid.major.y = ggplot2::element_line(colour="grey60", linetype="dashed"),
              axis.text.x = ggplot2::element_text(angle = 90)) +
        ggplot2::ylim(c(0,1)) + ggplot2::geom_hline(ggplot2::aes(yintercept = .5), color = "red")
      
      beta_data_plot <- ggplot2::ggplot() +
        ggplot2::geom_errorbar(data = NULL, ggplot2::aes(x = reorder(factor(colnames(beta_data_output2)[-1], 
                                                          levels = colnames(beta_data_output2)[-1]), 
                                                   rep(PIP_data$prob, table(indexes_covariates[-1]))), ymax=CICoefficients_data[1,-1], 
                                       ymin=CICoefficients_data[2,-1]),
                      width=0.2, size=1, color="black") +
        ggplot2::geom_point(data = NULL, ggplot2::aes(x = reorder(factor(colnames(beta_data_output2)[-1], 
                                                       levels = colnames(beta_data_output2)[-1]), 
                                                rep(PIP_data$prob, table(indexes_covariates[-1]))), 
                                    y=CICoefficients_data[3,-1]), size=4, shape=21, fill="white") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, 
                                                          margin = ggplot2::margin(0,0,5,0), size = 14, face = "bold"),
              panel.background = ggplot2::element_rect(fill = "white"),
              panel.border = ggplot2::element_rect(fill = NA, colour = "grey20"),
              panel.grid.major = ggplot2::element_line(colour = "grey92"),
              panel.grid.minor = ggplot2::element_line(colour = "grey92", size = 0.25),
              strip.background = ggplot2::element_rect(fill = "grey85", colour = "grey20"),
              legend.key = ggplot2::element_rect(fill = "white", colour = NA),
              axis.text.x = ggplot2::element_text(angle = 90))  +
        ggplot2::xlab("Variable")+ ggplot2::ylab("Coefficient") #+ coord_flip()
      
    }
    
  } else {
    beta0_data_plot <- NULL
    beta_data_plot <- NULL
    gamma_data_plot <- NULL
  }
  
  list("data_plot" = data_plot,
       "beta0_data_plot" = beta0_data_plot,
       "beta_data_plot" = beta_data_plot,
       "gamma_data_plot" = gamma_data_plot)
}

computeGewekeDiagnostics <- function(output){
  
  niter <- length(output)
  
  output_1 <- output[1:(niter/2)]
  output_2 <- output[niter/2 + 1:(niter/2)]
  
  X1 <- mean(output_1)
  X2 <- mean(output_2)
  
  s1 <- sd(output_1)
  s2 <- sd(output_2)
  
  T_stat <- (X1 - X2) / sqrt( s1*s1/(niter/2) + s2*s2/(niter/2) )
  
  2 * (1 - pnorm(abs(T_stat)))
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  # plots <- c(list(...), plotlist)
  
  # alex addition
  plots <- list()
  for(i in 1:length(plotlist)){
    for(j in 1:length(plotlist[[1]])){
      plots[[j + (i - 1) * length(plotlist[[1]])]] <- plotlist[[i]][[j]]
    }
  }
  #
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

