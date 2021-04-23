#include <RcppArmadilloExtensions/sample.h>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

const double TRUNC = .64;
const double TRUNC_RECIP = 1.0 / .64;

// Mathematical constants computed using Wolfram Alpha
#define MATH_PI        3.141592653589793238462643383279502884197169399375105820974
#define MATH_PI_2      1.570796326794896619231321691639751442098584699687552910487
#define MATH_2_PI      0.636619772367581343075535053490057448137838582961825794990
#define MATH_PI2       9.869604401089358618834490999876151135313699407240790626413
#define MATH_PI2_2     4.934802200544679309417245499938075567656849703620395313206
#define MATH_SQRT1_2   0.707106781186547524400844362104849039284835937688474036588
#define MATH_SQRT_PI_2 1.253314137315500251207882642405522626503493370304969158314
#define MATH_LOG_PI    1.144729885849400174143427351353058711647294812915311571513
#define MATH_LOG_2_PI  -0.45158270528945486472619522989488214357179467855505631739
#define MATH_LOG_PI_2  0.451582705289454864726195229894882143571794678555056317392

double aterm(int n, double x, double t) {
    double f = 0;
    if(x <= t) {
        f = MATH_LOG_PI + (double)std::log(n + 0.5) + 1.5*(MATH_LOG_2_PI- (double)std::log(x)) - 2*(n + 0.5)*(n + 0.5)/x;
    }
    else {
        f = MATH_LOG_PI + (double)std::log(n + 0.5) - x * MATH_PI2_2 * (n + 0.5)*(n + 0.5);
    }    
    return (double)exp(f);
}

double exprnd(double mu) {
    return -mu * (double)std::log(1.0 - (double)R::runif(0.0,1.0));
}

double truncgamma() {
    double c = MATH_PI_2;
    double X, gX;
    
    bool done = false;
    while(!done)
    {
        X = exprnd(1.0) * 2.0 + c;
        gX = MATH_SQRT_PI_2 / (double)std::sqrt(X);
        
        if(R::runif(0.0,1.0) <= gX) {
            done = true;
        }
    }
    
    return X;  
}

double randinvg(double mu) {
    // sampling
    double u = R::rnorm(0.0,1.0);
    double V = u*u;
    double out = mu + 0.5*mu * ( mu*V - (double)std::sqrt(4.0*mu*V + mu*mu * V*V) );
    
    if(R::runif(0.0,1.0) > mu /(mu+out)) {    
        out = mu*mu / out; 
    }    
    return out;
}

double tinvgauss(double z, double t) {
    double X, u;
    double mu = 1.0/z;
    
    // Pick sampler
    if(mu > t) {
        // Sampler based on truncated gamma 
        // Algorithm 3 in the Windle (2013) PhD thesis, page 128
        while(1) {
            u = R::runif(0.0, 1.0);
            X = 1.0 / truncgamma();
            
            if ((double)std::log(u) < (-z*z*0.5*X)) {
                break;
            }
        }
    }  
    else {
        // Rejection sampler
        X = t + 1.0;
        while(X >= t) {
            X = randinvg(mu);
        }
    }    
    return X;
}

double samplepg(double z) {
    //  PG(b, z) = 0.25 * J*(b, z/2)
    z = (double)std::fabs((double)z) * 0.5;
    
    // Point on the intersection IL = [0, 4/ log 3] and IR = [(log 3)/pi^2, \infty)
    double t = MATH_2_PI;
    
    // Compute p, q and the ratio q / (q + p)
    // (derived from scratch; derivation is not in the original paper)
    double K = z*z/2.0 + MATH_PI2/8.0;
    double logA = (double)std::log(4.0) - MATH_LOG_PI - z;
    double logK = (double)std::log(K);
    double Kt = K * t;
    double w = (double)std::sqrt(MATH_PI_2);
    
    double logf1 = logA + R::pnorm(w*(t*z - 1),0.0,1.0,1,1) + logK + Kt;
    double logf2 = logA + 2*z + R::pnorm(-w*(t*z+1),0.0,1.0,1,1) + logK + Kt;
    double p_over_q = (double)std::exp(logf1) + (double)std::exp(logf2);
    double ratio = 1.0 / (1.0 + p_over_q); 
    
    double u, X;
    
    // Main sampling loop; page 130 of the Windle PhD thesis
    while(1) 
    {
        // Step 1: Sample X ? g(x|z)
        u = R::runif(0.0,1.0);
        if(u < ratio) {
            // truncated exponential
            X = t + exprnd(1.0)/K;
        }
        else {
            // truncated Inverse Gaussian
            X = tinvgauss(z, t);
        }
        
        // Step 2: Iteratively calculate Sn(X|z), starting at S1(X|z), until U ? Sn(X|z) for an odd n or U > Sn(X|z) for an even n
        int i = 1;
        double Sn = aterm(0, X, t);
        double U = R::runif(0.0,1.0) * Sn;
        int asgn = -1;
        bool even = false;
        
        while(1) 
        {
            Sn = Sn + asgn * aterm(i, X, t);
            
            // Accept if n is odd
            if(!even && (U <= Sn)) {
                X = X * 0.25;
                return X;
            }
            
            // Return to step 1 if n is even
            if(even && (U > Sn)) {
                break;
            }
            
            even = !even;
            asgn = -asgn;
            i++;
        }
    }
    return X;
}

// [[Rcpp::export]]
double rpg(int n, double z){
    
    double x = 0;
    for(int i = 0; i < n; i++){
        x += samplepg(z);
    }
    
    return(x);
}

arma::vec mvrnormArma(arma::vec mu, arma::mat sigma) {
    int ncols = sigma.n_cols;
    arma::vec Y = arma::randn(ncols);
    return mu + arma::chol(sigma) * Y;
}

arma::vec mvrnormArmaQuick(arma::vec mu, arma::mat cholsigma) {
    int ncols = cholsigma.n_cols;
    arma::vec Y = arma::randn(ncols);
    return mu + cholsigma * Y;
}

// [[Rcpp::export]]
arma::mat diagMatrixProd(arma::mat& X, arma::vec& D){
    
    arma::mat result(X.n_rows, D.size());
    for(int i = 0; i < result.n_rows; i++){
        for(int j = 0; j < result.n_cols; j++){
            result(i, j) = X(i,j) * D(j);
        }  
    }
    
    return(result);
}

// [[Rcpp::export]]
arma::vec sample_beta_cpp(arma::mat& X, arma::mat& B, arma::vec& b, arma::vec& Omega, arma::vec& k){
    
    // arma::mat cov_matrix = arma::inv(arma::trans(X) * Omega * X + arma::inv(B));
    arma::mat tX = arma::trans(X);
    arma::mat tXOmega = diagMatrixProd(tX, Omega);
    // arma::mat cov_matrix = arma::inv(tXOmega * X + arma::inv(B));
    // arma::vec result = mvrnormArma(cov_matrix * (arma::trans(X) * k + arma::inv(B) * b), cov_matrix);
    
    arma::mat L = arma::trans(arma::chol(tXOmega * X + arma::inv(B))); 
    arma::vec tmp = arma::solve(arma::trimatl(L), tX * k + arma::inv(B) * b);
    arma::vec alpha = arma::solve(arma::trimatu(arma::trans(L)),tmp);
    
    arma::vec result = mvrnormArmaQuick(alpha, arma::trans(arma::inv(arma::trimatl(L))));
    
    return(result);
}

// [[Rcpp::export]]
arma::vec sample_Omega_cpp(arma::mat& X, arma::vec& beta, arma::vec& n){
    
    int nsize = n.size();
    arma::vec Omega_vec(nsize);
    
    for(int i = 0; i < nsize; i++){
        
        arma::vec b = X.row(i) * beta;
        Omega_vec[i] = rpg(n[i], b[0]);
        
    }
    
    return(Omega_vec);
}

// [[Rcpp::export]]
arma::vec sample_beta_nocov_cpp(arma::vec beta, arma::mat X, arma::vec b,
                                arma::mat B, arma::vec n, arma::vec k){
    
    arma::vec Omega = sample_Omega_cpp(X, beta, n);
    
    beta = sample_beta_cpp(X, B, b, Omega, k);
    
    return(beta);
}

double log_L_gamma_cpp(arma::vec gamma, arma::mat X, arma::vec indexes_covariates,
                       arma::vec b, arma::mat B, arma::vec Omega, arma::vec k){
    
    
    IntegerVector index_present(indexes_covariates.size());
    int l = 0;
    for(int i = 0; i < indexes_covariates.size(); i++){
        if(gamma[indexes_covariates[i]-1] == 1){
            index_present[l] = i;
            l += 1;
        }
    }
    
    arma::mat X_gamma(X.n_rows, l);
    arma::vec b_gamma(l);
    arma::mat B_gamma(l, l);
    
    for(int i = 0; i < l; i++){
        X_gamma.col(i) = X.col(index_present[i]);
        b_gamma[i] = b[index_present[i]];
        for(int j = 0; j < l; j++){
            B_gamma(i,j) = B(index_present[i], index_present[j]);
        }
    }
    
    arma::mat tX = arma::trans(X_gamma);
    arma::mat tXOmega = diagMatrixProd(tX, Omega);
    arma::mat cholXgOmX = arma::chol(tXOmega * X_gamma + arma::inv(B_gamma));
    
    double firstTerm = (.5) * log(det(arma::inv(B_gamma))) - log(det(cholXgOmX));
    
    arma::mat tXKbplusBb = arma::trans(X_gamma) * k + arma::inv(B_gamma) * b_gamma;
    
    arma::vec v = solve(arma::trimatl(arma::trans(cholXgOmX)),tXKbplusBb);
    arma::mat vtv = arma::trans(v) * v;
    
    arma::mat secondTerm = - .5 * ( (arma::trans(b_gamma) * arma::inv(B_gamma) * b_gamma) - vtv);
    
    // double firstTerm = (.5) * log(det(arma::inv(B_gamma))) - 
    //   (.5) * log(det(arma::trans(X_gamma) * Omega * X_gamma + arma::inv(B_gamma)));
    
    // arma::mat tXKbplusBb = arma::trans(X_gamma) * k + arma::inv(B_gamma) * b_gamma;
    
    // arma::mat secondTerm = - .5 * ( (arma::trans(b_gamma) * arma::inv(B_gamma) * b_gamma) -
    //     arma::trans(tXKbplusBb) * arma::inv(arma::trans(X_gamma) * Omega * X_gamma + 
    //     arma::inv(B_gamma)) * tXKbplusBb);
    
    return(firstTerm + secondTerm(0,0));
}

int sample_int(IntegerVector samples) {
    // Rcpp::IntegerVector pool = Rcpp::seq(1, 10);
    std::random_shuffle(samples.begin(), samples.end());
    return samples[0];
} 

arma::vec sample_gamma_cpp(arma::vec gamma, arma::mat X, arma::vec Omega, 
                           arma::vec b, arma::mat B, int ncov, arma::vec k, 
                           arma::vec indexes_covariates, int fixedIndexes, 
                           double d_bar){
    
    arma::vec gamma_star = gamma;
    
    double h_ratio = 1;
    
    if(R::runif(0, 1) < .33333){ // add
        
        if((sum(gamma) - fixedIndexes) != ncov){
            
            // Find zero covariates
            int numOfZeroCov = ncov - (sum(gamma) - fixedIndexes);
            IntegerVector zeroCov(numOfZeroCov);
            int i = 0;
            for(int l = fixedIndexes; l < gamma.size(); l++){
                if(gamma[l] == 0) {
                    zeroCov[i] = l;
                    i += 1;
                }
            }
            
            int covariate_to_update = sample_int(zeroCov);
            
            gamma_star[covariate_to_update] = 1;
            
            h_ratio = (ncov -(sum(gamma) - fixedIndexes)) / (ncov - (sum(gamma) - fixedIndexes) - 1 + ((ncov - d_bar) / (d_bar)) );
            
        }
        
    } else if(R::runif(0, 1) < .5){ // delete
        
        if((sum(gamma) - fixedIndexes) != 0){
            
            // Find non zero covariates
            int numOfNonZeroCov = sum(gamma) - fixedIndexes;
            IntegerVector nonZeroCov(numOfNonZeroCov);
            int i = 0;
            for(int l = fixedIndexes; l < gamma.size(); l++){
                if(gamma[l] == 1) {
                    nonZeroCov[i] = l;
                    i += 1;
                }
            }
            
            int covariate_to_update = sample_int(nonZeroCov);
            
            gamma_star[covariate_to_update] = 0;
            
            h_ratio = (ncov - (sum(gamma) - fixedIndexes) + ((ncov - d_bar) / (d_bar)) ) / (ncov - (sum(gamma) - fixedIndexes) + 1);
            
        }
        
    } else { // swap
        
        if((sum(gamma) - fixedIndexes) != 0 & (sum(gamma) - fixedIndexes) != ncov){
            
            // Find zero covariates
            int numOfZeroCov = ncov - (sum(gamma) - fixedIndexes);
            IntegerVector zeroCov(numOfZeroCov);
            int i = 0;
            for(int l = fixedIndexes; l < gamma.size(); l++){
                if(gamma[l] == 0) {
                    zeroCov[i] = l;
                    i += 1;
                }
            }
            int covariates2_to_swap = sample_int(zeroCov);
            
            // Find non zero covariates
            int numOfNonZeroCov = sum(gamma) - fixedIndexes;
            IntegerVector nonZeroCov(numOfNonZeroCov);
            i = 0;
            for(int l = fixedIndexes; l < gamma.size(); l++){
                if(gamma[l] == 1) {
                    nonZeroCov[i] = l;
                    i += 1;
                }
            }
            
            int covariates1_to_swap = sample_int(nonZeroCov);
            
            gamma_star[covariates1_to_swap] = 0;
            gamma_star[covariates2_to_swap] = 1;
            
            h_ratio = 1;
            
        }
        
    }
    
    double L_gamma_star = log_L_gamma_cpp(gamma_star, X, indexes_covariates, b, B, Omega, k);
    
    double L_gamma = log_L_gamma_cpp(gamma, X, indexes_covariates, b, B, Omega, k);
    
    h_ratio = h_ratio * exp(L_gamma_star - L_gamma);
    
    if(R::runif(0, 1) < h_ratio){
        gamma = gamma_star;
    }
    
    return(gamma);
}

// [[Rcpp::export]]
List sample_gamma_beta_cpp(arma::vec gamma, arma::vec beta, arma::mat X, 
                           arma::vec b, arma::mat B, int D, arma::vec n, arma::vec k, 
                           arma::vec indexes_covariates, int fixedIndexes, double d_bar){
    
    // resize beta and X
    IntegerVector index_present(indexes_covariates.size());
    int l = 0;
    for(int i = 0; i < indexes_covariates.size(); i++){
        if(gamma[indexes_covariates[i]-1] == 1){
            index_present[l] = i;
            l += 1;
        }
    }
    
    arma::mat X_gamma(X.n_rows, l);
    arma::vec beta_gamma(l);
    
    for(int i = 0; i < l; i++){
        X_gamma.col(i) = X.col(index_present[i]);
        beta_gamma[i] = beta[index_present[i]];
    }
    
    // sample Omega
    arma::vec Omega = sample_Omega_cpp(X_gamma, beta_gamma, n);
    
    // sample gamma
    gamma = sample_gamma_cpp(gamma, X, Omega, b, B, D, k, indexes_covariates, fixedIndexes, d_bar);
    
    // resize X, b and B
    l = 0;
    for(int i = 0; i < indexes_covariates.size(); i++){
        if(gamma[indexes_covariates[i]-1] == 1){
            index_present[l] = i;
            l += 1;
        }
    }
    
    arma::mat X_gamma2(X.n_rows, l);
    arma::vec b_gamma(l);
    arma::mat B_gamma(l, l);
    
    for(int i = 0; i < l; i++){
        X_gamma2.col(i) = X.col(index_present[i]);
        b_gamma[i] = b[index_present[i]];
        for(int j = 0; j < l; j++){
            B_gamma(i,j) = B(index_present[i], index_present[j]);
        }
    }
    
    // sample beta
    arma::vec beta_new = sample_beta_cpp(X_gamma2, B_gamma, b_gamma, Omega, k);
    
    for(int i = 0; i < l; i++){
        beta[index_present[i]] = beta_new[i];
    }  
    
    return List::create(_["gamma"] = gamma,
                        _["beta"] = beta,
                        _["Omega"] = Omega);
}

// [[Rcpp::export]]
arma::vec sample_z_cpp(arma::vec psi, arma::vec p, arma::mat k_s){
    
    arma::vec z = arma::zeros(k_s.n_rows);
    
    int l = 0;
    
    for(int i = 0; i < z.size(); i++){
        
        if(k_s(i, 0) == 1){
            
            z[i] = 1;
            l += k_s(i,1);
            
        } else {
            
            // Rcout << "ok here1" << std::endl;
            double prod1mp = 1; //prod(1 - p[i + seq_len(k_s[i,4])])
            for(int k = 0; k < k_s(i,1); k++){
                prod1mp *= (1 - p(l + k));
            }
            // Rcout << "ok here2" << std::endl;
            // Rcout << prod1mp << std::endl;
            double p_zsequal1 = (psi[i] * prod1mp) / (psi[i] * prod1mp + (1 - psi[i])) ;
            z[i] = R::rbinom(1, p_zsequal1);
            l += k_s(i,1);
            // Rcout << "ok here3" << std::endl;
        }
        
    }
    
    
    return(z);
}