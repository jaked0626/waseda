// RBC model 1
// So Kubota, Waseda, Econ research, 2021
close all;clc;  // house keeping

// #####  1 variables  #####
var C,N,K,z; // You can also add Y,I,r,w
varexo e;  // The shock is epsilon

// #####  2 parameters  #####
parameters alpha, beta, delta, rho, sigma, chi;
alpha = 0.33;
beta = 0.99;
delta = 0.025;
rho = 0.99;
sigma = 0.0089;
// steady state values to calculate chi
Nss=0.33; 
Kss = (alpha/(beta^(-1)-1+delta))^(1/(1-alpha))*Nss;
Css = (Kss^alpha)*(Nss^(1-alpha)) - delta*Kss;
chi = ((1-alpha)*(Kss^alpha)*(1-Nss))/(Nss^alpha*Css);

// #####  3 model  #####
// Don't forget to push K back for one period. 
// Dynare understands K as predetermined
// z should be unchanged. Dynare already knows that.

// main 4 equations
model;
(1/C)=beta*(alpha*z(+1)*K^(alpha-1)*N(+1)^(1-alpha)+1-delta)*(1/C(+1));
chi*C/(1-N) = (1-alpha)*z*K(-1)^alpha*N^(-alpha);
K+C=z*K(-1)^alpha*N^(1-alpha)+(1-delta)*K(-1);
log(z) = rho*log(z(-1))+sigma*e;

// additional equations if you want
// Y = z*K(-1)^alpha*N^(1-alpha);
// I = K - (1-delta)*K(-1);
// r = z*alpha*K(-1)^(alpha-1)*N^(1-alpha);
// w = z*(1-alpha)*K(-1)^(alpha)*N^(-alpha);

end;

// #####  4 stady state  #####
initval; // initial value in the steady state
//steady_state_model;  
z=1;  // normalization
N=Nss;
K=Kss;
C=Css;

// additional variables
// Y=Kss^alpha*Nss^(1-alpha);
// I = delta*Kss;
// r = alpha*Kss^(alpha-1)*Nss^(1-alpha);
// w = (1-alpha)*Kss^(alpha)*Nss^(-alpha);
end;
steady;
// "stady;" ask dynare to calculate the steady state, 
// but it's unnecessary becasue we derive the exact solutions.

// #####  5 shocks  #####
shocks;
var e; stderr 1;
end;
check;  // check eigenvalues condiion

// #####  6 simulation  #####
stoch_simul(order = 1, hp_filter = 1600, periods =100000);
