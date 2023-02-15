// RBC model 2. Replicate Miao's result
// So Kubota, Waseda, Econ research, 2021
close all;clc;  // house keeping


// #####  1 variables  #####
var Y,C,I,N,YN,w,z,K;
varexo e;  // The shock is epsilon

// #####  2 parameters  #####
parameters alpha, beta, delta, rho, sigma, chi;
alpha = 0.33;
beta = 0.99;
delta = 0.025;
rho = 0.99;
sigma = 0.0089;
Nss = log(0.33);  // each variable is log-transformed
Kss = log((alpha/(beta^(-1)-1+delta))^(1/(1-alpha))*exp(Nss));
Css = log((exp(Kss)^alpha)*(exp(Nss)^(1-alpha)) - delta*exp(Kss));
chi = ((1-alpha)*(exp(Kss)^alpha)*(1-exp(Nss)))/(exp(Nss)^alpha*exp(Css));

// #####  3 model  #####
model;
// main 4 equations
(1/exp(C))=beta*(alpha*exp(z(+1))*exp(K)^(alpha-1)*exp(N(+1))^(1-alpha)+1-delta)*(1/exp(C(+1)));
chi*exp(C)/(1-exp(N)) = (1-alpha)*exp(z)*exp(K(-1))^alpha*exp(N)^(-alpha);
exp(K)+exp(C)=exp(z)*exp(K(-1))^alpha*exp(N)^(1-alpha)+(1-delta)*exp(K(-1));
z = rho*z(-1)+sigma*e; // z is already log
// additional equation
exp(Y) = exp(z)*exp(K(-1))^alpha*exp(N)^(1-alpha);
exp(I) = exp(K) - (1-delta)*exp(K(-1));
exp(w) = exp(z)*(1-alpha)*exp(K(-1))^(alpha)*exp(N)^(-alpha);
exp(YN) = exp(Y)/exp(N);
end;

// #####  4 stady state  #####
initval; 
z=0;  // normalization
N=Nss;
K=Kss;
C=Css;
Y=alpha*Kss + (1-alpha)*Nss;  // log Cobb-Douglas
I = log(delta) + Kss;
w = log(1-alpha) + alpha*Kss - alpha*Nss;
YN = Y - N;
end;
steady;

// #####  5 shocks  #####
shocks;
var e; stderr 1;
end;
check;  // check eigenvalues condiion

// #####  6 simulation  #####
stoch_simul(order = 1, hp_filter = 1600, periods =100000);

%% ##### 7 results  #####
% Matlab program from here. 
% variances are stored in "oo_.var" 
% It is a general variance/covariance matrix
% diagonal elements are variances
Standard_Deviation = 100*sqrt(diag(oo_.var(1:6,1:6)));
SD_Relative_Y = Standard_Deviation./Standard_Deviation(1); % relative to GDP
% autocorrelations. {1} is for 1st order. diag for self.
Autocorrelation = 100*diag(oo_.autocorr{1}(1:6,1:6)');
Correlation_Y = 100*sqrt(oo_.var(1,1:6)'); % correlation with GDP

% summarized as a table
Names = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
T = table(Standard_Deviation,SD_Relative_Y,Autocorrelation,Correlation_Y,'RowNames',{'Y';'C';'I';'N';'Y/N';'w'});
disp('Stochastic simulation moments');
disp(T);
