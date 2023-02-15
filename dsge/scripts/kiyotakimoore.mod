// CREDIT CYCLES
// N.KIYOTAKI AND J.MOORE (1997)
// THE FULL MODEL : INVESTMENT AND CYCLES (SECTION III)
// THIS MOD FILE REPLICATE THE FIGURE 3
// CODED IN OCTOBER 2009 BY STEPHANE LHUISSIER (INTERN AT CEPREMAP)
// https://forum.dynare.org/t/kiyotaki-moore/1149

// slightly modified by So Kubota for DSGE class in Waseda, fall 2021.


// 0 preparation ////////////////////////////////////////////////////
// addpath /Applications/Dynare/4.6.4/matlab
// addpath C:\dynare\4.6.4\matlab
close all;clc;  // house keeping

// 1 variables ////////////////////////////////////////////////////
var q,K,B; // MIT shock to productivity
varexo del;  // 1% productivity shock

// 2 parameters ////////////////////////////////////////////////////
parameters R lambda pi phi a v qss Bss Kss delss;

R      = 1.01;
lambda = 0.975;
v      = 4.91606;
a      = 1;
pi     = 0.1;
phi    = 20;

// define steady state here for later use
qss    = (R/(R-1))*(pi*a - (1-lambda)*(1-R + pi*R)*phi )/(lambda*pi + (1-lambda)*(1-R + pi*R));
Kss    = qss*(1-1/R) + v;
Bss    = 1/(R-1)*(a-phi+lambda*phi)*Kss;
delss  = 0;  // This is for 1% productivity shock. 

// 3 model ////////////////////////////////////////////////////
model;
K = (1-pi)*lambda*K(-1) + pi/(phi+q-1/R*q(+1))*((a*(1+del)+q+lambda*phi)*K(-1)-R*B(-1));
B = R*B(-1) + q*(K-K(-1)) +phi*(K-lambda*K(-1)) - a*(1+del)*K(-1);
q - (1/R)*q(+1) = K - v;
end;

// 4 steady state ////////////////////////////////////////////////////
initval;
q   = qss;
K   = Kss;
B   = Bss;
del = delss;  // prouctivity shock is zero
end;
steady;
check;  // show the matrix of log-linearized system

// 5 shocks ////////////////////////////////////////////////////
// MIT shock. Measure zero shock in the deterministic model
// Deterministic simulation with shock in only the initial period. 
shocks;
var del;
periods 1;
values 0.01;
end;

// 6 simulation ////////////////////////////////////////////////////
simul(periods=400);  // long enough for convergence

// 7 plot ////////////////////////////////////////////////////
// Show only first 40 periods (10 years)
figure  % make a new figure window
plot(K(1:40)/Kss,'linewidth',2)
hold on  % do not erase the old plot
plot(B(1:40)/Bss,'.-','linewidth',2)
hold on
plot(q(1:40)/qss,'--','linewidth',2)
legend('K/K*','B/B*','q/q*')
