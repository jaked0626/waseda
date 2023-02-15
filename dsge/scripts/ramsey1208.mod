// Ramsey (Neoclassical Grwoth Model), So Kubota
// Waseda, econ research, DSGE
// In DYNARE, we use "//" to comment out instead of "%"
// simulation. beta changes from 0.95 to 0.8

// To let MATLAB know dynare, automatic setting at the start.
// Environment Menu --> Set Path --> add following folder
// or you can run the following command ate 
// Mac:  addpath /Applications/Dynare/4.6.4/matlab
// Win:  addpath \dynare\4.6.4\matlab

// Dynare codes are devided into four blocks.
// 1 variables
// 2 parameters
// 3 model
// 4 stady state
// 5 simulation
// 6 plot

// 0 preparation //////////////////////////
clc;
close all;
// You should not write "clear" It eliminates necessary files too.

// 1 variables //////////////////////////
var c k;  // endogeneous variables
varexo bet;  // exogeneous variables (shocks), discount factor beta

// 2 parameters ////////////////////////
parameters A sig del alp;  // definitions
A=1; // Productivity A
sig=2; // CRRA parameter, sigma 
del=.2; // depreciation rate, delta
alp=.33; // capital’s share, alpha

// 3 model /////////////////////////
// k_t is just k, k_{t-1} is represented as k(-1).
// Dynare defines k_t as the amount of capital at the end of period t.
// From the original notation, push back all k by one period. c_t is OK.
model;
k=A*k(-1)^alp+(1-del)*k(-1)-c;  // time t+1 variables are indicated as (+1)
c^(-sig) = bet*(c(+1)^(-sig))*(A*alp*k^(alp-1)+1-del);
end;

// 4 steady state //////////////////////
initval;
bet=.95;
k=0.5;
c=0.5;
end;
steady;
// put this in if you want to start from the initial steady state,
// comment it out to start from the indicated values

endval;
bet=.8;
k=1;
c=1;
end;
steady;


// 5 simulation ///////////////////////
perfect_foresight_setup(periods = 50);
perfect_foresight_solver;

%% 6 plot %%%%%%%%%%%%%%%%%%%%%
%% Matlab program from here %%%%%%%%%%%%%%%%

% simple plot, just to check the result.
figure;
plot(k);
figure;
plot(c);
figure;
plot(k,c);


% A little fancy plot
bet_old = 0.8;  % old steady state
k_old=((1/bet_old-1+del)/(A*alp))^(1/(alp-1)); % old steady state k
bet_new = 0.95;  % new steady state
k_new=((1/bet_new-1+del)/(A*alp))^(1/(alp-1)); % new steady state k
x_axis = linspace(0.01,3,100); % It is for x-axis of Delta c=0 line
delta_c0 = A*x_axis.^alp-del*x_axis; % Delta c=0 line. Calcularate for each x_axis grid
%
figure
xline(k_old,'Color','#0072BD','LineWidth',3);  % vertical line, old Delta k=0
hold on  % Draw a new line on the same figure with keeping the old one. 
xline(k_new,'--','Color','#7E2F8E','LineWidth',3); % vertical line, new Delta k=0, '--' means a dashed line
hold on  % Draw a new line on the same figure with keeping the old one. 
plot(x_axis,delta_c0,'Color','#0072BD','LineWidth',3);
hold on
plot(k,c,'Color','#D95319','LineWidth',3);
% add explanation for each line
legend({'Delta k=0, old','Delta k=0, new','Delta c=0','Transition path'},'Location','southeast');
xlabel('k, capital'); % add x axis name
ylabel('c, consumption'); % add y axis name
