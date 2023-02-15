%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lecture Name:
%   Topics in Economics (Dynamic Stochastic General Equilibrium (DSGE) Approach to Macroeconomics) [E] 01
% Lecturer: So Kubota
% TA Seesion
% Explanation and examples of Matlab function handel.
% The propose of this file is to show how function handel function.
% TA: Fei GAO
% This file is created at 11/24/2021 by F.GAO
% Last modfied:           11/24/2021
% Any comments is welcome. Contact: gaogoofyfei@suou.waseda.jp
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Start of this file.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section I
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic Model I
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Two period optimization problem
% \max_{c_0,c_1} u(c_0) + \beta u(c_1)      (1)
%  s.t.    a_1 = w_0 - c_0                  (2)
%          c_1 = a_1 R_1 + w_1              (3)
%
% Solve by substitution
% \max_{a_1} u(w_0 - a_1) + \beta u(a_1 R_1 + w_1)                      (4)
% FOC: u'(w_0 - a_1)[-1] + \beta u'(a_1 R_1 + w_1)[R_1] = 0             (5)
% Find a_1 is good enough.
% Since w_0, w_1, R_1 is given. a_1 gives c_0 and c_1 by (2) and (3)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Housekeeping
clearvars; clc; close all
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% So's code
% Diectly solve (5) by using solver: fzero.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sig=0.5;bt=0.95;R=1.02;w0=120;w1=90;
parameters = [sig bt R w0 w1];
f_h = @(x) f(x,parameters);
cstarSo = fzero(f_h,100);
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Variation I
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Use internal optimization problem solver to solve directly from (4).
% Solvers: (1) fminbnd        Single variable
%          (2) fzero          Single variable
%          (3) fminsearch     Multi variables
%          (4) fmincon        Multi variables
% Use "help ~" to look up for information.
% 
% We assume the utility function is CRRA form.
% U(c) = c^{1-\sigma} / (1-\sigma)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Load up utility function
sig=0.5;
UtilityFunction = @(c) c^(1-sig) / (1-sig);
% Load up c_0 and c_1 as two functions
% c_0
w0=120;
c0 = @(a1) w0-a1;
% c_1
w1=90; R=1.02;
c1 = @(a1) R * a1 + w1;
% Load up maximization equation
bt=0.95;
MaximizeMe = @(a1) UtilityFunction(c0(a1)) + bt * UtilityFunction(c1(a1));
MaxIsMin = @(a1) - MaximizeMe(a1);
% Solve by Matlab
[a1star,fval] = fminbnd(MaxIsMin,0,w0);
% Get c1 and c0
c1star = c1(a1star);
c0star = c0(a1star);
disp(['Solution (a1, c0, c1) is ',num2str([a1star,c0star,c1star])])
% Verify solution with So's c0
diff =c0star - cstarSo;
disp(diff)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Variation II
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Play utility functions. 
% Change shape of utility functions through function handles.
% 
% (1) U(c) = c^{1-\sigma} / (1-\sigma)
% (2) U(c) = c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set up utility function
UtilityFunction = @(c) c^(1-sig) / (1-sig);
UtilityFunction2 = @(c) c;
% Solve and print
[a1star,c0star,c1star] = f2(UtilityFunction,parameters);
disp('CRRA')
disp(['Solution (a1, c0, c1) is ',num2str([a1star,c0star,c1star])])

[a1star2,c0star2,c1star2] = f2(UtilityFunction2,parameters);
disp('Linear')
disp(['Solution (a1, c0, c1) is ',num2str([a1star2,c0star2,c1star2])])
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Variations III
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Play with parameters 
% Change shape of utility function in degree of relative risk aversion.
% 
% U(c) = c^{1-\sigma} / (1-\sigma)
% when sigma \in (0,10]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set up a loop
sig_interval = linspace(1e-4,10,100);
Soultion = zeros(length(sig_interval),3);
for i = 1 : length(sig_interval)
    [a1star,c0star,c1star] = f3(sig_interval(i));
    Soultion(i,:) = [a1star,c0star,c1star];
end
% Plot 
figure
subplot(1,2,1)
plot(sig_interval,Soultion(:,2:3))
subplot(1,2,2)
plot(sig_interval,Soultion(:,1))

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Variations IV (as an Exercise)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extend the basic model to a 3 periods model.
% Keep R as constant.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section II
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic Model II 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Cournot competition
% Two firms, symmetric, solve following profix maximization problem
%  \max_{q_i} \pi_i = P q_i - C(q_i)                                    (6)
%   s.t. P = g(q_i, q_{-i})                                             (7)
%
% Solve by substitution (7)
%  \max_{q_i} \pi_i = g(q_i, q_{-i})  q_i - C(q_i)
%  FOCs:
%  g'(q_i,q_{-i})_{q_i} q_i + g(q_i, q_{-i}) - C'(q_i) =0              (10)
%  g'(q_i,q_{-i})_{q_{-i}} q_{-i} + g(q_i, q_{-i}) - C'(q_{-i}) =0     (10)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Housekeeping
clearvars; clc; close all
% Demand Function:
% P = Q^{-epsilon}  where Q = \sum_i q_i
% foc: (q_i+q_{-i})^{-\epsilon-1} * - \epsilon  q_i + (q_i+q_{-i})^{-\epsilon} + mc = 0; 
eps = 0.51;
mc = 1;
% foc1 = @(q1,q2) (q1 + q2)^(-eps) - eps * q1 * (q1 + q2)^(-eps-1) + mc;
% foc2 = @(q1,q2) (q1 + q2)^(-eps) - eps * q2 * (q1 + q2)^(-eps-1) + mc;
% x = [q1 q2]
foc1 = @(x) (x(1) + x(2))^(-eps) - eps * x(1) * (x(1) + x(2))^(-eps-1) - mc;
foc2 = @(x) (x(1) + x(2))^(-eps) - eps * x(2) * (x(1) + x(2))^(-eps-1) - mc;
foc  = @(x) (foc1(x))^2 + (foc2(x))^2;
% Solve & display
[x,fval] = fminsearch(foc,[3,1]);
q1 = x(1);
q2 = x(2);
disp(num2str([q1,q2]))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Variation I
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Solve in a N firm enviorment.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check 
N = 2;
x = N_firm_Cournot(N,eps,mc);
disp(x-[q1;q2])
% Cournot Limitation Theorem
N_max =250; % Try a small number at your first attempt!
for i = 2:N_max+1
    CLTSoultion = N_firm_Cournot(i,eps,mc);
    CLTSoultion_pool{i} = CLTSoultion;
    x_track(i-1) = CLTSoultion(1);
    Q_track(i-1) = CLTSoultion(1) * i;
end
% Perfect Competitive market eqm
% From P = mc, P = Q^{-\epsilon}, we obtain Q = mc^{-1/\epsilon}
QPCM = mc^(-1/eps); 
% Plot 
figure
plot(1:N_max,Q_track,1:N_max,repmat(QPCM,N_max,1))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Variation II Exercise
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Try heterogenous mc.
% mc_i
% Hint: give a vector of mc as input.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This file is ended.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% End of this file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function y = f(c0,p)
% p contains parameters. unpacking
sig=p(1);bt=p(2);R=p(3);w0=p(4);w1=p(5);
y = c0^(-sig) - bt*R*(R*w0+w1-R*c0)^(-sig);
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [a1star,c0star,c1star] = f2(UtilityFunction,p)
bt=p(2);R=p(3);w0=p(4);w1=p(5);
% Load up c_0 and c_1 as two functions
% c_0
c0 = @(a1) w0-a1;
% c_1
c1 = @(a1) R * a1 + w1;
% Load up maximization equation
MaximizeMe = @(a1) UtilityFunction(c0(a1)) + bt * UtilityFunction(c1(a1));
MaxIsMin = @(a1) - MaximizeMe(a1);
[a1star,~] = fminbnd(MaxIsMin,0,w0);
c1star = c1(a1star);
c0star = c0(a1star);
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [a1star,c0star,c1star] = f3(sig)

bt=0.95;R=1.02;w0=120;w1=90;
% Load up c_0 and c_1 as two functions
% c_0
c0 = @(a1) w0-a1;
% c_1
c1 = @(a1) R * a1 + w1;
% Load up maximization equation
MaximizeMe = @(a1) UtilityFunction(c0(a1),sig) + bt * UtilityFunction(c1(a1),sig);
MaxIsMin = @(a1) - MaximizeMe(a1);
[a1star,~] = fminbnd(MaxIsMin,0,w0);
c1star = c1(a1star);
c0star = c0(a1star);

    function Utility = UtilityFunction(c,sig) 
        Utility = c^(1-sig) / (1-sig);
    end

end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function x = N_firm_Cournot(N,eps,mc) 
solveme = @(x)foc(x,N,eps,mc);
initial_guess = ones(N,1)/N;
options = optimset('Display','off');
[x,fval] = fminunc(solveme,initial_guess,options);
    function diff_foc_i = foc_i(x,i,eps,mc)         
        diff_foc_i = (sum(x))^(-eps) - eps * x(i) * (sum(x))^(-eps-1) - mc;
    end

    function diff_foc = foc(x,N,eps,mc)
        diff_foc = 0;
        for i = 1:N
        diff_foc = diff_foc + (foc_i(x,i,eps,mc))^2;
        end
    end
end

