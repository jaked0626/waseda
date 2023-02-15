%% comparative statics of two-period problem using newton method

% parameters 
bt = 0.98;
sig = 1.5;
w0 = 120;
w1 = 90;
% vector of possible R, for comparisons 
Rvec = linspace(0.8, 1.2, 100);
% initialize vector of solutions
c0vec = zeros(1, 100);
for i=1:100
    R = Rvec(i);
    % pack parameters
    parameters = [bt sig R w0 w1];
    
    %function handle 
    fh = @(x) f(x, parameters);
    % record solutions to c0
    c0vec(i) = fzero(fh, 10); 
end
plot(Rvec, c0vec);

% define actual function 
function y=f(c0, p)
    % p contains parameters, c0 is the initial consumption value 
    %unpack
    bt=p(1);sig=p(2);R=p(3);w0=p(4);w1=p(5);
    y = c0^sig - bt*R*(R*w0+w1-R*c0)^(-sig);
end

% raising error for function, look into it





    

