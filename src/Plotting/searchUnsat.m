clc; close all; clear variables;

n = 4;

[prefs, swaps] = feasibility(n);

sqp_fun = @(X)(0); % feasibility program => trivial objective
x0 = zeros(1,2*n);
options = optimset('Algorithm','sqp');
[x,fval,exitflag,output,lambda,grad,hessian] ...
    = fmincon(sqp_fun, x0, [], [], [], [], [], [], ...
    @(x)(feasibility(x, swaps)), options );

disp(x);

% plot(x0(1), x0(2), 'rp');
% plot(x(1), x(2), 'bp');
