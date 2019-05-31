function [c, ceq] = feasibility(X, swaps)


    A = [0, 2; 1, 1];
    B = [1, 2];
    C = [0.5];

    c = X*A*X' + B*X' + C;

    ceq = [];

end