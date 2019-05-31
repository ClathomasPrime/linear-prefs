function [ prefs, swaps] = singleCrossing(n)
    oldpref = 1:n ;
    prefs = [oldpref] ;
    swaps = zeros(0,2) ;
    
    function [indices, pair] = randomNewSwap()
        adjPairs = [ oldpref(1:n-1).' , oldpref(2:n).' ] ;
        filt = @(p)( p(1) < p(2) );
        % ^ check if i,j still in lex order. Can swap if so.
        availablePairs = adjPairs( adjPairs(:,1) < adjPairs(:,2), : );
        if (isempty(availablePairs))
            indices = [];
            pair = [];
        else
            i = randi(size(availablePairs,1));
            pair = availablePairs( i, :);
            indices = [find(oldpref==pair(1)), find(oldpref==pair(2))];
        end
    end

    while ( 1 )
        [ind, pair] = randomNewSwap(); 
        if (isempty(ind))
            break;
        end
        
        swaps = [pair ; swaps] ;
        newpref = oldpref ;
        newpref( ind(1) ) = oldpref( ind(2) );
        newpref( ind(2) ) = oldpref( ind(1) );
        disp(newpref);
        prefs = [newpref ; prefs];
        oldpref = newpref ;
    end
end