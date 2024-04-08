function ll = makeloglik(x1, y)
ll = @loglik;

    function [yy, J] = loglik(beta)
        
        mu = exp(x1*beta');
        
        yy = -sum(sum(log(pi/2)+log(y)-log(mu.^2)-(pi.*y.^2)./(4.*(mu.^2))));

        J = -(x1' * diag(mu) * ((pi.*(y.^2))./(2.*(mu.^3))-(2)./(mu)))';
    end

end