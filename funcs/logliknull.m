function ll = logliknull(y)
ll = @loglik;

    function [yy J] = loglik(beta)
       
        mu = beta;
        
        yy = -sum(sum(log(pi/2)+log(y)-log(mu.^2)-(pi.*y.^2)./(4.*(mu.^2))));
        
        J = -sum(sum((pi .* y.^2 - 4 .* mu.^2) ./ (2 .* mu.^3)));
       

    end

end