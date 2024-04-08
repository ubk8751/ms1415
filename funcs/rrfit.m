%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PROGRAM: This function is usefull to fit rayleigh regression 
%
% AUTHORS: Bruna Gregory Palm, Fabio Mariano Bayer and Renato J. Cintra
%
% DATE: 04/2018
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [coef, y, x, eta_hat, mu_hat, resid1, resid2, pvalues, vcov, stderror, aic, bic, R2] = rrfit(x,y)

%x = x';
y = y';
n_y = size(y);
y1 = ones(n_y(1,1),1);
x1 = [y1 x];
n_x1 = size(x1);

r = n_x1(1,2);
n = n_y(1,1);
  
ystar = log(y);

reg = regress(ystar,x1);
ini = reg'; % chute inicial

loglik = makeloglik(x1, y);
options = optimoptions('fminunc', 'SpecifyObjectiveGradient', true, 'Display', 'none');
[opt2,fval] = fminunc(loglik, ini, options);

coef = opt2;
eta_hat = x1 * coef';
mu_hat = exp(eta_hat);
W = diag(((4)/(mu_hat.^2))*(mu_hat.^2));
K = x1' * W * x1;
vcov = inv(chol(K)) * (inv(chol(K)))';

var_y = (mu_hat.^2)*(4/pi -1)';

resid1 = (y-mu_hat)./sqrt(var_y);

cumulativef = pr(y,mu_hat);

resid2 = norminv(cumulativef);

stderror = sqrt(diag(vcov));

zstat = abs(coef./stderror');

pvalues = 2*(1 - normcdf(zstat));

aic = 2.*fval+2.*(r);

bic = 2.*fval+log(n).*(r);

ini2 = mean(y);

loglik1 = logliknull(y);
options = optimoptions('fminunc', 'SpecifyObjectiveGradient', false, 'Display', 'none');
[x,fval1] = fminunc(loglik1, ini2, options);


R2 = 1 - exp(-((2/n)) .* (-fval + fval1));



end

   
