  function p = pr(x, mu) % cumulative function
  
  %mu = 1;
  p = 1- exp((-pi.*(x.^2))./(4.*mu.^2));
 
  end