function Y = simulate(W, X)
  [n m] = size(X);
  [l, p] = size(W);
  IN = [ones(n,1) X(:,:)];
  for layer = 1 : l - 1
    WL = W{layer};
    no = size(WL, 1);
    OUT = zeros(m, no);
    OUT = IN * WL';
    OUT = logistic_cdf(OUT);
    IN = [ones(n,1) OUT(:,:)];
  endfor
  WL = W{l};
  no = size(WL, 1);
  Y = zeros(m, no);
  Y = IN * WL';
endfunction