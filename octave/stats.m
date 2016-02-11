function [M S] = stats(X, k)
  [n m] = size(X);
  Y = reshape(X'(:), m, k, n / k);
  M = mean(Y, 2);
  M = reshape(M(:),m, n/k)';
  S = M;
endfunction