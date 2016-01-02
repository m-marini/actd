function Y = softmax(X)
  [n,m] = size(X);
  Y = zeros(n, m);
  Y = exp(X);
  Y = Y ./ sum(Y,2);
endfunction