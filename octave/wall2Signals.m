function Y = wall2Signals(X)
  [n m] = size(X);
  
  Y = zeros(n, 13 * 11 * 4 * 11);

  I = zeros(n, 1);
  I = 1 + X(:, 2) + X(:, 1) * 13 + X(:, 3) * (13 * 11) + X(:, 4) * (13 * 11 * 4);

  Y(:, I) = 1;
  
endfunction