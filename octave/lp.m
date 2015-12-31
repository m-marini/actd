function Y = lp(X, k)
  [n, m] = size(X);
  Y = zeros(n, m);
  for i = 2 : n
    Y(i,:) = Y(i - 1,:) * (1 - k) + k * X(i - 1);
  endfor
endfunction