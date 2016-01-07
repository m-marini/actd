function Y = returns(X, gamma)
  n = size(X, 1);
  Y = sum(X .* (gamma .^ (0 : n - 1)'));
endfunction