function Y = returnsBySteps(X, Ends, gamma)
  begin = 1;
  for i = 1 : size(Ends, 1)
    ex = Ends(i);
    Y(i) = returns(X(begin:ex, :), gamma);
    begin = ex;
  endfor
  Y = Y';
endfunction