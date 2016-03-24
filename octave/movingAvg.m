function MA = movingAvg(X, k)
	[ n m ] = size(X);
	MA = zeros(n, m);
	for i = 1 : n
    from = max(1, i-k+1);
    T = X(from:i, :);
  	MA(i,:) = mean(T, 1);
	endfor
endfunction