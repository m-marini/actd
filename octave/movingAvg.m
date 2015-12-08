function MA = movingAvg(X, k)
	[n ] = size(X, 1); 
	T = zeros(n - k + 1 , k);
	for i = 1 : k
		T( : , i) = X(i : end - k + i, 1);
	endfor
	MA = mean(T, 2);
endfunction