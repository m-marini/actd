function MA = lavg(X, k)
	[ n ] = size(X, 1); 
	MA= zeros(n, 1);
	for i = 1 : n
		MA( end - i +1 : end, :) = MA( end - i +1 : end, :) * k + X(1 : i, :) * (1 - k);
	endfor
endfunction