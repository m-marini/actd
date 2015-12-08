function Y=nlr(X,W1,W2) 
	n = size(X, 1);
	H0 = [ones(1, n);  X'];
	Z1 = W1 * H0;
	H1 = 1 ./ (1 + exp(-Z1));
	Y = W2 * [ones(1, n) ; H1];
endfunction