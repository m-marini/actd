function [G1 G2] = grad(X, EX, W1, W2)
	n = size(X, 1);
	H1 = [ones(1, n);  X']
	Z1 = W1 * H1;
	H2 = [ones(1, n) ;  1 ./ (1 + exp(-Z1))]
	H3 = [ones(1, n) ; W2 * H2]
	DH2 = H2 .* (1 - H2) 
	B1 = W2' .* DH2
	
	Delta2 = EX - H3(2 : end)
	Delta1 = (B1 * Delta2) ( 2:end)
	
	G2 = -H2' .* Delta2
	G1 = -H1' .* Delta1
	
endfunction