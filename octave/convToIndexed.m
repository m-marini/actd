function Y = convToIndexed(X)
	k = 5; 
	me1 = k * k + 1;
	other = k * k * 2 + 1;
	[n, m] = size(X);
	N1 = zeros(n, 1);
	N2 = zeros(n, 1);
	for i = 1 : n
		N1(i) = find(X(i, 1 : me1- 1));
		N2(i) = find(X(i, me1 : other - 1 ));
	endfor
	Y = [N1 N2 X(:, other : end) ];
endfunction