function E = poltCritic(file="../data/critic.csv")
	Y = csvread(file);
	semilogy(Y);
	grid on;
	m = size(Y, 2);
	lg = {};
	for i = 1 : m
		lg(i) = ["" (i+64)];
	endfor
	legend(lg);
endfunction
