function E = poltCritic
	semilogy(csvread("critic.csv"));
	grid on;
	legend("1","2","3","4");
endfunction
