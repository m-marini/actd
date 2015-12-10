function E = plotActor(filename="../data/actor.csv")
	plot(csvread(filename));
	grid on;
	legend("S0,A0","S0,A1","S1,A0","S1,A1", "S2,A0", "S2,A1");
endfunction
