function h = traceStatus(file)
 	Y = csvread(file);
  h = plot(Y(:, 4));
	grid on;

endfunction