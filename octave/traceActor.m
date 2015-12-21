function h = traceStatus(file)
  Y = csvread(file)(:,5:8);
  Y = exp(Y);
  Y = Y ./ sum(Y, 2);
  h = plot(Y);
 	legend("Up","Down","Left","Right");
endfunction