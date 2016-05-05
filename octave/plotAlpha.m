clear all;
movingAvgLength=1000;
Names = {
  "../docs/returns-alpha0.csv"
  "../docs/returns-baseline.csv"
  "../docs/returns-alpha-2.csv"
  "../docs/returns-alpha1.csv"};
X = zeros(1,0)
for i = 1 : size(Names,1)
  Y = csvread(Names{i});
  if i == 1
    X = Y;
    n = size(X, 1);
  else    
    n = min(n, size(Y, 1));
    X = [X(1:n, :) Y(1:n, :)];
  endif
endfor
plot(movingAvg(X(:,[2 4 6 8]),movingAvgLength));
legend("0","1e-6", "10e-3", "1");
title("Average lenght of episode by alpha");
xlabel("Episode count");
ylabel("Episode length");
grid on;
grid minor on;
box on;