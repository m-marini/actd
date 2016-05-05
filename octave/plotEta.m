clear all;
movingAvgLength=1000;
Names = {
  "../docs/returns-eta10.csv"
  "../docs/returns-eta30.csv"
  "../docs/returns-baseline.csv"
  "../docs/returns-eta300.csv"};
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
legend("0,01","0.03", "0.10", "0.30");
title("Average lenght of episode by eta");
xlabel("Episode count");
ylabel("Episode length");
grid on;
grid minor on;
box on;