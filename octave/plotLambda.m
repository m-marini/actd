clear all;
movingAvgLength=1000;
Names = {
  "../docs/returns-baseline.csv"
  "../docs/returns-lambda30.csv"
  "../docs/returns-lambda90.csv"
  "../docs/returns-lambda99.csv"};
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
legend("0","0.3", "0.9", "0.99");
title("Average lenght of episode by lambda");
xlabel("Episode count");
ylabel("Episode length");
grid on;
grid minor on;
box on;