clear all;
movingAvgLength=1000;
Names = {
  "../docs/returns-baseline.csv"
  "../docs/returns-baseline1.csv"};
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
plot(movingAvg(X(:,[2 4]),movingAvgLength));
legend("baseline","mix");
title("Average lenght of episode");
xlabel("Episode count");
ylabel("Episode length");
grid on;
grid minor on;
box on;