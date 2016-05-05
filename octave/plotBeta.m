clear all;
movingAvgLength=1000;
Names = {
  "../docs/returns-beta003.csv"
  "../docs/returns-beta01.csv"
  "../docs/returns-baseline.csv"
  "../docs/returns-beta1.csv"
  "../docs/returns-beta3.csv"};
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
plot(movingAvg(X(:,[2 4 6 8 10]),movingAvgLength));
legend("0.03","0.1", "0.3", "1","3");
title("Average lenght of episode by beta");
xlabel("Episode count");
ylabel("Episode length");
grid on;
grid minor on;
box on;