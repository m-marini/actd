clear all;
movingAvgLength=1000;
Names = {
  "../data/returns-l1.csv"
  "../data/returns-l2.csv"
  "../data/returns-l12.csv"};
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
plot(movingAvg(X(:,[2 4 6]),movingAvgLength));
legend("l1","l2","l1+l2");
title("Average lenght of episode by l1,l2");
xlabel("Episode count");
ylabel("Episode length");
grid on;
grid minor on;
box on;