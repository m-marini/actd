X=csvread("../docs/returns-all.csv");
plot(movingAvg(X(:,[1 3 5 7]),size(X,1)));
legend("on line","0 ms", "200 ms", "600 ms");
title("Average lenght of episode");
xlabel("Episode count");
ylabel("Episode length");
grid on;
grid minor on;
box on;