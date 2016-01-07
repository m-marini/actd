clear all;

# Indexes
gamma = 0.962;

R = readReturns("../data/benchmark-wall.csv", gamma);

subplot(2, 2, 1);
plot(lp(R, 50 / size(R, 1)));
grid on;
title("Returns");

subplot(2, 2, 2);
hist(R, 20, 1.0);
grid on;
title("Returns");

R = readReturns("../data/wall.csv", gamma);

subplot(2, 2, 3);
plot(lp(R, 50 / size(R, 1)));
grid on;
title("Returns");

subplot(2, 2, 4);
hist(R, 20, 1.0);
grid on;
title("Returns");
 
 clear R;
 
 waitfor(gcf);