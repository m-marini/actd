clear all;

# Indexes
gamma = 0.962;
f = 1200.0

R = readReturns("../data/benchmark-wall.csv", gamma);

subplot(2, 2, 1);
plot(lp(R, f / size(R, 1)));
#plot(R);
grid on;
title("Returns");

subplot(2, 2, 2);
hist(R, 100, 1.0);
grid on;
title("Returns");

R = readReturns("../data/wall.csv", gamma);

subplot(2, 2, 3);
plot(lp(R, f / size(R, 1)));
grid on;
title("Returns");

subplot(2, 2, 4);
hist(R, 100, 1.0);
grid on;
title("Returns");
 
 clear R;
 
 waitfor(gcf);