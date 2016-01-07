clear all;
X = csvread("../data/wall.csv");

# Indexes
row0 = 1;
col0 = 2;
speedy0 = 3;
speedx0 = 4;
pad0 = 5;
action = 6;
reward = 7;
row1 = 8;
col1 = 9;
speedy1 = 10;
speedx1 = 11;
pad1 = 12;

gamma = 0.962;

Ends = find(X(:, reward) < 0);

R = returnsBySteps(X(:, reward), Ends, gamma);

printf("Average:%f\n", mean(R));
subplot(2, 1, 2);
hist(R, 20, 1.0);
grid on;
title("Returns");

subplot(2, 1, 1);
plot(lp(R, 50/size(R, 1)));
grid on;
title("Returns");
