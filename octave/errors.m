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
sv00 = 13;
sv01 = 14;
left0 = 15;
rest0 = 16;
right0 = 17;
sv10 = 18;
left1 = 19;
rest1 = 20;
right1 = 21;

gamma = 0.962;

# Error = Exp - S00
# Exp = S01 * gamma + Rewards
# Error = S01 * gamma + Rewards - S00

  E = X(:, sv01) * gamma + X(:, reward) - X(:, sv00);
  E2 = E .^ 2;
  
  printf("Mean error %f\n", mean(E, 1));
  printf("RMS %f\n", sqrt(mean(E2, 1)));
  
  subplot(2,1,1);
  semilogy([E2]);
  title("Square error");
  grid on;

  LP = lp(E2, 100/size(E2,1), mean(E2, 1));
  subplot(2,1,2);
  semilogy([LP]);
  title("Mean");
  legend("Mean");
  grid on;
  