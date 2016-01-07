clear all;
X = csvread("../data/debug-wall.csv");

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

  Y = X(:, [action]);
  subplot(2,2,1);
  hist(Y, 10, 1);
  title("Action distribution");
  grid on;

  Y = [X(:, [sv00 sv01 sv10 reward]) X(:, sv01) * gamma + X(:, reward)];
  subplot(2,2,2);
  h = plot(Y);
  title("State values");
  grid on;
  legend("S0", "S1", "S0'", "reward", "expected");

  Y = X(:, [left0 rest0 right0]);
  subplot(2,2,3);
  h = semilogy(softmax(Y));
  title("Action probabilities");
  grid on;
  legend("Left", "Rest", "Right");
