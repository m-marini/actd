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

  Y = X(find(X(:, reward) != 0), reward);
  n = size(Y, 1);
  h = plot(lp(Y, 5 / n));
  title("Filtered returns");
  grid on;
  legend(["Returns"]);
  waitfor(h);

  # Filters ball out of field
  M = all(X(:, [row0]) == [10], 2);
  Y = X(find(M), [sv00, sv10]);
  mean(Y)
  plot(Y);
  title("State value for ball out of field");
  grid on;
  legend("State value", "State value updated");
  waitfor(gcf);
  hist(Y, 20);
  grid on;
  title("State value for ball out of field");
  legend("State value", "State value updated");
  waitfor(gcf);

  # Filters ball before out of field
  M = all(X(:, [row1]) == [10], 2);
  Y = X(find(M), [sv00, sv10]);
  mean(Y)
  plot(Y);
  title("State value before ball out of field");
  grid on;
  legend("State value", "State value updated");
  waitfor(gcf);
  hist(Y, 20);
  grid on;
  title("State value before ball out of field");
  legend("State value", "State value updated");
  waitfor(gcf);

  # Filters ball before bounces
  M = all(X(:, [reward]) == 1, 2);
  Y = X(find(M), [sv00, sv10]);
  mean(Y)
  plot(Y);
  title("State value ball before bounces");
  grid on;
  legend("State value", "State value updated");
  waitfor(gcf);
  hist(Y, 20);
  grid on;
  title("State value ball before bounces");
  legend("State value", "State value updated");
  waitfor(gcf);

  # Filters ball on left of pad
#  M = all(X(:, [row0 speedy0 speedx0]) == [9 1 1], 2) & (X(:, pad0) - X(:, col0)) == 0;
  M = all(X(:, [row0, col0]) == [10 0], 2);
  Y = X(find(M), [reward, sv00, sv01, sv10]);
  h = plot(Y);
#  title("State value for ball on left of pad");
  title("State value for ball out of field");
  grid on;
  legend("Reward", "State value", "State value'", "State value updated");
  waitfor(h);

  # Filters ball on left of pad
  L = all(X(:, [row0 speedy0 speedx0]) == [8 1 1], 2) & (X(:, pad0) - X(:, col0)) == 2;
  R = all(X(:, [row0 speedy0 speedx0]) == [8 1 -1], 2) & (X(:, pad0) - X(:, col0)) == 0;
  Y = X(find(R | L), [left0 rest0 right0]);
  h = semilogy(softmax(Y));
  title("Action prefs for on left of pad");
  grid on;
  legend("Left", "Rest", "Right");
  waitfor(h);

  # Filters ball on right of pad
  L = all(X(:, [row0 speedy0 speedx0]) == [8 1 1], 2) & (X(:, pad0) - X(:, col0)) == -2;
  R = all(X(:, [row0 speedy0 speedx0]) == [8 1 -1], 2) & (X(:, pad0) - X(:, col0)) == -4;
  Y = X(find(R | L), [left0 rest0 right0]);
  h = semilogy(softmax(Y));
  title("Action prefs for on right of pad");
  grid on;
  legend("Left", "Rest", "Right");
  waitfor(h);
