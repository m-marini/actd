function Y = readReturns(filename, gamma)
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

  X = csvread(filename)(:, reward);
  Ends = find(X < 0);
  Y = returnsBySteps(X, Ends, gamma);
endfunction