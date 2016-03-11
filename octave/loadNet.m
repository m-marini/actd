function Y = loadNet(file)
  N = csvread([file "-n.csv"]);
  Y = cell(1, N);
  for i = 1 : N
    file = sprintf("%s-%d.csv", file, i - 1);
    Y{i} = csvread(file);
  endfor
endfunction