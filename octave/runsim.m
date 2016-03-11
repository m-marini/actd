clear all;
X = csvread("../data/debug-wall.csv");
A = loadAgent("../data/agent");
#A = loadAgent("../aaaa");
X1 = wall2Signals(X);
Y = simulate(A.critic, X1);
[X(:,[1:4 6]) Y]
