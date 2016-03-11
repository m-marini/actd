X = csvread("../data/debug-wall.csv");
A = loadAgent("../data/agent");
X1 = wall2Signals(X);
Y = simulate(A.critic, X1);
