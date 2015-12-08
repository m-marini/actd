X = [
0 0;
0 1;
1 0;
1 1
]
H = 2;
epsilon = 1;

W1 = (rand(H, 3) * 2 - 1) * epsilon
W2 = (rand(1, H+1) * 2 - 1) * epsilon

Z1 = net(X, W1, W2)
