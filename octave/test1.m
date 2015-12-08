InRange = [-10 : 0.5 : 10];
H = 10;
W1 = (rand(H, 3) * 2 - 1) * 5;
W2 = (rand(1, H+1) * 2 - 1) * 3;

[XX, YY] = meshgrid(InRange, InRange);

[n, m] = size(XX)
I1 = XX(:);
I2 = YY(:);

Z1 = net([I1 I2], W1, W2);

ZZ = reshape(Z1, n, m);

h=surfc(XX, YY, ZZ);

waitfor(h);