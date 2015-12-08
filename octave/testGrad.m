clear all;
W1 = [-1 2 3; 1 -2 3]
W2 = [1 2 -2]
H1 = [-1 2];
O = 2;

W1 = rand(2, 3)
W2 = rand(1, 3)
H1 = rand(1, 2)
O = rand(1, 1);

printf("-----\n");
 [G0 G1] = grad(H1, O, W1, W2)
 
printf("-----\n");
DW=1e-6;
Y = nlr(H1, W1, W2);
JJ = cost(O - Y);

W111 = W1 + [ DW 0 0; 0 0 0];
DJW111 = (cost( O - nlr(H1, W111, W2)) - JJ) / DW;

W112 = W1 + [ 0 DW 0; 0 0 0];
DJW112 = (cost( O - nlr(H1, W112, W2)) - JJ) / DW;

W113 = W1 + [ 0 0 DW; 0 0 0];
DJW113 = (cost( O - nlr(H1, W113, W2)) - JJ) / DW;

W121 = W1 + [ 0 0 0; DW 0 0];
DJW121 = (cost( O - nlr(H1, W121, W2)) - JJ) / DW;

W122 = W1 + [ 0 0 0; 0 DW 0];
DJW122 = (cost( O - nlr(H1, W122, W2)) - JJ) / DW;

W123 = W1 + [ 0 0 0; 0 0  DW];
DJW123 = (cost( O - nlr(H1, W123, W2)) - JJ) / DW;

DJW1 =[DJW111 DJW112 DJW113 ; DJW121 DJW122 DJW123 ]

W211 = W2 + [DW 0 0];
DJW211 = (cost( O - nlr(H1, W1, W211)) - JJ) / DW;

W212 = W2 + [0 DW 0];
DJW212 = (cost( O - nlr(H1, W1, W212)) - JJ) / DW;

W213 = W2 + [0 0 DW];
DJW213 = (cost( O - nlr(H1, W1, W213)) - JJ) / DW;

DJW2 =[DJW211 DJW212 DJW213 ]
