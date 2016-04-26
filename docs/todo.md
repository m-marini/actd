
Tracciare i ritorni di una serie di episodi.

I ritorni dovrebbero aumentare man mano che l'agente impara.

Esperimento:

Misurare come varia il ritorno medio rallentando il ciclo di iterazione per permettere
al trainer di apprendere i dati in batch.


# Layer

## Forward

L'attività di forward consiste nel calcolare l'uscita di un layer in base ai segnali
d'ingresso, la matrice di pesi, e la funzione di attivazione.

in -> z = sum(in, weights) -> h = activation(z) -> h

## Backward

L'attività di back propagation consiste nel calcolare i valori di trace e di aggiornamento dei pesi
in base al gradiente della funzione di di attivazione e all'algoritmo di backproagation (con tutti i relativi parametri).

expected -> grad = grad(expected, h, inputs, weights, alpha) -> trace1 = backtrace(trace, grad, gamma, lambda)
-> weights1 = update(trace1, eta) -> cost = cost(expected, h, inputs, weights, alpha)-> expected1 = backprop(trace)

