carne(pollo).
carne(ternera).

contiene(paella, arroz).
contiene(paella, pollo).
contiene(hervido, patata).

receta_con_carne(X) :- contiene(X, Y), carne(Y).