% ####################################
% Calentando motores
% ####################################

%%% Ejercicio 1

% listaNats(+LInf,+LSup,?Nats), que unifica la lista Nats con los naturales en el rango [LInf, LSup], o una lista vacía si LSup < LInf.

% Comentario: como caso base, decimos que en un intervalo vacio no hay solucion
% 	si T es la solucion para listaNats entre H+1 y LSup, entonces [H|T] es solucion para listaNats
% 	entre H y LSup

listaNats(LInf,LSup,[])    :- LInf > LSup.
listaNats(LInf,LSup,[H|T]) :- LInf =< LSup, H is LInf, Hm1 is H+1, listaNats(Hm1,LSup,T).

%% Ejemplos:

%% ?- listaNats(5,10,S).
%% S = [5, 6, 7, 8, 9, 10] ;
%% false.

%% ?- listaNats(1,2,[1,2]).
%% true ;
%% false.

%% ?- listaNats(2,1,[1,2]).
%% false.

%% ?- listaNats(10,3,S).
%% S = [] .

%% ?- listaNats(4,3,[3,4]).
%% false.

%%% Ejercicio 2

% nPiezasDeCada(+Cant, +Tamaños, -Piezas), que instancia a Piezas con una lista que contiene
% una cantidad Cant de cada tamaño en la lista Tamaños.

% Comentario: si me pasan una lista de tamaños vacia, no voy a querer devolver ninguna pieza
%			  de ningun tamaño. De lo contrario, pido que cada elemento de la solucion sean
%			  piezas de los tamaños especificados.
nPiezasDeCada(_,[],[]).
nPiezasDeCada(C,[T1|T],[pieza(T1,C)|P]) :- nPiezasDeCada(C,T,P).

%% Ejemplos:

%% ?- nPiezasDeCada(4,[1,2,4,7],S).
%% S = [pieza(1, 4), pieza(2, 4), pieza(4, 4), pieza(7, 4)] ;
%% false.

%% ?- nPiezasDeCada(3,[3,7,10],S).
%% S = [pieza(3, 3), pieza(7, 3), pieza(10, 3)] ;
%% false.

%% ?- nPiezasDeCada(1,[1,2,3],S).
%% S = [pieza(1, 1), pieza(2, 1), pieza(3, 1)] ;
%% false.

%%% Ejercicio 3

% resumenPiezas(+SecPiezas, -Piezas), que permite instanciar Piezas con la lista de
% piezas incluidas en SecPiezas.

% Comentario: i) si la pieza T1 no esta en la solucion de S (O), agrego una (1) pieza de tipo T1 a
%				 dicha solucion O y de esta forma se arma la solucion final P.
%			 ii) si la pieza T1 ya estaba en la solucion de S, sumo 1 a la cantidad de piezas
%				 de tipo T1 que nos brinda la solucion S, y esta es mi nueva solucion, P.
resumenPiezas([], []).
resumenPiezas([T1|S], P) :- resumenPiezas(S,O), not(append(_,[pieza(T1,_)|_],O)),
							append([pieza(T1,1)],O,P).
resumenPiezas([T1|S], P) :- resumenPiezas(S,O), append(A,[pieza(T1,C)|B],O), Cm1 is C+1,
							append(A,[pieza(T1,Cm1)|B],P).

%% Ejemplos:

%% ?- resumenPiezas([1,2,2,3,5,2,3,1,7], L).
%% L = [pieza(5, 1), pieza(2, 3), pieza(3, 2), pieza(1, 2), pieza(7, 1)] ;
%% false.

%% ?- resumenPiezas([1,2,3,4,5], L).
%% L = [pieza(1, 1), pieza(2, 1), pieza(3, 1), pieza(4, 1), pieza(5, 1)] ;
%% false.

%% ?- resumenPiezas([78,90,34,20,20,34,90,78], L).
%% L = [pieza(20, 2), pieza(34, 2), pieza(90, 2), pieza(78, 2)] ;
%% false.

%% ?- resumenPiezas([5,5,5,5,5,5,5,5,5,5], L).
%% L = [pieza(5, 10)] ;
%% false.

% ####################################
% Enfoque naïve
% ####################################

%%% Ejercicio 4

% generar(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas
%  cuyos valores suman Total. Aquí no se pide controlar que la cantidad de cada pieza
%  esté acorde con la disponibilidad.

% Comentario: si me pasan un nro <=0, no se podra encontrar una lista de piezas que verifique, pues
%			  estamos trabajando con naturales.
%			  si no, busco que en P existan piezas (X) que contribuyen a una posible solucion
%			  factible. Dichas piezas X van a contribuir a la solucion S final.
%			  Al no tener en cuenta la cantidad disponible de piezas tipo X que hay en P, no
%			  estoy tomando en cuenta que pueda haber disponibilidad acotada de piezas.

generar(Tot,_,[]):- Tot =< 0.
generar(Tot,P,S) :- append(_,[pieza(X,_)|_],P), Tot2 is Tot-X, Tot2 >= 0,
					generar(Tot2,P,H), append(H,[X],S).

%% Ejemplos:

%% ?- generar(5,[pieza(1,2),pieza(2,2),pieza(5,1)],S).
%% S = [1, 1, 1, 1, 1] ;
%% S = [2, 1, 1, 1] ;
%% S = [1, 2, 1, 1] ;
%% S = [1, 1, 2, 1] ;
%% S = [2, 2, 1] ;
%% S = [1, 1, 1, 2] ;
%% S = [2, 1, 2] ;
%% S = [1, 2, 2] ;
%% S = [5] ;
%% false.

%% ?- generar(5,[pieza(10,2),pieza(12,2),pieza(15,1)],S).
%% false.

%% ?- generar(90,[pieza(10,10)],S).
%% S = [10, 10, 10, 10, 10, 10, 10, 10, 10] ;
%% false.

%% ?- generar(90,[pieza(10,3)],S).
%% S = [10, 10, 10, 10, 10, 10, 10, 10, 10] ;
%% false.

%%% Ejercicio 5

% cumpleLímite(+Piezas,+Solución) será verdadero cuando la cantidad de piezas utilizadas en Solución
% no exceda las cantidades disponibles indicadas en Piezas.

% Comentario: chequeo que para cada pieza utilizada en S se verifique que se utilizan como mucho
%			  la cantidad de piezas de ese tipo que hay disponibles en P.
%			  Asumo que S fue construido a partir de "generar": S = [X1,X2,X3,X3,..]

cumpleLimite(_,[]).
cumpleLimite(P,[X|S]) :- append(A1,[pieza(X,C)|A2],P), C > 0, Cm1 is C-1, append(A1,[pieza(X,Cm1)|A2],P2), cumpleLimite(P2,S).

%% Ejemplos:

%% ?- cumpleLimite([pieza(5,5)],[5,5,5,5,5]).
%% true ;
%% false.

%% ?- cumpleLimite([pieza(1,2),pieza(2,2),pieza(5,1)],[1,2,1,1]).
%% false.

%% ?- cumpleLimite([pieza(1,2),pieza(2,2),pieza(5,1)],[1,2,2]).
%% true ;
%% false.

%% ?- cumpleLimite([pieza(1,200)],[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]).
%% true ;
%% false.

%% ?- cumpleLimite([pieza(1,200)],[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2]).
%% false.



%%% Ejercicio 6

% construir1(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas cuyos valores
%  suman Total y, además, las cantidades utilizadas de cada pieza no exceden los declarados en Piezas.

% Comentario: aprovecho las dos funciones anteriores, utilizando la tecnica de generate & test.

construir1(Tot,P,S):- generar(Tot,P,S), cumpleLimite(P,S).

%% Ejemplos:

%% ?- construir1(5,[pieza(1,3),pieza(2,2),pieza(3,2),pieza(5,1)],S).
%% S = [2, 1, 1, 1] ;
%% S = [1, 2, 1, 1] ;
%% S = [3, 1, 1] ;
%% S = [1, 1, 2, 1] ;
%% S = [2, 2, 1] ;
%% S = [1, 3, 1] ;
%% S = [1, 1, 1, 2] ;
%% S = [2, 1, 2] ;
%% S = [1, 2, 2] ;
%% S = [3, 2] ;
%% S = [1, 1, 3] ;
%% S = [2, 3] ;
%% S = [5] ;
%% false.

%% ?- construir1(5,[pieza(1,3)],S).
%% false.

%% ?- construir1(11,[pieza(2,4),pieza(3,1)],S).
%% S = [3, 2, 2, 2, 2] ;
%% S = [2, 3, 2, 2, 2] ;
%% S = [2, 2, 3, 2, 2] ;
%% S = [2, 2, 2, 3, 2] ;
%% S = [2, 2, 2, 2, 3] ;
%% false.

%% ?- construir1(5,[pieza(2,3)],S).
%% false.

%% ?- construir1(3,[pieza(4,3),pieza(5,10),pieza(10,10),pieza(15,5)],S).
%% false.


% ####################################
% Enfoque dinámico
% ####################################

%%% Ejercicio 7

% construir2(+Total,+Piezas,-Solución), cuyo comportamiento es id ́entico a construir1/3 pero que utiliza
%  definiciones dinámicas para persistir los cálculos auxiliares realizados y evitar repetirlos.
%  No se espera que las soluciones aparezcan en el mismo orden entre construir1/3 y construir2/3,
%  pero sí, sean las mismas.

% Comentario:  si el tamanio maximo de las piezas es cero, entonces la lista vacia verifica
% Si no hay piezas de tamnio K en P, llamo recursivamente con los mismos parametros, pero usando como
% maximo piezas de tamanio K-1.
% Si hay al menos una pieza de tamanio K en P, pruebo todas las posiciones en las que podria
% colocar tal ficha de tamanio K en la solucion (posiciones 0..Tot-K), hago PI = P \ {K}, y llamo
% recursivamente a las subsolucion del intervalo de la izquierda: Asi obtengo una solucion S1.
% Luego, modifico PD = PI \ {piezas utilizadas en SI}. Y llamo recursivamente con Tot - Tot2 - K y con
% las fichas que quedaron disponibles en PD.
% Ademas, cada vez que un llamado recursivo es exitoso, me guardo en la(s) solucion(es) obtenida(s).

:- dynamic dp/4. % dp(Tot,P,K,S) : en la solucion S se usan piezas de la lista original P de largo <= K para sumar Tot
				 % llamo a construir2dp con K = maximo valor de pieza que se puede utilizar.
construir2(Tot,P,S) :- retractall(dp(_,_,_,_)), append(_,[pieza(Kmax,_)],P), construir2dp(Tot,P,Kmax,S2),
					   append(S2,[],S).

construir2dp(0,_,_,[]).
construir2dp(Tot,P,K,S) :- dp(Tot,P,K,S).
construir2dp(Tot,P,K,S) :- Tot > 0, K > 0, Km1 is K - 1, construir2dp(Tot,P,Km1,S),
									not(dp(Tot,P,K,S)), asserta(dp(Tot,P,K,S)).
construir2dp(Tot,P,K,S) :- Tot > 0, K > 0, member(pieza(K,C),P), C > 0 , Km1 is K - 1, Totmk is Tot - K,
									between(0,Totmk,TotI), decrementar(P,[K],PI), construir2dp(TotI,PI,Km1,SI),
									TotD is Totmk - TotI, decrementar(PI,SI,PD), construir2dp(TotD,PD,K,SD),
									append(SI,[K|SD],S), not(dp(Tot,P,K,S)), asserta(dp(Tot,P,K,S)).


% Comentario: funcion auxiliar: decrementar(+Piezas,+Sol,-Res) quita de Piezas las piezas de los tamaños que
% hay indicados en Sol (una lista de tamanios).
% REQUIERE: Que en P haya una cantidad de piezas X >= que la cantidad de piezas X que hay en S
decrementar([],[],[]).
decrementar(P,[],P).
decrementar([],_,[]).
decrementar(P,[K|S],P2) :- append(A,[pieza(K,X)|B],P), X>1, Xm1 is X - 1, append(A,[pieza(K,Xm1)|B], Aux),
						   decrementar(Aux,S,P2), !.
decrementar(P,[K|S],P2) :- append(A,[pieza(K,1)|B],P), append(A,B,Aux), decrementar(Aux,S,P2), !.


%% Ejemplos

%% ?- construir2(5,[pieza(1,3),pieza(2,2),pieza(3,2),pieza(5,1)],S).
%% S = [2, 1, 1, 1] ;
%% S = [2, 2, 1] ;
%% S = [2, 1, 2] ;
%% S = [1, 2, 1, 1] ;
%% S = [1, 2, 2] ;
%% S = [1, 1, 2, 1] ;
%% S = [1, 1, 1, 2] ;
%% S = [3, 2] ;
%% S = [3, 1, 1] ;
%% S = [1, 3, 1] ;
%% S = [2, 3] ;
%% S = [1, 1, 3] ;
%% S = [5] ;
%% false.

%% ?- construir2(5,[pieza(1,3)],S).
%% false.

%% ?- construir2(11,[pieza(2,4),pieza(3,1)],S).
%% S = [3, 2, 2, 2, 2] ;
%% S = [2, 3, 2, 2, 2] ;
%% S = [2, 2, 3, 2, 2] ;
%% S = [2, 2, 2, 3, 2] ;
%% S = [2, 2, 2, 2, 3] ;
%% false.

%% ?- construir2(5,[pieza(2,3)],S).
%% false.

%% ?- construir2(3,[pieza(4,3),pieza(5,10),pieza(10,10),pieza(15,5)],S).
%% false.


% ####################################
% Comparación de resultados y tiempos
% ####################################

%%% Ejercicio 8

% todosConstruir1(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas las
%  soluciones de longitud Total obtenidas con construir1/3, y N indica la cantidad de soluciones totales.

%% para medir tiempo
%% hacemos time(todosConstruir1(5,[pieza(1,5),pieza(5,1)],S,N))

todosConstruir1(Tot, P, S, N):- setof(X, construir1(Tot,P,X), S), length(S,N).

%% Ejemplos:

%% ?- time(todosConstruir1(28,[pieza(3,1),pieza(4,1),pieza(7,4)],S,N)).
%% % 33,731 inferences, 0.005 CPU in 0.014 seconds (33% CPU, 7415036 Lips)
%% S = [[3, 4, 7, 7, 7], [3, 7, 4, 7, 7], [3, 7, 7, 4, 7], [3, 7, 7, 7, 4],
%% 			[4, 3, 7, 7|...], [4, 7, 3|...], [4, 7|...], [4|...], [...|...]|...],
%% N = 21.

%% ?- time(todosConstruir1(5,[pieza(1,3),pieza(2,2),pieza(3,2),pieza(5,1)],S,N)).
%% % 944 inferences, 0.000 CPU in 0.000 seconds (93% CPU, 3210884 Lips)
%% S = [[1, 1, 1, 2], [1, 1, 2, 1], [1, 1, 3], [1, 2, 1, 1], [1, 2, 2],
%% 			[1, 3, 1], [2, 1|...], [2|...], [...|...]|...],
%% N = 13.

%% ?- time(todosConstruir1(15,[pieza(1,2),pieza(2,2),pieza(3,3),pieza(5,2),pieza(7,1)],S,N)).
%% % 1,084,467 inferences, 0.111 CPU in 0.122 seconds (92% CPU, 9737777 Lips)
%% S = [[1, 1, 2, 2, 3, 3, 3], [1, 1, 2, 3, 2, 3, 3], [1, 1, 2, 3, 3, 2|...],
%% 			[1, 1, 2, 3, 3|...], [1, 1, 2, 3|...], [1, 1, 2|...], [1, 1|...], [1|...], [...|...]|...],
%% N = 644.


%%% Ejercicio 9

% todosConstruir2(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas
%  las soluciones de longitud Total obtenidas con construir2/3, y N indica la cantidad de soluciones totales.

todosConstruir2(Tot, P, S, N):- setof(X, construir2(Tot,P,X), S), length(S,N).

%% Ejemplos:

%% ?- time(todosConstruir2(28,[pieza(3,1),pieza(4,1),pieza(7,4)],S,N)).
%% % 89,503 inferences, 0.009 CPU in 0.010 seconds (90% CPU, 10417016 Lips)
%% S = [[3, 4, 7, 7, 7], [3, 7, 4, 7, 7], [3, 7, 7, 4, 7], [3, 7, 7, 7, 4], [4, 3, 7, 7|...],
%% 		 [4, 7, 3|...], [4, 7|...], [4|...], [...|...]|...],
%% N = 21.

%% ?- time(todosConstruir2(5,[pieza(1,3),pieza(2,2),pieza(3,2),pieza(5,1)],S,N)).
%% % 1,927 inferences, 0.062 CPU in 0.085 seconds (74% CPU, 30888 Lips)
%% S = [[1, 1, 1, 2], [1, 1, 2, 1], [1, 1, 3], [1, 2, 1, 1], [1, 2, 2], [1, 3, 1],
%% 		 [2, 1|...], [2|...], [...|...]|...],
%% N = 13.

%% ?- time(todosConstruir2(15,[pieza(1,2),pieza(2,2),pieza(3,3),pieza(5,2),pieza(7,1)],S,N)).
%% % 366,259 inferences, 0.357 CPU in 0.368 seconds (97% CPU, 1025947 Lips)
%% S = [[1, 1, 2, 2, 3, 3, 3], [1, 1, 2, 3, 2, 3, 3], [1, 1, 2, 3, 3, 2|...], [1, 1, 2, 3, 3|...],
%% 		 [1, 1, 2, 3|...], [1, 1, 2|...], [1, 1|...], [1|...], [...|...]|...],
%% N = 644.


% ####################################
% Patrones
% ####################################

%%% Ejercicio 10

% construirConPatron(+Total, +Piezas, ?Patrón, -Solución) será verdadero cuando Solución sea una solución factible
%  en los términos definidos anteriormente y, además, sus piezas respeten el patrón indicado en Patrón.
%  Se sugiere definir un predicado tienePatrón(+Lista, ?Patrón) que decida si Lista presenta el Patrón especificado.

%% Comentario: Encaramos la resolucion de este ejercicio con la tecnica generate & test, nos generamos las
%% soluciones candidatas y luego nos quedamos con las validas, es decir las q cumplen el patron.

construirConPatron(Tot, P, Pat, S):- construir1(Tot,P,S), tienePatron(Pat,S).

%% tienePatron(?Patron,+Lista)
tienePatron(P,L) :- tienePatronAux(P,P,L).

%% tienePatronAux(?Patron_orig,?Patron,+Lista)
%% Matchea el patron y si se queda sin patron, pero con lista por consumir entonces comienza de nuevo
%%  el patron, solo da true si se queda al mismo tiempo sin patron ni lista, generando que el patron
%%  se repita una cantidad entera de veces.

tienePatronAux(_,[],[]).
tienePatronAux(P0,[],[Y|L]) :- tienePatronAux(P0,P0,[Y|L]).
tienePatronAux(P0,[X|P],[Y|L]) :- var(X), X = Y, tienePatronAux(P0,P,L).
tienePatronAux(P0,[X|P],[Y|L]) :- nonvar(X), X =:= Y, tienePatronAux(P0,P,L).

%% Ejemplos:

%% ?- construirConPatron(5,[pieza(1,3),pieza(2,2),pieza(3,1)],[A,B],S).
%% A = 3,
%% B = 2,
%% S = [3, 2] ;
%% A = 2,
%% B = 3,
%% S = [2, 3] ;
%% false.

%% ?- construirConPatron(5,[pieza(1,3),pieza(2,2),pieza(3,1)],[A,B,C],S).
%% A = 3,
%% B = C, C = 1,
%% S = [3, 1, 1] ;
%% A = B, B = 2,
%% C = 1,
%% S = [2, 2, 1] ;
%% A = C, C = 1,
%% B = 3,
%% S = [1, 3, 1] ;
%% A = C, C = 2,
%% B = 1,
%% S = [2, 1, 2] ;
%% A = 1,
%% B = C, C = 2,
%% S = [1, 2, 2] ;
%% A = B, B = 1,
%% C = 3,
%% S = [1, 1, 3] ;
%% false.

%% ?- construirConPatron(5,[pieza(1,3),pieza(2,2),pieza(3,1)],[A,2,C],S).
%% A = 2,
%% C = 1,
%% S = [2, 2, 1] ;
%% A = 1,
%% C = 2,
%% S = [1, 2, 2] ;
%% false.

%% ?- construirConPatron(5,[pieza(1,3),pieza(2,2),pieza(3,1)],[A],S).
%% false.

%% ?- construirConPatron(5,[pieza(1,3),pieza(2,2),pieza(3,1)],[1],S).
%% false.

%% ?- construirConPatron(5,[pieza(1,3),pieza(2,2),pieza(3,1)],[A,B,C,D],S).
%% A = 2,
%% B = C, C = D, D = 1,
%% S = [2, 1, 1, 1] ;
%% A = C, C = D, D = 1,
%% B = 2,
%% S = [1, 2, 1, 1] ;
%% A = B, B = D, D = 1,
%% C = 2,
%% S = [1, 1, 2, 1] ;
%% A = B, B = C, C = 1,
%% D = 2,
%% S = [1, 1, 1, 2] ;
%% false.

