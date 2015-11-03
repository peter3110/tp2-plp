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


%%% Ejercicio 2

% nPiezasDeCada(+Cant, +Tamaños, -Piezas), que instancia a Piezas con una lista que contiene
% una cantidad Cant de cada tamaño en la lista Tamaños.

% Comentario: si me pasan una lista de tamaños vacia, no voy a querer devolver ninguna pieza
%			  de ningun tamaño. De lo contrario, pido que cada elemento de la solucion sean
%			  piezas de los tamaños especificados.
nPiezasDeCada(_,[],[]).
nPiezasDeCada(C,[T1|T],[pieza(T1,C)|P]) :- nPiezasDeCada(C,T,P).

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


%%% Ejercicio 5

% cumpleLímite(+Piezas,+Solución) será verdadero cuando la cantidad de piezas utilizadas en Solución
% no exceda las cantidades disponibles indicadas en Piezas.

% Comentario: chequeo que para cada pieza utilizada en S se verifique que se utilizan como mucho
%			  la cantidad de piezas de ese tipo que hay disponibles en P. Corto la recursion cuando
%			  ya probe todas las combinaciones posibles.
%			  Asumo que S fue construido a partir de "generar": S = [X1,X2,X3,X3,..]
%			  Observacion: corto a recursion en el arbol para no repetir soluciones iguales

cumpleLimite(_,[]).
%% cumpleLimite(P,S):- append(A1,[pieza(X,C_disp)|B1],P), C_disp > 0, append(A2,[X|B2],S), C_new is C_disp - 1,
%% 					append(A1,[pieza(X,C_new)|B1],P2), append(A2,B2,S2), cumpleLimite(P2,S2), !.
cumpleLimite(P,[X|S]) :- append(A1,[pieza(X,C)|A2],P), C > 0, Cm1 is C-1, append(A1,[pieza(X,Cm1)|A2],P2), cumpleLimite(P2,S).

%%% Ejercicio 6

% construir1(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas cuyos valores
%  suman Total y, además, las cantidades utilizadas de cada pieza no exceden los declarados en Piezas.

% Comentario: aprovecho las dos funciones anteriores.

%construir1(Tot,P,S):- construido(Tot,P,S), !.
construir1(Tot,P,S):- generar(Tot,P,S), cumpleLimite(P,S).%, asserta(construido(Tot,P,S)).


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

:- dynamic dp/3. % dp(Tot,K,S) : en la solucion S se usan piezas de la lista original P de largo <= K para sumar Tot
				 % llamo a construir2dp con K = maximo valor de pieza que se puede utilizar.
construir2(Tot,P,S) :- retractall(dp(_,_,_)), append(_,[pieza(Kmax,_)],P), construir2dp(Tot,P,Kmax,S2),
					   append(S2,[],S).

construir2dp(0,_,_,[]).
construir2dp(Tot,_,K,S) :- dp(Tot,K,S).
construir2dp(Tot,P,K,S) :- Tot > 0, K > 0, Km1 is K - 1, construir2dp(Tot,P,Km1,S),
									not(dp(Tot,K,S)), asserta(dp(Tot,K,S)).
construir2dp(Tot,P,K,S) :- Tot > 0, K > 0, member(pieza(K,C),P), C > 0 , Km1 is K - 1, Totmk is Tot - K,
									between(0,Totmk,TotI), decrementar(P,[K],PI), construir2dp(TotI,PI,Km1,SI),
									TotD is Totmk - TotI, decrementar(PI,SI,PD), construir2dp(TotD,PD,K,SD),
									append(SI,[K|SD],S), not(dp(Tot,K,S)), asserta(dp(Tot,K,S)).


% Comentario: funcion auxiliar: decrementar(Piezas,Sol,Res) quita de Piezas las piezas de los tamaños que
% hay indicados en Sol (una lista de tamanios).
% REQUIERE: Que en P haya una cantidad de piezas X >= que la cantidad de piezas X que hay en S
decrementar([],[],[]).
decrementar(P,[],P).
decrementar([],_,[]).
decrementar(P,[K|S],P2) :- append(A,[pieza(K,X)|B],P), X>1, Xm1 is X - 1, append(A,[pieza(K,Xm1)|B], Aux),
						   decrementar(Aux,S,P2), !.
decrementar(P,[K|S],P2) :- append(A,[pieza(K,1)|B],P), append(A,B,Aux), decrementar(Aux,S,P2), !.


% ####################################
% Comparación de resultados y tiempos
% ####################################

%%% Ejercicio 8

% todosConstruir1(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas las
%  soluciones de longitud Total obtenidas con construir1/3, y N indica la cantidad de soluciones totales.

%% para medir tiempo
%% hacemos time(todosConstruir1(5,[pieza(1,5),pieza(5,1)],S,N))

todosConstruir1(Tot, P, S, N):- setof(X, construir1(Tot,P,X), S), length(S,N).


%%% Ejercicio 9

% todosConstruir2(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas
%  las soluciones de longitud Total obtenidas con construir2/3, y N indica la cantidad de soluciones totales.

todosConstruir2(Tot, P, S, N):- setof(X, construir2(Tot,P,X), S), length(S,N).


% ####################################
% Patrones
% ####################################

%%% Ejercicio 10

% construirConPatron(+Total, +Piezas, ?Patrón, -Solución) será verdadero cuando Solución sea una solución factible
%  en los términos definidos anteriormente y, además, sus piezas respeten el patrón indicado en Patrón.
%  Se sugiere definir un predicado tienePatrón(+Lista, ?Patrón) que decida si Lista presenta el Patrón especificado.
%% Generate & test.

construirConPatron(Tot, P, Pat, S):- construir1(Tot,P,S), tienePatron(Pat,S).

tienePatron(P,L) :- tienePatronAux(P,P,L).

tienePatronAux(_,[],[]).
tienePatronAux(P0,[],[Y|L]) :- tienePatronAux(P0,P0,[Y|L]).
%% tienePatronAux(P0,[X|P],[Y|L]) :- X = Y, tienePatronAux(P0,P,L).
tienePatronAux(P0,[X|P],[Y|L]) :- var(X), X = Y, tienePatronAux(P0,P,L).
tienePatronAux(P0,[X|P],[Y|L]) :- nonvar(X), X =:= Y, tienePatronAux(P0,P,L).
