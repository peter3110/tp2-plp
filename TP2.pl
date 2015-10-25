% ####################################
% Calentando motores
% ####################################

%%% Ejercicio 1

% listaNats(+LInf,+LSup,?Nats), que unifica la lista Nats con los naturales en el rango [LInf, LSup], o una lista vacía si LSup < LInf.

% Comentario: para que [H|T] sea la cabeza de Nats[LInf,LSup], si o si debe unificar con LInf 
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

% Comentario: i) si la pieza T1 no esta en la solucion de S, agrego una pieza de tipo T1 a 
%				 mi solucion, P.
%			 ii) si la pieza T1 ya estaba en la solucion de S, sumo 1 a la cantidad de piezas
%				 de tipo T1 que nos brinda la solucion S, y esta es mi nueva solucion, P.				 
resumenPiezas([], []).
resumenPiezas([T1|S], P) :- resumenPiezas(S,O), not(append(_,[pieza(T1,_)|_],O)),
							append([pieza(T1,1)],O,P).
resumenPiezas([T1|S], P) :- resumenPiezas(S,O), append(A,[pieza(T1,C)  |B],O), Cm1 is C+1, 
							append(A,[pieza(T1,Cm1)|B],P).
 							


% ####################################
% Enfoque naïve
% ####################################

%%% Ejercicio 4

% generar(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas
%  cuyos valores suman Total. Aquí no se pide controlar que la cantidad de cada pieza
%  esté acorde con la disponibilidad.

% Comentario: si me pasan un nro <=0, no se podra encontrar una lista de piezas que verifique.
%			  si no, busco en P piezas que podrian llegar a contribuir a una posible solucion,
%			  y me fijo si de esta forma se genera una solucion factible. Si es asi, la agrego.
%			  Como se pide en el enunciado, no tomo en cuenta disponibilidad acotada de piezas.

generar(Tot,_,[]):- Tot =< 0.
generar(Tot,P,S) :- append(_,[pieza(X,_)|_],P), Tot2 is Tot-X, Tot2 >= 0,
					generar(Tot2,P,H), append(H,[pieza(X,1)],S).


%%% Ejercicio 5 

% cumpleLímite(+Piezas,+Solución) será verdadero cuando la cantidad de piezas utilizadas en Solución 
% no exceda las cantidades disponibles indicadas en Piezas.

% Comentario: chequeo que para cada pieza utilizada en S se verifique que se utilizan como mucho
%			  la cantidad de piezas de ese tipo que hay disponibles en P. Corto la recursion cuando
%			  ya probe todas las combinaciones posibles.
%			  Asumo que su utilizo la funcion "generar" para obtener la lista "Solucion", y hago uso
%			  del hecho que en dicha lista la cantidad utilizada de cada pieza es siempre 1.

cumpleLimite(_,[]).
cumpleLimite(P,S):- append(A1,[pieza(X,C_disp)|B1],P), append(A2,[pieza(X,1)|B2],S),
					C_new is C_disp - 1,
					append(A1,[pieza(X,C_new)|B1],P2), append(A2,B2,S2), cumpleLimite(P2,S2), !.


%%% Ejercicio 6

% construir1(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas cuyos valores 
%  suman Total y, además, las cantidades utilizadas de cada pieza no exceden los declarados en Piezas.

% Comentario: aprovecho las dos funciones anteriores.
construir1(Tot,P,S):- generar(Tot,P,S), cumpleLimite(P,S).


% ####################################
% Enfoque dinámico
% ####################################

%%% Ejercicio 7

% construir2(+Total,+Piezas,-Solución), cuyo comportamiento es id ́entico a construir1/3 pero que utiliza 
%  definiciones dinámicas para persistir los cálculos auxiliares realizados y evitar repetirlos. 
%  No se espera que las soluciones aparezcan en el mismo orden entre construir1/3 y construir2/3, pero sí, sean las mismas.

 construir2(_,_,_):- fail.

% ####################################
% Comparación de resultados y tiempos
% ####################################

%%% Ejercicio 8

% todosConstruir1(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas las
%  soluciones de longitud Total obtenidas con construir1/3, y N indica la cantidad de soluciones totales.

todosConstruir1(_, _, _, _):- fail.


%%% Ejercicio 9

% todosConstruir2(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas 
%  las soluciones de longitud Total obtenidas con construir2/3, y N indica la cantidad de soluciones totales.

todosConstruir2(_, _, _, _):- fail.


% ####################################
% Patrones
% ####################################

%%% Ejercicio 10

% construirConPatron(+Total, +Piezas, ?Patrón, -Solución) será verdadero cuando Solución sea una solución factible 
%  en los términos definidos anteriormente y, además, sus piezas respeten el patrón indicado en Patrón. 
%  Se sugiere definir un predicado tienePatrón(+Lista, ?Patrón) que decida si Lista presenta el Patrón especificado.

construirConPatron(_, _, _, _):- fail.
