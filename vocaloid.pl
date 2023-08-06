%Base de conocimineto
cantante(megurineLuka, cancion(nightFever, 4)).
cantante(megurineLuka, cancion(foreverYoung, 5)).
cantante(hatsuneMiku, cancion(tellYourWorld, 4)).
cantante(gumi, cancion(foreverYoung, 4)).
cantante(gumi, cancion(tellYourWorld, 5)).
cantante(seeU, cancion(novemberRain, 6)).
cantante(seeU, cancion(nightFever, 5)).

%Punto  1:
cantanteNovedoso(Cantante):-
    sabePorLoMenosDosCanciones(Cantante),
    todasSusCancionesDuranMenosDe(Cantante, 15).
    
sabePorLoMenosDosCanciones(Cantante):-
    cantante(Cantante, Cancion),
    cantante(Cantante, OtraCancion),
    Cancion \= OtraCancion.
    
todasSusCancionesDuranMenosDe(Cantante, TiempoMaximo):-
    findall(Tiempo,cantante(Cantante, cancion(_,Tiempo)), Tiempos),
    sum_list(Tiempos, TiempoTotal),
    TiempoTotal < TiempoMaximo.
    

%Punto 2:
cantanteAcelerado(Cantante):-
    cantante(Cantante,_),
    not(tieneAlgunaCancionLarga(Cantante)).

tieneAlgunaCancionLarga(Cantante):-
    cantante(Cantante, cancion(_,TiempoDeDuracion)),
    TiempoDeDuracion > 4.

%Segunda Parte
concierto(mikuExpo, estadosUnidos, 2000, gigante(2,6)).
concierto(magicalMirai, japon, 3000, gigante(3,10)).
concierto(vocalektVisions, estadosUnidos, 1000, mediano(9)).
concierto(mikuFest, argentina, 100, pequenio(1)).

%Punto 1
puedeParticipar(Cantante, Concierto):-
    concierto(Concierto,_,_,Tipo),
    cantante(Cantante,_),
    Cantante \= hatsuneMiku,
    esApto(Cantante, Tipo).
puedeParticipar(hatsuneMiku, Concierto):-
    concierto(Concierto,_,_,_).

esApto(Cantante, gigante(CantidadMinimaDeCanciones, TiempoMinimo)):-
    not(todasSusCancionesDuranMenosDe(Cantante, TiempoMinimo)),
    sabePorLoMenos(Cantante, CantidadMinimaDeCanciones).
esApto(Cantante, mediano(DuracionMaxima)):-
    todasSusCancionesDuranMenosDe(Cantante, DuracionMaxima).
esApto(Cantante, pequenio(DuracionMinimaDeAlgunaCancion)):-
    cantante(Cantante, cancion(_,DuracionDeUnaCancion)),
    DuracionDeUnaCancion > DuracionMinimaDeAlgunaCancion.

sabePorLoMenos(Cantante, CantidadMinimaDeCanciones):-
    cantidadDeCancionesQueSabe(Cantante, CantidadDeCanciones),
    CantidadDeCanciones >= CantidadMinimaDeCanciones.

%Punto 2
cantanteMasFamoso(Cantante):-
    nivelDeFama(Cantante, Fama),
    forall(nivelDeFama(_, OtroNivelDeFama), Fama >= OtroNivelDeFama).

nivelDeFama(Cantante, FamaTotal):-
    cantante(Cantante,_),
    findall(Fama, famaPorConcierto(Cantante,_,Fama), Famas),
    sum_list(Famas, SubFamaTotal),
    cantidadDeCancionesQueSabe(Cantante, Cantidad),
    FamaTotal is SubFamaTotal * Cantidad.

cantidadDeCancionesQueSabe(Cantante, CantidadDeCanciones):-
    findall(Cancion, cantante(Cantante, Cancion), Canciones),
    length(Canciones, CantidadDeCanciones).

famaPorConcierto(Cantante, Concierto, Fama):-
    puedeParticipar(Cantante, Concierto),
    fama(Concierto, Fama).

fama(Concierto, Fama):-
    concierto(Concierto,_,Fama,_).

%Punto 4:
conoceA(megurineLuka, hatsuneMiku).
conoceA(megurineLuka, gumi).
conoceA(gumi, seeU).
conoceA(seeU, kaito).

participaSolo(Cantante, Concierto):-
    puedeParticipar(Cantante, Concierto),
    not(participaConocido(Cantante, Concierto)).

participaConocido(Cantante, Concierto):-
    conoceA(Cantante, OtroCantante),
    puedeParticipar(OtroCantante, Concierto).
participaConocido(Cantante, Concierto):-
    conoceA(Cantante, OtroCantante),
    participaConocido(OtroCantante, Concierto).