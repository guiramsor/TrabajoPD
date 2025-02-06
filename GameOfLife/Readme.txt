
Juego de la Vida de John Conway
================================

Implementación del famoso autómata celular en programación declarativa funcional. 
Se recomienda ejecutar en sistemas Linux para óptima compatibilidad.

Instrucciones de Ejecución:
----------------------------
1. Descargue la carpeta "GameOfLife" y colóquela en su ubicación preferida (ej. Escritorio).
2. Abra una terminal y navegue hasta la carpeta:
   > cd ~/Desktop/GameOfLife
3. Compile el código con GHCi:
   > ghci Main.hs
4. Ejecute el programa:
   > main
5. Seleccione una opción:
   - Elija un patrón predefinido (1-3) 
   - Ingrese 0 para personalizar el tablero:
     * Especifique ancho/alto
     * Use WASD y ENTER para colocar células
     * Presione I para iniciar

Controles:
----------
- Movimiento:         W (arriba), A (izquierda), S (abajo), D (derecha)
- Modificar tablero:  ENTER (añadir celula viva donde el cursor '@')
- Iniciar simulación: I
- Salir:              Ctrl + C

Patrones Predefinidos:
----------------------
1. Glider  : Estructura móvil con desplazamiento diagonal.
2. LWSS    : "Nave ligera" de planeador espacial.
3. Pulsar  : Oscilador periódico de 3 fases.

Reglas de Simulación:
---------------------
- Intervalo de actualización: 0.3 segundos
- Reglas para células:
  * Viva con <2  vecinos → Muerta (subpoblación)
  * Viva con 2-3 vecinos → Sobrevive
  * Viva con >3 vecinos  → Muerta (sobrepoblación)
  * Muerta con 3 vecinos → Revive (reproducción)
