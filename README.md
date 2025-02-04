# Juego de la vida de John Conway
El famoso juego llevado a la programación declarativa.
Para que el programa funcione perfectamente se recomienda ejecutarlo en el Sistema operativo Linux.
### Pasos a seguir:

 1. Descargas la carpeta "GameOfLife" y lo arrastras al escritorio o donde prefieras ejecutarlo.
 2. En la terminal accedemos al contenido de la carpeta, si es en el escritorio tendria que ser algo asi `~/Desktop/GameOfLife$`.
 3. A continuación compila este código: `ghci Main.hs`.
 4. Escribimos `main` para ejecutar el programa.
 5. **Selección de Patrones o Personalización:**

-   Elige un patrón predefinido (Glider, LWSS, Pulsar) ingresando el número correspondiente.
    
-   Opción  `0`: Personaliza el tablero ingresando ancho, alto y colocando células con el cursor.
### Controles:

-   **WASD**: Mover el cursor.
    
-   **ENTER**: Activar/desactivar célula en la posición del cursor.
    
-   **I**: Iniciar la simulación.
    
-   **Ctrl + C**: Salir del programa (en la terminal).
    

### Patrones Disponibles:

1.  **Glider**: Estructura que se desplaza diagonalmente.
    
2.  **LWSS (Nave ligera)**: Patrón móvil más complejo.
    
3.  **Pulsar**: Oscilador de periodo 3.
### Durante la Simulación:

-   El tablero avanza automáticamente cada 0.4 segundos.
    
-   Las células siguen las reglas clásicas de Conway:
    
    -   _Subpoblación_: Célula viva con <2 o >3 vecinos muere.
        
    -   _Sobrepoblación_: Célula muerta con exactamente 3 vecinos revive.

¡Disfruta explorando los patrones emergentes! 🚀