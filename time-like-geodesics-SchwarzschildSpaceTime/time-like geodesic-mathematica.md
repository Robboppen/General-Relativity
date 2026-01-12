# Órbitas Relativistas en Schwarzschild - Código Mathematica

## Código Completo
```mathematica
(*Código para graficar y animar órbitas relativistas en Schwarzschild
Animaciones detalladas para las tres órbitas (a),(b) y (c)*)
ClearAll["Global`*"]

(* =======================================================================*)
(*PARÁMETROS DE LAS ÓRBITAS*)
(* =======================================================================*)

(*Valores de la figura 7a con número de revoluciones para animación*)
paramSets = {
  {"(a) e=0.5, l=11, M=3/14", 0.5, 11.0, 3/14, 4},    (*4 revoluciones*)
  {"(b) e=0.5, l=7.5, M=3/14", 0.5, 7.5, 3/14, 6},    (*6 revoluciones*)
  {"(c) e=0.5, l=3, M=3/14", 0.5, 3.0, 3/14, 10}      (*10 revoluciones*)
};

(* =======================================================================*)
(*FUNCIONES AUXILIARES*)
(* =======================================================================*)

(*Función para calcular μ=M/l*)
CalcMu[M_, l_] := M/l;

(*Función para calcular A=1-6μ+2μe*)
CalcA[mu_, e_] := 1 - 6*mu + 2*mu*e;

(*Función para calcular k²=4μe/A*)
Calck2[mu_, e_, A_] := If[A != 0, 4*mu*e/A, 1.0];

(*Función para calcular φ(χ) usando la ecuación (132):
φ=2/√A*F(π/2-χ/2,k)*)
PhiFromChi[chi_, A_, k2_] := 
  Module[{sqrtA, psi}, 
    If[A <= 0, Return[Indeterminate]];
    sqrtA = Sqrt[A];
    psi = Pi/2 - chi/2;
    2*EllipticF[psi, k2]/sqrtA
  ];

(*Función para calcular datos orbitales para animación
Devuelve {orbitData,rHorizon,rCritical,phiPerRev,precession}*)
CalcOrbitDataForAnimation[e_, l_, M_, nRevolutions_] := 
  Module[{mu, A, k2, chiValsOneRev, phiValsOneRev, allChi, allPhi, 
    allR, allX, allY, phiPerRev, precession, orbitData, rHorizon, rCritical},
    
    mu = CalcMu[M, l];
    A = CalcA[mu, e];
    k2 = Calck2[mu, e, A];
    
    (*Verificar parámetros válidos*)
    If[A <= 0 || k2 < 0, 
      Print["Parámetros inválidos para animación: A = ", A, ", k² = ", k2];
      Return[{{}, 0, 0, 0, 0}, Module];
    ];
    
    (*Una revolución: χ de π (afelio) a -π (siguiente afelio)*)
    chiValsOneRev = Range[Pi, -Pi, -2*Pi/199]; (*200 puntos por rev*)
    phiValsOneRev = PhiFromChi[#, A, k2] & /@ chiValsOneRev;
    
    (*Ángulo φ por revolución y precesión*)
    phiPerRev = phiValsOneRev[[-1]] - phiValsOneRev[[1]];
    precession = phiPerRev - 2*Pi;
    
    (*Generar múltiples revoluciones*)
    allChi = {};
    allPhi = {};
    For[rev = 0, rev < nRevolutions, rev++,
      chiOffset = rev*2*Pi;
      phiOffset = rev*phiPerRev;
      chiValsAdj = chiValsOneRev - chiOffset;
      phiValsAdj = phiValsOneRev - phiValsOneRev[[1]] + phiOffset;
      allChi = Join[allChi, chiValsAdj];
      allPhi = Join[allPhi, phiValsAdj];
    ];
    
    (*Calcular r=l/(1+e cos χ)*)
    allR = l/(1 + e*Cos[allChi]);
    
    (*Convertir a coordenadas cartesianas*)
    allX = allR*Cos[allPhi];
    allY = allR*Sin[allPhi];
    
    (*Ajustar para que φ empiece en 0*)
    phiMin = Min[allPhi];
    allPhiAdj = allPhi - phiMin;
    allX = allR*Cos[allPhiAdj];
    allY = allR*Sin[allPhiAdj];
    
    (*Datos de órbita y parámetros importantes*)
    orbitData = Transpose[{allX, allY}];
    rHorizon = 2*M;
    rCritical = 3*M;
    
    {orbitData, rHorizon, rCritical, phiPerRev, precession}
  ];

(* =======================================================================*)
(*FUNCIÓN PARA CREAR UNA ANIMACIÓN INDIVIDUAL*)
(* =======================================================================*)

CreateOrbitAnimation[label_, e_, l_, M_, nRevs_, plotRange_] := 
  Module[{orbitData, rHorizon, rCritical, phiPerRev, precession, 
    nPoints, animation, mu, A, k2, perihelio, afelio},
    
    Print["Creando animación para: ", label];
    
    (*Calcular datos*)
    {orbitData, rHorizon, rCritical, phiPerRev, precession} = 
      CalcOrbitDataForAnimation[e, l, M, nRevs];
    
    If[Length[orbitData] == 0, 
      Print["  No se pudo crear animación: parámetros inválidos"];
      Return[Graphics[Text["Error en parámetros", {0, 0}]]];
    ];
    
    nPoints = Length[orbitData];
    
    (*Parámetros adicionales*)
    mu = CalcMu[M, l];
    A = CalcA[mu, e];
    k2 = Calck2[mu, e, A];
    perihelio = l/(1 + e);
    afelio = l/(1 - e);
    
    Print["  μ = ", NumberForm[mu, 4], ", k² = ", NumberForm[k2, 4]];
    Print["  Perihelio: r = ", NumberForm[perihelio, 4]];
    Print["  Afelio: r = ", NumberForm[afelio, 4]];
    Print["  Precesión: ", NumberForm[precession*180/Pi, 4], "°/rev"];
    Print["  Puntos totales: ", nPoints];
    
    (*Crear animación*)
    animation = Animate[
      Show[
        (*1. Fondo: órbita completa en gris claro*)
        Graphics[
          {White, Opacity[0.2], Thickness[0.002], Line[orbitData]},
          PlotRange -> plotRange
        ],
        
        (*2. Trayectoria recorrida hasta el momento (color progresivo)*)
        Graphics[
          {ColorData["Rainbow"][currentFraction], Thickness[0.006], 
           CapForm["Round"], 
           Line[Take[orbitData, Floor[currentFraction*nPoints]]]}
        ],
        
        (*3. Punto móvil (partícula)*)
        Graphics[
          {EdgeForm[Black], Yellow, 
           Disk[orbitData[[Floor[currentFraction*nPoints]]], 0.1], 
           Black, PointSize[0.02], 
           Point[orbitData[[Floor[currentFraction*nPoints]]]]}
        ],
        
        (*4. Línea desde el centro al punto actual*)
        Graphics[
          {Dashed, Gray, Opacity[0.6], Thickness[0.002], 
           Line[{{0, 0}, orbitData[[Floor[currentFraction*nPoints]]]}]}
        ],
        
        (*5. Elementos fijos del gráfico*)
        Graphics[
          {
            (*Horizonte de eventos*)
            {Black, Opacity[0.15], Disk[{0, 0}, rHorizon]},
            
            (*Órbita circular crítica*)
            {Red, Dashed, Opacity[0.4], Thickness[0.003], 
             Circle[{0, 0}, rCritical]},
            
            (*Centro del agujero negro*)
            {Black, PointSize[0.035], Point[{0, 0}]},
            
            (*Perihelio y afelio (líneas de referencia)*)
            {Green, Dashed, Opacity[0.3], Thickness[0.001], 
             Circle[{0, 0}, perihelio]},
            {Blue, Dashed, Opacity[0.3], Thickness[0.001], 
             Circle[{0, 0}, afelio]},
            
            (*Marcadores de perihelio y afelio*)
            {Green, PointSize[0.02], Point[{afelio, 0}]},
            {Red, PointSize[0.02], Point[{perihelio, 0}]}
          }
        ],
        
        (*6. Panel de información dinámica*)
        Graphics[
          {
            (*Fondo semitransparente para el panel*)
            {White, Opacity[0.8], 
             Rectangle[Scaled[{0.02, 0.02}], Scaled[{0.38, 0.18}]]},
            
            (*Texto informativo*)
            Text[Style[label, 14, Bold, Darker[Blue]], Scaled[{0.05, 0.16}]],
            Text[Style[
              StringForm["Revolución: `1`/`2`", 
                currentFraction*nPoints/(nPoints/nRevs), nRevs], 
              12, Darker[Green]], Scaled[{0.05, 0.12}]],
            Text[Style[
              StringForm["Progreso: `1`%", currentFraction*100], 
              11, Magenta], Scaled[{0.05, 0.08}]],
            Text[Style[
              StringForm["Precesión: `1`°/rev", precession*180/Pi], 
              10, Darker[Red]], Scaled[{0.05, 0.04}]]
          }
        ],
        
        (*7. Indicador de posición angular*)
        Graphics[
          {Thin, Gray, 
           Circle[{0, 0}, 
             0.8*Min[plotRange[[1, 2]], plotRange[[2, 2]]], 
             {0, 2*Pi*currentFraction}],
           Arrow[{{0, 0}, 
             0.9*Min[plotRange[[1, 2]], plotRange[[2, 2]]]*
               {Cos[2*Pi*currentFraction], Sin[2*Pi*currentFraction]}}]}
        ],
        
        (*Configuración del gráfico*)
        Frame -> True,
        FrameLabel -> {Style["x/M", 12, Bold], Style["y/M", 12, Bold]},
        FrameStyle -> Thickness[0.003],
        PlotRange -> plotRange,
        AspectRatio -> 1,
        ImageSize -> 500,
        GridLines -> Automatic,
        GridLinesStyle -> Directive[Gray, Dotted, Opacity[0.2]],
        Background -> Lighter[Gray, 0.95]
      ],
      
      (*Control de animación*)
      {currentFraction, 0, 1, 0.005},
      
      (*Opciones de animación*)
      AnimationRunning -> False,
      AnimationRepetitions -> 1,
      DisplayAllSteps -> False,
      DefaultDuration -> 30, (*30 segundos para animación completa*)
      
      (*Control deslizante personalizado*)
      ControlPlacement -> Bottom,
      AppearanceElements -> {"ProgressSlider", "PlayPauseButton", 
        "FasterSlowerButtons", "DirectionButton"},
      
      (*Etiquetas*)
      LabelStyle -> {FontSize -> 11, FontFamily -> "Arial"},
      TrackedSymbols :> {currentFraction}
    ];
    
    animation
  ];

(* =======================================================================*)
(*CREAR ANIMACIONES PARA LAS TRES ÓRBITAS*)
(* =======================================================================*)

Print[StringRepeat["=", 70]];
Print["CREANDO ANIMACIONES DETALLADAS PARA LAS TRES ÓRBITAS"];
Print[StringRepeat["=", 70]];

(*Definir rangos de gráfico apropiados para cada órbita*)
plotRanges = {
  {{-25, 25}, {-25, 25}},  (*Para (a): órbita más grande*)
  {{-20, 20}, {-20, 20}},  (*Para (b): órbita mediana*)
  {{-10, 10}, {-10, 10}}   (*Para (c): órbita más pequeña y cercana*)
};

(*Crear las tres animaciones*)
animations = Table[
  Module[{label, e, l, M, nRevs, plotRange},
    {label, e, l, M, nRevs} = paramSets[[i]];
    plotRange = plotRanges[[i]];
    Print["\n", StringRepeat["-", 60]];
    CreateOrbitAnimation[label, e, l, M, nRevs, plotRange]
  ],
  {i, 1, Length[paramSets]}
];

(* =======================================================================*)
(*MOSTRAR ANIMACIONES EN UNA CUADRÍCULA*)
(* =======================================================================*)

Print["\n" <> StringRepeat["=", 70]];
Print["MOSTRANDO ANIMACIONES EN CUADRÍCULA"];
Print["(Cada animación tiene controles independientes de reproducción)"];

(*Mostrar las tres animaciones en una cuadrícula*)
Grid[
  {
    {animations[[1]], animations[[2]]},
    {animations[[3]], 
     Graphics[
       {
         Text[Style["CONTROLES DE ANIMACIÓN", 14, Bold, Blue], {0, 0.3}],
         Text[Style["► Play/Pause: Reproducir/Pausar", 12], {0, 0.1}],
         Text[Style["◄► Slider: Control manual", 12], {0, -0.1}],
         Text[Style["⏩ Faster/Slower: Velocidad", 12], {0, -0.3}],
         Text[Style["🔄 Direction: Sentido", 12], {0, -0.5}]
       },
       ImageSize -> 500,
       PlotRange -> {{-1, 1}, {-1, 1}}
     ]
    }
  },
  Spacings -> {30, 30},
  Alignment -> Center
]

(* =======================================================================*)
(*ANIMACIÓN COMPARATIVA SIMULTÁNEA (OPCIONAL)*)
(* =======================================================================*)

Print["\n" <> StringRepeat["=", 70]];
Print["CREANDO ANIMACIÓN COMPARATIVA SIMULTÁNEA"];
Print["(Las tres órbitas animadas al mismo tiempo)"];

(*Preparar datos para animación comparativa*)
orbitDataList = Table[
  {label, e, l, M, nRevs} = paramSets[[i]];
  {orbitData, rHorizon, rCritical, phiPerRev, precession} = 
    CalcOrbitDataForAnimation[e, l, M, 3]; (*Solo 3 rev para comparación*)
  {label, orbitData, rHorizon, rCritical, precession},
  {i, 1, Length[paramSets]}
];

(* =======================================================================*)
(*ANIMACIÓN COMPARATIVA SIMULTÁNEA CON RANGOS AJUSTADOS*)
(* =======================================================================*)

Print["\n" <> StringRepeat["=", 70]];
Print["CREANDO ANIMACIÓN COMPARATIVA SIMULTÁNEA"];
Print["(Las tres órbitas animadas al mismo tiempo con rangos ajustados)"];

(*Preparar datos para animación comparativa con rangos individuales*)
orbitDataWithRanges = Table[
  Module[{label, e, l, M, orbitData, rHorizon, rCritical, 
    phiPerRev, precession, plotRangeIndividual, maxR},
    
    {label, e, l, M, nRevs} = paramSets[[i]];
    {orbitData, rHorizon, rCritical, phiPerRev, precession} = 
      CalcOrbitDataForAnimation[e, l, M, 3]; (*3 revoluciones para comparación*)
    
    (*Calcular rango individual basado en la órbita*)
    (*Encontrar el valor máximo absoluto en x e y*)
    allX = orbitData[[All, 1]];
    allY = orbitData[[All, 2]];
    maxX = Max[Abs[allX]];
    maxY = Max[Abs[allY]];
    maxR = Max[maxX, maxY]*1.2; (*Añadir 20% de margen*)
    
    (*Rangos individuales para cada órbita*)
    plotRangeIndividual = {{-maxR, maxR}, {-maxR, maxR}};
    
    {label, orbitData, rHorizon, rCritical, plotRangeIndividual}
  ],
  {i, 1, Length[paramSets]}
];

(*Mostrar los rangos calculados*)
Print["\nRangos individuales calculados:"];
Do[
  Print[orbitDataWithRanges[[i, 1]], ": ", 
    NumberForm[orbitDataWithRanges[[i, 5, 1, 2]], 4], " M"];,
  {i, 1, Length[orbitDataWithRanges]}
];

(*Crear animación comparativa con rangos ajustados*)
comparativeAnimationAdjusted = Animate[
  GraphicsGrid[
    Table[
      Module[{label, orbitData, rHorizon, rCritical, 
        plotRangeIndiv, nPoints, currentData},
        
        {label, orbitData, rHorizon, rCritical, plotRangeIndiv} = 
          orbitDataWithRanges[[i]];
        nPoints = Length[orbitData];
        currentData = Take[orbitData, Floor[currentTime*nPoints]];
        
        Show[
          (*Fondo con elementos fijos*)
          Graphics[
            {
              (*Órbita completa en gris*)
              LightGray, Opacity[0.15], Thickness[0.001], Line[orbitData],
              
              (*Horizonte de eventos*)
              Black, Opacity[0.1], Disk[{0, 0}, rHorizon],
              
              (*Órbita circular crítica*)
              Red, Opacity[0.2], Dashed, Thickness[0.0015], 
              Circle[{0, 0}, rCritical],
              
              (*Centro*)
              Black, PointSize[0.015], Point[{0, 0}]
            }
          ],
          
          (*Trayectoria recorrida hasta ahora*)
          Graphics[
            {
              (*Color diferente para cada órbita*)
              Switch[i,
                1, RGBColor[0.2, 0.4, 0.8],  (*Azul para (a)*)
                2, RGBColor[0.8, 0.4, 0.2],  (*Naranja para (b)*)
                3, RGBColor[0.4, 0.8, 0.2]   (*Verde para (c)*)
              ],
              Thickness[0.006], CapForm["Round"],
              Line[currentData]
            }
          ],
          
          (*Punto actual*)
          Graphics[
            {
              EdgeForm[Black],
              Switch[i,
                1, RGBColor[0.1, 0.3, 0.9],  (*Azul oscuro*)
                2, RGBColor[0.9, 0.3, 0.1],  (*Rojo anaranjado*)
                3, RGBColor[0.1, 0.9, 0.3]   (*Verde brillante*)
              ],
              Disk[If[Length[currentData] > 0, currentData[[-1]], {0, 0}], 
                0.03*plotRangeIndiv[[1, 2]]] (*Tamaño proporcional al rango*)
            }
          ],
          
          (*Etiquetas y información*)
          Graphics[
            {
              (*Etiqueta de la órbita*)
              Text[Style[label, 11, Bold, Black], Scaled[{0.05, 0.93}]],
              
              (*Radio máximo*)
              Text[Style[
                StringForm["Rango: ±`1`M", plotRangeIndiv[[1, 2]]], 
                8, Gray], Scaled[{0.05, 0.05}]],
              
              (*Contador de revolución*)
              Text[Style[
                StringForm["Rev: `1`", currentTime*nPoints/(nPoints/3)], 
                9, Darker[Gray]], Scaled[{0.85, 0.93}]]
            }
          ],
          
          (*Configuración del gráfico*)
          Frame -> True,
          FrameLabel -> If[i == 1 || i == 3, 
            {{"y/M", None}, {"x/M", None}}, 
            {{None, None}, {"x/M", None}}],
          FrameStyle -> If[i == 1 || i == 3, 
            Thickness[0.002], 
            Directive[Thickness[0.002], Opacity[0.5]]],
          PlotRange -> plotRangeIndiv,
          AspectRatio -> 1,
          ImageSize -> 280,
          GridLines -> Automatic,
          GridLinesStyle -> Directive[Gray, Dotted, Opacity[0.1]]
        ]
      ],
      {i, 1, 3}
    ],
    Spacings -> {5, 5}
  ],
  
  (*Control de animación*)
  {currentTime, 0, 1, 0.005},
  
  (*Opciones de animación*)
  AnimationRunning -> False,
  AnimationRepetitions -> 1,
  DefaultDuration -> 25,
  ControlPlacement -> Bottom,
  AppearanceElements -> {"ProgressSlider", "PlayPauseButton", 
    "FasterSlowerButtons"},
  
  (*Etiqueta del control*)
  Paneled -> True,
  FrameLabel -> {{None, None}, {None, 
    Style["Animación Comparativa - Tiempo Normalizado", 12, Bold]}}
];

(*Mostrar animación comparativa*)
Print["\nAnimación comparativa con rangos ajustados:"];
comparativeAnimationAdjusted
```

---

<p align="center"><img src ="time-like geodesic 7 (a).png" /></p>
<p align="center"><img src ="time-like geodesic 7 (b).png" /></p>
<p align="center"><img src ="time-like geodesic 7 (c).png" /></p>

<p align="center"><img src ="time-like geodesic 7(a)-2.png" /></p>

<p align="center"><img src ="time-like geodesic 7(b)-2.png" /></p>

<p align="center"><img src ="time-like geodesic 7 (c)-2.png" /></p>


## Explicación del Código

### Contexto Físico

Este código implementa la visualización y animación de **órbitas relativistas alrededor de un agujero negro** descritas por la métrica de Schwarzschild. A diferencia de las órbitas newtonianas (elipses cerradas), las órbitas relativistas presentan **precesión del perihelio** - el punto más cercano al agujero negro se desplaza gradualmente, creando un patrón en forma de roseta.

### Estructura General

El código está organizado en secciones claramente delimitadas que van desde la definición de parámetros hasta la creación de animaciones comparativas complejas.

---

### 1. Parámetros de las Órbitas (líneas 6-15)

Define tres configuraciones orbitales diferentes en la variable `paramSets`:
```mathematica
paramSets = {
  {"(a) e=0.5, l=11, M=3/14", 0.5, 11.0, 3/14, 4},
  {"(b) e=0.5, l=7.5, M=3/14", 0.5, 7.5, 3/14, 6},
  {"(c) e=0.5, l=3, M=3/14", 0.5, 3.0, 3/14, 10}
};
```

Cada fila contiene:
- **Etiqueta descriptiva**: identificación de la órbita
- **e = 0.5**: excentricidad (todas las órbitas son elípticas)
- **l**: momento angular específico (11, 7.5 y 3)
- **M = 3/14**: masa del agujero negro
- **Número de revoluciones**: cuántas vueltas animar (4, 6 y 10)

**Física**: El momento angular `l` controla qué tan cerca pasa la órbita del agujero negro. Valores menores de `l` implican órbitas más cercanas y mayor efecto relativista.

---

### 2. Funciones Auxiliares (líneas 17-36)

#### CalcMu[M_, l_]
Calcula el parámetro adimensional μ = M/l, que mide la intensidad del campo gravitacional relativista.

#### CalcA[mu_, e_]
Calcula A = 1 - 6μ + 2μe, un parámetro clave en la solución de las ecuaciones de movimiento relativistas.

#### Calck2[mu_, e_, A_]
Calcula k² = 4μe/A, el parámetro modular de las integrales elípticas que aparecen en la solución analítica.

#### PhiFromChi[chi_, A_, k2_]
**Función central**: Implementa la ecuación (132) que relaciona el ángulo verdadero χ con el ángulo azimutal φ:

φ = (2/√A) · F(π/2 - χ/2, k)

donde F es la **integral elíptica de primera especie**. Esta función es clave para resolver las órbitas relativistas analíticamente.

---

### 3. Cálculo de Datos Orbitales (líneas 38-85)

#### CalcOrbitDataForAnimation[e_, l_, M_, nRevolutions_]

Esta función realiza el cálculo completo de una órbita:

**Paso 1: Cálculo de parámetros** (líneas 43-45)
- Calcula μ, A y k²
- Valida que los parámetros sean físicamente válidos (A > 0, k² ≥ 0)

**Paso 2: Una revolución completa** (líneas 52-58)
- Genera 200 puntos de χ desde π (afelio) hasta -π (siguiente afelio)
- Calcula φ para cada χ usando la integral elíptica
- Determina el ángulo recorrido por revolución y la precesión

**Paso 3: Múltiples revoluciones** (líneas 60-69)
- Concatena múltiples revoluciones aplicando offsets apropiados
- Cada revolución adiciona la precesión acumulada al ángulo φ

**Paso 4: Conversión a coordenadas cartesianas** (líneas 71-79)
- Usa la ecuación de órbita elíptica: r = l/(1 + e·cos(χ))
- Convierte (r, φ) a coordenadas cartesianas (x, y)
- Ajusta φ para que empiece en 0

**Salida** (líneas 81-85)
Retorna:
- `orbitData`: array de puntos [x, y]
- `rHorizon = 2M`: radio del horizonte de eventos
- `rCritical = 3M`: radio de la órbita circular fotónica
- `phiPerRev`: ángulo recorrido por revolución
- `precession`: precesión del perihelio por revolución

---

### 4. Función de Animación Individual (líneas 87-206)

#### CreateOrbitAnimation[label_, e_, l_, M_, nRevs_, plotRange_]

Crea una animación interactiva completa con múltiples capas visuales.

**Preparación** (líneas 92-109)
- Calcula todos los datos orbitales
- Imprime información diagnóstica (μ, k², perihelio, afelio, precesión)
- Valida que los datos sean correctos

**Estructura de la animación** (líneas 111-195)

La función `Animate` controla una variable `currentFraction` (0 a 1) que representa el progreso de la animación. Se construyen 7 capas gráficas superpuestas:

**Capa 1: Órbita completa** (líneas 114-117)
- Muestra la trayectoria completa en gris muy claro
- Sirve como guía visual de referencia

**Capa 2: Trayectoria recorrida** (líneas 119-125)
- Dibuja la porción de órbita ya recorrida
- Usa colores del arcoíris que cambian con el progreso
- `Take[orbitData, Floor[currentFraction*nPoints]]` selecciona solo los puntos recorridos

**Capa 3: Partícula en movimiento** (líneas 127-134)
- Disco amarillo que representa la partícula/planeta
- Punto negro central para mejor visibilidad
- Se posiciona en `orbitData[[Floor[currentFraction*nPoints]]]`

**Capa 4: Línea radial** (líneas 136-141)
- Línea punteada desde el centro hasta la partícula
- Ayuda a visualizar la distancia radial variable

**Capa 5: Elementos de referencia fijos** (líneas 143-165)
- **Horizonte de eventos** (disco negro semi-transparente en r = 2M)
- **Órbita circular crítica** (círculo rojo punteado en r = 3M) - la órbita circular más interna posible para luz
- **Centro del agujero negro** (punto negro)
- **Círculos de perihelio y afelio** (verde y azul) para mostrar distancias extremas
- **Marcadores** en perihelio (rojo) y afelio (verde)

**Capa 6: Panel de información** (líneas 167-187)
- Rectángulo semi-transparente en esquina inferior izquierda
- Muestra dinámicamente:
  - Etiqueta de la órbita
  - Número de revolución actual
  - Porcentaje de progreso
  - Precesión por revolución en grados

**Capa 7: Indicador angular** (líneas 189-196)
- Arco que crece con el tiempo
- Flecha que rota mostrando la posición angular actual
- Visualiza el ángulo φ recorrido

**Configuración del gráfico** (líneas 198-209)
- Marco con etiquetas "x/M" e "y/M"
- Rango de graficación ajustable
- Relación de aspecto 1:1