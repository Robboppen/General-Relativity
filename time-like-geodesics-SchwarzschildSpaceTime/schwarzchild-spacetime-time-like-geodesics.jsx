import React, { useRef, useEffect, useState } from 'react';
import * as THREE from 'three';

const SchwarzschildGeodesics3D = () => {
  const mountRef = useRef(null);
  const [selectedOrbit, setSelectedOrbit] = useState('all');
  const [isAnimating, setIsAnimating] = useState(true);
  const [animationSpeed, setAnimationSpeed] = useState(1);
  const [showInfo, setShowInfo] = useState(true);
  const sceneRef = useRef(null);
  const rendererRef = useRef(null);
  const animationFrameRef = useRef(null);

  // Parámetros de las órbitas (del código Mathematica)
  const orbits = {
    a: { label: '(a) e=0.5, l=11, M=3/14', e: 0.5, l: 11.0, M: 3/14, color: 0x4080ff, nRevs: 4 },
    b: { label: '(b) e=0.5, l=7.5, M=3/14', e: 0.5, l: 7.5, M: 3/14, color: 0xff8040, nRevs: 6 },
    c: { label: '(c) e=0.5, l=3, M=3/14', e: 0.5, l: 3.0, M: 3/14, color: 0x40ff80, nRevs: 10 }
  };

  // Funciones auxiliares del código Mathematica
  const calcMu = (M, l) => M / l;
  
  const calcA = (mu, e) => 1 - 6 * mu + 2 * mu * e;
  
  const calcK2 = (mu, e, A) => A !== 0 ? (4 * mu * e) / A : 1.0;
  
  // Aproximación de la integral elíptica F(φ, k²)
  const ellipticF = (phi, k2) => {
    const steps = 100;
    const dt = phi / steps;
    let sum = 0;
    
    for (let i = 0; i < steps; i++) {
      const t = (i + 0.5) * dt;
      const integrand = 1 / Math.sqrt(1 - k2 * Math.sin(t) ** 2);
      sum += integrand * dt;
    }
    
    return sum;
  };
  
  const phiFromChi = (chi, A, k2) => {
    if (A <= 0) return 0;
    const sqrtA = Math.sqrt(A);
    const psi = Math.PI / 2 - chi / 2;
    return (2 * ellipticF(psi, k2)) / sqrtA;
  };

  // Calcular datos de órbita
  const calcOrbitData = (e, l, M, nRevolutions) => {
    const mu = calcMu(M, l);
    const A = calcA(mu, e);
    const k2 = calcK2(mu, e, A);
    
    if (A <= 0 || k2 < 0) {
      return { points: [], valid: false };
    }
    
    const pointsPerRev = 200;
    const allPoints = [];
    
    // Generar múltiples revoluciones
    for (let rev = 0; rev < nRevolutions; rev++) {
      const chiOffset = rev * 2 * Math.PI;
      
      for (let i = 0; i < pointsPerRev; i++) {
        const chi = Math.PI - (2 * Math.PI * i) / pointsPerRev - chiOffset;
        const phi = phiFromChi(chi, A, k2);
        const r = l / (1 + e * Math.cos(chi));
        
        // Convertir a coordenadas cartesianas
        const x = r * Math.cos(phi);
        const y = r * Math.sin(phi);
        const z = rev * 0.5; // Separación vertical para visualización 3D
        
        allPoints.push(new THREE.Vector3(x, y, z));
      }
    }
    
    return {
      points: allPoints,
      valid: true,
      rHorizon: 2 * M,
      rCritical: 3 * M,
      perihelion: l / (1 + e),
      aphelion: l / (1 - e),
      mu,
      A,
      k2
    };
  };

  useEffect(() => {
    if (!mountRef.current) return;

    // Configuración de la escena
    const scene = new THREE.Scene();
    scene.background = new THREE.Color(0x0a0a0a);
    sceneRef.current = scene;

    const camera = new THREE.PerspectiveCamera(
      45,
      mountRef.current.clientWidth / mountRef.current.clientHeight,
      0.1,
      1000
    );
    camera.position.set(30, 25, 40);
    camera.lookAt(0, 0, 0);

    const renderer = new THREE.WebGLRenderer({ antialias: true });
    renderer.setSize(mountRef.current.clientWidth, mountRef.current.clientHeight);
    rendererRef.current = renderer;
    mountRef.current.appendChild(renderer.domElement);

    // Iluminación
    const ambientLight = new THREE.AmbientLight(0xffffff, 0.4);
    scene.add(ambientLight);

    const pointLight1 = new THREE.PointLight(0xffffff, 0.8);
    pointLight1.position.set(20, 20, 20);
    scene.add(pointLight1);

    const pointLight2 = new THREE.PointLight(0x8888ff, 0.4);
    pointLight2.position.set(-20, -10, 20);
    scene.add(pointLight2);

    // Agujero negro (esfera central)
    const blackHoleGeometry = new THREE.SphereGeometry(0.8, 32, 32);
    const blackHoleMaterial = new THREE.MeshPhongMaterial({
      color: 0x000000,
      emissive: 0x111111,
      shininess: 100
    });
    const blackHole = new THREE.Mesh(blackHoleGeometry, blackHoleMaterial);
    scene.add(blackHole);

    // Horizonte de eventos (primera órbita como referencia)
    const horizonGeometry = new THREE.SphereGeometry(
      2 * orbits.a.M,
      32,
      32,
      0,
      Math.PI * 2,
      0,
      Math.PI
    );
    const horizonMaterial = new THREE.MeshBasicMaterial({
      color: 0x222222,
      transparent: true,
      opacity: 0.15,
      side: THREE.DoubleSide
    });
    const horizon = new THREE.Mesh(horizonGeometry, horizonMaterial);
    scene.add(horizon);

    // Ejes de referencia
    const axesHelper = new THREE.AxesHelper(15);
    scene.add(axesHelper);

    // Grid de referencia
    const gridHelper = new THREE.GridHelper(50, 20, 0x444444, 0x222222);
    gridHelper.position.y = -0.5;
    scene.add(gridHelper);

    // Crear órbitas
    const orbitObjects = {};
    Object.entries(orbits).forEach(([key, orbit]) => {
      const data = calcOrbitData(orbit.e, orbit.l, orbit.M, orbit.nRevs);
      
      if (!data.valid || data.points.length === 0) return;

      // Crear curva de la órbita
      const curve = new THREE.CatmullRomCurve3(data.points);
      const tubeGeometry = new THREE.TubeGeometry(curve, data.points.length * 2, 0.08, 8, false);
      const tubeMaterial = new THREE.MeshPhongMaterial({
        color: orbit.color,
        transparent: true,
        opacity: 0.7,
        emissive: orbit.color,
        emissiveIntensity: 0.3
      });
      const tube = new THREE.Mesh(tubeGeometry, tubeMaterial);
      scene.add(tube);

      // Partícula que se mueve por la órbita
      const particleGeometry = new THREE.SphereGeometry(0.3, 16, 16);
      const particleMaterial = new THREE.MeshPhongMaterial({
        color: orbit.color,
        emissive: orbit.color,
        emissiveIntensity: 0.5
      });
      const particle = new THREE.Mesh(particleGeometry, particleMaterial);
      scene.add(particle);

      // Trazo de la trayectoria
      const trailGeometry = new THREE.BufferGeometry();
      const trailMaterial = new THREE.LineBasicMaterial({
        color: orbit.color,
        linewidth: 2,
        transparent: true,
        opacity: 0.5
      });
      const trail = new THREE.Line(trailGeometry, trailMaterial);
      scene.add(trail);

      orbitObjects[key] = {
        tube,
        particle,
        trail,
        data,
        currentIndex: 0,
        trailPoints: []
      };
    });

    // Animación
    let time = 0;
    const animate = () => {
      animationFrameRef.current = requestAnimationFrame(animate);

      if (isAnimating) {
        time += 0.003 * animationSpeed;

        Object.entries(orbitObjects).forEach(([key, obj]) => {
          const orbit = orbits[key];
          const shouldShow = selectedOrbit === 'all' || selectedOrbit === key;
          
          obj.tube.visible = shouldShow;
          obj.particle.visible = shouldShow;
          obj.trail.visible = shouldShow;

          if (!shouldShow) return;

          // Actualizar posición de la partícula
          const index = Math.floor((time * obj.data.points.length) % obj.data.points.length);
          obj.currentIndex = index;
          
          const point = obj.data.points[index];
          obj.particle.position.copy(point);

          // Actualizar trazo
          obj.trailPoints.push(point.clone());
          if (obj.trailPoints.length > 100) {
            obj.trailPoints.shift();
          }
          
          const positions = new Float32Array(obj.trailPoints.length * 3);
          obj.trailPoints.forEach((p, i) => {
            positions[i * 3] = p.x;
            positions[i * 3 + 1] = p.y;
            positions[i * 3 + 2] = p.z;
          });
          
          obj.trail.geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
          obj.trail.geometry.attributes.position.needsUpdate = true;
        });
      }

      // Rotación suave de la cámara
      const radius = 50;
      camera.position.x = radius * Math.cos(time * 0.1);
      camera.position.z = radius * Math.sin(time * 0.1);
      camera.lookAt(0, 0, 0);

      renderer.render(scene, camera);
    };

    animate();

    // Manejo de redimensionamiento
    const handleResize = () => {
      if (!mountRef.current) return;
      camera.aspect = mountRef.current.clientWidth / mountRef.current.clientHeight;
      camera.updateProjectionMatrix();
      renderer.setSize(mountRef.current.clientWidth, mountRef.current.clientHeight);
    };
    window.addEventListener('resize', handleResize);

    return () => {
      window.removeEventListener('resize', handleResize);
      if (animationFrameRef.current) {
        cancelAnimationFrame(animationFrameRef.current);
      }
      if (mountRef.current && renderer.domElement) {
        mountRef.current.removeChild(renderer.domElement);
      }
      renderer.dispose();
    };
  }, [selectedOrbit, isAnimating, animationSpeed]);

  return (
    <div className="w-full h-screen bg-gray-900 relative">
      <div ref={mountRef} className="w-full h-full" />
      
      {/* Panel de control */}
      <div className="absolute top-4 left-4 bg-gray-800 bg-opacity-90 text-white p-4 rounded-lg shadow-lg max-w-xs">
        <h2 className="text-xl font-bold mb-3 text-blue-400">
          Geodésicas de Schwarzschild
        </h2>
        
        <div className="space-y-3">
          <div>
            <label className="block text-sm font-medium mb-1">Órbita:</label>
            <select
              value={selectedOrbit}
              onChange={(e) => setSelectedOrbit(e.target.value)}
              className="w-full bg-gray-700 text-white px-3 py-2 rounded border border-gray-600"
            >
              <option value="all">Todas</option>
              <option value="a">(a) e=0.5, l=11</option>
              <option value="b">(b) e=0.5, l=7.5</option>
              <option value="c">(c) e=0.5, l=3</option>
            </select>
          </div>

          <div>
            <label className="block text-sm font-medium mb-1">
              Velocidad: {animationSpeed.toFixed(1)}x
            </label>
            <input
              type="range"
              min="0.1"
              max="3"
              step="0.1"
              value={animationSpeed}
              onChange={(e) => setAnimationSpeed(parseFloat(e.target.value))}
              className="w-full"
            />
          </div>

          <button
            onClick={() => setIsAnimating(!isAnimating)}
            className="w-full bg-blue-600 hover:bg-blue-700 text-white font-medium py-2 px-4 rounded transition"
          >
            {isAnimating ? '⏸ Pausar' : '▶ Reproducir'}
          </button>

          <button
            onClick={() => setShowInfo(!showInfo)}
            className="w-full bg-gray-600 hover:bg-gray-700 text-white font-medium py-2 px-4 rounded transition"
          >
            {showInfo ? '👁️ Ocultar info' : '👁️ Mostrar info'}
          </button>
        </div>
      </div>

      {/* Panel de información */}
      {showInfo && (
        <div className="absolute bottom-4 right-4 bg-gray-800 bg-opacity-90 text-white p-4 rounded-lg shadow-lg max-w-md">
          <h3 className="text-lg font-bold mb-2 text-green-400">
            Información física
          </h3>
          <div className="text-sm space-y-1">
            <p><span className="text-blue-400">●</span> Órbita (a): l=11M, más exterior</p>
            <p><span className="text-orange-400">●</span> Órbita (b): l=7.5M, intermedia</p>
            <p><span className="text-green-400">●</span> Órbita (c): l=3M, más cercana al horizonte</p>
            <p className="mt-2 text-gray-300">
              • Todas con excentricidad e=0.5
            </p>
            <p className="text-gray-300">
              • M = 3/14 (masa del agujero negro)
            </p>
            <p className="text-gray-300">
              • Precesión orbital visible (efecto relativista)
            </p>
          </div>
        </div>
      )}

      {/* Leyenda de colores */}
      <div className="absolute top-4 right-4 bg-gray-800 bg-opacity-90 text-white p-3 rounded-lg shadow-lg">
        <h3 className="text-sm font-bold mb-2">Colores:</h3>
        <div className="space-y-1 text-xs">
          <div className="flex items-center gap-2">
            <div className="w-4 h-4 bg-blue-500 rounded"></div>
            <span>(a) l=11M</span>
          </div>
          <div className="flex items-center gap-2">
            <div className="w-4 h-4 bg-orange-500 rounded"></div>
            <span>(b) l=7.5M</span>
          </div>
          <div className="flex items-center gap-2">
            <div className="w-4 h-4 bg-green-500 rounded"></div>
            <span>(c) l=3M</span>
          </div>
        </div>
      </div>
    </div>
  );
};

export default SchwarzschildGeodesics3D;