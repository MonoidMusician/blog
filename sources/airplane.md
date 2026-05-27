::: {.widget .full-width widget="Airplane" widget-datakey="default"}
:::

<script type="importmap">
  {
    "imports": {
      "three": "./node_modules/three/build/three.module.js"
    }
  }
</script>

<script type="module">
  import * as THREE from 'three';
  import { OrbitControls } from './node_modules/three/examples/jsm/controls/OrbitControls.js';
  import { MTLLoader } from './node_modules/three/examples/jsm/loaders/MTLLoader.js';
  import { OBJLoader, LineMaterial } from './node_modules/three/examples/jsm/loaders/OBJLoader.js';
  //import Stats from './node_modules/three/examples/jsm/libs/stats.module.js';

  const _installSideChannel = name => cb => () => {
    if (typeof window.sideChannel === 'undefined') window.sideChannel = {};
    window.sideChannel[name] = cb;
  };
  _installSideChannel("render3d")(({ element, mesh }) => () => {
    let object = objLoader.parse(mesh);
    object.children[0].material = material;
    object.traverse(function (child) {
      if (child.isLineSegments || child.isLineSegments2 || child.type === 'LineSegments2') {
        child.material = lineMat;
      } else if (child.isMesh) {
        child.material = material;
      }
      console.log(child, child.material?.color);
    });
    scene.add(object);
    renderer.domElement.ondblclick = () => {
      navigator.clipboard.writeText(mesh);
    };
  })();

  const scene = new THREE.Scene();
  scene.add(new THREE.AxesHelper(5));

  const light = new THREE.PointLight(0xffffff, 1000);
  light.position.set(2.5, 7.5, 15);
  scene.add(light);

  const camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);
  camera.position.z = -10;

  const renderer = new THREE.WebGLRenderer({ antialias: true });
  renderer.setSize(window.innerWidth, window.innerHeight);
  renderer.domElement.classList.add('full-width');
  document.body.appendChild(renderer.domElement);

  const controls = new OrbitControls(camera, renderer.domElement);
  controls.enableDamping = true;

  //const material = new THREE.MeshBasicMaterial({ color: 0x001000, wireframe: false, opacity: 0.45, transparent: true });
  const material = new THREE.MeshBasicMaterial({ color: 0xff50aa, wireframe: false, opacity: 0.85, transparent: true });
  const lineMat = new LineMaterial({ color: 0xff50aa, linewidth: 2 });
  console.log(lineMat, lineMat.color);
  // const materials = await new MTLLoader().loadAsync('3d.mtl');

  const objLoader = new OBJLoader();
  //objLoader.setMaterials(materials);
  //console.log(materials);

  window.addEventListener('resize', onWindowResize, false);
  function onWindowResize() {
    let pct = 1.5;
    camera.aspect = window.innerWidth / (window.innerHeight / pct);
    camera.updateProjectionMatrix();
    renderer.setSize(window.devicePixelRatio * window.innerWidth, window.devicePixelRatio * (window.innerHeight / pct));
    Object.assign(renderer.domElement.style, {
      width: window.innerWidth + 'px',
      height: window.innerHeight / pct + 'px',
    });
    render();
  }
  //const stats = new Stats();
  //document.body.appendChild(stats.dom);

  function animate() {
    requestAnimationFrame(animate);
    controls.update();
    render();
    //stats.update();
  }

  function render() {
    renderer.render(scene, camera);
  }

  onWindowResize();
  requestAnimationFrame(animate);
</script>
