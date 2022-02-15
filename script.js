// Proudly powered by - Vanilla JavaScript.

"use strict";

window.addEventListener("load",function() {

  const rotSpeed = 0.5 / 1000; // radians / ms

  let canv, ctx;    // canvas and context
  let maxx, maxy;   // canvas dimensions
  let perspective, solids, solid;

  let dRot1, dRot2;
  let xc, yc;
  let ang1 = 0, ang2 = 0;
  let lightDir;

  let globRot = [1,0,0, 0,1,0, 0,0,1]; // global rotation matrix
// for animation
  let ui, uiv;
  let events, mouseEvents;

// shortcuts for Math.
  const mrandom = Math.random;
  const mfloor = Math.floor;
  const mround = Math.round;
  const mceil = Math.ceil;
  const mabs = Math.abs;
  const mmin = Math.min;
  const mmax = Math.max;

  const mPI = Math.PI;
  const mPIS2 = Math.PI / 2;
  const mPIS3 = Math.PI / 3;
  const m2PI = Math.PI * 2;
  const m2PIS3 = Math.PI * 2 / 3;
  const msin = Math.sin;
  const mcos = Math.cos;
  const mtan = Math.tan;
  const matan2 = Math.atan2;

  const mhypot = Math.hypot;
  const msqrt = Math.sqrt;

  const rac3   = msqrt(3);
  const rac3s2 = rac3 / 2;

//------------------------------------------------------------------------

function alea (mini, maxi) {
// random number in given range

  if (typeof(maxi) == 'undefined') return mini * mrandom(); // range 0..mini

  return mini + mrandom() * (maxi - mini); // range mini..maxi
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function intAlea (mini, maxi) {
// random integer in given range (mini..maxi - 1 or 0..mini - 1)
//
  if (typeof(maxi) == 'undefined') return mfloor(mini * mrandom()); // range 0..mini - 1
  return mini + mfloor(mrandom() * (maxi - mini)); // range mini .. maxi - 1
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

function Noise1DOneShot (period, min = 0, max = 1, random) {
/* returns a 1D single-shot noise generator.
   the (optional) random function must return a value between 0 and 1
  the returned function has no parameter, and will return a new number every tiime it is called.
  If the random function provides reproductible values (and is not used elsewhere), this
  one will return reproductible values too.
  period should be > 1. The bigger period is, the smoother output noise is
*/
  random = random || Math.random;
  let currx = random(); // start with random offset
  let y0 = min + (max - min) * random(); // 'previous' value
  let y1 = min + (max - min) * random(); // 'next' value
  let dx = 1 / period;

  return function() {
    currx += dx;
    if (currx > 1) {
      currx -= 1;
      y0 = y1;
      y1 = min + (max - min) * random();
    }
    let z = (3 - 2 * currx) * currx * currx;
    return z * y1 + (1 - z) * y0;
  }
} // Noise1DOneShot

//------------------------------------------------------------------------
function isDirect2(p0, p1, p2) {
  return ((p0[0] - p1[0]) * (p2[1] - p1[1]) - (p0[1] - p1[1]) * (p2[0] - p1[0])) <= 0;
}

//------------------------------------------------------------------------

function lerp2( p1, p2, alpha) {
  const umalpha = 1 - alpha;
  return [p1[0] * umalpha + p2[0] * alpha,
          p1[1] * umalpha + p2[1] * alpha];
} // lerp2

//------------------------------------------------------------------------
const Ar3 = {
  normalize : function(ar3) {
      const lng = mhypot(...ar3); // hope this is != 0
      return [ar3[0] / lng, ar3[1] / lng, ar3[2] / lng];
    },
  dotProduct : function(ar3a, ar3b) {
    return ar3a[0] * ar3b[0] + ar3a[1] * ar3b[1] + ar3a[2] * ar3b[2];
  },
  lerp : function(ar3a, ar3b, alpha) {
    const umalpha = 1 - alpha;
    return [ar3a[0] * umalpha + ar3b[0] * alpha,
            ar3a[1] * umalpha + ar3b[1] * alpha,
            ar3a[2] * umalpha + ar3b[2] * alpha];
  }

} //

//-----------------------------------------------------------------------------
/* matrices given a 9 elements array in order :
  0 1 2
  3 4 5
  6 7 8
*/

function mat33Prod(mat1, mat2) {

  return [mat1[0] * mat2[0] + mat1[1] * mat2[3] + mat1[2] * mat2[6],
          mat1[0] * mat2[1] + mat1[1] * mat2[4] + mat1[2] * mat2[7],
          mat1[0] * mat2[2] + mat1[1] * mat2[5] + mat1[2] * mat2[8],
          mat1[3] * mat2[0] + mat1[4] * mat2[3] + mat1[5] * mat2[6],
          mat1[3] * mat2[1] + mat1[4] * mat2[4] + mat1[5] * mat2[7],
          mat1[3] * mat2[2] + mat1[4] * mat2[5] + mat1[5] * mat2[8],
          mat1[6] * mat2[0] + mat1[7] * mat2[3] + mat1[8] * mat2[6],
          mat1[6] * mat2[1] + mat1[7] * mat2[4] + mat1[8] * mat2[7],
          mat1[6] * mat2[2] + mat1[7] * mat2[5] + mat1[8] * mat2[8]];
}
//-----------------------------------------------------------------------------

function createPerspective3 (pcam, th2, resx, resy) {
/* pcam : array of 3 coordinates, position of the camera
  pLookAt : removed - always looking at (0,0,0)
  th2 (tangent of half angle of the screen seen by the camera)
resx, resy : number of pixels of the screen
*/
  const resx2 = resx / 2;
  const resy2 = resy / 2;
  const proj = resx2 / th2;
  const D = mhypot(pcam[0], pcam[1], pcam[2]);
  const X = pcam[0] / D;
  const Y = pcam[1] / D;
  const Z = pcam[2] / D;
  const m11 = msqrt(1 - Y * Y); // Cx /!\ Y= + / - 1 => Cx = 0
  const m00 = Z / m11;  // Cy
  const m02 = - X / m11;  // -Sy
  const m10 = Y * m02; // -Y.Sy
  const m12 = -Y * m00; // -Y.Cy
  const m20 = X;
  const m21 = Y;
  const m22 = Z;

  function rotatePoint (point) {
// rotation for camera position
    return [m00 * point[0] + m02 * point[2],
            m10 * point[0] + m11 * point[1] + m12 * point[2],
            m20 * point[0] + m21 * point[1] + m22 * point[2]];
  } // rotatePoint

  function pointToScreen (point) {
// projection on canvas
      return [point[0] / (D - point[2]) * proj + resx2,
             - point[1] / (D - point[2]) * proj + resy2];
  } // pointToScreen

  function projection (spaceCoords) {

  // spaceCoords may be a single point or an array of points
    if (spaceCoords[0].length !== 3)             // single point
      return pointToScreen(rotatePoint(spaceCoords));
    else                     // array of points
      return spaceCoords.map(pt => pointToScreen(rotatePoint(pt)));
  }

  return {
    pcam: pcam,
    D: D, th2: th2, resx: resx, resy: resy,
    projection: projection
  }
} // createPerspective3

//------------------------------------------------------------------------
function Solid (vertices, faces) {

/* takes a list a vertices and of faces
vertices are given as array of three coordinates of points on a (0,0,0)
centered sphere. This constructor will normalize the radius of this sphere to 1
faces are given as arrays of 3 or more points, all oriented turning clockwise
(as seen from the sphere center)
*/
  if (vertices == undefined) return this;
  this.vertices = vertices.map(point => Ar3.normalize(point));
  this.faces = faces;
  this.calcFaceNormals();
// prepare arrays with elements for all division levels
  this.verticesD = [this.vertices];
  this.facesD = [this.faces];
  this.faceCentersD = [this.faceCenters];
  this.faceNormalsD = [this.faceNormals];
  for (let k = 1; k <= 4; ++k) {
    this.divide();
    this.verticesD.push(this.vertices);
    this.facesD.push(this.faces);
    this.faceCentersD.push(this.faceCenters);
    this.faceNormalsD.push(this.faceNormals);
  }
} // Solid

//------------------------------------------------------------------------
Solid.prototype.calcFaceNormals = function() {
// center of faces
// used (after normalization) as normals to faces

  this.faceCenters = [];
  this.faces.forEach (face =>{
    this.faceCenters.push (
      [face.reduce((sum, kvert) => sum + this.vertices[kvert][0], 0) / face.length,
       face.reduce((sum, kvert) => sum + this.vertices[kvert][1], 0) / face.length,
       face.reduce((sum, kvert) => sum + this.vertices[kvert][2], 0) / face.length]);
  });
// normalize vectors
  this.faceNormals = this.faceCenters.map(vec => Ar3.normalize(vec));
} //

//------------------------------------------------------------------------

Solid.prototype.divide = function() {

/* creates new vertices and faces arrays, by dividing existing faces into
 smaller faces and projecting corresponding vertices on the sphere.
The resulting faces are no longer equilateral, but this gives nice results */

  const nvertices = this.vertices.slice();
  const nfaces = [];
  let s0, s1, s2, s3, s4, s5;
  this.faces.forEach((face,k) => {
    if (this.faces[0].length == 3) {
      this.divideTriangle(face, nvertices, nfaces); // if triangle
    } else if (this.faces[0].length == 4) { 
      this.divideQuadrilateral(face, nvertices, nfaces, this.faceNormals[k]);
    } else {
      this.dividePentagon(face, nvertices, nfaces, this.faceNormals[k]);
    }

  });

  this.vertices = nvertices;
  this.faces = nfaces;
  this.calcFaceNormals();

} // divide

//------------------------------------------------------------------------

Solid.prototype.divideTriangle = function(face, nvertices, nfaces) {
/* creates new vertices and faces arrays, by dividing existing faces into
4 smaller faces and projecting them on the sphere.
The resulting faces are no longer equilateral, but this gives nice results
not optimal : vertices at the center of edges are duplicated!
Benchmarking shows this does not really matter, all the time is taken by graphic functions
*/

  const s0 = this.vertices[face[0]];
  const s1 = this.vertices[face[1]];
  const s2 = this.vertices[face[2]];
  const s3 = Ar3.normalize(Ar3.lerp(s0, s1, 0.5));
  const s4 = Ar3.normalize(Ar3.lerp(s1, s2, 0.5));
  const s5 = Ar3.normalize(Ar3.lerp(s2, s0, 0.5));
  const norgv = nvertices.length;
  nvertices.push(s3, s4, s5);
  nfaces.push([face[0], norgv, norgv + 2],      // 0 3 5
              [norgv, face[1], norgv + 1],  // 3 1 4
              [norgv + 2, norgv + 1, face[2]],  // 5 4 2
              [norgv + 1, norgv + 2, norgv]);   // 4 5 3

} // divide
//------------------------------------------------------------------------

Solid.prototype.divideQuadrilateral = function(face, nvertices, nfaces, center) {
/* creates new vertices and faces arrays, by dividing existing faces into
4 smaller faces and projecting them on the sphere.
Uses the faceNormal vector, pointing to the center of the polygon, and already normalized
The resulting faces are triangles */

  const s0 = this.vertices[face[0]];
  const s1 = this.vertices[face[1]];
  const s2 = this.vertices[face[2]];
  const s3 = this.vertices[face[3]];

  const norgv = nvertices.length;
  nvertices.push(center);
  nfaces.push([norgv, face[0], face[1]],
              [norgv, face[1], face[2]],
              [norgv, face[2], face[3]],
              [norgv, face[3], face[0]]);

} // divide
//------------------------------------------------------------------------

Solid.prototype.dividePentagon = function(face, nvertices, nfaces, center) {
/* creates new vertices and faces arrays, by dividing existing faces into
5 smaller faces 
Uses the faceNormal vector, pointing to the center of the polygon, and already normalized
The resulting faces are triangles */

  const s0 = this.vertices[face[0]];
  const s1 = this.vertices[face[1]];
  const s2 = this.vertices[face[2]];
  const s3 = this.vertices[face[3]];
  const s4 = this.vertices[face[4]];

  const norgv = nvertices.length;
  nvertices.push(center);
  nfaces.push([norgv, face[0], face[1]],
              [norgv, face[1], face[2]],
              [norgv, face[2], face[3]],
              [norgv, face[3], face[4]],
              [norgv, face[4], face[0]]);

} // divide
//------------------------------------------------------------------------

Solid.prototype.drawIt = function(dang1, dang2) {

  let pface, alpha, hue;

// work with current division level

  const vertices = this.verticesD[uiv.division];
  const faces = this.facesD[uiv.division];
  const faceNormals = this.faceNormalsD[uiv.division];
  const faceCenters = this.faceCentersD[uiv.division];
  const colors = this.colorsD[uiv.division];

  // rotation matrix coefficients
  let s1 = msin(dang1);
  let c1 = mcos(dang1);
  let s2 = msin(dang2);
  let c2 = mcos(dang2);

  let m00 = c2;
  let m01 = s2 * s1;
  let m02 = s2 * c1;
  let m11 = c1;
  let m12 = -s1;

  let m20 = -s2;
  let m21 = s1 * c2;
  let m22 = c1 * c2;

// 3D rotation of solid
  globRot = mat33Prod([m00,m01,m02, 0,m11,m12, m20,m21,m22], globRot);
  let projPoints = vertices.map(globRotate);
  let rotNormals = faceNormals.map(globRotate);

// perspective
  projPoints = perspective.projection(projPoints);

  let projCenters;
  if (uiv.hole) {
    let rotCenters = faceCenters.map(globRotate);
    projCenters = perspective.projection(rotCenters);
  }

// drawing
  ctx.clearRect(0, 0, maxx, maxy);

// sort visible / hidden faces
  let visible = [], hidden = [];

  faces.forEach ((face, k) => {
    pface = [projPoints[face[0]], projPoints[face[1]], projPoints[face[2]]];
    if (isDirect2(pface[0], pface[1], pface[2])) {
      hidden.push(k);
    } else {
      visible.push(k)
    }
  });

  if (uiv.hole) { // display hidden faces first
    alpha = uiv.hole;
    hidden.forEach(khidden => {
      const face = faces[khidden]
      ctx.beginPath();
      let p = projPoints[face[0]];
      ctx.moveTo(p[0], p[1]);
      for (let k = 1; k < face.length; ++k) {
        p = projPoints[face[k]];
        ctx.lineTo(p[0], p[1]);
      }
      ctx.closePath();
      let cent = projCenters[khidden];

      p = lerp2(cent, projPoints[face[0]], alpha);
      ctx.moveTo(p[0], p[1]);
      for (let k = 1; k < face.length; ++k) {
        p = lerp2(cent, projPoints[face[k]], alpha);
        ctx.lineTo(p[0], p[1]);
      }

      ctx.closePath();

      let alphaLum = mmax(0, (1 + Ar3.dotProduct(rotNormals[khidden], lightDir)) / 2 );
      alphaLum = 1- alphaLum; // inverted for inside side
      alphaLum *= alphaLum; // accentuate diff light / dark
      hue = (uiv.color < 1) ? (360 * uiv.color) : colors[khidden];
      ctx.fillStyle = `hsl(${hue},100%, ${10 + 25 * alphaLum}%)`;
      ctx.fill("evenodd");
      ctx.strokeStyle = "#000";
      ctx.lineWidth = 0.1;
      ctx.stroke();
    });
  }
// allways draw visible
  alpha = uiv.hole;
  visible.forEach(kvisible => {
    const face = faces[kvisible]
    ctx.beginPath();
    let p = projPoints[face[0]];
    ctx.moveTo(p[0], p[1]);
    for (let k = 1; k < face.length; ++k) {
      p = projPoints[face[k]];
      ctx.lineTo(p[0], p[1]);
    }
    ctx.closePath();
    if (uiv.hole) { // draw hole if any
      let cent = projCenters[kvisible];

      p = lerp2(cent, projPoints[face[0]], alpha);
      ctx.moveTo(p[0], p[1]);
      for (let k = 1; k < face.length; ++k) {
        p = lerp2(cent, projPoints[face[k]], alpha);
        ctx.lineTo(p[0], p[1]);
      }

      ctx.closePath();
    }
    let alphaLum = mmax(0, (1 + Ar3.dotProduct(rotNormals[kvisible], lightDir)) / 2 );
    alphaLum *= alphaLum; // accentuate diff light / dark
    hue = (uiv.color < 1) ? (360 * uiv.color) : colors[kvisible];
    ctx.strokeStyle = ctx.fillStyle = `hsl(${hue},100%, ${20 + 50 * alphaLum}%)`;
    ctx.fill("evenodd");
    ctx.strokeStyle = '#fff';
    ctx.lineWidth = 0.25;
    ctx.stroke();
  });

  function globRotate(p) {
    return [globRot[0] * p[0] + globRot[1] * p[1] + globRot[2] * p[2],
            globRot[3] * p[0] + globRot[4] * p[1] + globRot[5] * p[2],
            globRot[6] * p[0] + globRot[7] * p[1] + globRot[8] * p[2]];
  }
} // drawIt

//------------------------------------------------------------------------
function Icosahedron() {

/* based on the fact that vertices of an icosahedron are on the surface of a cube.
It is easy to calculate the positions on a cube with faces at coordinates -1 and +1
*/

  const nphi = (msqrt(5) - 1) / 2;

  const vertices = [[1, 0, nphi], [1, 0, -nphi],
                    [0, -nphi, 1], [0, nphi, 1],
                    [-1, 0, -nphi], [-1, 0, nphi],
                    [0, nphi, -1], [0, -nphi, -1],
                    [nphi, 1, 0], [-nphi, 1, 0],
                    [-nphi, -1, 0], [nphi, -1, 0]];

  const faces = [[0, 1, 8], [0, 11, 1],
                 [2, 0, 3], [2, 3, 5],
                 [4, 5, 9], [4, 10, 5],
                 [6, 1, 7], [6, 7, 4],
                 [8, 9, 3], [8, 6, 9],
                 [10, 7, 11], [10, 11, 2],
                 [0, 2, 11],
                 [0, 8, 3],
                 [1, 11, 7],
                 [1, 6, 8],
                 [3, 9, 5],
                 [2, 5, 10],
                 [4, 9, 6],
                 [4, 7, 10]
                ];

  Solid.call(this, vertices, faces);
}
Icosahedron.prototype = new Solid();
Icosahedron.constructor = Icosahedron;

//------------------------------------------------------------------------
function Dodecahedron() {

/* based on the fact that vertices of an icosahedron are on the surface of a cube.
It is easy to calculate the positions on a cube with faces at coordinates -1 and +1
*/

  const phi = (msqrt(5) + 1) / 2;
  const phi1 = phi - 1;

  const vertices = [ [phi, 0, phi1], [phi, 0, -phi1],
                      [phi1, phi, 0], [-phi1, phi, 0],
                     [-phi, 0, phi1], [-phi, 0, -phi1],
                    [-phi1, -phi, 0], [phi1,-phi, 0],
                    [0, phi1, phi],[0, -phi1, phi],
                    [0, phi1, -phi],[0, -phi1, -phi],
                    [1, 1, 1],[-1, 1, 1],
                    [-1, -1, 1],[1, -1, 1],
                    [1, 1, -1],[-1, 1, -1],
                    [-1, -1, -1],[1, -1, -1]];

  const faces = [[0, 1, 16, 2, 12], [1, 0, 15, 7, 19],
                 [2, 3, 13, 8, 12], [3, 2, 16, 10, 17],
                 [5, 4, 13, 3, 17], [4, 5, 18, 6, 14],
                 [6, 7, 15, 9, 14], [7, 6, 18, 11, 19],
                 [8, 9, 15, 0, 12], [9, 8, 13, 4, 14],
                 [10, 11, 18, 5, 17], [11, 10, 16, 1, 19]];

  Solid.call(this, vertices, faces);
}
Dodecahedron.prototype = new Solid();
Dodecahedron.constructor = Dodecahedron;

//------------------------------------------------------------------------
//------------------------------------------------------------------------
function Octahedron() {
  const vertices = [[1, 0, 0], [-1, 0, 0],
                    [0, 1, 0], [0, -1, 0],
                    [0, 0, 1], [0, 0, -1]];

/* order of vertices matter !
*/
  const faces = [[4, 0, 2], [1, 4, 2],
                 [5, 1, 2], [0, 5, 2],
                 [0, 4, 3], [4, 1, 3],
                 [1, 5, 3], [5, 0, 3]
                ];

  Solid.call(this, vertices, faces);

}

Octahedron.prototype = new Solid();
Octahedron.constructor = Octahedron;

//------------------------------------------------------------------------
//------------------------------------------------------------------------
function Cube() {
  const vertices = [[1, 1, 1], [1, -1, 1],
                    [1, -1, -1], [1, 1, -1],
                    [-1, 1, 1], [-1, -1, 1],
                    [-1, -1, -1], [-1, 1, -1]];

/* order of vertices matter !
*/
  const faces = [[0, 1, 2, 3], [0, 3, 7, 4],
                 [6, 5, 4, 7], [6, 2, 1, 5],
                 [0, 4, 5, 1], [6, 7, 3, 2]
                ];

  Solid.call(this, vertices, faces);

}

Cube.prototype = new Solid();
Cube.constructor = Cube;

//------------------------------------------------------------------------
//------------------------------------------------------------------------
function Tetrahedron() {
  const R = msqrt(8) / 3;

  const vertices = [[msqrt(3) / 2 * R, - 1 / 3, -R / 2],
                    [0, -1 / 3, R],
                    [-msqrt(3) / 2 * R, - 1 / 3, -R / 2],
                    [0, 1, 0]];

/* order of vertices matter !
*/
  const faces = [[0, 1, 2], [1, 3, 2],
                 [3, 0, 2], [0, 3, 1]
                ];

  Solid.call(this, vertices, faces);

}

Tetrahedron.prototype = new Solid();
Tetrahedron.constructor = Tetrahedron;

//------------------------------------------------------------------------
// User Interface (controls)
//------------------------------------------------------------------------
function toggleMenu() {
  if (menu.classList.contains("hidden")) {
    menu.classList.remove ("hidden");
    this.innerHTML ="close controls";
  } else {
    menu.classList.add ("hidden");
    this.innerHTML ="controls";
  }
} // toggleMenu
//------------------------------------------------------------------------
function prepareUI() {

// toggle menu handler

  document.querySelector("#controls").addEventListener("click", toggleMenu);

  ui = {};  // User Interface HTML elements
  uiv = {}; // User Interface values of controls

  ['solid', 'division', 'speed', 'color', 'hole', 'fps'].forEach(ctrlName => ui[ctrlName] = document.getElementById(ctrlName));

  registerControl("solid", readUIInt, "input");
  registerControl("division",readUIFloat,"input");
  registerControl("hole",readUIFloat,"input");
  registerControl("color",readUIFloat,"input");
  registerControl("speed",readSpeed,"input");
  readUI();
} // prepareUI

//------------------------------------------------------------------------
function readUI() {

  if (ui.registered) {
    for (const ctrl in ui.registered) ui.registered[ctrl].readF();
  }
} // readUI

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function registerControl (controlName, readFunction, changeEvent, changedFunction) {
/* provides simple way to associate controls with their read / update / changeEvent / changed functions
since many (but not all) controls work almost the same way */
/* changeEvent and changedEvent are optional */

  const ctrl = ui[controlName];
  ui.registered = ui.registered || [];
  ui.registered.push(ctrl); // NEVER register a control twice !!!
  ctrl.readF = readFunction;
  if (changeEvent) {
    ctrl.addEventListener(changeEvent, (event) => {
      readFunction.call(ctrl);
      if (changedFunction) changedFunction.call(ctrl,event);
    });
  }
} // registerControl
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function readUIFloat() {
  uiv[this.id] = parseFloat(this.value);
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function readUIInt(ctrl, event) {
  uiv[this.id] = parseInt(this.value);
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function readUICheck(ctrl, event) {
  uiv[this.id] = this.checked;
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function readSpeed() {
// non-linear law
  const speed = parseFloat(ui.speed.value);
  uiv.speed = Math.sign(speed) * Math.pow(mabs(speed), 1.5); // better than linear
}

//------------------------------------------------------------------------
//------------------------------------------------------------------------

let animate;

{ // scope for animate

let animState = 0;
let mouseState = 0;
let prevMouse;
let tPrev;
let fps = 0, tFps = 0;

const filtr = Math.exp(-1 / 20); // filter time constant = 20 frames
let tFiltr = performance.now();

animate = function(tStamp) {

  let event, dth1, dth2;

  if (tStamp > tFiltr) {
    fps = fps * filtr + (1 - filtr) * 1000 / (tStamp - tFiltr);
    if (tStamp - tFps > 500) { // do not refresh too often
      ui.fps.innerHTML = mround(10 * fps) / 10;
      tFps = tStamp;
    }
  }
  tFiltr = tStamp;

  window.requestAnimationFrame(animate);

// 1 - manage mouse movement

  dth1 = dth2 = 0;
  event = mouseEvents.shift();
  if (event) {
    switch (mouseState) {

      case 0: // released, waiting for mousedown
        if (event.event == "mousedown") {
          prevMouse = event.param;
          ++mouseState;
        }
        break;
      case 1 :
        if (event.event == "mouseup") {
          mouseState = 0; // stop moving
        } else if (event.event == "mousemove") {
          dth1 = (event.param.clientX - prevMouse.clientX) / maxx * 2;
          dth2 = (event.param.clientY - prevMouse.clientY) / maxx * 2;
          prevMouse = event.param;
        }
        break;

    } // switch mouseState

  } // if mouse event
// 2 - solid rotation

  event = events.shift();

  if (event && event.event == 'reset') animState = 0;


  switch (animState) {

    case 0 : startOver();
             ++animState;
              tPrev = tStamp;
    case 1 :
          let dt = tStamp - tPrev;
          tPrev = tStamp;
          solids[uiv.solid].drawIt(dRot1() * dt * uiv.speed + dth2, dRot2() * dt * uiv.speed + dth1);
          break;

    case 2:
      break;


  } // switch


} // animate
} // scope for animate

//------------------------------------------------------------------------
//------------------------------------------------------------------------

function startOver() {

// canvas dimensions

  maxx = window.innerWidth;
  maxy = window.innerHeight;

  xc = maxx / 2;
  yc = maxy / 2;

  canv.width = maxx;
  canv.height = maxy;
  ctx.lineJoin = 'round';
  ctx.lineCap = 'round';

  ctx.fillStyle = '#000';
  ctx.fillRect(0, 0, maxx, maxy);

  perspective = createPerspective3([0, 0, 5], 0.25 * maxx / mmin(maxx, maxy), maxx, maxy);

  // pick random hues
  solids.forEach(solid => {
    solid.colorsD = solid.facesD.map(faces => faces.map(face => intAlea(360)));
  });

  return true;

} // startOver

//------------------------------------------------------------------------

function mouseDown (event) {
  mouseEvents.push({event:'mousedown', param: event});
} // mouseDown
//------------------------------------------------------------------------

function lightFrom(event) {
  const lRef = mmin(maxx, maxy) / 2;
  const xRel = (event.clientX - maxx / 2) / lRef;
  const yRel = (maxy / 2 - event.clientY) / lRef;
  const max = (maxx * maxx + maxy * maxy) / lRef / lRef / 4;
  lightDir = Ar3.normalize([xRel, yRel, msqrt(max - xRel * xRel - yRel * yRel + 0.01)]);
}

//------------------------------------------------------------------------

function mouseMove (event) {

// mouseMove 1 : light direction

  lightFrom(event);

// mouseMove 2 : rotation

  let ev = {event:'mousemove', param: event}
  if (mouseEvents.length > 1 &&
        mouseEvents[mouseEvents.length - 1].event == "mousemove") {
    mouseEvents[mouseEvents.length - 1] = ev; // update last event if it was already a mousemove
  } else {
    mouseEvents.push(ev);
  }
} // mouseMove

//------------------------------------------------------------------------

function mouseUp (event) {
  mouseEvents.push({event:'mouseup', param: event});
} // mouseUp
//------------------------------------------------------------------------

function mouseLeave (event) {
  mouseUp(event);
} // mouseLeave
//------------------------------------------------------------------------
function touchStart(event) {
  if (event.touches.length != 1) return; // ignore if more than 1 touch
  lightFrom(event.touches[0]);
  mouseDown({clientX: event.touches[0].clientX, clientY: event.touches[0].clientY});
}

//------------------------------------------------------------------------

function touchMove(event) {
  if (event.touches.length != 1) return; // ignore if more than 1 touch
  mouseMove({clientX: event.touches[0].clientX, clientY: event.touches[0].clientY});
  event.preventDefault();
}
//------------------------------------------------------------------------
//------------------------------------------------------------------------
// beginning of execution

  {
    canv = document.createElement('canvas');
    canv.style.position="absolute";
    document.body.appendChild(canv);
    ctx = canv.getContext('2d');
  } // création CANVAS

  dRot1 = Noise1DOneShot(500,-rotSpeed, rotSpeed);
  dRot2 = Noise1DOneShot(500,-rotSpeed, rotSpeed);

  lightDir = Ar3.normalize([1, 1, 2]);

  prepareUI();

  solids = [new Tetrahedron(),
            new Cube(),
            new Octahedron(),
            new Dodecahedron(),
            new Icosahedron()];

  canv.addEventListener('mousedown',mouseDown);
  canv.addEventListener('mousemove',mouseMove);
  canv.addEventListener('mouseup',mouseUp);
  canv.addEventListener('mouseleave',mouseLeave);

  canv.addEventListener('touchstart',touchStart);
  canv.addEventListener('touchmove',touchMove);
  canv.addEventListener('touchend',mouseUp);
  canv.addEventListener('touchcancel',mouseUp);
  canv.addEventListener('touchleave',mouseUp);

  events = [{event:'reset'}];
  mouseEvents = [];
  requestAnimationFrame (animate);

  console.log('The number of vertices in this list includes MANY duplicated vertices, except where div = 0. The actual number is significantly lower. The number of faces is correct.');
  solids.forEach ((solid, ks) => {
    solid.verticesD.forEach((vertices, kd) => {
      console.log (`solid ${ks} div ${kd} vertices ${vertices.length} faces ${solid.facesD[kd].length}`);
    })
  })

}); // window load listener
