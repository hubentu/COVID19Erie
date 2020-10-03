// All material copyright ESRI, All Rights Reserved, unless otherwise specified.
// See http://js.arcgis.com/3.34/esri/copyright.txt for details.
//>>built
define("esri/geometry/geometryEngineAsync","require exports esri/kernel module esri/geometry/Geometry esri/geometry/Polygon esri/geometry/Polyline esri/geometry/Point esri/geometry/Extent esri/geometry/Multipoint esri/workers/WorkerClient esri/arcade/polyfill/promiseUtils".split(" "),function(A,P,J,K,u,B,C,z,D,E,L,k){function M(b){if(void 0===z.fromJson){if(void 0!==b.x&&void 0!==b.y)return new z(b);if(void 0!==b.paths)return new C(b);if(void 0!==b.rings)return new B(b);if(void 0!==b.points)return new E(b);
if(void 0!==b.xmin&&void 0!==b.ymin&&void 0!==b.xmax&&void 0!==b.ymax)return new D(b)}else{if(void 0!==b.x&&void 0!==b.y)return z.fromJson(b);if(void 0!==b.paths)return C.fromJson(b);if(void 0!==b.rings)return B.fromJson(b);if(void 0!==b.points)return E.fromJson(b);if(void 0!==b.xmin&&void 0!==b.ymin&&void 0!==b.xmax&&void 0!==b.ymax)return D.fromJson(b)}}function r(b,a){var c;if(null==b||void 0===b||"number"===typeof b)return b;var d=b.toString();if(""===d)return null;if(2==a){if(c=N[d],void 0!==
c)return c}else if(0==a){c=F[d];if(void 0!==c)return c;c=G[b];if(void 0!==c)return c}else if(3==a&&(c=F[d],void 0!==c))return c;if(1==a&&(c=G[b],void 0!==c))return c;if(!0===/^\d+$/.test(d))return parseInt(d);throw Error("Unrecognised Unit Type");}function x(b){if(void 0!==b&&null!==b)switch(b){case "loxodrome":return 1;case "great-elliptic":return 2;case "normal-section":return 3;case "shape-preserving":return 4}return 0}function h(b){if(null===b||void 0===b)return null;if(v)switch(b.type){case "point":return{x:b.x,
y:b.y,z:b.z,m:b.m};case "multipoint":return{points:b.points,hasZ:b.hasZ,hasM:b.hasM};case "polyline":return{paths:b.paths,hasZ:b.hasZ,hasM:b.hasM};case "polygon":return{rings:b.rings,hasZ:b.hasZ,hasM:b.hasM};case "extent":return{xmin:b.xmin,ymin:b.ymin,xmax:b.xmax,ymax:b.ymax,zmin:b.zmin,zmax:b.zmax,mmin:b.mmin,mmax:b.mmax}}else switch(b.type){case "point":return{x:b.x,y:b.y};case "multipoint":return{points:b.points};case "polyline":return{paths:b.paths};case "polygon":return{rings:b.rings};case "extent":return{xmin:b.xmin,
ymin:b.ymin,xmax:b.xmax,ymax:b.ymax}}return null}function p(b,a){if(null===b)return null;b=M(b);v?b.set("spatialReference",a):b.setSpatialReference(a);return b}function n(b){return null==b||void 0===b?null:-1!=b.wkid&&null!==b.wkid&&void 0!==b.wkid?{wkid:b.wkid}:""!==b.wkt&&void 0!==b.wkt&&null!==b.wkt?{wkt:b.wkt}:null}function y(b,a,c){return k.create(function(d,e){var g=a.spatialReference;l.a({action:b,geoma:h(a),geomb:h(c),spatialReference:n(a.spatialReference)}).then(function(a){0===a.status?
e(Error(a.error.message)):d(p(a.result,g))},function(a){e(a)})})}function t(b,a,c){return k.create(function(d,e){l.a({action:b,geoma:h(a),geomb:h(c),spatialReference:n(a.spatialReference)}).then(function(a){0===a.status?e(Error(a.error.message)):d(a.result)},function(a){e(a)})})}var v=0==J.version.indexOf("4."),H;(function(b){b[b.Linear=0]="Linear";b[b.Angular=1]="Angular";b[b.Area=2]="Area";b[b.LinearOrAngular=3]="LinearOrAngular"})(H||(H={}));var F={feet:9002,kilometers:9036,meters:9001,miles:9035,
"nautical-miles":9030,yards:9096},N={acres:109402,ares:109463,hectares:109401,"square-feet":109405,"square-kilometers":109414,"square-meters":109404,"square-miles":109413,"square-yards":109442},G={degrees:9102,radians:9101},O=function(){function b(){this.c=!0;this.i=null}b.prototype.A=function(a){var b=this;this.i=new L;this.i.setWorker(this.l(),function(){b.c=!1;a()})};b.prototype.l=function(){return A.B?A.B("./geometryenginewebworker"):K.id.replace(/\/[^\/]*$/ig,"/")+"./geometryenginewebworker"};
return b}(),l=function(){function b(){}b.a=function(a){return k.create(function(c,d){b.h.push({task:a,d:{resolve:c,reject:d}});b.f()})};b.f=function(){if(0<b.h.length){for(var a=null,c=0;c<b.b.length;c++)if(!1===b.b[c].c){a=b.b[c];break}null===a&&b.b.length<b.g&&(c=new O,b.b.push(c),c.A(function(){b.f()}));if(null!==a){var d=this.h.shift();a.c=!0;a.i.postMessage(d.task).then(function(c){a.c=!1;try{d.d.resolve(c)}catch(g){}b.f()},function(c){a.c=!1;try{d.d.reject(c)}catch(g){}b.f()})}}};b.b=[];b.h=
[];b.g=4;return b}();return function(){function b(){}b._removeAllWorkers=function(){l.b=[]};b._setMaxWorkers=function(a){b._removeAllWorkers();l.g=a};b._getMaxWorkers=function(){return l.g};b._getNumWorkers=function(){return l.b.length};b.extendedSpatialReferenceInfo=function(a){return k.create(function(b,d){l.a({action:"extendedspatialreferenceinfo",spatialReference:n(a)}).then(function(a){0===a.status?d(Error(a.error.message)):b(a.result)},function(a){d(a)})})};b.equals=function(a,b){return null===
a&&null!==b||null===b&&null!==a?!1:t("equals",a,b)};b.intersects=function(a,b){if(null===a||null===b)throw Error("Illegal Argument Exception");return t("intersects",a,b)};b.touches=function(a,b){if(null===a||null===b)throw Error("Illegal Argument Exception");return t("touches",a,b)};b.within=function(a,b){if(null===a||null===b)throw Error("Illegal Argument Exception");return t("within",a,b)};b.disjoint=function(a,b){if(null===a||null===b)throw Error("Illegal Argument Exception");return t("disjoint",
a,b)};b.overlaps=function(a,b){if(null===a||null===b)throw Error("Illegal Argument Exception");return t("overlaps",a,b)};b.crosses=function(a,b){if(null===a||null===b)throw Error("Illegal Argument Exception");return t("crosses",a,b)};b.contains=function(a,b){if(null===a||null===b)throw Error("Illegal Argument Exception");return t("contains",a,b)};b.isSimple=function(a){return t("issimple",a,null)};b.clip=function(a,b){return y("clip",a,b)};b.simplify=function(a){return k.create(function(b,d){var c=
a.spatialReference;l.a({action:"simplify",geoma:h(a),spatialReference:n(a.spatialReference)}).then(function(a){0===a.status?d(Error(a.error.message)):b(p(a.result,c))},function(a){d(a)})})};b.rotate=function(a,b,d){return k.create(function(c,g){var f=a.spatialReference;if(void 0===d||null===d)switch(a.type){case "point":d=a;break;case "extent":d=v?a.get("center"):a.getCenter();break;default:d=v?a.get("extent").get("center"):a.getExtent().getCenter()}l.a({action:"rotate",geoma:h(a),spatialReference:n(a.spatialReference),
angle:b,rotpt:h(d)}).then(function(a){0===a.status?g(Error(a.error.message)):c(p(a.result,f))},function(a){g(a)})})};b.flipHorizontal=function(a,b){return k.create(function(c,e){var d=a.spatialReference;if(void 0===b||null===b)switch(a.type){case "point":b=a;break;case "extent":b=v?a.get("center"):a.getCenter();break;default:b=v?a.get("extent").get("center"):a.getExtent().getCenter()}l.a({action:"fliph",geoma:h(a),spatialReference:n(a.spatialReference),flippt:h(b)}).then(function(a){0===a.status?
e(Error(a.error.message)):c(p(a.result,d))},function(a){e(a)})})};b.flipVertical=function(a,b){return k.create(function(c,e){var d=a.spatialReference;if(void 0===b||null===b)switch(a.type){case "point":b=a;break;case "extent":b=v?a.get("center"):a.getCenter();break;default:b=v?a.get("extent").get("center"):a.getExtent().getCenter()}l.a({action:"flipv",geoma:h(a),spatialReference:n(a.spatialReference),flippt:h(b)}).then(function(a){0===a.status?e(Error(a.error.message)):c(p(a.result,d))},function(a){e(a)})})};
b.distance=function(a,b,d){return k.create(function(c,g){l.a({action:"distance",geoma:h(a),geomb:h(b),spatialReference:n(a.spatialReference),distanceunits:r(d,3)}).then(function(a){0===a.status?g(Error(a.error.message)):c(a.result)},function(a){g(a)})})};b.relate=function(a,b,d){return k.create(function(c,g){l.a({action:"relate",geoma:h(a),geomb:h(b),relation:d,spatialReference:n(a.spatialReference)}).then(function(a){0===a.status?g(Error(a.error.message)):c(a.result)},function(a){g(a)})})};b.nearestCoordinate=
function(a,b,d){return k.create(function(c,g){var f=a.spatialReference;l.a({action:"nearestcoord",geoma:h(a),geomb:h(b),spatialReference:n(a.spatialReference),testinterior:void 0===d?!0:d}).then(function(a){0===a.status?g(Error(a.error.message)):(a.result.coordinate=p(a.result.coordinate,f),c(a.result))},function(a){g(a)})})};b.nearestVertex=function(a,b){return k.create(function(c,e){var d=a.spatialReference;l.a({action:"nearestvertex",geoma:h(a),geomb:h(b),spatialReference:n(a.spatialReference)}).then(function(a){0===
a.status?e(Error(a.error.message)):(a.result.coordinate=p(a.result.coordinate,d),c(a.result))},function(a){e(a)})})};b.nearestVertices=function(a,b,d,e){return k.create(function(c,f){var g=a.spatialReference;l.a({action:"nearestvertices",geoma:h(a),geomb:h(b),spatialReference:n(a.spatialReference),searchradius:d,maxreturn:e}).then(function(a){if(0===a.status)f(Error(a.error.message));else{for(var b=0;b<a.result.length;b++)a.result[b].coordinate=p(a.result[b].coordinate,g);c(a.result)}},function(a){f(a)})})};
b.cut=function(a,b){return k.create(function(c,e){var d=a.spatialReference;l.a({action:"cut",geoma:h(a),geomb:h(b),spatialReference:n(a.spatialReference)}).then(function(a){if(0===a.status)e(Error(a.error.message));else{for(var b=0;b<a.result.length;b++)a.result[b]=p(a.result[b],d);c(a.result)}},function(a){e(a)})})};b.generalize=function(a,b,d,e){return k.create(function(c,f){var g=a.spatialReference;l.a({action:"generalize",geoma:h(a),maxdeviation:b,removedegenerateparts:d,maxdeviationunit:r(e,
3),spatialReference:n(a.spatialReference)}).then(function(a){0===a.status?f(Error(a.error.message)):c(p(a.result,g))},function(a){f(a)})})};b.densify=function(a,b,d){return k.create(function(c,g){var f=a.spatialReference;l.a({action:"densify",geoma:h(a),maxsegmentlength:b,maxsegmentlengthunit:r(d,3),spatialReference:n(a.spatialReference)}).then(function(a){0===a.status?g(Error(a.error.message)):c(p(a.result,f))},function(a){g(a)})})};b.geodesicDensify=function(a,b,d,e){void 0===e&&(e=0);return k.create(function(c,
f){var g=a.spatialReference;l.a({action:"geodensify",geoma:h(a),maxsegmentlength:b,maxsegmentlengthunit:r(d,3),spatialReference:n(a.spatialReference),curveType:e}).then(function(a){0===a.status?f(Error(a.error.message)):c(p(a.result,g))},function(a){f(a)})})};b.intersect=function(a,c){return a instanceof u?y("intersect",a,c):b.u(a,c)};b.u=function(a,b){return k.create(function(c,e){for(var d=[],f=0;f<a.length;f++)d.push(h(a[f]));var m=b.spatialReference;l.a({action:"intersectmany",geom:h(b),geometries:d,
spatialReference:n(b.spatialReference)}).then(function(a){if(0===a.status)e(Error(a.error.message));else{for(var b=0;b<a.result.length;b++)a.result[b]=p(a.result[b],m);c(a.result)}},function(a){e(a)})})};b.difference=function(a,c){return a instanceof u?y("difference",a,c):b.s(a,c)};b.s=function(a,b){return k.create(function(c,e){for(var d=[],f=0;f<a.length;f++)d.push(h(a[f]));var m=b.spatialReference;l.a({action:"differencemany",geom:h(b),geometries:d,spatialReference:n(b.spatialReference)}).then(function(a){if(0===
a.status)e(Error(a.error.message));else{for(var b=0;b<a.result.length;b++)a.result[b]=p(a.result[b],m);c(a.result)}},function(a){e(a)})})};b.symmetricDifference=function(a,c){return a instanceof u?y("symdifference",a,c):b.w(a,c)};b.w=function(a,b){return k.create(function(c,e){for(var d=[],f=0;f<a.length;f++)d.push(h(a[f]));var m=b.spatialReference;l.a({action:"symdifferencemany",geom:h(b),geometries:d,spatialReference:n(b.spatialReference)}).then(function(a){if(0===a.status)e(Error(a.error.message));
else{for(var b=0;b<a.result.length;b++)a.result[b]=p(a.result[b],m);c(a.result)}},function(a){e(a)})})};b.union=function(a,b){void 0===b&&(b=null);return k.create(function(c,e){var d=[];if(null===a)c(null);else if(a instanceof u&&(a=[a],null!==b&&a.push(b)),0===a.length)c(null);else{for(var f=0;f<a.length;f++)d.push(h(a[f]));var m=a[0].spatialReference;l.a({action:"unionmany",geometries:d,spatialReference:n(m)}).then(function(a){0===a.status?e(Error(a.error.message)):c(p(a.result,m))},function(a){e(a)})}})};
b.buffer=function(a,c,d,e){void 0===e&&(e=!1);if(a instanceof u)return k.create(function(b,f){var e=a.spatialReference;l.a({action:"buffer",geoma:h(a),spatialReference:n(a.spatialReference),distance:c,unit:r(d,3),geodesic:!1,geodesicmaxdeviation:NaN,geodesiccurvetype:0}).then(function(a){0===a.status?f(Error(a.error.message)):b(p(a.result,e))},function(a){f(a)})});if("[object Array]"!==Object.prototype.toString.call(c)){for(var g=[],f=0;f<a.length;f++)g.push(c);c=g}if(c.length!=a.length){if(0==c.length)throw Error("Illegal Argument Exception");
for(var g=[],m=0,f=0;f<a.length;f++)void 0===c[f]?g.push(m):(g.push(c[f]),m=c[f]);c=g}return b.j(a,c,d,!1,e,"geodesic",NaN)};b.geodesicBuffer=function(a,c,d,e,g,f){if(a instanceof u)return k.create(function(b,f){void 0===g&&(g=NaN);var m=a.spatialReference;l.a({action:"buffer",geoma:h(a),spatialReference:n(a.spatialReference),distance:c,unit:r(d,0),geodesic:!0,geodesicmaxdeviation:g,geodesiccurvetype:x(e)}).then(function(a){0===a.status?f(Error(a.error.message)):b(p(a.result,m))},function(a){f(a)})});
if("[object Array]"!==Object.prototype.toString.call(c)){for(var m=[],q=0;q<a.length;q++)m.push(c);c=m}if(c.length!=a.length){if(0==c.length)throw Error("Illegal Argument Exception");for(var m=[],w=0,q=0;q<a.length;q++)void 0===c[q]?m.push(w):(m.push(c[q]),w=c[q]);c=m}return b.j(a,c,d,!0,e,g,f)};b.j=function(a,b,d,e,g,f,m){return k.create(function(c,k){var q=[];void 0===m&&(m=NaN);if(null===a)c(null);else if(0===a.length)c(null);else{for(var w=0;w<a.length;w++)q.push(h(a[w]));d=e?r(d,0):r(d,3);var I=
a[0].spatialReference;l.a({action:"buffermany",geometries:q,spatialReference:n(I),distances:b,tounionresults:g,unit:d,geodesic:e,geodesicmaxdeviation:m,geodesiccurvetype:x(f)}).then(function(a){if(0===a.status)k(Error(a.error.message));else{for(var b=0;b<a.result.length;b++)a.result[b]=p(a.result[b],I);c(a.result)}},function(a){k(a)})}})};b.convexHull=function(a,c){void 0===c&&(c=!1);return a instanceof u?k.create(function(b,c){var d=a.spatialReference;l.a({action:"convexhull",geoma:h(a),spatialReference:n(a.spatialReference)}).then(function(a){0===
a.status?c(Error(a.error.message)):b(p(a.result,d))},function(a){c(a)})}):b.o(a,c)};b.o=function(a,b){return k.create(function(c,e){for(var d=[],f=0;f<a.length;f++)d.push(h(a[f]));var k=0<a.length?a[0].spatialReference:null;l.a({action:"convexhullmany",geometries:d,merge:b}).then(function(a){if(0===a.status)e(Error(a.error.message));else{for(var b=0;b<a.result.length;b++)a.result[b]=p(a.result[b],k);c(a.result)}},function(a){e(a)})})};b.offset=function(a,c,d,e,g,f){var m=0;if(null!=e&&void 0!=e)switch(e){case "round":m=
0;break;case "bevel":m=1;break;case "miter":m=2;break;case "square":m=3}return a instanceof u?k.create(function(b,e){var k=a.spatialReference;l.a({action:"offset",geoma:h(a),spatialReference:n(a.spatialReference),distance:c,joins:m,bevelratio:g,flattenerror:f,offsetunit:r(d,3)}).then(function(a){0===a.status?e(Error(a.error.message)):b(p(a.result,k))},function(a){e(a)})}):b.v(a,c,d,m,g,f)};b.v=function(a,b,d,e,g,f){return k.create(function(c,k){for(var m=[],q=0;q<a.length;q++)m.push(h(a[q]));var t=
0<a.length?a[0].spatialReference:null;l.a({action:"offsetmany",geometries:m,spatialReference:n(t),distance:b,joins:e,bevelratio:g,offsetunit:r(d,3),flattenerror:f}).then(function(a){if(0===a.status)k(Error(a.error.message));else{for(var b=0;b<a.result.length;b++)a.result[b]=p(a.result[b],t);c(a.result)}},function(a){k(a)})})};b.planarArea=function(a,b){return k.create(function(c,e){l.a({action:"area",geoma:h(a),unit:r(b,2),spatialReference:n(a.spatialReference)}).then(function(a){0===a.status?e(Error(a.error.message)):
c(a.result)},function(a){e(a)})})};b.planarLength=function(a,b){return k.create(function(c,e){l.a({action:"length",geoma:h(a),unit:r(b,3),spatialReference:n(a.spatialReference)}).then(function(a){0===a.status?e(Error(a.error.message)):c(a.result)},function(a){e(a)})})};b.geodesicArea=function(a,b,d){return k.create(function(c,g){l.a({action:"geodesicarea",geoma:h(a),unit:r(b,2),geodesiccurvetype:x(d),spatialReference:n(a.spatialReference)}).then(function(a){0===a.status?g(Error(a.error.message)):
c(a.result)},function(a){g(a)})})};b.geodesicLength=function(a,b,d){return k.create(function(c,g){l.a({action:"geodesiclength",geoma:h(a),unit:r(b,0),geodesiccurvetype:x(d),spatialReference:n(a.spatialReference)}).then(function(a){0===a.status?g(Error(a.error.message)):c(a.result)},function(a){g(a)})})};return b}()});