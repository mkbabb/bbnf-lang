function f(x){return x+1;}
for(let i=0;i<1e6;i++) f(i);
%PrepareFunctionForOptimization(f);
f(1);
%OptimizeFunctionOnNextCall(f);
f(1);
const s = %GetOptimizationStatus(f);
console.log("status bitmask=", s);
const a={x:1}, b={x:2};
console.log("HaveSameMap=", %HaveSameMap(a,b));
