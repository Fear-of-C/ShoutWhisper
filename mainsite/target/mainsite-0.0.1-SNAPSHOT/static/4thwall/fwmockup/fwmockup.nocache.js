function fwmockup(){var P="",wb='" for "gwt:onLoadErrorFn"',ub='" for "gwt:onPropertyErrorFn"',ib='"><\/script>',Z="#",Ub=".cache.html",_="/",Ob="2F7540F10DFACCAAC9D86EF6255DD7C7",Pb="911EB831BADB26787A511BE4FD0AE9B5",Tb=":",ob="::",ac="<script defer=\"defer\">fwmockup.onInjectionDone('fwmockup')<\/script>",hb='<script id="',rb="=",$="?",tb='Bad handler "',Qb="C45ACFE7F8723AB3094FAF2715BD33F9",Rb="CCAA229E452549444A6A3574D94D0183",Sb="D9D6CAF016E95F498803786D955FB530",_b="DOMContentLoaded",jb="SCRIPT",gb="__gwt_marker_fwmockup",kb="base",cb="baseUrl",T="begin",S="bootstrap",bb="clear.cache.gif",qb="content",Y="end",Q="fwmockup",eb="fwmockup.nocache.js",nb="fwmockup::",Ib="gecko",Jb="gecko1_8",U="gwt.codesvr=",V="gwt.hosted=",W="gwt.hybrid",Vb="gwt/standard/standard.css",vb="gwt:onLoadErrorFn",sb="gwt:onPropertyErrorFn",pb="gwt:property",$b="head",Mb="hosted.html?fwmockup",Zb="href",Hb="ie6",Gb="ie8",xb="iframe",ab="img",yb="javascript:''",Wb="link",Lb="loadExternalRefs",lb="meta",Ab="moduleRequested",X="moduleStartup",Fb="msie",mb="name",Cb="opera",zb="position:absolute;width:0;height:0;border:none",Xb="rel",Eb="safari",db="script",Nb="selectingPermutation",R="startup",Yb="stylesheet",fb="undefined",Kb="unknown",Bb="user.agent",Db="webkit";
var l=window,m=document,n=l.__gwtStatsEvent?function(a){return l.__gwtStatsEvent(a)
}:null,o=l.__gwtStatsSessionId?l.__gwtStatsSessionId:null,p,q,r,s=P,t={},u=[],v=[],w=[],x=0,y,z;
n&&n({moduleName:Q,sessionId:o,subSystem:R,evtGroup:S,millis:(new Date).getTime(),type:T});
if(!l.__gwt_stylesLoaded){l.__gwt_stylesLoaded={}
}if(!l.__gwt_scriptsLoaded){l.__gwt_scriptsLoaded={}
}function A(){var b=false;
try{var c=l.location.search;
return(c.indexOf(U)!=-1||(c.indexOf(V)!=-1||l.external&&l.external.gwtOnLoad))&&c.indexOf(W)==-1
}catch(a){}A=function(){return b
};
return b
}function B(){if(p&&q){var b=m.getElementById(Q);
var c=b.contentWindow;
if(A()){c.__gwt_getProperty=function(a){return H(a)
}
}fwmockup=null;
c.gwtOnLoad(y,Q,s,x);
n&&n({moduleName:Q,sessionId:o,subSystem:R,evtGroup:X,millis:(new Date).getTime(),type:Y})
}}function C(){function e(a){var b=a.lastIndexOf(Z);
if(b==-1){b=a.length
}var c=a.indexOf($);
if(c==-1){c=a.length
}var d=a.lastIndexOf(_,Math.min(c,b));
return d>=0?a.substring(0,d+1):P
}function f(a){if(a.match(/^\w+:\/\//)){}else{var b=m.createElement(ab);
b.src=a+bb;
a=e(b.src)
}return a
}function g(){var a=F(cb);
if(a!=null){return a
}return P
}function h(){var a=m.getElementsByTagName(db);
for(var b=0;
b<a.length;
++b){if(a[b].src.indexOf(eb)!=-1){return e(a[b].src)
}}return P
}function i(){var a;
if(typeof isBodyLoaded==fb||!isBodyLoaded()){var b=gb;
var c;
m.write(hb+b+ib);
c=m.getElementById(b);
a=c&&c.previousSibling;
while(a&&a.tagName!=jb){a=a.previousSibling
}if(c){c.parentNode.removeChild(c)
}if(a&&a.src){return e(a.src)
}}return P
}function j(){var a=m.getElementsByTagName(kb);
if(a.length>0){return a[a.length-1].href
}return P
}var k=g();
if(k==P){k=h()
}if(k==P){k=i()
}if(k==P){k=j()
}if(k==P){k=e(m.location.href)
}k=f(k);
s=k;
return k
}function D(){var b=document.getElementsByTagName(lb);
for(var c=0,d=b.length;
c<d;
++c){var e=b[c],f=e.getAttribute(mb),g;
if(f){f=f.replace(nb,P);
if(f.indexOf(ob)>=0){continue
}if(f==pb){g=e.getAttribute(qb);
if(g){var h,i=g.indexOf(rb);
if(i>=0){f=g.substring(0,i);
h=g.substring(i+1)
}else{f=g;
h=P
}t[f]=h
}}else{if(f==sb){g=e.getAttribute(qb);
if(g){try{z=eval(g)
}catch(a){alert(tb+g+ub)
}}}else{if(f==vb){g=e.getAttribute(qb);
if(g){try{y=eval(g)
}catch(a){alert(tb+g+wb)
}}}}}}}}function F(a){var b=t[a];
return b==null?null:b
}function G(a,b){var c=w;
for(var d=0,e=a.length-1;
d<e;
++d){c=c[a[d]]||(c[a[d]]=[])
}c[a[e]]=b
}function H(a){var b=v[a](),c=u[a];
if(b in c){return b
}var d=[];
for(var e in c){d[c[e]]=e
}if(z){z(a,d,b)
}throw null
}var I;
function J(){if(!I){I=true;
var a=m.createElement(xb);
a.src=yb;
a.id=Q;
a.style.cssText=zb;
a.tabIndex=-1;
m.body.appendChild(a);
n&&n({moduleName:Q,sessionId:o,subSystem:R,evtGroup:X,millis:(new Date).getTime(),type:Ab});
a.contentWindow.location.replace(s+L)
}}v[Bb]=function(){var b=navigator.userAgent.toLowerCase();
var c=function(a){return parseInt(a[1])*1000+parseInt(a[2])
};
if(b.indexOf(Cb)!=-1){return Cb
}else{if(b.indexOf(Db)!=-1){return Eb
}else{if(b.indexOf(Fb)!=-1){if(document.documentMode>=8){return Gb
}else{var d=/msie ([0-9]+)\.([0-9]+)/.exec(b);
if(d&&d.length==3){var e=c(d);
if(e>=6000){return Hb
}}}}else{if(b.indexOf(Ib)!=-1){return Jb
}}}}return Kb
};
u[Bb]={gecko1_8:0,ie6:1,ie8:2,opera:3,safari:4};
fwmockup.onScriptLoad=function(){if(I){q=true;
B()
}};
fwmockup.onInjectionDone=function(){p=true;
n&&n({moduleName:Q,sessionId:o,subSystem:R,evtGroup:Lb,millis:(new Date).getTime(),type:Y});
B()
};
D();
C();
var K;
var L;
if(A()){if(l.external&&(l.external.initModule&&l.external.initModule(Q))){l.location.reload();
return 
}L=Mb;
K=P
}n&&n({moduleName:Q,sessionId:o,subSystem:R,evtGroup:S,millis:(new Date).getTime(),type:Nb});
if(!A()){try{G([Hb],Ob);
G([Jb],Pb);
G([Gb],Qb);
G([Cb],Rb);
G([Eb],Sb);
K=w[H(Bb)];
var M=K.indexOf(Tb);
if(M!=-1){x=Number(K.substring(M+1));
K=K.substring(0,M)
}L=K+Ub
}catch(a){return 
}}var N;
function O(){if(!r){r=true;
if(!__gwt_stylesLoaded[Vb]){var a=m.createElement(Wb);
__gwt_stylesLoaded[Vb]=a;
a.setAttribute(Xb,Yb);
a.setAttribute(Zb,s+Vb);
m.getElementsByTagName($b)[0].appendChild(a)
}B();
if(m.removeEventListener){m.removeEventListener(_b,O,false)
}if(N){clearInterval(N)
}}}if(m.addEventListener){m.addEventListener(_b,function(){J();
O()
},false)
}var N=setInterval(function(){if(/loaded|complete/.test(m.readyState)){J();
O()
}},50);
n&&n({moduleName:Q,sessionId:o,subSystem:R,evtGroup:S,millis:(new Date).getTime(),type:Y});
n&&n({moduleName:Q,sessionId:o,subSystem:R,evtGroup:Lb,millis:(new Date).getTime(),type:T});
m.write(ac)
}fwmockup();