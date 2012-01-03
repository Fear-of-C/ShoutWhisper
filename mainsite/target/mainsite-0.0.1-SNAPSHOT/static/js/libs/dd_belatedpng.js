var DD_belatedPNG={ns:"DD_belatedPNG",imgSize:{},delay:10,nodesFixed:0,createVmlNameSpace:function(){if(document.namespaces&&!document.namespaces[this.ns]){document.namespaces.add(this.ns,"urn:schemas-microsoft-com:vml")
}},createVmlStyleSheet:function(){var A,B;
A=document.createElement("style");
A.setAttribute("media","screen");
document.documentElement.firstChild.insertBefore(A,document.documentElement.firstChild.firstChild);
if(A.styleSheet){A=A.styleSheet;
A.addRule(this.ns+"\\:*","{behavior:url(#default#VML)}");
A.addRule(this.ns+"\\:shape","position:absolute;");
A.addRule("img."+this.ns+"_sizeFinder","behavior:none; border:none; position:absolute; z-index:-1; top:-10000px; visibility:hidden;");
this.screenStyleSheet=A;
B=document.createElement("style");
B.setAttribute("media","print");
document.documentElement.firstChild.insertBefore(B,document.documentElement.firstChild.firstChild);
B=B.styleSheet;
B.addRule(this.ns+"\\:*","{display: none !important;}");
B.addRule("img."+this.ns+"_sizeFinder","{display: none !important;}")
}},readPropertyChange:function(){var A,C,B;
A=event.srcElement;
if(!A.vmlInitiated){return 
}if(event.propertyName.search("background")!=-1||event.propertyName.search("border")!=-1){DD_belatedPNG.applyVML(A)
}if(event.propertyName=="style.display"){C=(A.currentStyle.display=="none")?"none":"block";
for(B in A.vml){if(A.vml.hasOwnProperty(B)){A.vml[B].shape.style.display=C
}}}if(event.propertyName.search("filter")!=-1){DD_belatedPNG.vmlOpacity(A)
}},vmlOpacity:function(A){if(A.currentStyle.filter.search("lpha")!=-1){var B=A.currentStyle.filter;
B=parseInt(B.substring(B.lastIndexOf("=")+1,B.lastIndexOf(")")),10)/100;
A.vml.color.shape.style.filter=A.currentStyle.filter;
A.vml.image.fill.opacity=B
}},handlePseudoHover:function(A){setTimeout(function(){DD_belatedPNG.applyVML(A)
},1)
},fix:function(B){if(this.screenStyleSheet){var C,A;
C=B.split(",");
for(A=0;
A<C.length;
A++){this.screenStyleSheet.addRule(C[A],"behavior:expression(DD_belatedPNG.fixPng(this))")
}}},applyVML:function(A){A.runtimeStyle.cssText="";
this.vmlFill(A);
this.vmlOffsets(A);
this.vmlOpacity(A);
if(A.isImg){this.copyImageBorders(A)
}},attachHandlers:function(B){var F,G,C,E,A,D;
F=this;
G={resize:"vmlOffsets",move:"vmlOffsets"};
if(B.nodeName=="A"){E={mouseleave:"handlePseudoHover",mouseenter:"handlePseudoHover",focus:"handlePseudoHover",blur:"handlePseudoHover"};
for(A in E){if(E.hasOwnProperty(A)){G[A]=E[A]
}}}for(D in G){if(G.hasOwnProperty(D)){C=function(){F[G[D]](B)
};
B.attachEvent("on"+D,C)
}}B.attachEvent("onpropertychange",this.readPropertyChange)
},giveLayout:function(A){A.style.zoom=1;
if(A.currentStyle.position=="static"){A.style.position="relative"
}},copyImageBorders:function(A){var C,B;
C={borderStyle:true,borderWidth:true,borderColor:true};
for(B in C){if(C.hasOwnProperty(B)){A.vml.color.shape.style[B]=A.currentStyle[B]
}}},vmlFill:function(E){if(!E.currentStyle){return 
}else{var F,D,C,A,B,G;
F=E.currentStyle
}for(A in E.vml){if(E.vml.hasOwnProperty(A)){E.vml[A].shape.style.zIndex=F.zIndex
}}E.runtimeStyle.backgroundColor="";
E.runtimeStyle.backgroundImage="";
D=true;
if(F.backgroundImage!="none"||E.isImg){if(!E.isImg){E.vmlBg=F.backgroundImage;
E.vmlBg=E.vmlBg.substr(5,E.vmlBg.lastIndexOf('")')-5)
}else{E.vmlBg=E.src
}C=this;
if(!C.imgSize[E.vmlBg]){B=document.createElement("img");
C.imgSize[E.vmlBg]=B;
B.className=C.ns+"_sizeFinder";
B.runtimeStyle.cssText="behavior:none; position:absolute; left:-10000px; top:-10000px; border:none; margin:0; padding:0;";
G=function(){this.width=this.offsetWidth;
this.height=this.offsetHeight;
C.vmlOffsets(E)
};
B.attachEvent("onload",G);
B.src=E.vmlBg;
B.removeAttribute("width");
B.removeAttribute("height");
document.body.insertBefore(B,document.body.firstChild)
}E.vml.image.fill.src=E.vmlBg;
D=false
}E.vml.image.fill.on=!D;
E.vml.image.fill.color="none";
E.vml.color.shape.style.backgroundColor=F.backgroundColor;
E.runtimeStyle.backgroundImage="none";
E.runtimeStyle.backgroundColor="transparent"
},vmlOffsets:function(K){var G,A,L,J,H,B,I,C,E,F,D;
G=K.currentStyle;
A={W:K.clientWidth+1,H:K.clientHeight+1,w:this.imgSize[K.vmlBg].width,h:this.imgSize[K.vmlBg].height,L:K.offsetLeft,T:K.offsetTop,bLW:K.clientLeft,bTW:K.clientTop};
L=(A.L+A.bLW==1)?1:0;
J=function(M,Q,P,R,O,N){M.coordsize=R+","+O;
M.coordorigin=N+","+N;
M.path="m0,0l"+R+",0l"+R+","+O+"l0,"+O+" xe";
M.style.width=R+"px";
M.style.height=O+"px";
M.style.left=Q+"px";
M.style.top=P+"px"
};
J(K.vml.color.shape,(A.L+(K.isImg?0:A.bLW)),(A.T+(K.isImg?0:A.bTW)),(A.W-1),(A.H-1),0);
J(K.vml.image.shape,(A.L+A.bLW),(A.T+A.bTW),(A.W),(A.H),1);
H={X:0,Y:0};
if(K.isImg){H.X=parseInt(G.paddingLeft,10)+1;
H.Y=parseInt(G.paddingTop,10)+1
}else{for(E in H){if(H.hasOwnProperty(E)){this.figurePercentage(H,A,E,G["backgroundPosition"+E])
}}}K.vml.image.fill.position=(H.X/A.W)+","+(H.Y/A.H);
B=G.backgroundRepeat;
I={T:1,R:A.W+L,B:A.H,L:1+L};
C={X:{b1:"L",b2:"R",d:"W"},Y:{b1:"T",b2:"B",d:"H"}};
if(B!="repeat"||K.isImg){F={T:(H.Y),R:(H.X+A.w),B:(H.Y+A.h),L:(H.X)};
if(B.search("repeat-")!=-1){D=B.split("repeat-")[1].toUpperCase();
F[C[D].b1]=1;
F[C[D].b2]=A[C[D].d]
}if(F.B>A.H){F.B=A.H
}K.vml.image.shape.style.clip="rect("+F.T+"px "+(F.R+L)+"px "+F.B+"px "+(F.L+L)+"px)"
}else{K.vml.image.shape.style.clip="rect("+I.T+"px "+I.R+"px "+I.B+"px "+I.L+"px)"
}},figurePercentage:function(E,F,C,B){var A,D;
D=true;
A=(C=="X");
switch(B){case"left":case"top":E[C]=0;
break;
case"center":E[C]=0.5;
break;
case"right":case"bottom":E[C]=1;
break;
default:if(B.search("%")!=-1){E[C]=parseInt(B,10)/100
}else{D=false
}}E[C]=Math.ceil(D?((F[A?"W":"H"]*E[C])-(F[A?"w":"h"]*E[C])):parseInt(B,10));
if(E[C]%2===0){E[C]++
}return E[C]
},fixPng:function(F){F.style.behavior="none";
var C,A,D,B,E;
if(F.nodeName=="BODY"||F.nodeName=="TD"||F.nodeName=="TR"){return 
}F.isImg=false;
if(F.nodeName=="IMG"){if(F.src.toLowerCase().search(/\.png$/)!=-1){F.isImg=true;
F.style.visibility="hidden"
}else{return 
}}else{if(F.currentStyle.backgroundImage.toLowerCase().search(".png")==-1){return 
}}C=DD_belatedPNG;
F.vml={color:{},image:{}};
A={shape:{},fill:{}};
for(B in F.vml){if(F.vml.hasOwnProperty(B)){for(E in A){if(A.hasOwnProperty(E)){D=C.ns+":"+E;
F.vml[B][E]=document.createElement(D)
}}F.vml[B].shape.stroked=false;
F.vml[B].shape.appendChild(F.vml[B].fill);
F.parentNode.insertBefore(F.vml[B].shape,F)
}}F.vml.image.shape.fillcolor="none";
F.vml.image.fill.type="tile";
F.vml.color.fill.on=false;
C.attachHandlers(F);
C.giveLayout(F);
C.giveLayout(F.offsetParent);
F.vmlInitiated=true;
C.applyVML(F)
}};
try{document.execCommand("BackgroundImageCache",false,true)
}catch(r){}DD_belatedPNG.createVmlNameSpace();
DD_belatedPNG.createVmlStyleSheet();