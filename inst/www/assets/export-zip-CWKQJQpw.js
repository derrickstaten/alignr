import{J as F}from"./jszip.min-C4CWc3bC.js";import{e as L,t as I}from"./index-CjhpYNU5.js";import{d as N,p as E}from"./export-CTEIgQew.js";const U=["ggplot2","readr","dplyr"],q=new Set(["svglite","base64enc"]),T=["matplotlib","numpy","pandas","seaborn"],R=new Set(["micropip"]);function _(e){return e.replace(/[^a-z0-9_\-\s]/gi,"_").replace(/\s+/g,"_").toLowerCase()}function G(e){return[...new Set(e.map(t=>t.trim()).filter(t=>t&&!q.has(t)))]}function O(e){return`"${e.replace(/\\/g,"\\\\").replace(/"/g,'\\"')}"`}function j(e){return`required_packages <- c(${G([...U,...e]).map(O).join(", ")})
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required R packages: ",
    paste(missing_packages, collapse = ", "),
    ". Install them with install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))`}function w(e,t){return Number.isFinite(e)?String(e):String(t)}function D(e){let t="",s=0;for(;s<e.length;){const a=e.indexOf("ggsave",s);if(a===-1){t+=e.slice(s);break}const g=e.indexOf("(",a+6);if(g===-1){t+=e.slice(s);break}t+=e.slice(s,a);let u=0,l=-1,c=null;for(let f=g;f<e.length;f++){const p=e[f],m=e[f-1];if(c){p===c&&m!=="\\"&&(c=null);continue}if(p==='"'||p==="'")c=p;else if(p==="(")u++;else if(p===")"&&(u--,u===0)){l=f;break}}if(l===-1){t+=e.slice(a);break}const h=e.slice(a,l+1).replace(/\bwidth\s*=\s*[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?/g,"width = width").replace(/\bheight\s*=\s*[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?/g,"height = height");t+=h,s=l+1}return t}function v(e){return e.replace(/^(\s*USE_ALIGN\s*<-\s*)TRUE\b/m,"$1FALSE")}function H(e,t){const s=[j(t),"",`width <- ${w(e.widthInches,8)}`,`height <- ${w(e.heightInches,6)}`,`font_scale <- ${w(e.fontScale,1)}`].join(`
`),a=v(D(e.sourceCode).trimStart());return`${s}

${a}`}function M(e){return[...new Set(e.map(t=>t.trim()).filter(t=>t&&!R.has(t)))]}function B(e){const t=M([...T,...e]),s=t.map(g=>JSON.stringify(g)).join(", "),a=t.join(" ");return`import importlib.util
import sys

required_packages = [${s}]
# pip install ${a}
missing_packages = [p for p in required_packages if importlib.util.find_spec(p) is None]
if missing_packages:
    sys.exit(
        "Missing required Python packages: "
        + ", ".join(missing_packages)
        + ". Install them with: pip install "
        + " ".join(missing_packages)
    )`}function P(e,t){return Number.isFinite(e)?String(e):String(t)}function K(e){return e.replace(/^(\s*USE_ALIGN\s*=\s*)True\b/m,"$1False")}function Y(e,t){const s=[B(t),"",`width = ${P(e.widthInches,8)}`,`height = ${P(e.heightInches,6)}`,`font_scale = ${P(e.fontScale,1)}`].join(`
`),a=K(e.sourceCode.trimStart());return`${s}

${a}`}async function Z(e,t,s,a=[],g=[]){var x,A;const u=new F,l=u.folder(_(t)+"-export"),c=[],h=new Map(e.map(i=>[i.id,L(i.pageSize)])),f=e.length>1;for(let i=0;i<e.length;i++){const n=e[i],r=f?`page${i+1}_`:"";for(const o of n.visualizations)c.push({...o,name:r+(o.name||o.id)})}if(c.length===0&&e.length<=1)throw new Error("No page data found for current document.");const p=c.filter(i=>(i.language??"r")==="r"),m=c.filter(i=>i.language==="python");if(p.length>0){const i=l.folder("r_code");for(const n of p)i.file(_(n.name||n.id)+".R",H(n,a))}if(m.length>0){const i=l.folder("python_code");for(const n of m)i.file(_(n.name||n.id)+".py",Y(n,g))}const S=l.folder("data"),$=new Set,k=new Set;for(const i of c)for(const n of i.uploadedFiles){const r=n.content;if(!r){k.add(n.name);continue}const o=n.hash??`${n.name}\0${r}`;if(!$.has(o))if($.add(o),/\.rds$/i.test(n.name)){const d=atob(r),b=new Uint8Array(d.length);for(let y=0;y<d.length;y++)b[y]=d.charCodeAt(y);S.file(n.name,b)}else S.file(n.name,r)}if(k.size>0&&S.file("MISSING_FILES.txt",["These input files were not available on this computer when the export ran,","so they are not included. Relink them in Align and export again, or copy them","into this folder yourself:","",...[...k].map(i=>`- ${i}`),""].join(`
`)),s.length===1){const i=await N(s[0]);if(i){const n=h.get((x=e[0])==null?void 0:x.id);l.file("figure.png",i.split(",")[1],{base64:!0});const r=E(i,(n==null?void 0:n.widthPx)??816,(n==null?void 0:n.heightPx)??1056);l.file("figure.pdf",r)}}else{const i=l.folder("figures");for(let n=0;n<s.length;n++){const r=await N(s[n]);if(!r)continue;const o=h.get((A=e[n])==null?void 0:A.id),d=`page${n+1}`;i.file(`${d}.png`,r.split(",")[1],{base64:!0});const b=E(r,(o==null?void 0:o.widthPx)??816,(o==null?void 0:o.heightPx)??1056);i.file(`${d}.pdf`,b)}}return u.generateAsync({type:"blob"})}function X(e){return`${_(e)}-${I()}-export.zip`}export{Z as buildExportZipBlob,X as exportZipFileName};
