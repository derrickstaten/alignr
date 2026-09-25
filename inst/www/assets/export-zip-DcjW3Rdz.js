import{J as E}from"./jszip.min-DYkqbRfo.js";import{e as F,t as L}from"./index-QjxVhxK6.js";import{d as x,p as N}from"./export-DKwyaG8a.js";const U=["ggplot2","readr","dplyr"],q=new Set(["svglite","base64enc"]),I=["matplotlib","numpy","pandas","seaborn"],T=new Set(["micropip"]);function y(e){return e.replace(/[^a-z0-9_\-\s]/gi,"_").replace(/\s+/g,"_").toLowerCase()}function R(e){return[...new Set(e.map(t=>t.trim()).filter(t=>t&&!q.has(t)))]}function G(e){return`"${e.replace(/\\/g,"\\\\").replace(/"/g,'\\"')}"`}function O(e){return`required_packages <- c(${R([...U,...e]).map(G).join(", ")})
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

invisible(lapply(required_packages, library, character.only = TRUE))`}function P(e,t){return Number.isFinite(e)?String(e):String(t)}function D(e){let t="",s=0;for(;s<e.length;){const o=e.indexOf("ggsave",s);if(o===-1){t+=e.slice(s);break}const g=e.indexOf("(",o+6);if(g===-1){t+=e.slice(s);break}t+=e.slice(s,o);let u=0,r=-1,c=null;for(let f=g;f<e.length;f++){const p=e[f],m=e[f-1];if(c){p===c&&m!=="\\"&&(c=null);continue}if(p==='"'||p==="'")c=p;else if(p==="(")u++;else if(p===")"&&(u--,u===0)){r=f;break}}if(r===-1){t+=e.slice(o);break}const h=e.slice(o,r+1).replace(/\bwidth\s*=\s*[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?/g,"width = width").replace(/\bheight\s*=\s*[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?/g,"height = height");t+=h,s=r+1}return t}function j(e){return e.replace(/^(\s*USE_ALIGN\s*<-\s*)TRUE\b/m,"$1FALSE")}function v(e,t){const s=[O(t),"",`width <- ${P(e.widthInches,8)}`,`height <- ${P(e.heightInches,6)}`,`font_scale <- ${P(e.fontScale,1)}`].join(`
`),o=j(D(e.sourceCode).trimStart());return`${s}

${o}`}function H(e){return[...new Set(e.map(t=>t.trim()).filter(t=>t&&!T.has(t)))]}function B(e){const t=H([...I,...e]),s=t.map(g=>JSON.stringify(g)).join(", "),o=t.join(" ");return`import importlib.util
import sys

required_packages = [${s}]
# pip install ${o}
missing_packages = [p for p in required_packages if importlib.util.find_spec(p) is None]
if missing_packages:
    sys.exit(
        "Missing required Python packages: "
        + ", ".join(missing_packages)
        + ". Install them with: pip install "
        + " ".join(missing_packages)
    )`}function k(e,t){return Number.isFinite(e)?String(e):String(t)}function K(e){return e.replace(/^(\s*USE_ALIGN\s*=\s*)True\b/m,"$1False")}function M(e,t){const s=[B(t),"",`width = ${k(e.widthInches,8)}`,`height = ${k(e.heightInches,6)}`,`font_scale = ${k(e.fontScale,1)}`].join(`
`),o=K(e.sourceCode.trimStart());return`${s}

${o}`}async function Z(e,t,s,o=[],g=[]){var $,A;const u=new E,r=u.folder(y(t)+"-export"),c=[],h=new Map(e.map(i=>[i.id,F(i.pageSize)])),f=e.length>1;for(let i=0;i<e.length;i++){const n=e[i],l=f?`page${i+1}_`:"";for(const a of n.visualizations)c.push({...a,name:l+(a.name||a.id)})}if(c.length===0&&e.length<=1)throw new Error("No page data found for current document.");const p=c.filter(i=>(i.language??"r")==="r"),m=c.filter(i=>i.language==="python");if(p.length>0){const i=r.folder("r_code");for(const n of p)i.file(y(n.name||n.id)+".R",v(n,o))}if(m.length>0){const i=r.folder("python_code");for(const n of m)i.file(y(n.name||n.id)+".py",M(n,g))}const S=r.folder("data"),w=new Set;for(const i of c)for(const n of i.uploadedFiles){const l=`${n.name}:${n.content.length}`;if(w.has(l))continue;w.add(l);const a=n.content;if(a)if(/\.rds$/i.test(n.name)){const d=atob(a),b=new Uint8Array(d.length);for(let _=0;_<d.length;_++)b[_]=d.charCodeAt(_);S.file(n.name,b)}else S.file(n.name,a)}if(s.length===1){const i=await x(s[0]);if(i){const n=h.get(($=e[0])==null?void 0:$.id);r.file("figure.png",i.split(",")[1],{base64:!0});const l=N(i,(n==null?void 0:n.widthPx)??816,(n==null?void 0:n.heightPx)??1056);r.file("figure.pdf",l)}}else{const i=r.folder("figures");for(let n=0;n<s.length;n++){const l=await x(s[n]);if(!l)continue;const a=h.get((A=e[n])==null?void 0:A.id),d=`page${n+1}`;i.file(`${d}.png`,l.split(",")[1],{base64:!0});const b=N(l,(a==null?void 0:a.widthPx)??816,(a==null?void 0:a.heightPx)??1056);i.file(`${d}.pdf`,b)}}return u.generateAsync({type:"blob"})}function z(e){return`${y(e)}-${L()}-export.zip`}export{Z as buildExportZipBlob,z as exportZipFileName};
