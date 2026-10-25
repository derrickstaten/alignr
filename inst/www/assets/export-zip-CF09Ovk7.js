import{J as F}from"./jszip.min-B39yTfqZ.js";import{e as L,t as I}from"./index-BV71j0Xg.js";import{d as N,p as E}from"./export-B3OTeudt.js";const U=["ggplot2","readr","dplyr"],q=new Set(["svglite","base64enc"]),T=["matplotlib","numpy","pandas","seaborn"],R=new Set(["micropip"]);function P(e){return e.replace(/[^a-z0-9_\-\s]/gi,"_").replace(/\s+/g,"_").toLowerCase()}function G(e){return[...new Set(e.map(n=>n.trim()).filter(n=>n&&!q.has(n)))]}function O(e){return`"${e.replace(/\\/g,"\\\\").replace(/"/g,'\\"')}"`}function j(e){return`required_packages <- c(${G([...U,...e]).map(O).join(", ")})
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

invisible(lapply(required_packages, library, character.only = TRUE))`}function S(e,n){return Number.isFinite(e)?String(e):String(n)}function D(e){let n="",o=0;for(;o<e.length;){const s=e.indexOf("ggsave",o);if(s===-1){n+=e.slice(o);break}const g=e.indexOf("(",s+6);if(g===-1){n+=e.slice(o);break}n+=e.slice(o,s);let h=0,r=-1,l=null;for(let f=g;f<e.length;f++){const p=e[f],m=e[f-1];if(l){p===l&&m!=="\\"&&(l=null);continue}if(p==='"'||p==="'")l=p;else if(p==="(")h++;else if(p===")"&&(h--,h===0)){r=f;break}}if(r===-1){n+=e.slice(s);break}const u=e.slice(s,r+1).replace(/\bwidth\s*=\s*[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?/g,"width = width").replace(/\bheight\s*=\s*[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?/g,"height = height");n+=u,o=r+1}return n}function v(e){return e.replace(/^(\s*USE_ALIGN\s*<-\s*)TRUE\b/m,"$1FALSE")}function H(e,n){const o=[j(n),"",`width <- ${S(e.widthInches,8)}`,`height <- ${S(e.heightInches,6)}`,`font_scale <- ${S(e.fontScale,1)}`].join(`
`),s=v(D(e.sourceCode).trimStart());return`${o}

${s}`}function M(e){return[...new Set(e.map(n=>n.trim()).filter(n=>n&&!R.has(n)))]}function B(e){const n=M([...T,...e]),o=n.map(g=>JSON.stringify(g)).join(", "),s=n.join(" ");return`import importlib.util
import sys

required_packages = [${o}]
# pip install ${s}
missing_packages = [p for p in required_packages if importlib.util.find_spec(p) is None]
if missing_packages:
    sys.exit(
        "Missing required Python packages: "
        + ", ".join(missing_packages)
        + ". Install them with: pip install "
        + " ".join(missing_packages)
    )`}function x(e,n){return Number.isFinite(e)?String(e):String(n)}function K(e){return e.replace(/^(\s*USE_ALIGN\s*=\s*)True\b/m,"$1False")}function Y(e,n){const o=[B(n),"",`width = ${x(e.widthInches,8)}`,`height = ${x(e.heightInches,6)}`,`font_scale = ${x(e.fontScale,1)}`].join(`
`),s=K(e.sourceCode.trimStart());return`${o}

${s}`}async function X(e,n,o,s=[],g=[]){var $,A;const h=new F,r=h.folder(P(n)+"-export"),l=[],u=new Map(e.map(t=>[t.id,L(t.pageSize)])),f=e.length>1;for(let t=0;t<e.length;t++){const i=e[t],a=f?`page${t+1}_`:"";for(const c of i.visualizations)l.push({...c,name:a+(c.name||c.id)})}if(l.length===0&&e.length<=1)throw new Error("No page data found for current document.");const p=l.filter(t=>(t.language??"r")==="r"),m=l.filter(t=>t.language==="python");if(p.length>0){const t=r.folder("r_code");for(const i of p)t.file(P(i.name||i.id)+".R",H(i,s))}if(m.length>0){const t=r.folder("python_code");for(const i of m)t.file(P(i.name||i.id)+".py",Y(i,g))}const _=r.folder("data"),k=new Set,w=new Set;for(const t of l)for(const i of t.uploadedFiles){const a=i.content;if(!a){w.add(i.name);continue}const c=i.hash??`${i.name}\0${a}`;if(!k.has(c))if(k.add(c),/\.rds$/i.test(i.name)){const d=atob(a),b=new Uint8Array(d.length);for(let y=0;y<d.length;y++)b[y]=d.charCodeAt(y);_.file(i.name,b)}else _.file(i.name,a)}if(w.size>0&&_.file("MISSING_FILES.txt",["These input files were not available on this computer when the export ran,","so they are not included. Relink them in Align and export again, or copy them","into this folder yourself:","",...[...w].map(t=>`- ${t}`),""].join(`
`)),o.length===1){const t=u.get(($=e[0])==null?void 0:$.id),i=await N(o[0],8,{widthPx:(t==null?void 0:t.widthPx)??816,heightPx:(t==null?void 0:t.heightPx)??1056});if(i){r.file("figure.png",i.split(",")[1],{base64:!0});const a=E(i,(t==null?void 0:t.widthPx)??816,(t==null?void 0:t.heightPx)??1056);r.file("figure.pdf",a)}}else{const t=r.folder("figures");for(let i=0;i<o.length;i++){const a=u.get((A=e[i])==null?void 0:A.id),c=await N(o[i],8,{widthPx:(a==null?void 0:a.widthPx)??816,heightPx:(a==null?void 0:a.heightPx)??1056});if(!c)continue;const d=`page${i+1}`;t.file(`${d}.png`,c.split(",")[1],{base64:!0});const b=E(c,(a==null?void 0:a.widthPx)??816,(a==null?void 0:a.heightPx)??1056);t.file(`${d}.pdf`,b)}}return h.generateAsync({type:"blob"})}function C(e){return`${P(e)}-${I()}-export.zip`}export{X as buildExportZipBlob,C as exportZipFileName};
