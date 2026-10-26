import{J as L}from"./jszip.min-DCnDV5AH.js";import{e as I,i as U,w as q,b as T,t as R}from"./index-DT8asgE3.js";import{d as F,p as E}from"./export-BaiP3Ksc.js";import"./svg-fonts-B9HpRH8S.js";const G=["ggplot2","readr","dplyr"],O=new Set(["svglite","base64enc"]),j=["matplotlib","numpy","pandas","seaborn"],D=new Set(["micropip"]);function P(e){return e.replace(/[^a-z0-9_\-\s]/gi,"_").replace(/\s+/g,"_").toLowerCase()}function v(e){return[...new Set(e.map(i=>i.trim()).filter(i=>i&&!O.has(i)))]}function M(e){return`"${e.replace(/\\/g,"\\\\").replace(/"/g,'\\"')}"`}function B(e){return`required_packages <- c(${v([...G,...e]).map(M).join(", ")})
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

invisible(lapply(required_packages, library, character.only = TRUE))`}function k(e,i){return Number.isFinite(e)?String(e):String(i)}function H(e){let i="",a=0;for(;a<e.length;){const s=e.indexOf("ggsave",a);if(s===-1){i+=e.slice(a);break}const g=e.indexOf("(",s+6);if(g===-1){i+=e.slice(a);break}i+=e.slice(a,s);let h=0,r=-1,l=null;for(let p=g;p<e.length;p++){const f=e[p],b=e[p-1];if(l){f===l&&b!=="\\"&&(l=null);continue}if(f==='"'||f==="'")l=f;else if(f==="(")h++;else if(f===")"&&(h--,h===0)){r=p;break}}if(r===-1){i+=e.slice(s);break}const m=e.slice(s,r+1).replace(/\bwidth\s*=\s*[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?/g,"width = width").replace(/\bheight\s*=\s*[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?/g,"height = height");i+=m,a=r+1}return i}function K(e){return e.replace(/^(\s*USE_ALIGN\s*<-\s*)TRUE\b/m,"$1FALSE")}function Y(e,i){const a=[B(i),"",`width <- ${k(e.widthInches,8)}`,`height <- ${k(e.heightInches,6)}`,`font_scale <- ${k(e.fontScale,1)}`].join(`
`),s=K(H(e.sourceCode).trimStart());return`${a}

${s}`}function J(e){return[...new Set(e.map(i=>i.trim()).filter(i=>i&&!D.has(i)))]}function V(e){const i=J([...j,...e]),a=i.map(g=>JSON.stringify(g)).join(", "),s=i.join(" ");return`import importlib.util
import sys

required_packages = [${a}]
# pip install ${s}
missing_packages = [p for p in required_packages if importlib.util.find_spec(p) is None]
if missing_packages:
    sys.exit(
        "Missing required Python packages: "
        + ", ".join(missing_packages)
        + ". Install them with: pip install "
        + " ".join(missing_packages)
    )`}function x(e,i){return Number.isFinite(e)?String(e):String(i)}function X(e){return e.replace(/^(\s*USE_ALIGN\s*=\s*)True\b/m,"$1False")}function Z(e,i){const a=[V(i),"",`width = ${x(e.widthInches,8)}`,`height = ${x(e.heightInches,6)}`,`font_scale = ${x(e.fontScale,1)}`].join(`
`),s=X(e.sourceCode.trimStart());return`${a}

${s}`}async function ee(e,i,a,s=[],g=[]){var A,N;const h=new L,r=h.folder(P(i)+"-export"),l=[],m=new Map(e.map(t=>[t.id,I(t.pageSize)])),p=e.length>1;for(let t=0;t<e.length;t++){const n=e[t],o=p?`page${t+1}_`:"";for(const c of n.visualizations)l.push({...c,name:o+(c.name||c.id)})}if(l.length===0&&e.length<=1)throw new Error("No page data found for current document.");const f=l.filter(t=>(t.language??"r")==="r"),b=l.filter(t=>t.language==="python");if(f.length>0){const t=r.folder("r_code");for(const n of f)t.file(P(n.name||n.id)+".R",Y(n,s))}if(b.length>0){const t=r.folder("python_code");for(const n of b)t.file(P(n.name||n.id)+".py",Z(n,g))}const d=r.folder("data"),S=new Set,_=new Set,$=[];for(const t of l)for(const n of t.uploadedFiles){const o=n.content;if(!o){_.add(n.name);continue}const c=n.hash??`${n.name}\0${o}`;if(!S.has(c)){if(S.add(c),U(n.name)){$.push({name:n.name,content:o});continue}if(/\.rds$/i.test(n.name)){const u=atob(o),y=new Uint8Array(u.length);for(let w=0;w<u.length;w++)y[w]=u.charCodeAt(w);d.file(n.name,y)}else d.file(n.name,o)}}for(const t of q($))d.file(t.fileName)||d.file(t.fileName,await T(t.sheets));if(_.size>0&&d.file("MISSING_FILES.txt",["These input files were not available on this computer when the export ran,","so they are not included. Relink them in Align and export again, or copy them","into this folder yourself:","",...[..._].map(t=>`- ${t}`),""].join(`
`)),a.length===1){const t=m.get((A=e[0])==null?void 0:A.id),n=await F(a[0],8,{widthPx:(t==null?void 0:t.widthPx)??816,heightPx:(t==null?void 0:t.heightPx)??1056});if(n){r.file("figure.png",n.split(",")[1],{base64:!0});const o=E(n,(t==null?void 0:t.widthPx)??816,(t==null?void 0:t.heightPx)??1056);r.file("figure.pdf",o)}}else{const t=r.folder("figures");for(let n=0;n<a.length;n++){const o=m.get((N=e[n])==null?void 0:N.id),c=await F(a[n],8,{widthPx:(o==null?void 0:o.widthPx)??816,heightPx:(o==null?void 0:o.heightPx)??1056});if(!c)continue;const u=`page${n+1}`;t.file(`${u}.png`,c.split(",")[1],{base64:!0});const y=E(c,(o==null?void 0:o.widthPx)??816,(o==null?void 0:o.heightPx)??1056);t.file(`${u}.pdf`,y)}}return h.generateAsync({type:"blob"})}function te(e){return`${P(e)}-${R()}-export.zip`}export{ee as buildExportZipBlob,te as exportZipFileName};
