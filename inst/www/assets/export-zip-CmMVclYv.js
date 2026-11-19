import{J as L}from"./jszip.min-n9HdXDhN.js";import{e as O,i as U,w as G,b as Z,t as j}from"./index-CM8KoorY.js";import{d as F,p as I}from"./export-Bn5jObr0.js";import"./svg-fonts-B9HpRH8S.js";const k="[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]|[A-Za-z]";function R(t,n){let e="",o=null;for(let a=0;a<t.length;a++){const s=t[a];if(o){if(s==="\\"&&a+1<t.length){e+=n?"  ":s+t[a+1],a++;continue}if(s===o){o=null,e+=s;continue}e+=n&&s!==`
`?" ":s;continue}if(s==='"'||s==="'"||s==="`"){o=s,e+=s;continue}if(s==="#"){for(;a<t.length&&t[a]!==`
`;)a++;a<t.length&&(e+=`
`);continue}e+=s}return e}function D(t){const n=new Set,e=R(t,!1),o=R(t,!0),a=/\b(?:library|require|requireNamespace|loadNamespace)\s*\(/g,s=new RegExp(String.raw`^\s*(["'\x60]?)(${k})\1\s*([,)])`);for(const c of o.matchAll(a)){const u=(c.index??0)+c[0].length,p=s.exec(e.slice(u,u+200));if(!p)continue;if(!(p[1]!=="")&&p[3]===","){const h=e.slice(u+p[0].length,u+p[0].length+200),m=h.indexOf(")");if(/character\.only\s*=\s*(TRUE|T)\b/.test(m>=0?h.slice(0,m):h))continue}n.add(p[2])}for(const c of o.matchAll(/\bp_load\s*\(/g)){const u=(c.index??0)+c[0].length,p=o.indexOf(")",u),g=e.slice(u,p>=0?p:void 0);for(const h of g.split(",")){if(h.includes("="))break;const m=h.trim().replace(/^["'\x60]|["'\x60]$/g,"");new RegExp(`^(?:${k})$`).test(m)&&n.add(m)}}const f=new RegExp(String.raw`(?<![A-Za-z0-9._$@])(${k})\s*:::?(?!:)`,"g");for(const c of o.matchAll(f))n.add(c[1]);return[...n]}const T={sklearn:"scikit-learn",skimage:"scikit-image",PIL:"Pillow",cv2:"opencv-python",bs4:"beautifulsoup4",yaml:"pyyaml",umap:"umap-learn",Bio:"biopython",dateutil:"python-dateutil",attr:"attrs",mpl_toolkits:"matplotlib"};function v(t){return T[t]??t}function M(t){const n=Object.entries(T).find(([,e])=>e.toLowerCase()===t.toLowerCase());return n?n[0]:t.replace(/-/g,"_")}function B(t){let n="",e=0;for(;e<t.length;){const o=t[e];if(o==="#"){for(;e<t.length&&t[e]!==`
`;)e++;continue}if(o==='"'||o==="'"){const a=t.slice(e,e+3)===o.repeat(3),s=a?o.repeat(3):o;for(e+=s.length;e<t.length&&t.slice(e,e+s.length)!==s;){if(t[e]==="\\"){e+=2;continue}if(t[e]===`
`){if(!a)break;n+=`
`}e++}t.slice(e,e+s.length)===s&&(e+=s.length),n+='""';continue}n+=o,e++}return n}function C(t){var o;const n=new Set,e=B(t);for(const a of e.matchAll(/^[ \t]*import[ \t]+([^\n;]+)/gm))for(const s of a[1].split(",")){const f=(o=s.trim().split(/\s+/)[0])==null?void 0:o.split(".")[0];f&&/^[A-Za-z_]\w*$/.test(f)&&n.add(f)}for(const a of e.matchAll(/^[ \t]*from[ \t]+([A-Za-z_][\w.]*)[ \t]+import\b/gm))n.add(a[1].split(".")[0]);for(const a of t.matchAll(/\b(?:import_module|__import__)\s*\(\s*["']([A-Za-z_][\w.]*)["']/g))n.add(a[1].split(".")[0]);return[...n]}const K=["ggplot2","readr","dplyr"],H=new Set(["svglite","base64enc"]),Y=["matplotlib","numpy","pandas","seaborn"],J=new Set(["micropip"]);function _(t){return t.replace(/[^a-z0-9_\-\s]/gi,"_").replace(/\s+/g,"_").toLowerCase()}function q(t){return[...new Set(t.map(n=>n.trim()).filter(n=>n&&!H.has(n)))]}function V(t){return`"${t.replace(/\\/g,"\\\\").replace(/"/g,'\\"')}"`}function X(t,n){const e=q([...K,...n]),o=q([...e,...D(t)]),a=s=>s.map(V).join(", ");return`required_packages <- c(${a(o)})
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

invisible(lapply(c(${a(e)}), library, character.only = TRUE))`}function x(t,n){return Number.isFinite(t)?String(t):String(n)}function z(t){let n="",e=0;for(;e<t.length;){const o=t.indexOf("ggsave",e);if(o===-1){n+=t.slice(e);break}const a=t.indexOf("(",o+6);if(a===-1){n+=t.slice(e);break}n+=t.slice(e,o);let s=0,f=-1,c=null;for(let p=a;p<t.length;p++){const g=t[p],h=t[p-1];if(c){g===c&&h!=="\\"&&(c=null);continue}if(g==='"'||g==="'")c=g;else if(g==="(")s++;else if(g===")"&&(s--,s===0)){f=p;break}}if(f===-1){n+=t.slice(o);break}const u=t.slice(o,f+1).replace(/\bwidth\s*=\s*[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?/g,"width = width").replace(/\bheight\s*=\s*[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?/g,"height = height");n+=u,e=f+1}return n}function W(t){return t.replace(/^(\s*USE_ALIGN\s*<-\s*)TRUE\b/m,"$1FALSE")}function Q(t,n){const e=[X(t.sourceCode,n),"",`width <- ${x(t.widthInches,8)}`,`height <- ${x(t.heightInches,6)}`,`font_scale <- ${x(t.fontScale,1)}`].join(`
`),o=W(z(t.sourceCode).trimStart());return`${e}

${o}`}function tt(t){return[...new Set(t.map(n=>n.trim()).filter(n=>n&&!J.has(n)))]}function et(t,n){return`import importlib.util
import sys

required_packages = {${tt([...Y,...n.map(M),...C(t)]).map(a=>`${JSON.stringify(a)}: ${JSON.stringify(v(a))}`).join(", ")}}  # import name: pip package
missing_packages = [pkg for mod, pkg in required_packages.items() if importlib.util.find_spec(mod) is None]
if missing_packages:
    sys.exit(
        "Missing required Python packages: "
        + ", ".join(missing_packages)
        + ". Install them with: pip install "
        + " ".join(missing_packages)
    )`}function S(t,n){return Number.isFinite(t)?String(t):String(n)}function nt(t){return t.replace(/^(\s*USE_ALIGN\s*=\s*)True\b/m,"$1False")}function it(t,n){const e=[et(t.sourceCode,n),"",`width = ${S(t.widthInches,8)}`,`height = ${S(t.heightInches,6)}`,`font_scale = ${S(t.fontScale,1)}`].join(`
`),o=nt(t.sourceCode.trimStart());return`${e}

${o}`}async function lt(t,n,e,o=[],a=[]){var E,N;const s=new L,f=s.folder(_(n)+"-export"),c=[],u=new Map(t.map(i=>[i.id,O(i.pageSize)])),p=t.length>1;for(let i=0;i<t.length;i++){const r=t[i],l=p?`page${i+1}_`:"";for(const d of r.visualizations)c.push({...d,name:l+(d.name||d.id)})}if(c.length===0&&t.length<=1)throw new Error("No page data found for current document.");const g=c.filter(i=>(i.language??"r")==="r"),h=c.filter(i=>i.language==="python");if(g.length>0){const i=f.folder("r_code");for(const r of g)i.file(_(r.name||r.id)+".R",Q(r,o))}if(h.length>0){const i=f.folder("python_code");for(const r of h)i.file(_(r.name||r.id)+".py",it(r,a))}const m=f.folder("data"),A=new Set,P=new Set,$=[];for(const i of c)for(const r of i.uploadedFiles){const l=r.content;if(!l){P.add(r.name);continue}const d=r.hash??`${r.name}\0${l}`;if(!A.has(d)){if(A.add(d),U(r.name)){$.push({name:r.name,content:l});continue}if(/\.rds$/i.test(r.name)){const b=atob(l),w=new Uint8Array(b.length);for(let y=0;y<b.length;y++)w[y]=b.charCodeAt(y);m.file(r.name,w)}else m.file(r.name,l)}}for(const i of G($))m.file(i.fileName)||m.file(i.fileName,await Z(i.sheets));if(P.size>0&&m.file("MISSING_FILES.txt",["These input files were not available on this computer when the export ran,","so they are not included. Relink them in Align and export again, or copy them","into this folder yourself:","",...[...P].map(i=>`- ${i}`),""].join(`
`)),e.length===1){const i=u.get((E=t[0])==null?void 0:E.id),r=await F(e[0],8,{widthPx:(i==null?void 0:i.widthPx)??816,heightPx:(i==null?void 0:i.heightPx)??1056});if(r){f.file("figure.png",r.split(",")[1],{base64:!0});const l=I(r,(i==null?void 0:i.widthPx)??816,(i==null?void 0:i.heightPx)??1056);f.file("figure.pdf",l)}}else{const i=f.folder("figures");for(let r=0;r<e.length;r++){const l=u.get((N=t[r])==null?void 0:N.id),d=await F(e[r],8,{widthPx:(l==null?void 0:l.widthPx)??816,heightPx:(l==null?void 0:l.heightPx)??1056});if(!d)continue;const b=`page${r+1}`;i.file(`${b}.png`,d.split(",")[1],{base64:!0});const w=I(d,(l==null?void 0:l.widthPx)??816,(l==null?void 0:l.heightPx)??1056);i.file(`${b}.pdf`,w)}}return s.generateAsync({type:"blob"})}function ct(t){return`${_(t)}-${j()}-export.zip`}export{lt as buildExportZipBlob,ct as exportZipFileName};
