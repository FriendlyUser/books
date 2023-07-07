import"./index.53209a6f.js";import{r as a}from"./index.934aab12.js";import{h as n}from"./textConverter.40085d3b.js";import{I as i}from"./index.5f322692.js";import{j as e}from"./jsx-runtime.9c340f8c.js";import"./marked.esm.cdc2cd67.js";import"./index.02c8dfb6.js";var c=a.GenIcon,d=function(s){return c({tag:"svg",attr:{fill:"currentColor",viewBox:"0 0 16 16"},child:[{tag:"path",attr:{d:"M9.828.722a.5.5 0 0 1 .354.146l4.95 4.95a.5.5 0 0 1 0 .707c-.48.48-1.072.588-1.503.588-.177 0-.335-.018-.46-.039l-3.134 3.134a5.927 5.927 0 0 1 .16 1.013c.046.702-.032 1.687-.72 2.375a.5.5 0 0 1-.707 0l-2.829-2.828-3.182 3.182c-.195.195-1.219.902-1.414.707-.195-.195.512-1.22.707-1.414l3.182-3.182-2.828-2.829a.5.5 0 0 1 0-.707c.688-.688 1.673-.767 2.375-.72a5.922 5.922 0 0 1 1.013.16l3.134-3.133a2.772 2.772 0 0 1-.04-.461c0-.43.108-1.022.589-1.503a.5.5 0 0 1 .353-.146z"}}]})(s)};const b=({item:r})=>{const s=i[n(r.icon)];return e.jsx("div",{className:"mt-8 px-3 md:col-6 lg:col-4 lg:mt-0",children:e.jsxs("div",{className:`rounded-xl bg-white px-8 py-10 shadow-lg ${r.featured?"-mt-16 border border-primary ":void 0}`,children:[e.jsxs("div",{className:"flex items-center justify-between",children:[e.jsxs("div",{children:[e.jsx("h2",{className:"h3",children:r.title}),e.jsxs("p",{className:"mt-3 text-2xl text-dark",children:[r.pre_currency," ",r.price,".00 ",r.post_currency]})]}),e.jsx("span",{className:`inline-flex h-16 w-16 items-center justify-center rounded-full font-bold ${r.featured?"bg-gradient text-white":"bg-theme-light text-dark"}`,children:e.jsx(s,{className:"font-semibold"})})]}),e.jsx("p",{className:"mt-6",children:r.description}),e.jsxs("div",{className:"my-6 border-y border-border py-6",children:[e.jsx("h4",{className:"h6",children:r.services.title}),e.jsx("ul",{className:"mt-6",children:r.services.list.map((l,t)=>e.jsxs("li",{className:"mb-3 text-sm",children:[e.jsx("span",{className:"mr-2",children:e.jsx(d,{className:`mr-1 inline h-[14px] w-[14px] ${r.featured?"text-primary":void 0}`})}),l]},`service-${t}`))})]}),e.jsxs("div",{className:"text-center",children:[e.jsx("a",{className:`btn ${r.featured?"btn-primary":"btn-outline-white"} block h-[48px] w-full rounded-[50px] leading-[30px]`,href:r.buttons.buy_now.link,children:r.buttons.buy_now.label}),e.jsxs("a",{className:"mt-6 inline-flex items-center text-dark",href:r.buttons.free_trial.link,children:[r.buttons.free_trial.label,e.jsx("svg",{className:"ml-1.5",width:"13",height:"16",viewBox:"0 0 13 16",fill:"none",xmlns:"http://www.w3.org/2000/svg",children:e.jsx("path",{d:"M12.7071 8.70711C13.0976 8.31658 13.0976 7.68342 12.7071 7.29289L6.34315 0.928932C5.95262 0.538408 5.31946 0.538408 4.92893 0.928932C4.53841 1.31946 4.53841 1.95262 4.92893 2.34315L10.5858 8L4.92893 13.6569C4.53841 14.0474 4.53841 14.6805 4.92893 15.0711C5.31946 15.4616 5.95262 15.4616 6.34315 15.0711L12.7071 8.70711ZM0 9H12V7H0V9Z",fill:"currentColor"})})]})]})]})},r.title)};export{b as default};