import"./hoisted.dfc8849a.js";var z={exports:{}};function D(){}D.prototype={on:function(n,t,e){var s=this.e||(this.e={});return(s[n]||(s[n]=[])).push({fn:t,ctx:e}),this},once:function(n,t,e){var s=this;function i(){s.off(n,i),t.apply(e,arguments)}return i._=t,this.on(n,i,e)},emit:function(n){var t=[].slice.call(arguments,1),e=((this.e||(this.e={}))[n]||[]).slice(),s=0,i=e.length;for(s;s<i;s++)e[s].fn.apply(e[s].ctx,t);return this},off:function(n,t){var e=this.e||(this.e={}),s=e[n],i=[];if(s&&t)for(var r=0,o=s.length;r<o;r++)s[r].fn!==t&&s[r].fn._!==t&&i.push(s[r]);return i.length?e[n]=i:delete e[n],this}};z.exports=D;z.exports.TinyEmitter=D;var q=function(t,e,s){s||(typeof e=="function"?(s=e,e=null):s=U);var i=t&&t.length;if(!i)return s(null,[]);var r=!1,o=new Array(i);t.forEach(e?function(u,a){u.call(e,h(a))}:function(u,a){u(h(a))});function h(u){return function(a,f){if(!r){if(a){s(a,o),r=!0;return}o[u]=f,--i||s(null,o)}}}};function U(){}function c(n){return parseFloat(n)||0}class b{constructor(t,e){this.x=c(t),this.y=c(e)}static equals(t,e){return t.x===e.x&&t.y===e.y}}class C{constructor(t,e,s,i,r){this.id=r,this.left=t,this.top=e,this.width=s,this.height=i}static intersects(t,e){return t.left<e.left+e.width&&e.left<t.left+t.width&&t.top<e.top+e.height&&e.top<t.top+t.height}}var m={BASE:"shuffle",SHUFFLE_ITEM:"shuffle-item",VISIBLE:"shuffle-item--visible",HIDDEN:"shuffle-item--hidden"};let M=0;class d{constructor(t,e){M+=1,this.id=M,this.element=t,this.isRTL=e,this.isVisible=!0,this.isHidden=!1}show(){this.isVisible=!0,this.element.classList.remove(m.HIDDEN),this.element.classList.add(m.VISIBLE),this.element.removeAttribute("aria-hidden")}hide(){this.isVisible=!1,this.element.classList.remove(m.VISIBLE),this.element.classList.add(m.HIDDEN),this.element.setAttribute("aria-hidden",!0)}init(){this.addClasses([m.SHUFFLE_ITEM,m.VISIBLE]),this.applyCss(d.Css.INITIAL),this.applyCss(this.isRTL?d.Css.DIRECTION.rtl:d.Css.DIRECTION.ltr),this.scale=d.Scale.VISIBLE,this.point=new b}addClasses(t){t.forEach(e=>{this.element.classList.add(e)})}removeClasses(t){t.forEach(e=>{this.element.classList.remove(e)})}applyCss(t){Object.keys(t).forEach(e=>{this.element.style[e]=t[e]})}dispose(){this.removeClasses([m.HIDDEN,m.VISIBLE,m.SHUFFLE_ITEM]),this.element.removeAttribute("style"),this.element=null}}d.Css={INITIAL:{position:"absolute",top:0,visibility:"visible",willChange:"transform"},DIRECTION:{ltr:{left:0},rtl:{right:0}},VISIBLE:{before:{opacity:1,visibility:"visible"},after:{transitionDelay:""}},HIDDEN:{before:{opacity:0},after:{visibility:"hidden",transitionDelay:""}}};d.Scale={VISIBLE:1,HIDDEN:.001};let S=null;var F=()=>{if(S!==null)return S;const n=document.body||document.documentElement,t=document.createElement("div");t.style.cssText="width:10px;padding:2px;box-sizing:border-box;",n.appendChild(t);const{width:e}=window.getComputedStyle(t,null);return S=Math.round(c(e))===10,n.removeChild(t),S};function y(n,t){let e=arguments.length>2&&arguments[2]!==void 0?arguments[2]:window.getComputedStyle(n,null),s=c(e[t]);return!F()&&t==="width"?s+=c(e.paddingLeft)+c(e.paddingRight)+c(e.borderLeftWidth)+c(e.borderRightWidth):!F()&&t==="height"&&(s+=c(e.paddingTop)+c(e.paddingBottom)+c(e.borderTopWidth)+c(e.borderBottomWidth)),s}function Y(n){let t=n.length;for(;t;){t-=1;const e=Math.floor(Math.random()*(t+1)),s=n[e];n[e]=n[t],n[t]=s}return n}const k={reverse:!1,by:null,compare:null,randomize:!1,key:"element"};function w(n,t){const e={...k,...t},s=Array.from(n);let i=!1;return n.length?e.randomize?Y(n):(typeof e.by=="function"?n.sort((r,o)=>{if(i)return 0;const h=e.by(r[e.key]),u=e.by(o[e.key]);return h===void 0&&u===void 0?(i=!0,0):h<u||h==="sortFirst"||u==="sortLast"?-1:h>u||h==="sortLast"||u==="sortFirst"?1:0}):typeof e.compare=="function"&&n.sort(e.compare),i?s:(e.reverse&&n.reverse(),n)):[]}const T={},A="transitionend";let x=0;function Q(){return x+=1,A+x}function R(n){return T[n]?(T[n].element.removeEventListener(A,T[n].listener),T[n]=null,!0):!1}function G(n,t){const e=Q(),s=i=>{i.currentTarget===i.target&&(R(e),t(i))};return n.addEventListener(A,s),T[e]={element:n,listener:s},e}function H(n){return Math.max(...n)}function K(n){return Math.min(...n)}function B(n,t,e,s){let i=n/t;return Math.abs(Math.round(i)-i)<s&&(i=Math.round(i)),Math.min(Math.ceil(i),e)}function P(n,t,e){if(t===1)return n;const s=[];for(let i=0;i<=e-t;i++)s.push(H(n.slice(i,i+t)));return s}function W(n,t){const e=K(n);for(let s=0,i=n.length;s<i;s++)if(n[s]>=e-t&&n[s]<=e+t)return s;return 0}function j(n){let{itemSize:t,positions:e,gridSize:s,total:i,threshold:r,buffer:o}=n;const h=B(t.width,s,i,r),u=P(e,h,i),a=W(u,o),f=new b(s*a,u[a]),E=u[a]+t.height;for(let p=0;p<h;p++)e[a+p]=E;return f}function V(n,t){const e={};n.forEach(o=>{e[o.top]?e[o.top].push(o):e[o.top]=[o]});let s=[];const i=[],r=[];return Object.keys(e).forEach(o=>{const h=e[o];i.push(h);const u=h[h.length-1],a=u.left+u.width,f=Math.round((t-a)/2);let E=h,p=!1;if(f>0){const v=[];p=h.every(I=>{const g=new C(I.left+f,I.top,I.width,I.height,I.id),_=!s.some(L=>C.intersects(g,L));return v.push(g),_}),p&&(E=v)}if(!p){let v;if(h.some(g=>s.some(_=>{const L=C.intersects(g,_);return L&&(v=_),L}))){const g=r.findIndex(_=>_.includes(v));r.splice(g,1,i[g])}}s=s.concat(E),r.push(E)}),r.flat().sort((o,h)=>o.id-h.id).map(o=>new b(o.left,o.top))}function J(n){return n.replace(/([A-Z])/g,(t,e)=>`-${e.toLowerCase()}`)}function N(n){return Array.from(new Set(n))}let O=0;class l extends z.exports{constructor(t){let e=arguments.length>1&&arguments[1]!==void 0?arguments[1]:{};super(),this.options={...l.options,...e},this.lastSort={},this.group=l.ALL_ITEMS,this.lastFilter=l.ALL_ITEMS,this.isEnabled=!0,this.isDestroyed=!1,this.isInitialized=!1,this._transitions=[],this.isTransitioning=!1,this._queue=[];const s=this._getElementOption(t);if(!s)throw new TypeError("Shuffle needs to be initialized with an element.");this.element=s,this.id=`shuffle_${O}`,O+=1,this._init(),this.isInitialized=!0}_init(){if(this.items=this._getItems(),this.sortedItems=this.items,this.options.sizer=this._getElementOption(this.options.sizer),this.element.classList.add(l.Classes.BASE),this._initItems(this.items),document.readyState!=="complete"){const s=this.layout.bind(this);window.addEventListener("load",function i(){window.removeEventListener("load",i),s()})}const t=window.getComputedStyle(this.element,null),e=l.getSize(this.element).width;this._validateStyles(t),this._setColumns(e),this.filter(this.options.group,this.options.initialSort),this._rafId=null,"ResizeObserver"in window&&(this._resizeObserver=new ResizeObserver(this._handleResizeCallback.bind(this)),this._resizeObserver.observe(this.element)),this.element.offsetWidth,this.setItemTransitions(this.items),this.element.style.transition=`height ${this.options.speed}ms ${this.options.easing}`}_getElementOption(t){return typeof t=="string"?this.element.querySelector(t):t&&t.nodeType&&t.nodeType===1?t:t&&t.jquery?t[0]:null}_validateStyles(t){t.position==="static"&&(this.element.style.position="relative"),t.overflow!=="hidden"&&(this.element.style.overflow="hidden")}_filter(){let t=arguments.length>0&&arguments[0]!==void 0?arguments[0]:this.lastFilter,e=arguments.length>1&&arguments[1]!==void 0?arguments[1]:this.items;const s=this._getFilteredSets(t,e);return this._toggleFilterClasses(s),this.lastFilter=t,typeof t=="string"&&(this.group=t),s}_getFilteredSets(t,e){let s=[];const i=[];return t===l.ALL_ITEMS?s=e:e.forEach(r=>{this._doesPassFilter(t,r.element)?s.push(r):i.push(r)}),{visible:s,hidden:i}}_doesPassFilter(t,e){if(typeof t=="function")return t.call(e,e,this);const s=e.dataset[l.FILTER_ATTRIBUTE_KEY],i=this.options.delimiter?s.split(this.options.delimiter):JSON.parse(s);function r(o){return i.includes(o)}return Array.isArray(t)?this.options.filterMode===l.FilterMode.ANY?t.some(r):t.every(r):i.includes(t)}_toggleFilterClasses(t){let{visible:e,hidden:s}=t;e.forEach(i=>{i.show()}),s.forEach(i=>{i.hide()})}_initItems(t){t.forEach(e=>{e.init()})}_disposeItems(t){t.forEach(e=>{e.dispose()})}_updateItemCount(){this.visibleItems=this._getFilteredItems().length}setItemTransitions(t){const{speed:e,easing:s}=this.options,i=this.options.useTransforms?["transform"]:["top","left"],r=Object.keys(d.Css.HIDDEN.before).map(h=>J(h)),o=i.concat(r).join();t.forEach(h=>{h.element.style.transitionDuration=`${e}ms`,h.element.style.transitionTimingFunction=s,h.element.style.transitionProperty=o})}_getItems(){return Array.from(this.element.children).filter(t=>t.matches(this.options.itemSelector)).map(t=>new d(t,this.options.isRTL))}_mergeNewItems(t){const e=Array.from(this.element.children);return w(this.items.concat(t),{by(s){return e.indexOf(s)}})}_getFilteredItems(){return this.items.filter(t=>t.isVisible)}_getConcealedItems(){return this.items.filter(t=>!t.isVisible)}_getColumnSize(t,e){let s;return typeof this.options.columnWidth=="function"?s=this.options.columnWidth(t):this.options.sizer?s=l.getSize(this.options.sizer).width:this.options.columnWidth?s=this.options.columnWidth:this.items.length>0?s=l.getSize(this.items[0].element,!0).width:s=t,s===0&&(s=t),s+e}_getGutterSize(t){let e;return typeof this.options.gutterWidth=="function"?e=this.options.gutterWidth(t):this.options.sizer?e=y(this.options.sizer,"marginLeft"):e=this.options.gutterWidth,e}_setColumns(){let t=arguments.length>0&&arguments[0]!==void 0?arguments[0]:l.getSize(this.element).width;const e=this._getGutterSize(t),s=this._getColumnSize(t,e);let i=(t+e)/s;Math.abs(Math.round(i)-i)<this.options.columnThreshold&&(i=Math.round(i)),this.cols=Math.max(Math.floor(i||0),1),this.containerWidth=t,this.colWidth=s}_setContainerSize(){this.element.style.height=`${this._getContainerSize()}px`}_getContainerSize(){return H(this.positions)}_getStaggerAmount(t){return Math.min(t*this.options.staggerAmount,this.options.staggerAmountMax)}_dispatch(t){let e=arguments.length>1&&arguments[1]!==void 0?arguments[1]:{};this.isDestroyed||(e.shuffle=this,this.emit(t,e))}_resetCols(){let t=this.cols;for(this.positions=[];t;)t-=1,this.positions.push(0)}_layout(t){const e=this._getNextPositions(t);let s=0;t.forEach((i,r)=>{function o(){i.applyCss(d.Css.VISIBLE.after)}if(b.equals(i.point,e[r])&&!i.isHidden){i.applyCss(d.Css.VISIBLE.before),o();return}i.point=e[r],i.scale=d.Scale.VISIBLE,i.isHidden=!1;const h=this.getStylesForTransition(i,d.Css.VISIBLE.before);h.transitionDelay=`${this._getStaggerAmount(s)}ms`,this._queue.push({item:i,styles:h,callback:o}),s+=1})}_getNextPositions(t){if(this.options.isCentered){const e=t.map((s,i)=>{const r=l.getSize(s.element,!0),o=this._getItemPosition(r);return new C(o.x,o.y,r.width,r.height,i)});return this.getTransformedPositions(e,this.containerWidth)}return t.map(e=>this._getItemPosition(l.getSize(e.element,!0)))}_getItemPosition(t){return j({itemSize:t,positions:this.positions,gridSize:this.colWidth,total:this.cols,threshold:this.options.columnThreshold,buffer:this.options.buffer})}getTransformedPositions(t,e){return V(t,e)}_shrink(){let t=arguments.length>0&&arguments[0]!==void 0?arguments[0]:this._getConcealedItems(),e=0;t.forEach(s=>{function i(){s.applyCss(d.Css.HIDDEN.after)}if(s.isHidden){s.applyCss(d.Css.HIDDEN.before),i();return}s.scale=d.Scale.HIDDEN,s.isHidden=!0;const r=this.getStylesForTransition(s,d.Css.HIDDEN.before);r.transitionDelay=`${this._getStaggerAmount(e)}ms`,this._queue.push({item:s,styles:r,callback:i}),e+=1})}_handleResizeCallback(t){if(!(!this.isEnabled||this.isDestroyed))for(const e of t)Math.round(e.contentRect.width)!==Math.round(this.containerWidth)&&(cancelAnimationFrame(this._rafId),this._rafId=requestAnimationFrame(this.update.bind(this)))}getStylesForTransition(t,e){const s={...e};if(this.options.useTransforms){const i=this.options.isRTL?"-":"",r=this.options.roundTransforms?Math.round(t.point.x):t.point.x,o=this.options.roundTransforms?Math.round(t.point.y):t.point.y;s.transform=`translate(${i}${r}px, ${o}px) scale(${t.scale})`}else this.options.isRTL?s.right=`${t.point.x}px`:s.left=`${t.point.x}px`,s.top=`${t.point.y}px`;return s}_whenTransitionDone(t,e,s){const i=G(t,r=>{e(),s(null,r)});this._transitions.push(i)}_getTransitionFunction(t){return e=>{t.item.applyCss(t.styles),this._whenTransitionDone(t.item.element,t.callback,e)}}_processQueue(){this.isTransitioning&&this._cancelMovement();const t=this.options.speed>0,e=this._queue.length>0;e&&t&&this.isInitialized?this._startTransitions(this._queue):e?(this._styleImmediately(this._queue),this._dispatch(l.EventType.LAYOUT)):this._dispatch(l.EventType.LAYOUT),this._queue.length=0}_startTransitions(t){this.isTransitioning=!0;const e=t.map(s=>this._getTransitionFunction(s));q(e,this._movementFinished.bind(this))}_cancelMovement(){this._transitions.forEach(R),this._transitions.length=0,this.isTransitioning=!1}_styleImmediately(t){if(t.length){const e=t.map(s=>s.item.element);l._skipTransitions(e,()=>{t.forEach(s=>{s.item.applyCss(s.styles),s.callback()})})}}_movementFinished(){this._transitions.length=0,this.isTransitioning=!1,this._dispatch(l.EventType.LAYOUT)}filter(t,e){this.isEnabled&&((!t||t&&t.length===0)&&(t=l.ALL_ITEMS),this._filter(t),this._shrink(),this._updateItemCount(),this.sort(e))}sort(){let t=arguments.length>0&&arguments[0]!==void 0?arguments[0]:this.lastSort;if(!this.isEnabled)return;this._resetCols();const e=w(this._getFilteredItems(),t);this.sortedItems=e,this._layout(e),this._processQueue(),this._setContainerSize(),this.lastSort=t}update(){let{recalculateSizes:t=!0,force:e=!1}=arguments.length>0&&arguments[0]!==void 0?arguments[0]:{};(this.isEnabled||e)&&(t&&this._setColumns(),this.sort())}layout(){this.update({recalculateSizes:!0})}add(t){const e=N(t).map(a=>new d(a,this.options.isRTL));this._initItems(e),this._resetCols();const s=this._mergeNewItems(e),i=w(s,this.lastSort),r=this._filter(this.lastFilter,i),o=a=>e.includes(a),h=a=>{a.scale=d.Scale.HIDDEN,a.isHidden=!0,a.applyCss(d.Css.HIDDEN.before),a.applyCss(d.Css.HIDDEN.after)},u=this._getNextPositions(r.visible);r.visible.forEach((a,f)=>{o(a)&&(a.point=u[f],h(a),a.applyCss(this.getStylesForTransition(a,{})))}),r.hidden.forEach(a=>{o(a)&&h(a)}),this.element.offsetWidth,this.setItemTransitions(e),this.items=this._mergeNewItems(e),this.filter(this.lastFilter)}disable(){this.isEnabled=!1}enable(){let t=arguments.length>0&&arguments[0]!==void 0?arguments[0]:!0;this.isEnabled=!0,t&&this.update()}remove(t){if(!t.length)return;const e=N(t),s=e.map(r=>this.getItemByElement(r)).filter(r=>!!r),i=()=>{this._disposeItems(s),e.forEach(r=>{r.parentNode.removeChild(r)}),this._dispatch(l.EventType.REMOVED,{collection:e})};this._toggleFilterClasses({visible:[],hidden:s}),this._shrink(s),this.sort(),this.items=this.items.filter(r=>!s.includes(r)),this._updateItemCount(),this.once(l.EventType.LAYOUT,i)}getItemByElement(t){return this.items.find(e=>e.element===t)}resetItems(){this._disposeItems(this.items),this.isInitialized=!1,this.items=this._getItems(),this._initItems(this.items),this.once(l.EventType.LAYOUT,()=>{this.setItemTransitions(this.items),this.isInitialized=!0}),this.filter(this.lastFilter)}destroy(){this._cancelMovement(),this._resizeObserver&&(this._resizeObserver.unobserve(this.element),this._resizeObserver=null),this.element.classList.remove("shuffle"),this.element.removeAttribute("style"),this._disposeItems(this.items),this.items.length=0,this.sortedItems.length=0,this._transitions.length=0,this.options.sizer=null,this.element=null,this.isDestroyed=!0,this.isEnabled=!1}static getSize(t){let e=arguments.length>1&&arguments[1]!==void 0?arguments[1]:!1;const s=window.getComputedStyle(t,null);let i=y(t,"width",s),r=y(t,"height",s);if(e){const o=y(t,"marginLeft",s),h=y(t,"marginRight",s),u=y(t,"marginTop",s),a=y(t,"marginBottom",s);i+=o+h,r+=u+a}return{width:i,height:r}}static _skipTransitions(t,e){const s="0ms",i=t.map(r=>{const{style:o}=r,h=o.transitionDuration,u=o.transitionDelay;return o.transitionDuration=s,o.transitionDelay=s,{duration:h,delay:u}});e(),t[0].offsetWidth,t.forEach((r,o)=>{r.style.transitionDuration=i[o].duration,r.style.transitionDelay=i[o].delay})}}l.ShuffleItem=d;l.ALL_ITEMS="all";l.FILTER_ATTRIBUTE_KEY="groups";l.EventType={LAYOUT:"shuffle:layout",REMOVED:"shuffle:removed"};l.Classes=m;l.FilterMode={ANY:"any",ALL:"all"};l.options={group:l.ALL_ITEMS,speed:250,easing:"cubic-bezier(0.4, 0.0, 0.2, 1)",itemSelector:"*",sizer:null,gutterWidth:0,columnWidth:0,delimiter:null,buffer:0,columnThreshold:.01,initialSort:null,staggerAmount:15,staggerAmountMax:150,useTransforms:!0,filterMode:l.FilterMode.ANY,isCentered:!1,isRTL:!1,roundTransforms:!0};l.Point=b;l.Rect=C;l.__sorter=w;l.__getColumnSpan=B;l.__getAvailablePositions=P;l.__getShortColumn=W;l.__getCenteredPositions=V;const $=document.getElementById("photo-gallery"),Z=$.querySelector(".js-shuffle-sizer");new l($,{itemSelector:".picture-item",sizer:Z,columnWidth:1});
