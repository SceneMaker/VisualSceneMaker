(this["webpackJsonpreact-studymaster"]=this["webpackJsonpreact-studymaster"]||[]).push([[0],{10:function(e,t,n){},12:function(e,t,n){"use strict";n.r(t);var a=n(4),c=n.n(a),i=(n(9),n(2)),r=n(1),s=(n(10),n(0));var o=function(){var e=Object(r.useState)(""),t=Object(i.a)(e,2),n=(t[0],t[1]),a=Object(r.useState)(new WebSocket("ws://"+document.location.host+"/ws")),c=Object(i.a)(a,2),o=c[0],l=c[1],d=Object(r.useState)(),u=Object(i.a)(d,2),b=u[0],j=u[1],p=new Map;function h(e){var t=b.variable[e];if("text"===b.type[e])return Object(s.jsx)(s.Fragment,{children:Object(s.jsxs)("p",{children:[Object(s.jsxs)("label",{children:[" ",b.variable[e]," "]}),Object(s.jsx)("input",{type:"text",name:t,placeholder:b.options[e],id:t,onChange:function(e){return p.set(t,e.target.value)}})]})});if("number"===b.type[e])return Object(s.jsx)(s.Fragment,{children:Object(s.jsxs)("p",{children:[Object(s.jsxs)("label",{children:[" ",b.variable[e]," "]}),Object(s.jsx)("input",{type:"number",name:t,placeholder:b.options[e],id:t,onChange:function(e){return p.set(t,e.target.value)}})]})});if("radio"===b.type[e]){var n=b.options[e].split(",");return Object(s.jsx)(s.Fragment,{children:Object(s.jsxs)("p",{children:[Object(s.jsxs)("label",{children:[" ",b.variable[e]," "]}),n.map((function(e){return Object(s.jsxs)(s.Fragment,{children:[Object(s.jsx)("input",{type:"radio",id:e,name:t,value:e}),Object(s.jsxs)("label",{children:[" ",e," "]})]})}))]})})}return"checkbox"===b.type[e]?Object(s.jsx)(s.Fragment,{children:Object(s.jsxs)("p",{children:[Object(s.jsxs)("label",{children:[" ",b.variable[e]," "]}),Object(s.jsx)("input",{type:"checkbox",id:t,name:t,value:b.options[e]}),Object(s.jsxs)("label",{children:[" ",b.options[e]," "]})]})}):void 0}return Object(r.useEffect)((function(){var e=new WebSocket("ws://"+document.location.host+"/ws");n("Connecting..."),e.onopen=function(){n("Connected!")},e.onclose=function(){n("Lost connection")},e.onmessage=function(e){console.log(e.data);var t=e.data.split("#"),a=t[1];"REQUEST"===a&&j({action:a,variable:t[3].split(";"),options:t[4].split(";"),type:t[5].split(";"),timestamp:t[2]}),n(e.data)},l(e),document.title="VSM StudyMaster";var t=document.querySelector("link[rel*='icon']")||document.createElement("link");t.type="image/x-icon",t.rel="shortcut icon",t.href="http://scenemaker.dfki.de/images/scenemaker/logo.png",document.getElementsByTagName("head")[0].appendChild(t)}),[]),Object(s.jsx)("div",{className:"App",children:Object(s.jsx)("header",{className:"App-header",children:Object(s.jsxs)("form",{children:[Object(s.jsxs)("fieldset",{children:[b&&"REQUEST"===b.action&&function(){var e,t=[];for(e=0;e<b.variable.length;e++){var n=h(e);t.push(n)}return Object(s.jsx)(s.Fragment,{children:t})}(),b&&"SUCCESSFULSEND"===b.action&&Object(s.jsx)("div",{children:Object(s.jsx)("h2",{children:"Successfully posted!"})})]}),b&&"REQUEST"===b.action&&Object(s.jsxs)("div",{children:[Object(s.jsx)("button",{onClick:function(e){(function(e){e.preventDefault();var t,n=!0;for(t=0;t<b.variable.length;t++){var a=b.variable[t];if("radio"===b.type[t]){var c=void 0,i=b.options[t].split(","),r=!1;for(c=0;c<i.length;c++){var s=i[c];document.getElementById(s).checked&&(r=!0)}r||(alert("Please ensure to fill in radio input for "+b.variable[t]),n=!1)}else"text"===b.type[t]?p.has(a)||(alert("Please ensure to fill in text input for "+b.variable[t]),n=!1):"number"===b.type[t]&&(p.has(a)||(alert("Please ensure to fill in number input for "+b.variable[t]),n=!1))}if(n)for(t=0;t<b.variable.length;t++){var l=b.variable[t];if("radio"===b.type[t]){var d=void 0,u=b.options[t].split(",");for(d=0;d<u.length;d++){var j=u[d];document.getElementById(j).checked&&o.send("VSMMessage#VAR#".concat(l,"#").concat(j))}}else"text"===b.type[t]||"number"===b.type[t]?p.has(l)&&o.send("VSMMessage#VAR#".concat(l,"#").concat(p.get(l))):"checkbox"===b.type[t]&&(document.getElementById(l).checked?o.send("VSMMessage#VAR#".concat(l,"#true")):o.send("VSMMessage#VAR#".concat(l,"#false")))}return n})(e)?(o.send("VSMMessage#VAR#request_result#SUBMIT"),j({action:"SUCCESSFULSEND",timestamp:b.timestamp})):window.location.reload()},children:" submit"}),Object(s.jsx)("button",{onClick:function(){o.send("VSMMessage#VAR#request_result#CANCEL"),j(void 0)},children:" cancel"})]}),!(b&&"REQUEST"===b.action)&&Object(s.jsx)("div",{children:Object(s.jsx)("h2",{children:"No active requests."})})]})})})};Boolean("localhost"===window.location.hostname||"[::1]"===window.location.hostname||window.location.hostname.match(/^127(?:\.(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}$/));c.a.render(Object(s.jsx)(o,{}),document.getElementById("root")),"serviceWorker"in navigator&&navigator.serviceWorker.ready.then((function(e){e.unregister()}))},9:function(e,t,n){}},[[12,1,2]]]);
//# sourceMappingURL=main.f1091193.chunk.js.map