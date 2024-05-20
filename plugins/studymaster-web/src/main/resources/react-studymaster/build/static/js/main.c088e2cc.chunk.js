(this["webpackJsonpreact-studymaster"]=this["webpackJsonpreact-studymaster"]||[]).push([[0],{10:function(e,t,n){},12:function(e,t,n){"use strict";n.r(t);var c=n(4),a=n.n(c),s=(n(9),n(2)),i=n(1),r=(n(10),n(0));var o=function(){var e=Object(i.useState)(""),t=Object(s.a)(e,2),n=(t[0],t[1]),c="https:"===document.location.protocol?"wss://":"ws://",a=Object(i.useState)(new WebSocket(c+document.location.host+"/ws")),o=Object(s.a)(a,2),l=o[0],d=o[1],u=Object(i.useState)(),b=Object(s.a)(u,2),j=b[0],h=b[1],p=new Map;function m(){console.log("Send client alive message to server"),l.send("VSMMessage#STATUS#alive"),setTimeout(m,1e3)}function v(e){var t=j.variable[e];if("text"===j.type[e])return Object(r.jsx)(r.Fragment,{children:Object(r.jsxs)("p",{children:[Object(r.jsxs)("label",{children:[" ",j.variable[e]," "]}),Object(r.jsx)("input",{type:"text",name:t,placeholder:j.options[e],id:t,onChange:function(e){return p.set(t,e.target.value)}})]})});if("number"===j.type[e])return Object(r.jsx)(r.Fragment,{children:Object(r.jsxs)("p",{children:[Object(r.jsxs)("label",{children:[" ",j.variable[e]," "]}),Object(r.jsx)("input",{type:"number",name:t,placeholder:j.options[e],id:t,onChange:function(e){return p.set(t,e.target.value)}})]})});if("radio"===j.type[e]){var n=j.options[e].split(",");return Object(r.jsx)(r.Fragment,{children:Object(r.jsxs)("p",{children:[Object(r.jsxs)("label",{children:[" ",j.variable[e]," "]}),n.map((function(e){return Object(r.jsxs)(r.Fragment,{children:[Object(r.jsx)("input",{type:"radio",id:e,name:t,value:e}),Object(r.jsxs)("label",{children:[" ",e," "]})]})}))]})})}return"checkbox"===j.type[e]?Object(r.jsx)(r.Fragment,{children:Object(r.jsxs)("p",{children:[Object(r.jsxs)("label",{children:[" ",j.variable[e]," "]}),Object(r.jsx)("input",{type:"checkbox",id:t,name:t,value:j.options[e]}),Object(r.jsxs)("label",{children:[" ",j.options[e]," "]})]})}):void 0}return Object(i.useEffect)((function(){var e=new WebSocket(c+document.location.host+"/ws");n("Connecting..."),e.onopen=function(){n("Connected!"),m()},e.onclose=function(){n("Lost connection")},e.onmessage=function(e){console.log(e.data);var t=e.data.split("#"),c=t[1];"REQUEST"===c&&h({action:c,variable:t[3].split(";"),options:t[4].split(";"),type:t[5].split(";"),timestamp:t[2]}),n(e.data)},d(e),document.title="VSM StudyMaster";var t=document.querySelector("link[rel*='icon']")||document.createElement("link");t.type="image/x-icon",t.rel="shortcut icon",t.href="http://scenemaker.dfki.de/images/scenemaker/logo.png",document.getElementsByTagName("head")[0].appendChild(t)}),[]),Object(r.jsx)("div",{className:"App",children:Object(r.jsxs)("header",{className:"App-header",children:[Object(r.jsx)("div",{children:Object(r.jsx)("h3",{children:"VSM StudyMaster"})}),Object(r.jsxs)("form",{children:[Object(r.jsxs)("fieldset",{children:[j&&"REQUEST"===j.action&&function(){var e,t=[];for(e=0;e<j.variable.length;e++){var n=v(e);t.push(n)}return Object(r.jsx)(r.Fragment,{children:t})}(),j&&"SUCCESSFULSEND"===j.action&&Object(r.jsx)("div",{children:Object(r.jsx)("h2",{children:"Successfully posted!"})})]}),j&&"REQUEST"===j.action&&Object(r.jsxs)("div",{children:[Object(r.jsx)("button",{onClick:function(e){(function(e){e.preventDefault();var t,n=!0;for(t=0;t<j.variable.length;t++){var c=j.variable[t];if("radio"===j.type[t]){var a=void 0,s=j.options[t].split(","),i=!1;for(a=0;a<s.length;a++){var r=s[a];document.getElementById(r).checked&&(i=!0)}i||(alert("Please ensure to fill in radio input for "+j.variable[t]),n=!1)}else"text"===j.type[t]?p.has(c)||(alert("Please ensure to fill in text input for "+j.variable[t]),n=!1):"number"===j.type[t]&&(p.has(c)||(alert("Please ensure to fill in number input for "+j.variable[t]),n=!1))}if(n)for(t=0;t<j.variable.length;t++){var o=j.variable[t];if("radio"===j.type[t]){var d=void 0,u=j.options[t].split(",");for(d=0;d<u.length;d++){var b=u[d];document.getElementById(b).checked&&l.send("VSMMessage#VAR#".concat(o,"#").concat(b))}}else"text"===j.type[t]||"number"===j.type[t]?p.has(o)&&l.send("VSMMessage#VAR#".concat(o,"#").concat(p.get(o))):"checkbox"===j.type[t]&&(document.getElementById(o).checked?l.send("VSMMessage#VAR#".concat(o,"#true")):l.send("VSMMessage#VAR#".concat(o,"#false")))}return n})(e)?(l.send("VSMMessage#VAR#request_result#SUBMIT"),h({action:"SUCCESSFULSEND",timestamp:j.timestamp})):window.location.reload()},children:" submit"}),Object(r.jsx)("button",{onClick:function(){l.send("VSMMessage#VAR#request_result#CANCEL"),h(void 0)},children:" cancel"})]}),!(j&&"REQUEST"===j.action)&&Object(r.jsx)("div",{children:Object(r.jsx)("h2",{children:"No active requests."})})]})]})})};Boolean("localhost"===window.location.hostname||"[::1]"===window.location.hostname||window.location.hostname.match(/^127(?:\.(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}$/));a.a.render(Object(r.jsx)(o,{}),document.getElementById("root")),"serviceWorker"in navigator&&navigator.serviceWorker.ready.then((function(e){e.unregister()}))},9:function(e,t,n){}},[[12,1,2]]]);
//# sourceMappingURL=main.c088e2cc.chunk.js.map