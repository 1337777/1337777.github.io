!function(e){var n={};function t(r){if(n[r])return n[r].exports;var o=n[r]={i:r,l:!1,exports:{}};return e[r].call(o.exports,o,o.exports,t),o.l=!0,o.exports}t.m=e,t.c=n,t.d=function(e,n,r){t.o(e,n)||Object.defineProperty(e,n,{enumerable:!0,get:r})},t.r=function(e){"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(e,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(e,"__esModule",{value:!0})},t.t=function(e,n){if(1&n&&(e=t(e)),8&n)return e;if(4&n&&"object"==typeof e&&e&&e.__esModule)return e;var r=Object.create(null);if(t.r(r),Object.defineProperty(r,"default",{enumerable:!0,value:e}),2&n&&"string"!=typeof e)for(var o in e)t.d(r,o,function(n){return e[n]}.bind(null,o));return r},t.n=function(e){var n=e&&e.__esModule?function(){return e.default}:function(){return e};return t.d(n,"a",n),n},t.o=function(e,n){return Object.prototype.hasOwnProperty.call(e,n)},t.p="",t(t.s=307)}({307:function(e,n){function t(e,n,t,r,o,c,i){try{var u=e[c](i),a=u.value}catch(e){return void t(e)}u.done?n(a):Promise.resolve(a).then(r,o)}function r(e){return function(){var n=this,r=arguments;return new Promise((function(o,c){var i=e.apply(n,r);function u(e){t(i,o,c,u,a,"next",e)}function a(e){t(i,o,c,u,a,"throw",e)}u(void 0)}))}}var o,c=[],i={show:!0,line_numbers:"continue",prelude:!0,base_path:"../",init_pkgs:["init","qoc"],all_pkgs:["init","qoc","coq-base","coq-collections","coq-arith","coq-reals","math-comp"],implicit_libs:!0,editor:{mode:{"company-coq":!0},keyMap:"default"}};function u(){Excel.run(function(){var e=r(regeneratorRuntime.mark((function e(n){var t,r;return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:return t=n.workbook.worksheets.getActiveWorksheet(),(r=t.findAllOrNullObject("(?excoq?)*",{completeMatch:!0,matchCase:!1})).load(["areas"]),e.next=5,n.sync();case 5:if(r.isNullObject){e.next=12;break}return r.areas.items.forEach((function(e,n,t){e.load("values")})),e.next=9,n.sync();case 9:r.areas.items.forEach((function(e,n,t){o.provider.snippets[n]&&o.provider.snippets[n].editor.setValue(e.values.map((function(e){return e.join(" ")})).join("\n"))})),e.next=13;break;case 12:console.log("No Excel Coq code with reserved keyword");case 13:return e.abrupt("return",n.sync());case 15:case"end":return e.stop()}}),e)})));return function(n){return e.apply(this,arguments)}}()).catch((function(e){console.log("Error: "+e),e instanceof OfficeExtension.Error&&console.log("Debug info: "+JSON.stringify(e.debugInfo))}))}function a(){Excel.run(function(){var e=r(regeneratorRuntime.mark((function e(n){var t,r;return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:return t=n.workbook.worksheets.getActiveWorksheet(),(r=t.findAllOrNullObject("(?excoq?)*",{completeMatch:!0,matchCase:!1})).load(["areas"]),e.next=5,n.sync();case 5:return r.isNullObject?console.log("No Excel Coq code with reserved keyword"):r.areas.items.forEach((function(e,n,t){o.provider.snippets[n]&&(e.values=[[o.provider.snippets[n].editor.getValue()]])})),e.abrupt("return",n.sync());case 8:case"end":return e.stop()}}),e)})));return function(n){return e.apply(this,arguments)}}()).catch((function(e){console.log("Error: "+e),e instanceof OfficeExtension.Error&&console.log("Debug info: "+JSON.stringify(e.debugInfo))}))}Office.onReady((function(e){e.host===Office.HostType.Excel&&Excel.run(function(){var e=r(regeneratorRuntime.mark((function e(n){var t,r,s,l;return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:return t=n.workbook.worksheets.getActiveWorksheet(),r=document.querySelector("#document"),(l=t.findAllOrNullObject("(?excoq?)*",{completeMatch:!0,matchCase:!1})).load(["areas"]),e.next=6,n.sync();case 6:return l.isNullObject?console.log("No Excel Coq code with reserved keyword"):l.areas.items.forEach((function(e,n,t){(s=document.createElement("TEXTAREA")).setAttribute("id","workspace"+n),r.appendChild(s),c.push("workspace"+n)})),loadJsCoq(i.base_path).then((function(){o=new CoqManager(c,i),u();var e=document.createElement("BUTTON");e.innerText="READ",document.querySelector("#buttons").appendChild(e),e.onclick=u;var n=document.createElement("BUTTON");n.innerText="WRITE",document.querySelector("#buttons").appendChild(n),n.onclick=a})),e.abrupt("return",n.sync().then((function(){console.log("Finished Excel.Run here")})));case 10:case"end":return e.stop()}}),e)})));return function(n){return e.apply(this,arguments)}}())}))}});
//# sourceMappingURL=taskpane.js.map