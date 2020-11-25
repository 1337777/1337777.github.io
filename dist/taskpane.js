!function(e){var t={};function o(n){if(t[n])return t[n].exports;var r=t[n]={i:n,l:!1,exports:{}};return e[n].call(r.exports,r,r.exports,o),r.l=!0,r.exports}o.m=e,o.c=t,o.d=function(e,t,n){o.o(e,t)||Object.defineProperty(e,t,{enumerable:!0,get:n})},o.r=function(e){"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(e,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(e,"__esModule",{value:!0})},o.t=function(e,t){if(1&t&&(e=o(e)),8&t)return e;if(4&t&&"object"==typeof e&&e&&e.__esModule)return e;var n=Object.create(null);if(o.r(n),Object.defineProperty(n,"default",{enumerable:!0,value:e}),2&t&&"string"!=typeof e)for(var r in e)o.d(n,r,function(t){return e[t]}.bind(null,r));return n},o.n=function(e){var t=e&&e.__esModule?function(){return e.default}:function(){return e};return o.d(t,"a",t),t},o.o=function(e,t){return Object.prototype.hasOwnProperty.call(e,t)},o.p="",o(o.s=307)}({307:function(module,exports){function asyncGeneratorStep(e,t,o,n,r,c,a){try{var s=e[c](a),i=s.value}catch(e){return void o(e)}s.done?t(i):Promise.resolve(i).then(n,r)}function _asyncToGenerator(e){return function(){var t=this,o=arguments;return new Promise((function(n,r){var c=e.apply(t,o);function a(e){asyncGeneratorStep(c,n,r,a,s,"next",e)}function s(e){asyncGeneratorStep(c,n,r,a,s,"throw",e)}a(void 0)}))}}var jscoq_src="../../ui-js/jscoq-loader.js",jscoq_ids=[],jscoq_opts={show:!0,line_numbers:"continue",prelude:!0,base_path:"../",init_pkgs:["init","qoc"],all_pkgs:["init","qoc","coq-base","coq-collections","coq-arith","coq-reals","math-comp"],implicit_libs:!0,editor:{mode:{"company-coq":!0},keyMap:"default"}},coq,excoq_keyword="(?coq365?)*",coq365script_keyword="/?coq365?/*";function readFrom(){Excel.run(function(){var e=_asyncToGenerator(regeneratorRuntime.mark((function e(t){var o,n;return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:return o=t.workbook.worksheets.getActiveWorksheet(),(n=o.findAllOrNullObject(excoq_keyword,{completeMatch:!0,matchCase:!1})).load(["areas"]),e.next=5,t.sync();case 5:if(n.isNullObject){e.next=12;break}return n.areas.items.forEach((function(e,t,o){e.load("values")})),e.next=9,t.sync();case 9:n.areas.items.forEach((function(e,t,o){coq.provider.snippets[t]&&coq.provider.snippets[t].editor.setValue(e.values.map((function(e){return e.join(" ")})).join("\n"))})),e.next=13;break;case 12:console.log("No Coq365 code with reserved keyword: coq365");case 13:return e.abrupt("return",t.sync());case 15:case"end":return e.stop()}}),e)})));return function(t){return e.apply(this,arguments)}}()).catch((function(e){console.log("Error: "+e),e instanceof OfficeExtension.Error&&console.log("Debug info: "+JSON.stringify(e.debugInfo))}))}function writeBack(){Excel.run(function(){var e=_asyncToGenerator(regeneratorRuntime.mark((function e(t){var o,n;return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:return o=t.workbook.worksheets.getActiveWorksheet(),(n=o.findAllOrNullObject(excoq_keyword,{completeMatch:!0,matchCase:!1})).load(["areas"]),e.next=5,t.sync();case 5:return n.isNullObject?console.log("No Coq365 code with reserved keyword: coq365"):n.areas.items.forEach((function(e,t,o){coq.provider.snippets[t]&&(e.values=[[coq.provider.snippets[t].editor.getValue()]])})),e.abrupt("return",t.sync());case 8:case"end":return e.stop()}}),e)})));return function(t){return e.apply(this,arguments)}}()).catch((function(e){console.log("Error: "+e),e instanceof OfficeExtension.Error&&console.log("Debug info: "+JSON.stringify(e.debugInfo))}))}Office.onReady((function(info){info.host===Office.HostType.Excel&&Excel.run(function(){var _ref=_asyncToGenerator(regeneratorRuntime.mark((function _callee(context){var sheet,workspaces,workspace,foundRanges,foundRangesScript,scriptFile;return regeneratorRuntime.wrap((function _callee$(_context){for(;;)switch(_context.prev=_context.next){case 0:return sheet=context.workbook.worksheets.getActiveWorksheet(),workspaces=document.querySelector("#document"),foundRanges=sheet.findAllOrNullObject(excoq_keyword,{completeMatch:!0,matchCase:!1}),foundRanges.load(["areas"]),_context.next=6,context.sync();case 6:return foundRanges.isNullObject?console.log("No Coq365 code with reserved keyword: coq365"):foundRanges.areas.items.forEach((function(e,t,o){(workspace=document.createElement("TEXTAREA")).setAttribute("id","workspace"+t),workspaces.appendChild(workspace),jscoq_ids.push("workspace"+t)})),foundRangesScript=sheet.findAllOrNullObject(coq365script_keyword,{completeMatch:!0,matchCase:!1}),foundRangesScript.load(["areas"]),_context.next=12,context.sync();case 12:if(foundRangesScript.isNullObject){_context.next=19;break}return foundRangesScript.areas.items.forEach((function(e,t,o){e.load("values")})),_context.next=16,context.sync();case 16:foundRangesScript.areas.items.forEach((function(valR,iR,aR){var evalresult=eval(valR.values.map((function(e){return e.join(" ")})).join("\n"));console.log("Eval Coq365 javascript, result: "+evalresult)})),_context.next=20;break;case 19:console.log("No Coq365 javascript with reserved keyword: coq365");case 20:return scriptFile=document.createElement("script"),document.body.append(scriptFile),scriptFile.type="text/javascript",scriptFile.src=jscoq_src,console.log("SET scriptFile.src"),scriptFile.onload=function(){console.log("START RUN scriptFile.onload"),loadJsCoq(jscoq_opts.base_path).then((function(){coq=new CoqManager(jscoq_ids,jscoq_opts),readFrom();var e=document.createElement("BUTTON");e.innerText="READ",document.querySelector("#buttons").appendChild(e),e.onclick=readFrom;var t=document.createElement("BUTTON");t.innerText="WRITE",document.querySelector("#buttons").appendChild(t),t.onclick=writeBack}))},_context.abrupt("return",context.sync().then((function(){console.log("Finished Excel.Run here")})));case 28:case"end":return _context.stop()}}),_callee)})));return function(e){return _ref.apply(this,arguments)}}())}))}});
//# sourceMappingURL=taskpane.js.map