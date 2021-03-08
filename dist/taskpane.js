!function(e){var t={};function n(r){if(t[r])return t[r].exports;var o=t[r]={i:r,l:!1,exports:{}};return e[r].call(o.exports,o,o.exports,n),o.l=!0,o.exports}n.m=e,n.c=t,n.d=function(e,t,r){n.o(e,t)||Object.defineProperty(e,t,{enumerable:!0,get:r})},n.r=function(e){"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(e,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(e,"__esModule",{value:!0})},n.t=function(e,t){if(1&t&&(e=n(e)),8&t)return e;if(4&t&&"object"==typeof e&&e&&e.__esModule)return e;var r=Object.create(null);if(n.r(r),Object.defineProperty(r,"default",{enumerable:!0,value:e}),2&t&&"string"!=typeof e)for(var o in e)n.d(r,o,function(t){return e[t]}.bind(null,o));return r},n.n=function(e){var t=e&&e.__esModule?function(){return e.default}:function(){return e};return n.d(t,"a",t),t},n.o=function(e,t){return Object.prototype.hasOwnProperty.call(e,t)},n.p="",n(n.s=310)}({310:function(module,exports,__webpack_require__){(function(global){function asyncGeneratorStep(e,t,n,r,o,c,a){try{var s=e[c](a),i=s.value}catch(e){return void n(e)}s.done?t(i):Promise.resolve(i).then(r,o)}function _asyncToGenerator(e){return function(){var t=this,n=arguments;return new Promise((function(r,o){var c=e.apply(t,n);function a(e){asyncGeneratorStep(c,r,o,a,s,"next",e)}function s(e){asyncGeneratorStep(c,r,o,a,s,"throw",e)}a(void 0)}))}}Array.prototype.last=function(){return this[this.length-1]};var sizes={"ide-wrapper":.5,"code-wrapper":.5};self.Resizable.initialise("main",sizes),window.addEventListener("resize",(function(){self.Resizable.activeContentWindows[0].changeSize(window.innerWidth,window.innerHeight),self.Resizable.activeContentWindows[0].childrenResize()})),self.Resizable.activeContentWindows[0].changeSize(window.innerWidth,window.innerHeight),self.Resizable.activeContentWindows[0].childrenResize();var jscoq_src="/ui-js/jscoq-loader.js",jscoq_ids=[],jscoq_opts={show:!0,line_numbers:"continue",prelude:!0,base_path:"",init_pkgs:["init","qoc"],all_pkgs:["init","qoc","coq-base","coq-collections","coq-arith","coq-reals","math-comp"],implicit_libs:!0,editor:{mode:{"company-coq":!0},keyMap:"default"}},coq;function getGlobal(){return"undefined"!=typeof self?self:"undefined"!=typeof window?window:void 0!==global?global:void 0}var g=getGlobal(),coq365script_regexp=RegExp("[(][*]/[*]coq365[*]/([^]*?)[*][)]","ig"),transcriptButtonChecked;function tryCatch(e){return _tryCatch.apply(this,arguments)}function _tryCatch(){return(_tryCatch=_asyncToGenerator(regeneratorRuntime.mark((function e(t){return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:return e.prev=0,console.log("Now executing some WorkSchool 365 function..."),e.next=4,t();case 4:e.next=9;break;case 6:e.prev=6,e.t0=e.catch(0),console.error(e.t0);case 9:case"end":return e.stop()}}),e,null,[[0,6]])})))).apply(this,arguments)}window.transcriptSeparator="             ▽";var coqObserver={};function selectStatement(e){var t;coq.doc&&coq.doc.sentences?t=coq.doc.sentences.filter((function(t){return t.coq_sid==e})).last():coq.sentences&&(t=coq.sentences.filter((function(t){return t.coq_sid==e})).last()),Word.run(function(){var e=_asyncToGenerator(regeneratorRuntime.mark((function e(n){var r,o,c,a;return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:return(r=n.document.contentControls.getById(t.sp.ccid).getRange("Content").split(["\v","\n","\r","\v"])).load(["length","items","text"]),e.next=4,n.sync();case 4:return console.log(r.items),(o=""==t.text.split("\n")[0]?r.items[t.start.line+1].search(t.text.split("\n")[1]):r.items[t.start.line].search(t.text.split("\n")[0])).load(["length"]),e.next=9,n.sync();case 9:return(c=r.items[t.end.line].search(t.text.split("\n").last())).load(["length"]),e.next=13,n.sync();case 13:return a=o.items[0],r.items.slice(t.start.line+1,t.end.line).forEach((function(e){a=a.expandTo(e)})),(a=a.expandTo(c.items[0])).select(),e.next=19,n.sync();case 19:case"end":return e.stop()}}),e)})));return function(t){return e.apply(this,arguments)}}())}function transcriptStatement(e,t,n){return _transcriptStatement.apply(this,arguments)}function _transcriptStatement(){return(_transcriptStatement=_asyncToGenerator(regeneratorRuntime.mark((function e(t,n,r){var o;return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:coq.doc&&coq.doc.sentences?o=coq.doc.sentences.filter((function(e){return e.coq_sid==t})).last():coq.sentences&&(o=coq.sentences.filter((function(e){return e.coq_sid==t})).last()),Word.run(function(){var e=_asyncToGenerator(regeneratorRuntime.mark((function e(t){var c,a,s;return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:return(c=t.document.contentControls.getById(o.sp.ccid)).load(["length","title","items"]),e.next=4,t.sync();case 4:return a=c.title.replace(/\s*([^/\s]+)(.*)/," O_$1 / output "),pccc=t.document.contentControls.getByTitle(a),pccc.load(["items","length","text"]),e.next=9,t.sync();case 9:if(0!=pccc.items.length){e.next=20;break}return(s=c.insertParagraph("",Word.InsertLocation.after).insertContentControl()).appearance=Word.ContentControlAppearance.tags,s.color="purple",s.tag="ws365_code_output",s.title=a,s.load(["text"]),e.next=18,t.sync();case 18:e.next=21;break;case 20:s=pccc.items[0];case 21:return""!=s.text&&(s.insertParagraph(transcriptSeparator,"End"),s.insertParagraph("","End")),n?s.insertHtml(r,"End"):s.insertText(r,"End"),e.next=25,t.sync();case 25:return e.next=27,formatCC(t,s,"text");case 27:case"end":return e.stop()}}),e)})));return function(t){return e.apply(this,arguments)}}());case 2:case"end":return e.stop()}}),e)})))).apply(this,arguments)}function goals2DOM(e){if(0==e.goals.length)return $(document.createTextNode("No more goals"));var t=e.goals.length,n=$("<p>").text(1===t?"★ 1 goal.":"★ ".concat(t," goals")),r=goal2DOM(e.goals[0]),o=e.goals.slice(1).map((function(e,t){return $("<div>").append($("<p>").text("★ subgoal "+(t+2)+" is:")).append(coq.pprint.pp2Text(e.ty))}));return $("<div>").append(n,r,o)}function goal2DOM(e){var t=e.hyp.map((function(e){return $("<p>").append($("<span>").text(e[0]+" : ")).append(coq.pprint.pp2Text(e[2]))})),n=coq.pprint.pp2Text(e.ty);return $("<div>").append(t,$("<p>").text("____________________________"),n)}function formatCC(e,t,n){return _formatCC.apply(this,arguments)}function _formatCC(){return(_formatCC=_asyncToGenerator(regeneratorRuntime.mark((function e(t,n,r){var o,c,a,s,i,u;return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:if(o=document.createElement("PRE"),Office.context.platform!=Office.PlatformType.PC){e.next=16;break}return(c=n.split(["\n"],!1,!1)).load(["items","text"]),e.next=6,t.sync();case 6:console.log(c),a=c.items.map((function(e){return e.text})).join(""),o.innerHTML=hljs.highlight(r,a.substr(0,a.length)).value,document.body.getElementsByClassName("ws365_canvas")[0].appendChild(o),computedStyleToInlineStyle(o,{recursive:!0,properties:["color","font-style","font-weight"]}),console.log(o.innerHTML),n.insertHtml("<pre>"+o.innerHTML.replace(/\u000B/g,"<br/>").replace(/\n/g,"\r\n")+"\r\n</pre>","Replace"),n.styleBuiltIn="NoSpacing",e.next=28;break;case 16:return(c=n.split(["\n"],!1,!1)).load(["items","text"]),e.next=20,t.sync();case 20:for(s=c.items.map((function(e){return e.text})).join(""),o.innerHTML=hljs.highlight(r,s.substr(0,s.length-1)).value,document.body.getElementsByClassName("ws365_canvas")[0].appendChild(o),computedStyleToInlineStyle(o,{recursive:!0,properties:["color","font-style","font-weight"]}),i=o.innerHTML.split(/\r\n|\n|\r|\u000B/g),console.log(i),n.clear(),u=0;u<i.length;u++)u!=i.length-1?n.insertHtml(i[u].replace(/  /g,"&nbsp;&nbsp;")+"<br/>","End"):0!=i[u].length&&n.insertHtml(i[u].replace(/  /g,"&nbsp;&nbsp;"),"End");case 28:return n.font.name="Consolas",e.next=31,t.sync();case 31:document.body.getElementsByClassName("ws365_canvas")[0].removeChild(o),o.innerHTML="";case 33:case"end":return e.stop()}}),e)})))).apply(this,arguments)}function readSelected(){return _readSelected.apply(this,arguments)}function _readSelected(){return(_readSelected=_asyncToGenerator(regeneratorRuntime.mark((function e(){return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:Word.run(function(){var e=_asyncToGenerator(regeneratorRuntime.mark((function e(t){var n;return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:return(n=t.document.getSelection().parentContentControlOrNullObject).load(["text","title"]),e.next=4,t.sync();case 4:readCC(coq.provider.snippets.find((function(e){return e.title==n.title.split("/")[0].trim()})),n.id);case 5:case"end":return e.stop()}}),e)})));return function(t){return e.apply(this,arguments)}}());case 1:case"end":return e.stop()}}),e)})))).apply(this,arguments)}function writeSelected(){return _writeSelected.apply(this,arguments)}function _writeSelected(){return(_writeSelected=_asyncToGenerator(regeneratorRuntime.mark((function e(){return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:writeBack(!1);case 1:case"end":return e.stop()}}),e)})))).apply(this,arguments)}function readCC(e,t){return _readCC.apply(this,arguments)}function _readCC(){return(_readCC=_asyncToGenerator(regeneratorRuntime.mark((function e(t,n){return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:Word.run(function(){var e=_asyncToGenerator(regeneratorRuntime.mark((function e(r){var o,c;return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:if(o=r.document.contentControls.getById(n),Office.context.platform!=Office.PlatformType.PC){e.next=9;break}return(c=o.split([])).load(["items","text"]),e.next=6,r.sync();case 6:t.editor.setValue(c.items.map((function(e){return e.text})).join("\n").replace(/\r\n/g,"\n").replace(/\r/g,"\n").replace(/\u000B/g,"\n").normalize("NFKD")),e.next=14;break;case 9:return(c=o.split([])).load(["items","text"]),e.next=13,r.sync();case 13:t.editor.setValue(c.items.map((function(e){return e.text})).join("\n").replace(/\r/g,"").replace(/\u000B/g,"\n").normalize("NFKD"));case 14:case"end":return e.stop()}}),e)})));return function(t){return e.apply(this,arguments)}}());case 1:case"end":return e.stop()}}),e)})))).apply(this,arguments)}function readFrom(){return _readFrom.apply(this,arguments)}function _readFrom(){return(_readFrom=_asyncToGenerator(regeneratorRuntime.mark((function e(){return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:Word.run(function(){var e=_asyncToGenerator(regeneratorRuntime.mark((function e(t){var n;return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:return(n=t.document.contentControls.getByTag("ws365_code_coq")).load(["length","items","text","title"]),e.next=4,t.sync();case 4:return n.items.forEach(function(){var e=_asyncToGenerator(regeneratorRuntime.mark((function e(t,n,r){var o;return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:coq.provider.snippets[n]&&(coq.provider.snippets[n].title=t.title.split("/")[0].trim(),coq.provider.snippets[n].ccid=t.id,o=t.id,t.title.split("/")[0].trim(),window.setTimeout(_asyncToGenerator(regeneratorRuntime.mark((function e(){return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:return e.next=2,readCC(coq.provider.snippets[n],o);case 2:console.log("-----------------settimeout",n);case 3:case"end":return e.stop()}}),e)}))),50*n));case 1:case"end":return e.stop()}}),e)})));return function(t,n,r){return e.apply(this,arguments)}}()),e.abrupt("return",t.sync());case 6:case"end":return e.stop()}}),e)})));return function(t){return e.apply(this,arguments)}}());case 1:case"end":return e.stop()}}),e)})))).apply(this,arguments)}function writeBack(e){return _writeBack.apply(this,arguments)}function _writeBack(){return(_writeBack=_asyncToGenerator(regeneratorRuntime.mark((function e(t){return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:Word.run(function(){var e=_asyncToGenerator(regeneratorRuntime.mark((function e(n){var r,o,c;return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:return(r=n.document.contentControls.getByTag("ws365_code_coq")).load(["length","title"]),e.next=4,n.sync();case 4:o=0;case 5:if(!(o<r.items.length)){e.next=13;break}if(c=r.items[o],!t&&coq.provider.currentFocus.title!=c.title.split("/")[0].trim()){e.next=10;break}return e.next=10,write(n,c);case 10:o++,e.next=5;break;case 13:case"end":return e.stop()}}),e)})));return function(t){return e.apply(this,arguments)}}());case 1:case"end":return e.stop()}}),e)})))).apply(this,arguments)}function write(e,t){return _write.apply(this,arguments)}function _write(){return(_write=_asyncToGenerator(regeneratorRuntime.mark((function e(t,n){var r,o,c;return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:if(e.prev=0,(r=document.createElement("PRE")).innerHTML=hljs.highlight("coq",coq.provider.snippets.find((function(e){return e.title==n.title.split("/")[0].trim()})).editor.getValue()).value,document.body.getElementsByClassName("ws365_canvas")[0].appendChild(r),computedStyleToInlineStyle(r,{recursive:!0,properties:["color","font-style","font-weight"]}),Office.context.platform==Office.PlatformType.PC)console.log(r.innerHTML),n.insertHtml("<pre>"+r.innerHTML.replace(/\u000B/g,"<br/>").replace(/\n/g,"\r\n")+"\r\n</pre>","Replace"),n.styleBuiltIn="NoSpacing";else for(console.log(r.innerHTML),o=r.innerHTML.split(/\r\n|\n|\r|\u000B/g),n.clear(),c=0;c<o.length;c++)c!=o.length-1?n.insertHtml(o[c].replace(/  /g,"&nbsp;&nbsp;")+"<br/>","End"):0!=o[c].length&&n.insertHtml(o[c].replace(/  /g,"&nbsp;&nbsp;"),"End");return n.font.name="Consolas",r.innerHTML="",document.body.getElementsByClassName("ws365_canvas")[0].removeChild(r),e.next=11,t.sync();case 11:e.next=16;break;case 13:e.prev=13,e.t0=e.catch(0),console.error(e.t0);case 16:case"end":return e.stop()}}),e,null,[[0,13]])})))).apply(this,arguments)}coqObserver.coqGoalInfo=function(){var e=_asyncToGenerator(regeneratorRuntime.mark((function e(t,n){return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:console.log("_______________MYGOALINFO______________________"),transcriptButtonChecked&&n?setTimeout(_asyncToGenerator(regeneratorRuntime.mark((function e(){return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:return e.next=2,transcriptStatement(t,!0,goals2DOM(n)[0].outerHTML);case 2:selectStatement(t);case 3:case"end":return e.stop()}}),e)}))),50):selectStatement(t);case 3:case"end":return e.stop()}}),e)})));return function(t,n){return e.apply(this,arguments)}}(),coqObserver.coqCancelled=function(){var e=_asyncToGenerator(regeneratorRuntime.mark((function e(t){return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:console.log("_______________MYCOQCANCELLED______________________"),selectStatement(coq.doc.sentences.filter((function(e){return!t.includes(e.coq_sid)})).last().coq_sid);case 2:case"end":return e.stop()}}),e)})));return function(t){return e.apply(this,arguments)}}(),coqObserver.feedMessage=function(){var e=_asyncToGenerator(regeneratorRuntime.mark((function e(t,n,r,o){return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:if(console.log("______________MYFEEDMESSAGE______________________"),!transcriptButtonChecked){e.next=4;break}return e.next=4,transcriptStatement(t,!1,coq.pprint.pp2Text(o));case 4:selectStatement(t);case 6:case"end":return e.stop()}}),e)})));return function(t,n,r,o){return e.apply(this,arguments)}}(),Office.onReady((function(info){info.host===Office.HostType.Word&&Word.run(function(){var _ref5=_asyncToGenerator(regeneratorRuntime.mark((function _callee8(context){var workspaces,workspace,foundRanges,scriptFile;return regeneratorRuntime.wrap((function _callee8$(_context8){for(;;)switch(_context8.prev=_context8.next){case 0:return workspaces=document.querySelector("#document"),foundRanges=context.document.contentControls.getByTag("ws365_code_coq"),foundRanges.load(["id","title","paragraphs/text"]),_context8.next=5,context.sync();case 5:return console.log(foundRanges.items),foundRanges.items.forEach((function(valR,iR,aR){try{valR.paragraphs.items.map((function(e){return e.text})).join("\n").match(coq365script_regexp).forEach((function(match){console.log("Eval Coq365 javascript",match.substring(2,match.length-2));var evalresult=eval(match.substring(2,match.length-2));console.log("Result Eval Coq365 javascript",evalresult)}))}catch(e){console.error(e)}workspace=document.createElement("TEXTAREA"),workspace.setAttribute("id","workspace_"+valR.id),workspaces.appendChild(workspace),jscoq_ids.push("workspace_"+valR.id)})),scriptFile=document.createElement("script"),scriptFile.type="text/javascript",scriptFile.src=jscoq_src,console.log("SET scriptFile.src"),scriptFile.onload=function(){console.log("START RUN scriptFile.onload"),loadJsCoq(jscoq_opts.base_path).then((function(){coq=new CoqManager(jscoq_ids,jscoq_opts),g.coq=coq,readFrom();try{coq.coq.observers.push(coqObserver)}catch(n){var e=coq.layout.log.bind(coq.layout);coq.layout.log=function(){var t=_asyncToGenerator(regeneratorRuntime.mark((function t(n,r){var o;return regeneratorRuntime.wrap((function(t){for(;;)switch(t.prev=t.next){case 0:if(t.prev=0,console.log("coq.layout.log_____________________________",r,n,coq.sentences.length,coq.sentences),o=coq.sentences.last().coq_sid,"info"!=r&&"error"!=r&&"warn"!=r){t.next=7;break}if(!transcriptButtonChecked){t.next=7;break}return t.next=7,transcriptStatement(o,!1,n.replace(/============================/g,"____________________________").replace(/<br [/]>/g,"\n").replace(/&lt;/g,"<").replace(/&amp;/g,"&"));case 7:selectStatement(o=coq.sentences.last().coq_sid),t.next=13;break;case 11:t.prev=11,t.t0=t.catch(0);case 13:return t.abrupt("return",e(n,r));case 14:case"end":return t.stop()}}),t,null,[[0,11]])})));return function(e,n){return t.apply(this,arguments)}}();var t=coq.layout.update_goals.bind(coq.layout);coq.layout.update_goals=function(){var e=_asyncToGenerator(regeneratorRuntime.mark((function e(n){return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:try{console.log("coq.layout.update_goals_____________________________",n,coq.sentences.length,coq.sentences),""!=n&&(transcriptButtonChecked?setTimeout(_asyncToGenerator(regeneratorRuntime.mark((function e(){var t;return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:return t=coq.sentences.last().coq_sid,e.next=3,transcriptStatement(t,!1,"★ "+n.replace(/============================/g,"____________________________").replace(/\nsubgoal (\d+) is:/g,"\n★ subgoal $1 is"));case 3:selectStatement(t=coq.sentences.last().coq_sid);case 5:case"end":return e.stop()}}),e)}))),50):selectStatement(coq.sentences.last().coq_sid))}catch(e){}return e.abrupt("return",t(n));case 2:case"end":return e.stop()}}),e)})));return function(t){return e.apply(this,arguments)}}()}var n=document.querySelector("#buttons"),r=document.createElement("BUTTON");r.innerText="READ",n.appendChild(r),r.onclick=function(){return tryCatch(readSelected)};var o=document.createElement("BUTTON");o.innerText="WRITE",n.appendChild(o),o.onclick=function(){return tryCatch((function(){return writeBack(!1)}))};var c=document.createElement("BUTTON");c.innerText="WRITEALL",n.appendChild(c),c.onclick=function(){return tryCatch((function(){return writeBack(!0)}))},n.append(" ");var a=document.createElement("INPUT");a.setAttribute("type","checkbox"),a.setAttribute("id","transcriptButton"),n.appendChild(a),a.onclick=function(){transcriptButtonChecked=document.querySelector("#transcriptButton").checked},n.append("TRANSCRIPT")}))},document.body.append(scriptFile),_context8.next=15,context.sync();case 15:case"end":return _context8.stop()}}),_callee8)})));return function(e){return _ref5.apply(this,arguments)}}())}))}).call(this,__webpack_require__(88))},88:function(e,t){var n;n=function(){return this}();try{n=n||new Function("return this")()}catch(e){"object"==typeof window&&(n=window)}e.exports=n}});
//# sourceMappingURL=taskpane.js.map