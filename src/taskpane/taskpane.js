/*
 * Copyright (c) Microsoft Corporation. All rights reserved. Licensed under the MIT license.
 * See LICENSE in the project root for license information.
 */


/* global console, document, Excel, Office */


//import * as Resizable from './resizable.js';

// Note: Word consolas 12 gives adjustWidth() "Setting printing width to: "  67 

Array.prototype.last = function () { return this[this.length - 1]; };

var sizes = {
  "ide-wrapper": 0.5,
  "code-wrapper": 0.5
};

self.Resizable.initialise("main", sizes);

window.addEventListener("resize", () => {
  self.Resizable.activeContentWindows[0].changeSize(window.innerWidth, window.innerHeight);
  self.Resizable.activeContentWindows[0].childrenResize();
});

self.Resizable.activeContentWindows[0].changeSize(window.innerWidth, window.innerHeight);
self.Resizable.activeContentWindows[0].childrenResize();


var jscoq_src = "../../ui-js/jscoq-loader.js";
var jscoq_ids = [];
var jscoq_opts = {
  show: true,
  line_numbers: 'continue',
  prelude: true,
  base_path: '../',
  init_pkgs: ['init', 'qoc'],
  all_pkgs: ['init', 'qoc', 'coq-base', 'coq-collections', 'coq-arith', 'coq-reals', 'math-comp'],
  implicit_libs: true,
  editor: { mode: { 'company-coq': true }, keyMap: 'default' }
};

/*
jscoq_src  = "/jscoq-builds-v88/ui-js/jscoq-loader.js";
jscoq_opts = {
  show: true,
  line_numbers: 'continue',
  prelude: true,
  base_path: '/jscoq-builds-v88/',
  init_pkgs: ['init'],
  all_pkgs: ['init', 'math-comp'],
  implicit_libs: true,
  editor: { mode: { 'company-coq': true }, keyMap: 'default' }
};

jscoq_src = "../../ui-js/jscoq-loader.js";
jscoq_opts = {
  show: true,
  line_numbers: 'continue',
  prelude: true,
  base_path: '../',
  init_pkgs: ['init', 'qoc'],
  all_pkgs: ['init', 'qoc', 'coq-base', 'coq-collections', 'coq-arith', 'coq-reals', 'math-comp'],
  implicit_libs: true,
  editor: { mode: { 'company-coq': true }, keyMap: 'default' }
};

*/

/* Global reference */
var coq;

function getGlobal() {
  return typeof self !== "undefined"
    ? self
    : typeof window !== "undefined"
      ? window
      : typeof global !== "undefined"
        ? global
        : undefined;
}

const g = getGlobal();

// the add-in command functions need to be available in global scope

const coq365script_regexp = RegExp('[(][*]/[*]coq365[*]/([^]*?)[*][)]', 'ig');

/** Default helper for invoking an action and handling errors. */
async function tryCatch(callback) {
  try {
    await callback();
  } catch (error) {
    // Note: In a production add-in, you'd want to notify the user through your add-in's UI.
    console.error(error);
  }
}

var transcriptButtonChecked;
window.transcriptSeparator = '             ▽'; //'▽▽▽▽▽▽▽▽▽▽▽▽▽▽▽▽▽▽▽▽▽▽▽▽▽';

var coqObserver = {};

coqObserver['coqGoalInfo'] = async (span_id, cont) => {
  if (transcriptButtonChecked && cont) {
    setTimeout(async () => {
      await transcriptStatement(span_id, true, goals2DOM(cont)[0].outerHTML);
      selectStatement(span_id);
    }, 50);
  } else {
    selectStatement(span_id);
  };
};

coqObserver['coqCancelled'] = async (span_ids, ...cont) => {
  selectStatement(coq.doc.sentences.filter(s => !span_ids.includes(s.coq_sid)).last().coq_sid);
};

coqObserver['feedMessage'] = async (span_id, lvl, loc, msg) => {
  if (transcriptButtonChecked) {
    await transcriptStatement(span_id, false, coq.pprint.pp2Text(msg))
  };
  selectStatement(span_id);


};

//document.body.innerHTML = "<p>KOKOK" + Office.HostType.Word + "OKOK</p>";
Office.onReady(info => {


  if (info.host === Office.HostType.Word) {
    Word.run(async context => {
      var workspaces = document.querySelector("#document");
      var workspace;
      var foundRanges = context.document.contentControls.getByTag('ws365_code_coq');

      foundRanges.load(['id', 'title', 'length', 'paragraphs/text']);
      await context.sync();

      if (foundRanges.items.length != 0) {
      foundRanges.items.forEach(
        (valR, iR, aR) => {
          try {
            /* for (const match of valR.paragraphs.items.map(i => i.text).join('\n').matchAll(coq365script_regexp)) {
              console.log("Eval Coq365 javascript", match[1]);
              var evalresult = eval(match[1]);
              console.log("Result Eval Coq365 javascript", evalresult);
            } */
            valR.paragraphs.items.map(i => i.text).join('\n').match(coq365script_regexp).forEach(
              function (match) {
                var evalresult = eval(match.substring(2, match.length - 2));
              })
          } catch (err) { console.error(err); }
          workspace = document.createElement("TEXTAREA");
          //workspace.setAttribute("id", "workspace_" + valR.title.split('/')[0].trim());
          workspace.setAttribute("id", "workspace_" + valR.id);
          workspaces.appendChild(workspace);
          //jscoq_ids.push("workspace_" + valR.title.split('/')[0].trim());
          jscoq_ids.push("workspace_" + valR.id);
        }
      );
      }

      var scriptFile = document.createElement("script");
      //document.body.append(scriptFile);
      scriptFile.type = "text/javascript";
      scriptFile.src = jscoq_src;
      scriptFile.onload = /* setTimeout */(function () {
        loadJsCoq(jscoq_opts.base_path)
          .then(
            () => {
              coq = new CoqManager(jscoq_ids, jscoq_opts);
              g.coq = coq;

              readFrom();
              try { coq.coq.observers.push(coqObserver); }

              catch {
                //document.querySelector('#buttons').setAttribute('style', 'height : 100px; ' + document.querySelector('#buttons').getAttribute('style'));

                var coqlayoutlog = coq.layout.log.bind(coq.layout);
                coq.layout.log = (async (text, level) => {
                  try {
                    var stm = coq.sentences.last().coq_sid;
                    if (level == "info" || level == "error" || level == "warn" /* || !( /^feedback for [[]sid:/i.test(text)) */) {
                      if (transcriptButtonChecked) {
                        await transcriptStatement(stm, false, text.replace(/============================/g,
                          '____________________________').replace(/<br [/]>/g, '\n').replace(/&lt;/g, '<').replace(/&amp;/g, '&'));
                      }
                    }
                    stm = coq.sentences.last().coq_sid;
                    selectStatement(stm);
                  }
                  catch { }
                  return coqlayoutlog(text, level);
                })

                var coqlayoutupdate_goals = coq.layout.update_goals.bind(coq.layout);
                coq.layout.update_goals = (async (str) => {
                  try {
                    if (str != "") {
                      if (transcriptButtonChecked) {
                        setTimeout(async () => {
                          var stm = coq.sentences.last().coq_sid;
                          await transcriptStatement(stm, false, '★ ' + str.replace(/============================/g,
                            '____________________________').replace(/\nsubgoal (\d+) is:/g, '\n★ subgoal $1 is'));
                          stm = coq.sentences.last().coq_sid;
                          selectStatement(stm);
                        }, 50);
                      } else {
                        var stm = coq.sentences.last().coq_sid;
                        selectStatement(stm);
                      }
                    }
                  }
                  catch { }
                  return coqlayoutupdate_goals(str);
                })

              }



              let buttons = document.querySelector("#buttons");

              var readSelectedButton = document.createElement("BUTTON");
              readSelectedButton.innerText = "READ";
              //readSelectedButton.setAttribute("style", "height : 30px");
              buttons.appendChild(readSelectedButton);
              readSelectedButton.onclick = (() => tryCatch(readSelected));

              var writeSelectedButton = document.createElement("BUTTON");
              writeSelectedButton.innerText = "WRITE";
              buttons.appendChild(writeSelectedButton);
              writeSelectedButton.onclick = (() => tryCatch(() => writeBack(false)));

              var writeBackButton = document.createElement("BUTTON");
              writeBackButton.innerText = "WRITEALL";
              buttons.appendChild(writeBackButton);
              writeBackButton.onclick = (() => tryCatch(() => writeBack(true)));

              buttons.append(" ");
              var transcriptButton = document.createElement("INPUT");
              transcriptButton.setAttribute("type", "checkbox");
              transcriptButton.setAttribute("id", "transcriptButton");
              buttons.appendChild(transcriptButton);
              transcriptButton.onclick = (() => { transcriptButtonChecked = document.querySelector("#transcriptButton").checked; });
              buttons.append("TRANSCRIPT");


            }
          )
      } /* , 3000 */);
      document.body.append(scriptFile);

      await context.sync();

    });
  }

});




function selectStatement(span_id) {
  var stm;
  if (coq.doc && coq.doc.sentences) {
    stm = coq.doc.sentences.filter(s => s.coq_sid == span_id).last()
  } else if (coq.sentences) {
    stm = coq.sentences.filter(s => s.coq_sid == span_id).last()
  }

  Word.run(async function (context) {
    var v = context.document.contentControls.getById(stm.sp.ccid).getRange('Content').split(["\u000B", "\n", "\r", ""]);
    v.load(['length', 'items', 'text']);
    await context.sync();
    var rgstart;

    if (stm.text.split("\n")[0] == "") {
      rgstart = v.items[stm.start.line + 1].search(stm.text.split("\n")[1]);
    }
    else {
      rgstart = v.items[stm.start.line].search(stm.text.split("\n")[0]);
    }
    rgstart.load(['length']);
    await context.sync();
    var rgend = v.items[stm.end.line].search(stm.text.split("\n").last());
    rgend.load(['length']);
    await context.sync();

    var rg = rgstart.items[0];
    v.items.slice(stm.start.line + 1, stm.end.line).forEach(function (r) { rg = rg.expandTo(r); });
    rg = rg.expandTo(rgend.items[0]);
    rg.select();
    await context.sync();
  });
}


async function transcriptStatement(span_id, isHtml, cont) {
  var stm;
  if (coq.doc && coq.doc.sentences) {
    stm = coq.doc.sentences.filter(s => s.coq_sid == span_id).last()
  } else if (coq.sentences) {
    stm = coq.sentences.filter(s => s.coq_sid == span_id).last()
  }

  Word.run(async function (context) {
    var v = context.document.contentControls.getById(stm.sp.ccid);
    v.load(['length', 'title', 'items']);
    await context.sync();

    //let codelang = v.tag.match(/ws365_code_(.*)/i)[1];
    let cctitle = v.title.replace(/\s*([^/\s]+)(.*)/, ' O_$1 / output ');
    // Note: Word consolas 12 gives adjustWidth() "Setting printing width to: "  67 
    pccc = context.document.contentControls.getByTitle(cctitle);
    pccc.load(['items', 'length', 'text']);
    await context.sync();
    var cc;

    if (pccc.items.length == 0) {
      cc = v.insertParagraph('', Word.InsertLocation.after).insertContentControl();
      cc.appearance = Word.ContentControlAppearance.tags;
      cc.color = "purple";
      cc.tag = "ws365_code_output";
      cc.title = cctitle;
      cc.load(['text']);
      await context.sync();
    }
    else {
      cc = pccc.items[0]
    }

    // if (stm.start.line == 0 && stm.start.ch == 0) {
    // /!\INFO AND GOAL DUPLICATE CLEAR PROBLEM/!\ cc.clear(); }
    if (cc.text != '')  {
      cc.insertParagraph(transcriptSeparator, "End");
      cc.insertParagraph('', "End");       //v.insertBreak("Line", "After"); 
    }

    if (isHtml) {
      // coq.company_coq.markup.applyToDOM(goals2DOM(cont)[0])
      cc.insertHtml(cont, "End");
    } else {
      //  cont_ = '<div>' + coq.pprint.pp2Text(cont).replace(/\n/g, '<p></p>').replace(/  /g, '&nbsp;&nbsp;') + '</div>';
      cc.insertText(cont, "End");
    }
    await context.sync();

    await formatCC(context, cc, 'text');
  });
}


/**
 * Formats the current proof state.
 * @param {object} goals a record of proof goals 
 *                       ({goals, stack, shelf, given_up})
 */
function goals2DOM(goals) {
  if (goals.goals.length == 0) {
    return $(document.createTextNode("No more goals"));
  }
  else {
    let ngoals = goals.goals.length;
    let head = $('<p>')/* .addClass('num-goals') */
      .text(ngoals === 1 ? `★ 1 goal.` : `★ ${ngoals} goals`);

    let focused_goal = goal2DOM(goals.goals[0]);

    let pending_goals = goals.goals.slice(1).map((goal, i) =>
      $('<div>')/* .addClass('coq-subgoal-pending') */
        .append($('<p>').text('★ subgoal ' + (i + 2) + ' is:'))
        .append(coq.pprint.pp2Text(goal.ty)));

    return $('<div>').append(head, focused_goal, pending_goals);
  }
}

/**
 * Formats a single, focused goal.
 * Shows an environment containing hypothesis and goal type.
 * @param {object} goal current goal record ({name, hyp, ty})
 */
function goal2DOM(goal) {
  let hyps = goal.hyp/* .reverse() */.map(h =>
    $('<p>')/* .addClass('coq-hypothesis') */
      .append($('<span>').text(h[0] + ' : '))
      .append(coq.pprint.pp2Text(h[2])));
  let ty = coq.pprint.pp2Text(goal.ty);
  return $('<div>')/* .addClass('coq-env') */.append(hyps, $('<p>').text('____________________________'), ty);
}

async function formatCC(context, pccc, codelang) {
  let docEl = document.createElement("PRE");

  if (Office.context.platform == Office.PlatformType.PC) {
    /* pccc.load('text');
    await context.sync(); */
    var valRtext = pccc.split(["\n"], false, false); //.getTextRanges(["\n", "\u000B"], false) //.split(["\n"], false, false); // getTextRanges([], false);          
    valRtext.load(['items', 'text']);
    await context.sync();
    // coq.provider.snippets[iR].editor.setValue(valRtext.items.map(i => i.text).join('').replace(/\u000B/g, '\n').normalize('NFKD'));
    let op = valRtext.items.map(i => i.text).join('');

    docEl.innerHTML = hljs.highlight(codelang, op.substr(0, op.length)).value;
    document.body.getElementsByClassName('ws365_canvas')[0].appendChild(docEl);
    //docEl.style = "font-family: consolas; color: yellow";
    computedStyleToInlineStyle(docEl, { recursive: true, properties: ["color", "font-style", "font-weight"] });
    //pccc.insertHtml('<pre>' + docEl.innerHTML.replace(/\u000B/g, '\n').replace(/\n\n/g, '\n<p> </p>').replace( /<[/]p>\n/g , '</p><p> </p>').replace(/\n/g, '<p/>').replace(/  /g, '  ') + '</pre>', "Replace");
    pccc.insertHtml('<pre>' + docEl.innerHTML.replace(/\u000B/g, '<br/>').replace(/\n/g, '\r\n') + '\r\n</pre>', "Replace");
    //pccc.insertHtml('<pre>' + docEl.innerHTML/* .replace(/\u000B/g, '\n') */.replace(/\n\n/g, '\n<br/>').replace( /<br[/]>\n/g , '<br/><br/>').replace(/\n/g, '<p/>').replace(/  /g, '  ') + '</pre>', "Replace");
    pccc.styleBuiltIn = "NoSpacing";
  }
  else {
    /* pccc.load('text');
    await context.sync(); */
    var valRtext = pccc.split(["\n"], false, false); // getTextRanges([], false);          
    valRtext.load(['items', 'text']);
    await context.sync();
    //var arr = valRtext.items.map(i => i.text.split(["\u000B"]) ).flat(1) ;
    // coq.provider.snippets[iR].editor.setValue(valRtext.items.map(i => i.text).join('').replace(/\u000B/g, '\n').normalize('NFKD'));
    let op = valRtext.items.map(i => i.text).join('');
    docEl.innerHTML = hljs.highlight(codelang, op.substr(0, op.length - 1)).value;
    document.body.getElementsByClassName('ws365_canvas')[0].appendChild(docEl);
    //docEl.style = "font-family: consolas; color: yellow";
    computedStyleToInlineStyle(docEl, { recursive: true, properties: ["color", "font-style", "font-weight"] });
    var jk = docEl.innerHTML.split(/\r\n|\n|\r|\u000B/g);
    pccc.clear();

    for (let j = 0; j < jk.length; j++) {
      //      pccc.insertHtml('<pre>' + docEl.innerHTML.replace(/\u000B/g, '<br/>').replace(/\n/g, '<p></p>') + '</pre>', "Replace");
      if (j != jk.length - 1) {
        pccc.insertHtml(jk[j].replace(/  /g, '&nbsp;&nbsp;') + '<br/>', "End");

      } else if (jk[j].length != 0) {
        pccc.insertHtml(jk[j].replace(/  /g, '&nbsp;&nbsp;'), "End");
      }

      //                pccc.insertHtml('' + docEl.innerHTML.replace(/\u000B/g, '\n').replace(/\n\n/g, '\n<p>&nbsp;</p>').replace( /<[/]p>\n/g , '</p><p>&nbsp;</p>').replace(/\n/g, '<p/>').replace(/  /g, '&nbsp;&nbsp;') + '', "Replace");
    }
    /* /!\ pccc.styleBuiltIn = "NoSpacing"; */
  }

  pccc.font.name = "Consolas";
  await context.sync();
  document.body.getElementsByClassName('ws365_canvas')[0].removeChild(docEl);
  docEl.innerHTML = '';
}

async function readSelected() {
  Word.run(async function (context) {

    var foundRanges = context.document.getSelection().parentContentControlOrNullObject;
    foundRanges.load(['text', 'title']);
    await context.sync();

    readCC(coq.provider.snippets.find(s => s.title == foundRanges.title.split("/")[0].trim()), foundRanges.id);
    //coq.provider.snippets.find(s => s.title == foundRanges.title.split("/")[0].trim()).editor.setValue(foundRanges.text);
  });
}


async function writeSelected() {
  writeBack(false);
}

async function readCC(siR, cid) {
  Word.run(async function (context) {
    var valR = context.document.contentControls.getById(cid);
    if (Office.context.platform == Office.PlatformType.PC) {
      var valRtext = valR.split([]); // getTextRanges([], false);          
      valRtext.load(['items', 'text']);
      /* valR.load([ 'text']); */
      await context.sync();
      siR.editor.setValue(/* valR.text */valRtext.items.map(i => i.text).join('\n').replace(/\r\n/g, '\n').replace(/\r/g, '\n').replace(/\u000B/g, '\n').normalize('NFKD'));
    } else {
      var valRtext = valR.split([]); // getTextRanges([], false);          
      valRtext.load(['items', 'text']);
      await context.sync();
      siR.editor.setValue(valRtext.items.map(i => i.text).join('\n').replace(/\r/g, '').replace(/\u000B/g, '\n').normalize('NFKD'));
    }
    //  coq.provider.snippets[iR].editor.setValue(valRtext.items.map(i => i.text).join('\n').replace(/\u000B/g, '\n').normalize('NFKD'));
  });
  //  coq.provider.snippets[iR].editor.setValue(valR.text.replace(/\u000B/g, '\n').normalize('NFKD'));

}

async function readFrom() {
  Word.run(async function (context) {

    var foundRanges = context.document.contentControls.getByTag('ws365_code_coq');
    foundRanges.load(['length', 'items', 'text', 'title']);
    await context.sync();

    foundRanges.items.forEach(
      async (valR, iR, aR) => {
        if (coq.provider.snippets[iR]) {
          coq.provider.snippets[iR].title = valR.title.split('/')[0].trim();
          coq.provider.snippets[iR].ccid = valR.id;
          var cid = valR.id;
          var ctitle = valR.title.split('/')[0].trim();
          window.setTimeout(async () => {
            await readCC(coq.provider.snippets[iR], cid);
            //context.sync();
          }, 50 * iR);
        }
      }
    );

    return context.sync();
  })
}


async function writeBack(writeAll) {
  Word.run(async function (context) {

    var foundRanges = context.document.contentControls.getByTag('ws365_code_coq');

    foundRanges.load(['length', 'title']);
    await context.sync();

    //foundRanges.items.forEach(
    //    (valR, iR, aR) => {
    for (let iR = 0; iR < foundRanges.items.length; iR++) {
      const valR = foundRanges.items[iR];
      if (writeAll || coq.provider.currentFocus.title == valR.title.split("/")[0].trim()) {
        await write(context, valR);
      }
    }

  })
}


async function write(context, valR) {
  try {
    //valR.insertText(coq.provider.snippets.find(s => s.title == foundRanges.title.split("/")[0].trim()).editor.getValue(), Word.InsertLocation.replace);
    // valR.insertHtml("<pre>" + coq.provider.snippets[iR].editor.getValue().replace(/\n/g, "<p></p>") + "</pre>", Word.InsertLocation.replace);
    var docEl = document.createElement("PRE");
    docEl.innerHTML = hljs.highlight('coq', coq.provider.snippets.find(s => s.title == valR.title.split("/")[0].trim()).editor.getValue()).value;
    document.body.getElementsByClassName('ws365_canvas')[0].appendChild(docEl);
    computedStyleToInlineStyle(docEl, { recursive: true, properties: ["color", "font-style", "font-weight"] });
    //console.log('-----------------replace', docEl.outerHTML.replaceAll(/\n(.*?)\n/g, '<p>$1</p>').replaceAll(/  /g, '&nbsp&nbsp'));
    //valR.insertHtml('<pre><p>'+ docEl.innerHTML.replaceAll(/\n/g, '</p><p>') + '</p></pre>', "Replace");

    if (Office.context.platform == Office.PlatformType.PC) {
      valR.insertHtml('<pre>' + docEl.innerHTML.replace(/\u000B/g, '<br/>').replace(/\n/g, '\r\n') + '\r\n</pre>', "Replace");
      valR.styleBuiltIn = "NoSpacing";
    }
    else {
      var jk = docEl.innerHTML.split(/\r\n|\n|\r|\u000B/g);
      valR.clear();
      for (let j = 0; j < jk.length; j++) {
        if (j != jk.length - 1) {
          valR.insertHtml(jk[j].replace(/  /g, '&nbsp;&nbsp;') + '<br/>', "End");

        } else if (jk[j].length != 0) {
          valR.insertHtml(jk[j].replace(/  /g, '&nbsp;&nbsp;'), "End");
        }
      }
      /* /!\ valR.styleBuiltIn = "NoSpacing"; */
    }
    //valR.insertHtml('' + docEl.innerHTML.replace(/\u000B/g, '\n').replace(/\n\n/g, '\n<p>&nbsp;</p>').replace( /<[/]p>\n/g , '</p><p>&nbsp;</p>').replace(/\n/g, '<p/>').replace(/  /g, '&nbsp;&nbsp;') + '', "Replace");


    valR.font.name = "Consolas";
    docEl.innerHTML = '';
    document.body.getElementsByClassName('ws365_canvas')[0].removeChild(docEl);
    await context.sync();


    // window.setTimeout( () => {
    //   console.log('-----------------settimeout', iR);
    //    context.sync();            
    // }, 10000*iR);
  } catch (err) { console.error(err); }
}