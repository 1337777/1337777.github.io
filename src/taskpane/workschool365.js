/*
 * Copyright (c) Microsoft Corporation. All rights reserved. Licensed under the MIT license.
 * See LICENSE in the project root for license information.
 */


/* global console, document, Excel, Office */



document.getElementById('process-tags').onclick = (() => tryCatch(processWSTags));
document.getElementById('reformat-code').onclick = (() => tryCatch(reFormatSelected));
document.getElementById('clear-output').onclick = (() => tryCatch(() => clearTags('ws365_code_output')));
document.getElementById('clear-quiz').onclick = (() => tryCatch(() => clearTags('ws365_quiz')));
document.getElementById('coq-taskpane').onclick = (() => { self.location = "./taskpane.html?et="; });
document.getElementById("anon-user").onclick = (() => tryCatch(anonGrade));
document.getElementById("signed-in-user").onclick = (() => tryCatch(signedGrade));

// Config object to be passed to Msal on creation
const msalConfig = {
  auth: {
    clientId: "9623fe44-f664-4152-a5ec-85eae8bd1ebb", // this is a fake id
    authority: "https://login.microsoftonline.com/anthroplogic.com",
    redirectUri: "https://localhost:3000/taskpane.html",
  },
  cache: {
    cacheLocation: "sessionStorage", // This configures where your cache will be stored
    storeAuthStateInCookie: false, // Set this to "true" if you are having issues on IE11 or Edge
  }
};

const msalInstance = new Msal.UserAgentApplication(msalConfig);

msalInstance.handleRedirectCallback(function (error, response) {
  // handle redirect response or error
  console.log("----HANDLE--------------");
  console.log("----REDIRCALLBACK--------------");
  console.log(response);
  if (response !== null) {
  }

});



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

/* Global reference */
var coq;

var excoq_keyword = "(?coq365?)*";
var coq365script_keyword = "/?coq365?/*";



function keyRot(text, key, reverse) {
  var bound = 0x10000;
  return String.fromCharCode.apply(null,
    text.split('').map(function (v, i) {
      var rot = key[i % key.length].charCodeAt();
      if (reverse) rot = -rot;
      return (v.charCodeAt() + rot + bound) % bound;
    })
  );
};

async function signedGrade() {
  if (/[/][/].*[.]sharepoint[.]com.*/i.test(Office.context.document.url)) {
    self.location = Office.context.document.url.replace(/(.*)[/][^/]*$/i, '$1');
  }
  else {
    self.location = "https://anthroplogic.com";
  }
}

async function anonGrade() {

  Word.run(async context => {

    var foundRanges = context.document.contentControls.getByTag('ws365_quiz');
    foundRanges.load(['length', 'items', 'text', 'title']);
    var solutionCC = context.document.contentControls.getByTag('ws365_solutions');
    solutionCC.load(['length', 'items', 'text', 'title']);
    await context.sync();

    var cc, okko;
    var pp = new DOMParser();
    var xmlDoc = pp.parseFromString(keyRot(solutionCC.items[0].title, solutionCC.items[0].id.toString(), true), "text/xml");

    var xmlDocResultsEl = xmlDoc.evaluate('/solutions[1]', xmlDoc, null, XPathResult.ANY_TYPE, null).iterateNext();
    if (xmlDocResultsEl != null) {

      xmlDocResultsEl.childNodes.forEach((value, key, parent) => {
        try {
          console.log('value', value);
          var valueID = xmlDoc.evaluate('id', value, null, XPathResult.ANY_TYPE, null).iterateNext().childNodes[0].nodeValue.trim();
          console.log('valueID', valueID);

          var valueContent = xmlDoc.evaluate('content', value, null, XPathResult.ANY_TYPE, null).iterateNext().childNodes[0].nodeValue.trim();
          var questionCC = foundRanges.items.filter(it =>
            it.title.split('/')[0].split(';')[0].trim() == valueID);

          console.log('questionCC', questionCC);
          if (questionCC.length == 0) {
            okko = false;
            cc = solutionCC.items[0].insertParagraph(okko ? "OK" : "KO", Word.InsertLocation.after).insertContentControl();
          } else {
            okko = (questionCC[0].text.trim() == valueContent);
            cc = questionCC[0].insertParagraph(okko ? "OK" : "KO", Word.InsertLocation.after).insertContentControl();
          }
          cc.appearance = Word.ContentControlAppearance.tags;
          cc.color = okko ? "green" : "red";
          cc.font.highlightColor = okko ? "green" : "red";
          cc.tag = "ws365_grade";
          cc.title = " " + valueID + " / ws365_grade ";
        } catch (err) { console.log(err) };
      });
      await context.sync();
    }
  });
}

Office.onReady(info => {

  /*  if (info.host === Office.HostType.Excel) {
 
     Excel.run(async context => {
       var sheet = context.workbook.worksheets.getActiveWorksheet();
       var workspaces = document.querySelector("#document");
       var workspace;
 
       const foundRanges = sheet.findAllOrNullObject(excoq_keyword, {
         completeMatch: true,
         matchCase: false
       });
       foundRanges.load(["areas"]);
       await context.sync();
 
       if (!foundRanges.isNullObject) {
         foundRanges.areas.items.forEach(
           (valR, iR, aR) => {
             workspace = document.createElement("TEXTAREA");
             workspace.setAttribute("id", "workspace" + iR);
             workspaces.appendChild(workspace);
             jscoq_ids.push("workspace" + iR);
           }
         )
       } else {
         console.log("No Coq365 code with reserved keyword: coq365");
       };
 
       const foundRangesScript = sheet.findAllOrNullObject(coq365script_keyword, {
         completeMatch: true,
         matchCase: false
       });
       foundRangesScript.load(["areas"]);
       await context.sync();
 
       if (!foundRangesScript.isNullObject) {
         foundRangesScript.areas.items.forEach(
           (valR, iR, aR) => {
             valR.load("values");
           }
         )
         await context.sync();
         foundRangesScript.areas.items.forEach(
           (valR, iR, aR) => {
             var evalresult = eval(
               valR.values.map((x) => x.join(' ')).join("\n"));
             // valR.values[0][0]
             console.log("Eval Coq365 javascript, result: " + evalresult);
           }
         )
       } else {
         console.log("No Coq365 javascript with reserved keyword: coq365");
       };
 
       var scriptFile = document.createElement("script");
       document.body.append(scriptFile);
       scriptFile.type = "text/javascript";
       scriptFile.src = jscoq_src;
       console.log("SET scriptFile.src");
       scriptFile.onload = function () {
         console.log("START RUN scriptFile.onload");
         loadJsCoq(jscoq_opts.base_path)
           .then(
             () => {
               coq = new CoqManager(jscoq_ids, jscoq_opts);
 
               readFrom();
 
               var readFromButton = document.createElement("BUTTON");
               readFromButton.innerText = "READ";
               document.querySelector("#buttons").appendChild(readFromButton);
               readFromButton.onclick = readFrom;
 
               var writeBackButton = document.createElement("BUTTON");
               writeBackButton.innerText = "WRITE";
               document.querySelector("#buttons").appendChild(writeBackButton);
               writeBackButton.onclick = writeBack;
             }
           )
       };
 
       return context.sync()
         .then(function () {
           console.log("Finished Excel.Run here");
         });
     });
   } */

  if (info.host === Office.HostType.Word) {

    Word.run(async context => {
      /* 
            var readFromButton = document.createElement("BUTTON");
            readFromButton.innerText = "READ";
            document.body.appendChild(readFromButton);
            readFromButton.onclick = (function () { signWS(doMaill); });
       */
      /* if (!msalInstance.getAccount()) {
       // user is not logged in, you will need to log them in to acquire a token
       var loginRequest = {
         scopes: ["openid", "profile", "email", "User.Read"],
       };

       msalInstance.loginPopup(loginRequest)
         .then((loginResponse) => {
           //Login Success callback code here
           console.log("----LOGICNSTART--------------");
           console.log(loginResponse);
         }).catch(function (error) {
           console.log(error);
         });

     } */
      /* 
            var readFromButton2 = document.createElement("BUTTON");
            readFromButton2.innerText = "READ";
            document.body.appendChild(readFromButton2);
            readFromButton2.onclick =
              (async function () {
                let bootstrapToken = await OfficeRuntime.auth.getAccessToken({ allowSignInPrompt: true, allowConsentPrompt: true, forMSGraphAccess: true });
      
                // let lll = await OfficeRuntime.auth.getAccessToken()
                // .then(function (res)  {console.log(res)});
                console.log(bootstrapToken);
              });
       */
      /* 
            var readFromButton3 = document.createElement("BUTTON");
            readFromButton3.innerText = "READ3";
            document.body.appendChild(readFromButton3);
            readFromButton3.onclick =
              (function () {
      
                console.log(Office.context.document.url);
              }); */

      /* 
            var readFromButton4 = document.createElement("BUTTON");
            readFromButton4.innerText = "addQuizCC";
            document.body.appendChild(readFromButton4);
            readFromButton4.onclick = addQuizCC;
      
      
            var readFromButton5 = document.createElement("BUTTON");
            readFromButton5.innerText = "addfetch";
            document.body.appendChild(readFromButton5);
            readFromButton5.onclick = addfetch;
      
      
            var readFromButton6 = document.createElement("BUTTON");
            readFromButton6.innerText = "READ6";
            document.body.appendChild(readFromButton6);
            readFromButton6.onclick = (function () { signWS(doGrap); });
      
      
            var readFromButton7 = document.createElement("BUTTON");
            readFromButton7.innerText = "READ7";
            document.body.appendChild(readFromButton7);
            readFromButton7.onclick = (function () { signWS(doGrap2); });
       */

      /* document.getElementById('button1').onclick = (() => {
        Word.run(async function (context) {

          var cc = context.document.body.insertParagraph("PARRRRRRRR2", "End").insertContentControl();
          cc.appearance = Word.ContentControlAppearance.hidden;
          cc.color = "blue";
          cc.tag = "DODOD";
          cc.title = "ujdoieeeeeeeeeeeeeeeeeeeeeeee";
          await context.sync();
        });
      }); */


      console.log("----WORD--------------");

    });
  }

});

async function formatCC(context, pccc, codelang) {
  let docEl = document.createElement("PRE");

  if (Office.context.platform == Office.PlatformType.PC) {
    /* pccc.load('text');
    await context.sync(); */
    var valRtext = pccc.split(["\n"], false, false); //.getTextRanges(["\n", "\u000B"], false) //.split(["\n"], false, false); // getTextRanges([], false);          
    valRtext.load(['items', 'text']);
    await context.sync();
    console.log(valRtext);
    // coq.provider.snippets[iR].editor.setValue(valRtext.items.map(i => i.text).join('').replace(/\u000B/g, '\n').normalize('NFKD'));
    let op = valRtext.items.map(i => i.text).join('');

    docEl.innerHTML = hljs.highlight(codelang, op.substr(0, op.length)).value;
    document.body.getElementsByClassName('ws365_canvas')[0].appendChild(docEl);
    //docEl.style = "font-family: consolas; color: yellow";
    computedStyleToInlineStyle(docEl, { recursive: true, properties: ["color", "font-style", "font-weight"] });
    console.log(docEl.innerHTML);
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
    console.log(jk);
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

async function reFormatSelected() {
  await Word.run(async function (context) {
    var searchResults;
    let documentgetSelection = context.document.getSelection();
    documentgetSelection.load(['isEmpty']);
    await context.sync();

    if (documentgetSelection.isEmpty) {
      searchResults = context.document.body.contentControls;
    } else {
      searchResults = documentgetSelection.contentControls;
    }
    searchResults.load(['items', 'tag']);
    await context.sync();

    for (var i = 0; i < searchResults.items.length; i++) {
      if (/ws365_code_.*/i.test(searchResults.items[i].tag)) {
        let codelang = searchResults.items[i].tag.match(/ws365_code_(.*)/i)[1];
        codelang = (codelang == 'output') ? 'text' : codelang;
        await formatCC(context, searchResults.items[i], codelang);
      }
    }

    // var pccc = context.document.getSelection().parentContentControlOrNullObject;
    // pccc.load(['isNullObject', 'tag']);
    // await context.sync();

    // if (!pccc.isNullObject && /ws365_code_.*/i.test(pccc.tag)) {
    //   let codelang = pccc.tag.match(/ws365_code_(.*)/i);
    //   await formatCC(context, pccc, codelang[1]);
    // }
    // else {
    //   let ccs = context.document.contentControls;
    //   ccs.load(['items', 'tag']);
    //   await context.sync();
    //   for (var i = 0; i < ccs.items.length; i++) {
    //     if (/ws365_code_.*/i.test(ccs.items[i].tag)) {
    //       let codelang = ccs.items[i].tag.match(/ws365_code_(.*)/i);
    //       await formatCC(context, ccs.items[i], codelang[1]);
    //     }
    //   }
    // }
  });
}


async function clearTags(tagName /* 'ws365_code_output' or 'ws365_quiz' */) {
  await Word.run(async function (context) {
    var searchResults;
    let documentgetSelection = context.document.getSelection();
    documentgetSelection.load(['isEmpty']);
    await context.sync();

    if (documentgetSelection.isEmpty) {
      searchResults = context.document.body.contentControls.getByTag(tagName);
    } else {
      searchResults = documentgetSelection.contentControls.getByTag(tagName);
    }
    searchResults.load(['items', 'tag']);
    await context.sync();

    for (var i = 0; i < searchResults.items.length; i++) {
      searchResults.items[i].clear();
      if (tagName == 'ws365_quiz') {
        searchResults.items[i].color = "yellow";
        searchResults.items[i].font.highlightColor = "yellow";
      }
    }
    await context.sync();
  });
}

async function processWSTags() {
  await Word.run(async function (context) {

    var searchResults;
    let documentgetSelection = context.document.getSelection();
    documentgetSelection.load(['isEmpty']);
    await context.sync();

    if (documentgetSelection.isEmpty) {
      searchResults = context.document.body.search('[<]ws365[>]*[<]/ws365[>]', { matchWildcards: true });
    } else {
      searchResults = documentgetSelection.search('[<]ws365[>]*[<]/ws365[>]', { matchWildcards: true });
    }
    searchResults.load(['text']);
    await context.sync();
    var pp = new DOMParser();
    var xmlDoc, xmlDocResultsEl;
    var codelang, ccweight, cctitle;
    var fetchResponse, fetchBody = "", fetchBodys;
    var cc, pccc;

    var docEl = document.createElement("PRE");

    for (var i = 0; i < searchResults.items.length; i++) {

      try {
        let patt = new RegExp('^[^]*$', 'g');
        xmlDoc = pp.parseFromString(searchResults.items[i].text, "text/xml");

        xmlDocResultsEl = xmlDoc.evaluate('/ws365/content[child::title and child::tag][1]', xmlDoc, null, XPathResult.ANY_TYPE, null).iterateNext();
        if (xmlDocResultsEl != null) {
          cctitle = xmlDocResultsEl.getElementsByTagName("title")[0].childNodes[0].nodeValue;
          pccc = context.document.contentControls.getByTitle(cctitle);
          pccc.load('length');
          await context.sync();

          if (pccc.items.length == 0) {
            cc = searchResults.items[i].insertParagraph('', Word.InsertLocation.after).insertContentControl();
            cc.appearance = Word.ContentControlAppearance.tags;
            cc.color = "blue";
            cc.tag = xmlDocResultsEl.getElementsByTagName("tag")[0].childNodes[0].nodeValue;
            cc.title = cctitle;
          }

          if (xmlDocResultsEl.getElementsByTagName("clean").length != 0) {
            searchResults.items[i].delete();
            searchResults.items[i].paragraphs.getLast().delete();
            /* searchResults.items[i].load(['paragraphs']);
            await context.sync();
            searchResults.items[i].paragraphs.items.forEach((par, idx, ar) => {par.delete();}); */
            //searchResults.items[i].delete();
          }
        }

        xmlDocResultsEl = xmlDoc.evaluate('/ws365/quiz[child::id][1]', xmlDoc, null, XPathResult.ANY_TYPE, null).iterateNext();
        if (xmlDocResultsEl != null) {
          if (xmlDocResultsEl.getElementsByTagName("weight").length != 0) {
            ccweight = xmlDocResultsEl.getElementsByTagName("weight")[0].childNodes[0].nodeValue.trim();
          } else {
            ccweight = "10";
          }
          cctitle = " " + xmlDocResultsEl.getElementsByTagName("id")[0].childNodes[0].nodeValue.trim() + " ; " + ccweight + " / quiz ";
          pccc = context.document.contentControls.getByTitle(cctitle);
          pccc.load('length');
          await context.sync();

          if (pccc.items.length == 0) {
            cc = searchResults.items[i].insertParagraph('', Word.InsertLocation.after).insertContentControl();
            cc.appearance = Word.ContentControlAppearance.tags;
            cc.color = "yellow";
            cc.font.highlightColor = "yellow";
            cc.tag = "ws365_quiz";
            cc.title = cctitle;
          }

          if (xmlDocResultsEl.getElementsByTagName("clean").length != 0) {
            searchResults.items[i].delete();
            searchResults.items[i].paragraphs.getLast().delete();
            /*  searchResults.items[i].load(['paragraphs']);
             await context.sync();
             searchResults.items[i].paragraphs.items.forEach((par, idx, ar) => {par.delete();}); */
            // searchResults.items[i].delete();
          }
        }

        xmlDocResultsEl = xmlDoc.evaluate('/ws365/solutions[1]', xmlDoc, null, XPathResult.ANY_TYPE, null).iterateNext();
        if (xmlDocResultsEl != null) {

          pccc = context.document.contentControls.getByTag('ws365_solutions');
          pccc.load(['length', 'title']);
          await context.sync();

          // var cxml = context.document.customXmlParts.add(xmlDocResultsEl.parentNode.textContent);
          if (pccc.items.length == 0) {
            cc = searchResults.items[i].insertParagraph('ws365_solutions', Word.InsertLocation.after).insertContentControl();
            cc.appearance = Word.ContentControlAppearance.hidden;
            cc.color = "cyan";
            cc.font.highlightColor = "cyan";
            cc.tag = "ws365_solutions";
            cc.cannotEdit = true;
            await context.sync();
            cc.title = keyRot(xmlDocResultsEl.parentNode.innerHTML, cc.id.toString(), false) /* cxml.id */;
          } else {
            //context.document.customXmlParts.getItem(pccc.items[0].title.split('/')[0].trim()).delete();
            pccc.items[0].title = keyRot(xmlDocResultsEl.parentNode.innerHTML, pccc.items[0].id.toString(), false) /* cxml.id */;
          }

          if (xmlDocResultsEl.getElementsByTagName("clean").length != 0) {
            searchResults.items[i].delete();
            searchResults.items[i].paragraphs.getLast().delete();
            /* searchResults.items[i].load(['paragraphs']);
            await context.sync();
            searchResults.items[i].paragraphs.items.forEach((par, idx, ar) => {par.delete();}); */
            // searchResults.items[i].delete();
          }
        }

        xmlDocResultsEl = xmlDoc.evaluate('/ws365/code[child::id and child::lang][1]', xmlDoc, null, XPathResult.ANY_TYPE, null).iterateNext();
        if (xmlDocResultsEl != null) {
          codelang = xmlDocResultsEl.getElementsByTagName("lang")[0].childNodes[0].nodeValue.trim();
          cctitle = " " + xmlDocResultsEl.getElementsByTagName("id")[0].childNodes[0].nodeValue.trim() + " / " + codelang + " ";
          pccc = context.document.contentControls.getByTitle(cctitle);
          pccc.load('length');
          await context.sync();

          if (pccc.items.length == 0) {
            cc = searchResults.items[i].insertParagraph('', Word.InsertLocation.after).insertContentControl();
            cc.appearance = Word.ContentControlAppearance.tags;
            cc.color = "purple";
            cc.tag = "ws365_code_" + codelang;
            cc.title = cctitle;

            if (xmlDocResultsEl.getElementsByTagName("url").length != 0) {

              if (xmlDocResultsEl.getElementsByTagName("regex").length != 0) {
                patt = new RegExp(xmlDocResultsEl.getElementsByTagName("regex")[0].childNodes[0].nodeValue, 'g');
              }

              fetchResponse = await fetch(xmlDocResultsEl.getElementsByTagName("url")[0].childNodes[0].nodeValue.trim());
              fetchBodys = await fetchResponse.text();
              /* fetchBodys.match(patt).forEach((fetchBody, j, jA) => {
                if (j != 0) { cc.insertParagraph('', Word.InsertLocation.end) }
                cc.insertText(fetchBody, "End");
              }); */
              let pattj = 0;
              let array1;
              while ((array1 = patt.exec(fetchBodys)) !== null) {
                if (pattj != 0) { cc.insertParagraph('', Word.InsertLocation.end) }
                (array1[1] == undefined) ? cc.insertText(array1[0], "End") : cc.insertText(array1[1], "End");
                pattj++;
              }
            }

            if (xmlDocResultsEl.getElementsByTagName("content").length != 0) {
              if (xmlDocResultsEl.getElementsByTagName("content")[0].hasChildNodes()) {
                cc.insertText((xmlDocResultsEl.getElementsByTagName("content")[0].childNodes[0].nodeValue), Word.InsertLocation.end);
              }
            }

            await context.sync();
            pccc = context.document.contentControls.getByTitle(cctitle);
            pccc.load('length');
            await context.sync();
          }

          if (xmlDocResultsEl.getElementsByTagName("format").length != 0) {

            await formatCC(context, pccc.items[0], codelang);
            // if (Office.context.platform == Office.PlatformType.PC) {
            //   /* pccc.items[0].load('text');
            //   await context.sync(); */
            //   var valRtext = pccc.items[0].split(["\n"], false, false); //.getTextRanges(["\n", "\u000B"], false) //.split(["\n"], false, false); // getTextRanges([], false);          
            //   valRtext.load(['items', 'text']);
            //   await context.sync();
            //   console.log(valRtext);
            //   // coq.provider.snippets[iR].editor.setValue(valRtext.items.map(i => i.text).join('').replace(/\u000B/g, '\n').normalize('NFKD'));
            //   let op = valRtext.items.map(i => i.text).join('');

            //   docEl.innerHTML = hljs.highlight(codelang, op.substr(0, op.length)).value;
            //   document.body.getElementsByClassName('ws365_canvas')[0].appendChild(docEl);
            //   //docEl.style = "font-family: consolas; color: yellow";
            //   computedStyleToInlineStyle(docEl, { recursive: true, properties: ["color", "font-style", "font-weight"] });
            //   console.log(docEl.innerHTML);
            //   //pccc.items[0].insertHtml('<pre>' + docEl.innerHTML.replace(/\u000B/g, '\n').replace(/\n\n/g, '\n<p> </p>').replace( /<[/]p>\n/g , '</p><p> </p>').replace(/\n/g, '<p/>').replace(/  /g, '  ') + '</pre>', "Replace");
            //   pccc.items[0].insertHtml('<pre>' + docEl.innerHTML.replace(/\u000B/g, '<br/>').replace(/\n/g, '\r\n') + '\r\n</pre>', "Replace");
            //   //pccc.items[0].insertHtml('<pre>' + docEl.innerHTML/* .replace(/\u000B/g, '\n') */.replace(/\n\n/g, '\n<br/>').replace( /<br[/]>\n/g , '<br/><br/>').replace(/\n/g, '<p/>').replace(/  /g, '  ') + '</pre>', "Replace");
            //   pccc.items[0].styleBuiltIn = "NoSpacing";
            // }
            // else {
            //   /* pccc.items[0].load('text');
            //   await context.sync(); */
            //   var valRtext = pccc.items[0].split(["\n"], false, false); // getTextRanges([], false);          
            //   valRtext.load(['items', 'text']);
            //   await context.sync();
            //   //var arr = valRtext.items.map(i => i.text.split(["\u000B"]) ).flat(1) ;
            //   // coq.provider.snippets[iR].editor.setValue(valRtext.items.map(i => i.text).join('').replace(/\u000B/g, '\n').normalize('NFKD'));
            //   let op = valRtext.items.map(i => i.text).join('');
            //   docEl.innerHTML = hljs.highlight(codelang, op.substr(0, op.length - 1)).value;
            //   document.body.getElementsByClassName('ws365_canvas')[0].appendChild(docEl);
            //   //docEl.style = "font-family: consolas; color: yellow";
            //   computedStyleToInlineStyle(docEl, { recursive: true, properties: ["color", "font-style", "font-weight"] });
            //   var jk = docEl.innerHTML.split(/\r\n|\n|\r|\u000B/g);
            //   console.log(jk);
            //   pccc.items[0].clear();

            //   for (let j = 0; j < jk.length; j++) {
            //     //      pccc.items[0].insertHtml('<pre>' + docEl.innerHTML.replace(/\u000B/g, '<br/>').replace(/\n/g, '<p></p>') + '</pre>', "Replace");
            //     if (j != jk.length - 1) {
            //       pccc.items[0].insertHtml(jk[j].replace(/  /g, '&nbsp;&nbsp;') + '<br/>', "End");

            //     } else if (jk[j].length != 0) {
            //       pccc.items[0].insertHtml(jk[j].replace(/  /g, '&nbsp;&nbsp;'), "End");
            //     }

            //     //                pccc.items[0].insertHtml('' + docEl.innerHTML.replace(/\u000B/g, '\n').replace(/\n\n/g, '\n<p>&nbsp;</p>').replace( /<[/]p>\n/g , '</p><p>&nbsp;</p>').replace(/\n/g, '<p/>').replace(/  /g, '&nbsp;&nbsp;') + '', "Replace");
            //   }
            //   /* /!\ pccc.items[0].styleBuiltIn = "NoSpacing"; */
            // }

            // pccc.items[0].font.name = "Consolas";
            // document.body.getElementsByClassName('ws365_canvas')[0].removeChild(docEl);
            // docEl.innerHTML = '';

          }

          if (xmlDocResultsEl.getElementsByTagName("clean").length != 0) {
            searchResults.items[i].delete();
            searchResults.items[i].paragraphs.getLast().delete();
            /* searchResults.items[i].load(['paragraphs']);
            await context.sync();
            searchResults.items[i].paragraphs.items.forEach((par, idx, ar) => {par.delete();}); */
            //searchResults.items[i].delete();
          }
        }
      } catch (error) {
        console.log('ERROR TO PROCESS: ', searchResults.items[i].text);
        console.error(error);
      }
      await context.sync();
    }
    return context.sync();
  });
}

function doGrap(token) {

  console.log("----TOKEN--------------");
  console.log(token);

  var headers = new Headers();
  var bearer = "Bearer " + token;
  headers.append("Authorization", bearer);
  var options = {
    method: "GET",
    headers: headers
  };
  var graphEndpoint = "https://graph.microsoft.com/v1.0/drives/b!43OgZpt9zUKzwVYZ-zkvxbTL7LsxnLBNk5_bR03tcz-EZHJcG_x6QrKZxi_tCR1X/root:/General/test123.docx";

  fetch(graphEndpoint, options)
    .then(resp => {
      //do something with response
      console.log("----GRAPH--------------");
      console.log(resp);
      resp.json().then(function (data) {
        console.log(data);

      });



    });
}


function doGrap2(token) {

  console.log("----TOKEN--------------");
  console.log(token);

  var headers = new Headers();
  var bearer = "Bearer " + token;
  headers.append("Authorization", bearer);
  var options = {
    method: "GET",
    headers: headers
  };
  var graphEndpoint = "https://graph.microsoft.com/v1.0/sites/anthroplogic.sharepoint.com:/sites/anthroplogic:/drives";

  fetch(graphEndpoint, options)
    .then(resp => {
      //do something with response
      console.log("----GRAPH--------------");
      console.log(resp);
      resp.json().then(function (data) {
        console.log(data);

      });



    });
}


async function addfetch() {
  var xc;

  fetch("https://anthroplogic.sharepoint.com/sites/AnthropLOGIC/_layouts/15/download.aspx?UniqueId=29cec068-f6c1-4f7f-a41f-4d6897923314&Translate=false&tempauth=eyJ0eXAiOiJKV1QiLCJhbGciOiJub25lIn0.eyJhdWQiOiIwMDAwMDAwMy0wMDAwLTBmZjEtY2UwMC0wMDAwMDAwMDAwMDAvYW50aHJvcGxvZ2ljLnNoYXJlcG9pbnQuY29tQDkyYzZjMmQwLTVmYjctNDc3OC1hZjM4LWJiNTg0MjM5NDRkZCIsImlzcyI6IjAwMDAwMDAzLTAwMDAtMGZmMS1jZTAwLTAwMDAwMDAwMDAwMCIsIm5iZiI6IjE2MTAyNjMyNTYiLCJleHAiOiIxNjEwMjY2ODU2IiwiZW5kcG9pbnR1cmwiOiJLS0daU1g3OE1HTll4TUJESzJpcWhmU0F6eWpLazNsK3MydFlxVjIyQVowPSIsImVuZHBvaW50dXJsTGVuZ3RoIjoiMTQyIiwiaXNsb29wYmFjayI6IlRydWUiLCJjaWQiOiJObVUxTWprd01ERXROV1l3WmkwME9ETmlMVGcxTkRrdE1qTXlZMkZqTW1NM1pEaGoiLCJ2ZXIiOiJoYXNoZWRwcm9vZnRva2VuIiwic2l0ZWlkIjoiTmpaaE1EY3paVE10TjJRNVlpMDBNbU5rTFdJell6RXROVFl4T1daaU16a3labU0xIiwiYXBwX2Rpc3BsYXluYW1lIjoiV29ya1NjaG9vbCAzNjUiLCJnaXZlbl9uYW1lIjoibW96ZXJ0MV9nbWFpbC5jb20iLCJmYW1pbHlfbmFtZSI6InVua25vd24iLCJzaWduaW5fc3RhdGUiOiJbXCJrbXNpXCJdIiwiYWMiOiJsfG18aCIsImFwcGlkIjoiOTYyM2ZlNDQtZjY2NC00MTUyLWE1ZWMtODVlYWU4YmQxZWJiIiwidGlkIjoiOTJjNmMyZDAtNWZiNy00Nzc4LWFmMzgtYmI1ODQyMzk0NGRkIiwidXBuIjoibW96ZXJ0MV9nbWFpbC5jb21AYW50aHJvcGxvZ2ljLm9ubWljcm9zb2Z0LmNvbSIsInB1aWQiOiIxMDAzMjAwMEYzREI1QTVGIiwiY2FjaGVrZXkiOiIwaC5mfG1lbWJlcnNoaXB8MTAwMzIwMDBmM2RiNWE1ZkBsaXZlLmNvbSIsInNjcCI6Im15ZmlsZXMucmVhZCBhbGxmaWxlcy5yZWFkIGFsbHByb2ZpbGVzLnJlYWQiLCJ0dCI6IjIiLCJ1c2VQZXJzaXN0ZW50Q29va2llIjpudWxsfQ.OUxVVnIxaXcwcHFCMm5CMHZBZFZRZTlHK1ZUK2V5Mi9KZlJHczdHS3pldz0&ApiVersion=2.0")
    .then(response => response.arrayBuffer())
    .then(async (data) => {
      xc = btoa(String.fromCharCode(...new Uint8Array(data)));
      await Word.run(async (context) => {
        context.document.body.insertFileFromBase64(xc, Word.InsertLocation.start);
        await context.sync();
      })
    }
    );
}

async function addQuizCC() {
  await Word.run(async (context) => {
    // Check out how wildcard expression are built, also use the second parameter of the search method to include search modes
    // (i.e. use wildcards).
    let results = context.document.body.getRange(Word.RangeLocation.whole).split(["<ws365>", "</ws365>"], true, true);
    results.load(["length", "text"]);

    await context.sync();
    console.log(results);
    // Let's traverse the search results... and highlight...
    for (let i = 0; i < results.items.length; i++) {
      console.log('----')
      console.log(
        results.items[i].text);
    }

    await context.sync();
  });


  // Run a batch operation against the Word object model.
  Word.run(async function (context) {

    // Queue a command to search the document with a wildcard
    // for any string of characters that starts with 'to' and ends with 'n'.
    var searchResults = context.document.body.search('[<]ws365[>]*[<]/ws365[>]', { matchWildcards: true });

    // Queue a command to load the search results and get the font property values.
    context.load(searchResults, ['text', 'font']);

    // Synchronize the document state by executing the queued commands,
    // and return a promise to indicate task completion.
    return context.sync().then(async function () {
      console.log('Found count: ' + searchResults.items.length);

      var pp = new DOMParser();
      var xmlDoc;
      var cc, pcc, pccc, dc, npar;

      // Queue a set of commands to change the font for each found item.
      for (var i = 0; i < searchResults.items.length; i++) {
        searchResults.items[i].font.color = 'purple';
        searchResults.items[i].font.highlightColor = 'pink';
        searchResults.items[i].font.bold = true;
        xmlDoc = pp.parseFromString(searchResults.items[i].text, "text/xml");
        console.log(xmlDoc.getElementsByTagName("ws365")[0].childNodes[0].nodeValue);
        console.log(xmlDoc);

        pccc = searchResults.items[i].paragraphs.getLast().getNextOrNullObject().parentContentControlOrNullObject;

        pccc.load('isNullObject');
        await context.sync();

        if (pccc.isNullObject) {

          cc = searchResults.items[i].insertParagraph('Content of a new paragraph' + i, Word.InsertLocation.after).insertContentControl();
          cc.appearance = Word.ContentControlAppearance.tags;
          cc.color = "blue";
          cc.title = 'cc ' + i;

        }
      }

      // Synchronize the document state by executing the queued commands,
      // and return a promise to indicate task completion.
      return context.sync();
    });
  })
    .catch(function (error) {
      console.log('Error: ' + JSON.stringify(error));
      if (error instanceof OfficeExtension.Error) {
        console.log('Debug info: ' + JSON.stringify(error.debugInfo));
      }
    });

}

async function insertContentControls() {
  // Traverses each paragraph of the document and wraps a content control on each with either a even or odd tags.
  await Word.run(async (context) => {
    let paragraphs = context.document.body.paragraphs;
    paragraphs.load(["items/parentContentControlOrNullObject/isNullObject"]); // Don't need any properties; just wrap each paragraph with a content control.

    await context.sync();
    var kk;
    for (let i = 0; i < paragraphs.items.length; i++) {
      console.log(paragraphs.items[i].parentContentControlOrNullObject.toJSON() === {});
      console.log(null);
      paragraphs.items[i].parentContentControlOrNullObject.load("$none");
      await context.sync();
      if (paragraphs.items[i].parentContentControlOrNullObject.isNullObject) {
        let contentControl = paragraphs.items[i].insertContentControl();
        // For even, tag "even".
        if (i % 2 === 0) {
          contentControl.tag = "even";
        } else {
          contentControl.tag = "odd";
        }
      }
    }
    console.log("Content controls inserted: " + paragraphs.items.length);
    context.document.contentControls.load("items");
    await context.sync();
    console.log(context.document.contentControls.items.length);

    await context.sync();
  });
}

function signWS(doMail) {
  console.log("----READMAIL--------------");
  var token;

  var loginRequest = {
    scopes: ["openid", "profile", "email", "User.Read"],
  };

  if (!msalInstance.getAccount()) {

    msalInstance.loginPopup(loginRequest)
      .then((loginResponse) => {
        //Login Success callback code here
        console.log("----LOGICN--------------");
        console.log(loginResponse);
        signWS(doMail);
      }).catch(function (error) {
        console.log(error);
      });
  }

  if (msalInstance.getAccount()) {
    var tokenRequest = {
      scopes: ["User.Read", "Files.Read.All"]
    };
    msalInstance.acquireTokenSilent(tokenRequest)
      .then(response => {
        // get access token from response
        // response.accessToken
        console.log("----ACQUIRESILENT--------------");
        console.log(response);
        token = response.accessToken;
        doMail(token);
      })
      .catch(err => {
        // could also check if err instance of InteractionRequiredAuthError if you can import the class.
        if (err.name === "InteractionRequiredAuthError") {
          return msalInstance.acquireTokenPopup(tokenRequest)
            .then(response => {
              // get access token from response
              // response.accessToken
              console.log("----ACQUIREREDIR--------------");
              console.log(response);
              token = response.accessToken;
              doMail(token);
            })
            .catch(err => {
              // handle error
            });
        }
      });
  }

}

function doMaill(token) {

  console.log("----TOKEN--------------");
  console.log(token);

  var headers = new Headers();
  var bearer = "Bearer " + token;
  headers.append("Authorization", bearer);
  var options = {
    method: "GET",
    headers: headers
  };
  var graphEndpoint = "https://graph.microsoft.com/v1.0/me";

  fetch(graphEndpoint, options)
    .then(resp => {
      //do something with response
      console.log("----GRAPH--------------");
      console.log(resp);
      resp.json().then(function (data) {
        console.log(data);

        Word.run(async (context) => {
          context.document.body.clear();
          context.document.body.insertParagraph(JSON.stringify(data), "Start"
          );
        })
      });



    });
}


function readFrom() {
  Excel.run(async function (context) {

    var sheet = context.workbook.worksheets.getActiveWorksheet();

    const foundRanges = sheet.findAllOrNullObject(excoq_keyword, {
      completeMatch: true,
      matchCase: false
    });
    foundRanges.load(["areas"]);
    await context.sync();

    if (!foundRanges.isNullObject) {
      foundRanges.areas.items.forEach(
        (valR, iR, aR) => {
          valR.load("values");
        }
      )
      await context.sync();
      foundRanges.areas.items.forEach(
        (valR, iR, aR) => {
          if (coq.provider.snippets[iR]) {
            coq.provider.snippets[iR].editor.setValue(
              valR.values.map((x) => x.join(' ')).join("\n"));
            // valR.values[0][0]

          }
        }
      )
    } else {
      console.log("No Coq365 code with reserved keyword: coq365");
    };

    return context.sync();
  })
    .catch(function (error) {
      console.log("Error: " + error);
      if (error instanceof OfficeExtension.Error) {
        console.log("Debug info: " + JSON.stringify(error.debugInfo));
      }
    });
}

function writeBack() {
  Excel.run(async function (context) {

    var sheet = context.workbook.worksheets.getActiveWorksheet();

    const foundRanges = sheet.findAllOrNullObject(excoq_keyword, {
      completeMatch: true,
      matchCase: false
    });
    foundRanges.load(["areas"]);
    await context.sync();

    if (!foundRanges.isNullObject) {
      foundRanges.areas.items.forEach(
        (valR, iR, aR) => {
          if (coq.provider.snippets[iR]) {
            valR.values = [[coq.provider.snippets[iR].editor.getValue()]];
          }
        }
      )
    } else {
      console.log("No Coq365 code with reserved keyword: coq365");
    };

    return context.sync();
  })
    .catch(function (error) {
      console.log("Error: " + error);
      if (error instanceof OfficeExtension.Error) {
        console.log("Debug info: " + JSON.stringify(error.debugInfo));
      }
    });
}




/** Default helper for invoking an action and handling errors. */
async function tryCatch(callback) {
  try {
    await callback();
  } catch (error) {
    // Note: In a production add-in, you'd want to notify the user through your add-in's UI.
    console.error(error);
  }
}
