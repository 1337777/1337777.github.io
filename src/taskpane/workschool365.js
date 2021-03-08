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

  if (info.host === Office.HostType.Word) {

    Word.run(async context => {


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


          }

          if (xmlDocResultsEl.getElementsByTagName("clean").length != 0) {
            searchResults.items[i].delete();
            searchResults.items[i].paragraphs.getLast().delete();
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

/** Default helper for invoking an action and handling errors. */
async function tryCatch(callback) {
  try {
    await callback();
  } catch (error) {
    // Note: In a production add-in, you'd want to notify the user through your add-in's UI.
    console.error(error);
  }
}
