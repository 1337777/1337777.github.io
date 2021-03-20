/*
 * Copyright (c) Microsoft Corporation. All rights reserved. Licensed under the MIT license.
 * See LICENSE in the project root for license information.
 */


/* global console, document, Excel, Office */


Office.onReady(info => {

  if (info.host === Office.HostType.Word) {

    Word.run(async context => {


    });
  }

});


if (document.documentMode) {
  console.error('Unsupported Browser!');
  var el = document.createElement("div");
  el.innerHTML = (`<h1 style="color: orange">Unsupported Browser!</h1>
   <h3>This add-in does not support the Internet Explorer web browser, 
  consider upgrading your host to Microsoft Edge 
  ( <a target="_blank" href="https://developer.microsoft.com/en-us/microsoft-edge/">https://developer.microsoft.com/en-us/microsoft-edge/</a> ) 
  or to Microsoft Word with the latest Microsoft Edge WebView 2 
  ( <a target="_blank" href="https://developer.microsoft.com/en-us/microsoft-edge/webview2/">https://developer.microsoft.com/en-us/microsoft-edge/webview2/</a> ) </h3>`);
  document.body.insertBefore(el, document.body.firstChild);
}

document.getElementById('process-tags').onclick = (() => tryCatch(processTags));
document.getElementById('reformat-code').onclick = (() => tryCatch(reFormatSelected));
document.getElementById('clear-output').onclick = (() => tryCatch(() => clearTags('ws365_code_output')));
document.getElementById('clear-quiz').onclick = (() => tryCatch(() => clearTags('ws365_quiz')));
document.getElementById('coq-taskpane').onclick = (() => { self.location = "./taskpane.html?et="; });
document.getElementById("anon-user").onclick = (() => tryCatch(anonGrade));
document.getElementById("signed-in-user").onclick = (() => tryCatch(signedGrade));
document.querySelectorAll(".doc-tags").forEach(function (el) {
  el.addEventListener('click', (() => tryCatch(() => docTags(el))))
});

/** Default helper for invoking an action and handling errors. */
async function tryCatch(callback) {
  try {
    await callback();
  } catch (error) {
    // Note: In a production add-in, you'd want to notify the user through your add-in's UI.
    console.error(error);
  }
}

async function docTags(el) {
  Word.run(async context => {
    var par = context.document.getSelection().insertParagraph(el.textContent.replace(/\s+/g, ' ').replace(/> </g, '><').trim(), "After");
    par.select();
    await context.sync();
  })
}

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
  if (/^https:[/][/][^/]*sharepoint[.]com.*/i.test(Office.context.document.url)) {
    self.location = Office.context.document.url.replace(/(.*)[/][^/]*$/i, '$1');
  }
  else {
    self.location = "https://anthroplogic.com";
  }
}

function tokenizeSolution(str) {

  //  return str.replace(/[`~!@#$%^&*()=+[{\]}\\|;:'"‘’“”,.<>/?-]/g, ' ').replace(/[\n\s]+/g, ' ').trim().toLowerCase();
  return str.replace(/[^\w]/g, ' ').replace(/[\n\s]+/g, ' ').trim().toLowerCase();

}

async function anonGrade() {

  Word.run(async context => {

    var foundRanges = context.document.contentControls.getByTag('ws365_quiz');
    foundRanges.load(['length', 'items', 'text', 'title']);
    var solutionCC = context.document.contentControls.getByTag('ws365_solutions');
    solutionCC.load(['length', 'items', 'text', 'title']);
    await context.sync();

    if (solutionCC.items.length != 0) {
      var cc, okko;
      var pp = new DOMParser();
      var xmlDoc = pp.parseFromString(keyRot(solutionCC.items[0].title, solutionCC.items[0].id.toString(), true), "text/xml");

      var xmlDocResultsEl = xmlDoc.evaluate('/solutions[1]', xmlDoc, null, XPathResult.ANY_TYPE, null).iterateNext();
      if (xmlDocResultsEl != null) {
        xmlDocResultsEl.childNodes.forEach((value, key, parent) => {
          try {
            var valueID = xmlDoc.evaluate('id', value, null, XPathResult.ANY_TYPE, null).iterateNext().childNodes[0].nodeValue.trim();

            var valueContent = xmlDoc.evaluate('content', value, null, XPathResult.ANY_TYPE, null).iterateNext().childNodes[0].nodeValue;
            var questionCC = foundRanges.items.filter(it =>
              it.title.split('/')[0].split(';')[0].trim() == valueID);

            if (questionCC.length == 0) {
              okko = false;
              cc = solutionCC.items[0].insertParagraph(okko ? "OK" : "KO", Word.InsertLocation.after).insertContentControl();
            } else {
              okko = (tokenizeSolution(questionCC[0].text) == tokenizeSolution(valueContent));
              cc = questionCC[0].insertParagraph(okko ? "OK" : "KO", Word.InsertLocation.after).insertContentControl();
            }
            cc.cannotEdit = true;
            cc.cannotDelete = true;
            cc.appearance = Word.ContentControlAppearance.tags;
            cc.color = okko ? "green" : "red";
            cc.font.highlightColor = okko ? "green" : "red";
            cc.font.color = "white";
            cc.tag = "ws365_grade";
            cc.title = " " + valueID + " / grade ";
          } catch (err) { console.error(err) };
        });
        await context.sync();
      }
    }
  });
}



async function formatCC(context, pccc, codelang) {
  let docEl = document.createElement("PRE");

  if (Office.context.platform != Office.PlatformType.OfficeOnline) {
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

async function processTags() {
  await Word.run(async function (context) {
    var searchResults;
    let documentgetSelection = context.document.getSelection();
    documentgetSelection.load(['isEmpty']);
    await context.sync();

    if (documentgetSelection.isEmpty) {
      searchResults = context.document.body.search('[<]ws365[>]*[<]/ws365[>]', { matchWildcards: true, matchCase: false });
    } else {
      searchResults = documentgetSelection.search('[<]ws365[>]*[<]/ws365[>]', { matchWildcards: true, matchCase: false });
    }
    searchResults.load(['text']);
    await context.sync();
    var pp = new DOMParser();

    for (var i = 0; i < searchResults.items.length; i++) {
      try {
        let xmlDoc, xmlDocResultsEl;
        let codelang, ccweight, cctitle;
        let fetchResponse, fetchBodys;
        let cc, pccc;
        let patt = new RegExp('^[^]*$', 'g');
        xmlDoc = pp.parseFromString(searchResults.items[i].text, "text/xml");

        // /!\ IE 11
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
          await context.sync();
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
          await context.sync();
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
          await context.sync();
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
          } else {
            cc = pccc.items[0];
          }
          await context.sync();

          if (xmlDocResultsEl.getElementsByTagName("format").length != 0) {
            await formatCC(context, cc, codelang);
          }

          if (xmlDocResultsEl.getElementsByTagName("clean").length != 0) {
            searchResults.items[i].delete();
            searchResults.items[i].paragraphs.getLast().delete();
          }
          await context.sync();
        }

        xmlDocResultsEl = xmlDoc.evaluate('/ws365/ereview[1]', xmlDoc, null, XPathResult.ANY_TYPE, null).iterateNext();
        if (xmlDocResultsEl != null) {
          pccc = context.document.contentControls.getByTag('ws365_ereview_reviewers');
          pccc.load(['length', 'title']);
          await context.sync();

          let rr = searchResults.items[i].insertParagraph('', Word.InsertLocation.after);
          if (xmlDocResultsEl.getElementsByTagName("clean").length != 0) {
            //searchResults.items[i].delete();
            //searchResults.items[i].paragraphs.getLast().delete();
            searchResults.items[i].delete();
            searchResults.items[i].paragraphs.getLast().delete();
            await context.sync();
          }

          if (pccc.items.length == 0) {
            if (xmlDocResultsEl.getElementsByTagName("title").length != 0) {
              let cctemplate_title_ = cctemplate_title;
              if (xmlDocResultsEl.getElementsByTagName("title")[0].hasChildNodes()) {
                cctemplate_title_ = cctemplate_title.replace('<w:t>Click or tap here to enter text.</w:t>', '<w:t>' + xmlDocResultsEl.getElementsByTagName("title")[0].childNodes[0].nodeValue + '</w:t>');
              }
              //searchResults.items[i].insertParagraph('', Word.InsertLocation.before).insertOoxml(cctemplate_title_, "End");
              rr = rr.insertParagraph('', Word.InsertLocation.after).insertOoxml(cctemplate_title_, "End");
              await context.sync();
            }

            if (xmlDocResultsEl.getElementsByTagName("short").length != 0) {
              let cctemplate_short_ = cctemplate_short;
              if (xmlDocResultsEl.getElementsByTagName("short")[0].hasChildNodes()) {
                cctemplate_short_ = cctemplate_short.replace('<w:t>Click or tap here to enter text.</w:t>', '<w:t>' + xmlDocResultsEl.getElementsByTagName("short")[0].childNodes[0].nodeValue + '</w:t>');
              }
              //searchResults.items[i].insertParagraph('', Word.InsertLocation.before).insertOoxml(cctemplate_short_, "End");
              rr = rr.insertParagraph('', Word.InsertLocation.after).insertOoxml(cctemplate_short_, "End");
              await context.sync();
            }

            if (xmlDocResultsEl.getElementsByTagName("reviewers").length != 0) {
              let cctemplate_reviewers_ = cctemplate_reviewers;
              if (xmlDocResultsEl.getElementsByTagName("url").length != 0) {
                let label = '';
                if (xmlDocResultsEl.getElementsByTagName("label").length != 0) {
                  label = xmlDocResultsEl.getElementsByTagName("label")[0].textContent.trim();
                }
               
                let found = (xmlDocResultsEl.getElementsByTagName("url")[0].childNodes[0].nodeValue.trim()).match(/(https:[/][/])?[^/]*github.com[/]([^/]+)[/]([^/]+)/i);
                fetchResponse = await fetch(`https://api.github.com/repos/${found[2]}/${found[3]}/pulls?state=closed`);
                fetchBodys = await fetchResponse.text();
                cctemplate_reviewers_ = cctemplate_reviewers.replace(`" reviewers / ereview "`, `" github.com/${found[2]}/${found[3]} / ereview "`)
                  .replace(/<w:sdtContent>[^]*<[/]w:sdtContent>/i,
                    '<w:sdtContent>' +
                    JSON.parse(fetchBodys)
                      .filter(o => o.merged_at && (label ? o.labels.map(l => l.name.toLowerCase()).includes(label.toLowerCase()) : true))
                      .map(o => `<w:p><w:r><w:rPr><w:b/><w:bCs/></w:rPr><w:t>` + o.user.login.replace(/&/g, '&#38;').replace(/</g, '&#60;').replace(/>/g, '&#62;') + `  @  ` + o.html_url.replace(/&/g, '&#38;').replace(/</g, '&#60;').replace(/>/g, '&#62;') + `</w:t></w:r></w:p>` +
                        `<w:p><w:r><w:t>` + o.title.replace(/&/g, '&#38;').replace(/</g, '&#60;').replace(/>/g, '&#62;') + `</w:t></w:r></w:p>` +
                        `<w:p><w:r><w:t>` + o.body.replace(/&/g, '&#38;').replace(/</g, '&#60;').replace(/>/g, '&#62;') + `</w:t></w:r></w:p>` +
                        `<w:p><w:r><w:t></w:t></w:r></w:p>`)
                      .join('') +
                    '</w:sdtContent>');
              }
              rr = rr.insertParagraph('', Word.InsertLocation.after).insertOoxml(cctemplate_reviewers_, "End");
              await context.sync();
            }
          }
        }

      } catch (error) {
        console.error(error);
        console.error('ERROR TO PROCESS: ', searchResults.items[i].text);
      }
      await context.sync();
    }
    return context.sync();
  });
}

const cctemplate_title =
  `<pkg:package xmlns:pkg='http://schemas.microsoft.com/office/2006/xmlPackage'>
<pkg:part pkg:name='/_rels/.rels' pkg:contentType='application/vnd.openxmlformats-package.relationships+xml' pkg:padding='512'>
    <pkg:xmlData>
        <Relationships xmlns='http://schemas.openxmlformats.org/package/2006/relationships'>
            <Relationship Id='rId1' Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument' Target='word/document.xml'/>
        </Relationships>
    </pkg:xmlData>
</pkg:part>
<pkg:part pkg:name='/word/document.xml' pkg:contentType='application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml'>
    <pkg:xmlData>
        <w:document xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas"
xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex"
xmlns:cx1="http://schemas.microsoft.com/office/drawing/2015/9/8/chartex"
xmlns:cx2="http://schemas.microsoft.com/office/drawing/2015/10/21/chartex"
xmlns:cx3="http://schemas.microsoft.com/office/drawing/2016/5/9/chartex"
xmlns:cx4="http://schemas.microsoft.com/office/drawing/2016/5/10/chartex"
xmlns:cx5="http://schemas.microsoft.com/office/drawing/2016/5/11/chartex"
xmlns:cx6="http://schemas.microsoft.com/office/drawing/2016/5/12/chartex"
xmlns:cx7="http://schemas.microsoft.com/office/drawing/2016/5/13/chartex"
xmlns:cx8="http://schemas.microsoft.com/office/drawing/2016/5/14/chartex"
xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
xmlns:aink="http://schemas.microsoft.com/office/drawing/2016/ink"
xmlns:am3d="http://schemas.microsoft.com/office/drawing/2017/model3d"
xmlns:o="urn:schemas-microsoft-com:office:office"
xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
xmlns:v="urn:schemas-microsoft-com:vml"
xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"
xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
xmlns:w10="urn:schemas-microsoft-com:office:word"
xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"
xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml"
xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex"
xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid"
xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml"
xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex"
xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup"
xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk"
xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" mc:Ignorable="w14 w15 w16se w16cid w16 w16cex wp14">
<w:body>
<w:sdt>
<w:sdtPr>
    <w:rPr>
        <w:rStyle w:val="TitleChar"/>
    </w:rPr>
    <w:alias w:val=" title / ereview "/>
    <w:tag w:val="ws365_ereview_title"/>
    <w:id w:val="-1786179243"/>
    <!-- w:lock w:val="sdtLocked"/ -->
    <w:placeholder>
        <w:docPart w:val="C18670A167294F038A07CB72887BF386"/>
    </w:placeholder>
    <w:showingPlcHdr/>
    <w15:color w:val="008000"/>
    <w15:appearance w15:val="tags"/>
    <w:text/>
</w:sdtPr>
<w:sdtEndPr>
    <w:rPr>
        <w:rStyle w:val="TitleChar"/>
    </w:rPr>
</w:sdtEndPr>
<w:sdtContent>
    <w:p w14:paraId="3705D272" w14:textId="4C39D160" w:rsidR="009A7AC0" w:rsidRDefault="00CC2804">
        <w:r w:rsidRPr="00CC2804">
            <w:rPr>
                <w:rStyle w:val="TitleChar"/>
            </w:rPr>
            <w:t>Click or tap here to enter text.</w:t>
        </w:r>
    </w:p>
</w:sdtContent>
</w:sdt>
</w:body>
</w:document>
    </pkg:xmlData>
</pkg:part>
</pkg:package>`

const cctemplate_short =
  `<pkg:package xmlns:pkg='http://schemas.microsoft.com/office/2006/xmlPackage'>
<pkg:part pkg:name='/_rels/.rels' pkg:contentType='application/vnd.openxmlformats-package.relationships+xml' pkg:padding='512'>
    <pkg:xmlData>
        <Relationships xmlns='http://schemas.openxmlformats.org/package/2006/relationships'>
            <Relationship Id='rId1' Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument' Target='word/document.xml'/>
        </Relationships>
    </pkg:xmlData>
</pkg:part>
<pkg:part pkg:name='/word/document.xml' pkg:contentType='application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml'>
    <pkg:xmlData>
        <w:document xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas"
xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex"
xmlns:cx1="http://schemas.microsoft.com/office/drawing/2015/9/8/chartex"
xmlns:cx2="http://schemas.microsoft.com/office/drawing/2015/10/21/chartex"
xmlns:cx3="http://schemas.microsoft.com/office/drawing/2016/5/9/chartex"
xmlns:cx4="http://schemas.microsoft.com/office/drawing/2016/5/10/chartex"
xmlns:cx5="http://schemas.microsoft.com/office/drawing/2016/5/11/chartex"
xmlns:cx6="http://schemas.microsoft.com/office/drawing/2016/5/12/chartex"
xmlns:cx7="http://schemas.microsoft.com/office/drawing/2016/5/13/chartex"
xmlns:cx8="http://schemas.microsoft.com/office/drawing/2016/5/14/chartex"
xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
xmlns:aink="http://schemas.microsoft.com/office/drawing/2016/ink"
xmlns:am3d="http://schemas.microsoft.com/office/drawing/2017/model3d"
xmlns:o="urn:schemas-microsoft-com:office:office"
xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
xmlns:v="urn:schemas-microsoft-com:vml"
xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"
xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
xmlns:w10="urn:schemas-microsoft-com:office:word"
xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"
xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml"
xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex"
xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid"
xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml"
xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex"
xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup"
xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk"
xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" mc:Ignorable="w14 w15 w16se w16cid w16 w16cex wp14">
<w:body>
<w:sdt>
<w:sdtPr>
    <w:alias w:val=" short / ereview "/>
    <w:tag w:val="ws365_ereview_short"/>
    <w:id w:val="2116171124"/>
    <!-- w:lock w:val="sdtLocked"/ -->
    <w:placeholder>
        <w:docPart w:val="9427B7AC5A924BBEBF828D613A096981"/>
    </w:placeholder>
    <w:showingPlcHdr/>
    <w15:color w:val="008000"/>
    <w15:appearance w15:val="tags"/>
    <w:text w:multiLine="1"/>
</w:sdtPr>
<w:sdtEndPr/>
<w:sdtContent>
    <w:p w14:paraId="2AFC5604" w14:textId="671ED5D6" w:rsidR="009A7AC0" w:rsidRDefault="00CC2804" w:rsidP="0077216D">
        <w:r w:rsidRPr="00CC2804">
            <w:t>Click or tap here to enter text.</w:t>
        </w:r>
    </w:p>
</w:sdtContent>
</w:sdt>
</w:body>
</w:document>
    </pkg:xmlData>
</pkg:part>
</pkg:package>`

const cctemplate_reviewers =
  `<pkg:package xmlns:pkg='http://schemas.microsoft.com/office/2006/xmlPackage'>
<pkg:part pkg:name='/_rels/.rels' pkg:contentType='application/vnd.openxmlformats-package.relationships+xml' pkg:padding='512'>
    <pkg:xmlData>
        <Relationships xmlns='http://schemas.openxmlformats.org/package/2006/relationships'>
            <Relationship Id='rId1' Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument' Target='word/document.xml'/>
        </Relationships>
    </pkg:xmlData>
</pkg:part>
<pkg:part pkg:name='/word/document.xml' pkg:contentType='application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml'>
    <pkg:xmlData>
        <w:document xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas"
xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex"
xmlns:cx1="http://schemas.microsoft.com/office/drawing/2015/9/8/chartex"
xmlns:cx2="http://schemas.microsoft.com/office/drawing/2015/10/21/chartex"
xmlns:cx3="http://schemas.microsoft.com/office/drawing/2016/5/9/chartex"
xmlns:cx4="http://schemas.microsoft.com/office/drawing/2016/5/10/chartex"
xmlns:cx5="http://schemas.microsoft.com/office/drawing/2016/5/11/chartex"
xmlns:cx6="http://schemas.microsoft.com/office/drawing/2016/5/12/chartex"
xmlns:cx7="http://schemas.microsoft.com/office/drawing/2016/5/13/chartex"
xmlns:cx8="http://schemas.microsoft.com/office/drawing/2016/5/14/chartex"
xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
xmlns:aink="http://schemas.microsoft.com/office/drawing/2016/ink"
xmlns:am3d="http://schemas.microsoft.com/office/drawing/2017/model3d"
xmlns:o="urn:schemas-microsoft-com:office:office"
xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
xmlns:v="urn:schemas-microsoft-com:vml"
xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"
xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
xmlns:w10="urn:schemas-microsoft-com:office:word"
xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"
xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml"
xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex"
xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid"
xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml"
xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex"
xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup"
xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk"
xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" mc:Ignorable="w14 w15 w16se w16cid w16 w16cex wp14">
<w:body>
<w:sdt>
<w:sdtPr>
    <w:alias w:val=" reviewers / ereview "/>
    <w:tag w:val="ws365_ereview_reviewers"/>
    <w:id w:val="-158768229"/>
    <w:lock w:val="sdtContentLocked"/>
    <w:placeholder>
        <w:docPart w:val="4671E73BE31647F8A3FD43B319FCD03C"/>
    </w:placeholder>
    <w:showingPlcHdr/>
    <w15:color w:val="008000"/>
    <w15:appearance w15:val="tags"/>
    <w:text w:multiLine="1"/>
</w:sdtPr>
<w:sdtEndPr/>
<w:sdtContent>
    <w:p w14:paraId="6E0C1900" w14:textId="616D3880" w:rsidR="009A7AC0" w:rsidRDefault="009A7AC0">
        <w:r w:rsidRPr="00272804">
            <w:rPr>
                <w:b/>
                <w:bCs/>
            </w:rPr>
            <w:t></w:t>
        </w:r>
    </w:p>
</w:sdtContent>
</w:sdt>
</w:body>
</w:document>
    </pkg:xmlData>
</pkg:part>
</pkg:package>`

/// TODO:
/* async function insertContentControls() {
  await Word.run(async (context) => {
    let paragraphs = context.document.body.insertOoxml(
      `<pkg:package xmlns:pkg='http://schemas.microsoft.com/office/2006/xmlPackage'>
    <pkg:part pkg:name='/_rels/.rels' pkg:contentType='application/vnd.openxmlformats-package.relationships+xml' pkg:padding='512'>
        <pkg:xmlData>
            <Relationships xmlns='http://schemas.openxmlformats.org/package/2006/relationships'>
                <Relationship Id='rId1' Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument' Target='word/document.xml'/>
            </Relationships>
        </pkg:xmlData>
    </pkg:part>
    <pkg:part pkg:name='/word/document.xml' pkg:contentType='application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml'>
        <pkg:xmlData>
            <w:document xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas"
    xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex"
    xmlns:cx1="http://schemas.microsoft.com/office/drawing/2015/9/8/chartex"
    xmlns:cx2="http://schemas.microsoft.com/office/drawing/2015/10/21/chartex"
    xmlns:cx3="http://schemas.microsoft.com/office/drawing/2016/5/9/chartex"
    xmlns:cx4="http://schemas.microsoft.com/office/drawing/2016/5/10/chartex"
    xmlns:cx5="http://schemas.microsoft.com/office/drawing/2016/5/11/chartex"
    xmlns:cx6="http://schemas.microsoft.com/office/drawing/2016/5/12/chartex"
    xmlns:cx7="http://schemas.microsoft.com/office/drawing/2016/5/13/chartex"
    xmlns:cx8="http://schemas.microsoft.com/office/drawing/2016/5/14/chartex"
    xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
    xmlns:aink="http://schemas.microsoft.com/office/drawing/2016/ink"
    xmlns:am3d="http://schemas.microsoft.com/office/drawing/2017/model3d"
    xmlns:o="urn:schemas-microsoft-com:office:office"
    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
    xmlns:v="urn:schemas-microsoft-com:vml"
    xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"
    xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
    xmlns:w10="urn:schemas-microsoft-com:office:word"
    xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"
    xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml"
    xmlns:w16cex="http://schemas.microsoft.com/office/word/2018/wordml/cex"
    xmlns:w16cid="http://schemas.microsoft.com/office/word/2016/wordml/cid"
    xmlns:w16="http://schemas.microsoft.com/office/word/2018/wordml"
    xmlns:w16se="http://schemas.microsoft.com/office/word/2015/wordml/symex"
    xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup"
    xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk"
    xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
    xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" mc:Ignorable="w14 w15 w16se w16cid w16 w16cex wp14">
    <w:body>
        <w:sdt>
            <w:sdtPr>
                <w:alias w:val="cctitle"/>
                <w:tag w:val="cctag"/>
                <w:id w:val="-980158935"/>
                <w:placeholder>
                    <w:docPart w:val="DefaultPlaceholder_-1854013440"/>
                </w:placeholder>
                <w:showingPlcHdr/>
                <w15:appearance w15:val="tags"/>
                <w:text w:multiLine="1"/>
            </w:sdtPr>
            <w:sdtContent>
                <w:p w14:paraId="35A65FD4" w14:textId="215FA48B" w:rsidR="0061040A" w:rsidRDefault="002510E8">
                    <w:r w:rsidRPr="00DA6B7D">
                        <w:rPr>
                            <w:rStyle w:val="PlaceholderText"/>
                        </w:rPr>
                        <w:t>Click or tap here to enter text.</w:t>
                    </w:r>
                </w:p>
            </w:sdtContent>
        </w:sdt>
        <w:sectPr w:rsidR="0061040A">
            <w:pgSz w:w="12240" w:h="15840"/>
            <w:pgMar w:top="1440" w:right="1800" w:bottom="1440" w:left="1800" w:header="720" w:footer="720" w:gutter="0"/>
            <w:cols w:space="720"/>
            <w:docGrid w:linePitch="360"/>
        </w:sectPr>
    </w:body>
</w:document>
        </pkg:xmlData>
    </pkg:part>
</pkg:package>
`, "End");
    paragraphs.load("text");

    await context.sync();

    console.log("Content controls inserted: " + paragraphs.text);

  });
} */