/*
 * Copyright (c) Microsoft Corporation. All rights reserved. Licensed under the MIT license.
 * See LICENSE in the project root for license information.
 */


/* global console, document, Excel, Office */

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

Office.onReady(info => {
  if (info.host === Office.HostType.Excel) {

    Excel.run(async context => {
      var sheet = context.workbook.worksheets.getActiveWorksheet();
      var workspaces = document.querySelector("#document");
      var workspace;

      const foundRanges = sheet.findAllOrNullObject("(**WSCOQ*)", {
        completeMatch: false,
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
            workspace = document.createElement("TEXTAREA");
            workspace.setAttribute("id", "workspace" + iR);
            //txtResult += valR.values[0][0] + "\n";
            workspace.innerText = valR.values.map((x) => x.join(' ')).join("\n");
            workspaces.appendChild(workspace);
            jscoq_ids.push("workspace" + iR);
          }
        )
      } else {
        console.log("No Coq code with reserved keyword (**WSCOQ*)");
      };

      loadJsCoq(jscoq_opts.base_path)
        .then(
          () => {
            coq = new CoqManager(jscoq_ids, jscoq_opts);

            var readFromButton = document.createElement("BUTTON");
            readFromButton.innerText = "READ";
            document.querySelector("#buttons").appendChild(readFromButton);
            readFromButton.onclick = readFrom;

            var writeBackButton = document.createElement("BUTTON");
            writeBackButton.innerText = "WRITE";
            document.querySelector("#buttons").appendChild(writeBackButton);
            writeBackButton.onclick = writeBack;
          }
        );

      return context.sync()
        .then(function () {
          console.log("Finished Excel.Run here");
        });
    });
  }
});

function readFrom() {
  Excel.run(async function (context) {

    var sheet = context.workbook.worksheets.getActiveWorksheet();

    const foundRanges = sheet.findAllOrNullObject("(**WSCOQ*)", {
      completeMatch: false,
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
          }
        }
      )
    } else {
      console.log("No Coq code with reserved keyword (**WSCOQ*)");
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

    const foundRanges = sheet.findAllOrNullObject("(**WSCOQ*)", {
      completeMatch: false,
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
            valR.values = [[coq.provider.snippets[iR].editor.getValue()]];
          }
        }
      )
    } else {
      console.log("No Coq code with reserved keyword (**WSCOQ*)");
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