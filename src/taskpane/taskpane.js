/*
 * Copyright (c) Microsoft Corporation. All rights reserved. Licensed under the MIT license.
 * See LICENSE in the project root for license information.
 */


/* global console, document, Excel, Office */

var jscoq_ids = ['workspace'];
var jscoq_opts = {
  prelude: true,
  base_path: '../',
  init_pkgs: ['init', 'qoc'],
  all_pkgs: ['init', 'qoc', 'coq-base', 'coq-collections', 'coq-arith', 'coq-reals', 'math-comp'],
  implicit_libs: true,
  editor: { mode: { 'company-coq': true }, keyMap: 'default' }
};

/* Global reference */
var coq;

var txtResult = "";

Office.onReady(info => {
  if (info.host === Office.HostType.Excel) {
    //    document.getElementById("sideload-msg").style.display = "none";
    //    document.getElementById("app-body").style.display = "flex";
    //    document.getElementById("run").onclick = run;        

    Excel.run(async context => {
      var sheet = context.workbook.worksheets.getActiveWorksheet();
      var searchRange = sheet.getRange("A1:A1");
      var foundRange = null;

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
            /*valR.load("values");
            await context.sync(); */
            console.log("888888888888888888888888888", valR.values);
            txtResult += valR.values.map((x) => x.join(' ')).join("\n") + "\n";
            //txtResult += valR.values[0][0] + "\n";
          }
        )
      } else {
        console.log("No Coq code with reserved keyword (**WSCOQ*)");
      };

      loadJsCoq(jscoq_opts.base_path)
        .then(
          () => {
            coq = new CoqManager(jscoq_ids, jscoq_opts);
            coq.provider.snippets[0].editor.setValue(
              txtResult +
              "\n(*END*)");
          }
        );

      return context.sync()
        .then(function () {
          console.log("Finished Excel.Run here");
        });
    });
  }
});
