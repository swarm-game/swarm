function mkLink(text, url) {
    const anchor = document.createElement("a");
    anchor.href = url
    anchor.textContent = text;
    return anchor;
}

function insertTableRows(myTableBody, entries) {
    for (const entry of entries) {
        const rowItem = document.createElement("tr");

        const fieldVals = [
            entry.derivedAttrs.hasActorTarget,
            entry.derivedAttrs.pureComputation,
            entry.derivedAttrs.modifiesEnvironment,
            entry.derivedAttrs.modifiesRobot,
            entry.derivedAttrs.movesRobot,
            entry.derivedAttrs.returnsValue,
        ]

        const cellVals = [
            mkLink(entry.cmd, "https://github.com/swarm-game/swarm/wiki/Commands-Cheat-Sheet#" + entry.cmd),
        ];

        for (const val of fieldVals) {
            const span = document.createElement("span");
            span.className = val ? "trueValue" : "falseValue";
            span.appendChild(document.createTextNode(val));
            cellVals.push(span);
        }

        // cellVals.push(document.createTextNode(entry.derivedAttrs.outputType));

        for (const val of cellVals) {
            const cellElement = document.createElement("td");
            cellElement.appendChild(val);
            rowItem.append(cellElement);
        }

        myTableBody.appendChild(rowItem);
    }
}

function doFetch(myTable) {
    fetch("data/commands.json")
    .then((response) => {
    if (!response.ok) {
        throw new Error(`HTTP error, status = ${response.status}`);
    }
    return response.json();
    })
    .then((data) => {
        const myTableBody = myTable.querySelector("tbody");
        insertTableRows(myTableBody, data.entries);
        // Documentation: http://tristen.ca/tablesort/demo/
        new Tablesort(document.getElementById('my-table'));
    })
    .catch((error) => {
        const p = document.createElement("p");
        p.appendChild(document.createTextNode(`Error: ${error.message}`));
        document.body.insertBefore(p, myTable);
    });
}