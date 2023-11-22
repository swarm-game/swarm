function mkLink(text, url) {
    const anchor = document.createElement("a");
    anchor.href = url
    anchor.textContent = text;
    return anchor;
}

function insertTableRows(myTableBody, entries) {
    for (const entry of entries) {
        const rowItem = document.createElement("tr");

        const vals = [
            mkLink(entry.cmd, "https://github.com/swarm-game/swarm/wiki/Commands-Cheat-Sheet#" + entry.cmd),
            document.createTextNode(entry.derivedAttrs.hasActorTarget),
            document.createTextNode(entry.derivedAttrs.pureComputation),
        ];

        for (const val of vals) {
            const cellElement = document.createElement("td");
            cellElement.appendChild(val);
            rowItem.append(cellElement);
        }

        myTableBody.appendChild(rowItem);
    }
}

function doFetch(myTable) {
    fetch("commands")
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