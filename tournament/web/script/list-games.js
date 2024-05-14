
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
            entry.scenarioUploader,
            entry.submissionCount,
            entry.swarmGitSha1,
        ];

        const cellVals = [
            mkLink(entry.originalFilename, "scenario/" + entry.scenarioHash + "/fetch"),
        ];

        for (const val of fieldVals) {
            const span = document.createElement("span");
            span.appendChild(document.createTextNode(val));
            cellVals.push(span);
        }

        for (const val of cellVals) {
            const cellElement = document.createElement("td");
            cellElement.appendChild(val);
            rowItem.append(cellElement);
        }

        myTableBody.appendChild(rowItem);
    }
}

function doFetch(myTable) {
    document.getElementById("spinner-container").style.display = 'flex';
    
    fetch("games")
    .then((response) => {
        if (!response.ok) {
            throw new Error(`HTTP error, status = ${response.status}`);
        }
        return response.json();
    })
    .then((data) => {
        const myTableBody = myTable.querySelector("tbody");
        insertTableRows(myTableBody, data);
        // Documentation: http://tristen.ca/tablesort/demo/
        new Tablesort(document.getElementById('my-table'));
        document.getElementById("spinner-container").style.display = 'none';
    })
    .catch((error) => {
        const p = document.createElement("p");
        p.appendChild(document.createTextNode(`Error: ${error.message}`));
        document.body.insertBefore(p, myTable);
        document.getElementById("spinner-container").style.display = 'none';
    });
}