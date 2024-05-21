function insertTableRows(myTableBody, entries) {
    for (const entry of entries) {
        const rowItem = document.createElement("tr");

        const cellVals = [
            regularSpan(entry.scenarioTitle),
            mkLink(entry.originalFilename, "scenario/" + entry.scenarioHash + "/fetch"),
            mkLink(entry.scenarioUploader, "https://github.com/" + entry.scenarioUploader),
            mkLink("View (" + entry.submissionCount + ")", "list-solutions.html?scenario=" + entry.scenarioHash),
            renderGitHash(entry.swarmGitSha1),
        ];

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
    
    fetch("list/games")
    .then((response) => {
        if (response.ok) {

            const data = response.json().then(data => {

                const myTableBody = myTable.querySelector("tbody");
                insertTableRows(myTableBody, data);
                // Documentation: http://tristen.ca/tablesort/demo/
                new Tablesort(document.getElementById('my-table'));
            });

        } else {
            const p = document.createElement("p");
            p.appendChild(document.createTextNode(`Error: HTTP error, status = ${response.status}`));
            document.body.insertBefore(p, myTable);
        }

        document.getElementById("spinner-container").style.display = 'none';
    });
}
