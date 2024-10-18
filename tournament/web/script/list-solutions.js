function insertTableRows(myTableBody, entries) {
    for (const entry of entries) {
        const rowItem = document.createElement("tr");
        const cellVals = [
            mkLink(entry.solutionSubmitter, "https://github.com/" + entry.solutionSubmitter),
            regularSpan(entry.submissionTime),
            regularSpan(entry.submissionScore.solutionCharacterization.scenarioSeed),
            regularSpan(entry.submissionScore.solutionCharacterization.solutionCodeMetrics.sourceTextLength),
            regularSpan(entry.submissionScore.solutionCharacterization.solutionCodeMetrics.astSize),
            regularSpan(entry.submissionScore.solutionCharacterization.solutionTicks),
            regularSpan(entry.submissionScore.solutionCharacterization.solutionWallTime.toFixed(2) + "s"),
            mkLink("Download", "solution/" + entry.submissionScore.solutionHash + "/fetch"),
        ];

        for (const val of cellVals) {
            const cellElement = document.createElement("td");
            cellElement.appendChild(val);
            rowItem.append(cellElement);
        }

        myTableBody.appendChild(rowItem);
    }
}

function mkDefinitionEntryElements(title, element) {
    return [
        wrapWithElement("dt", document.createTextNode(title)),
        wrapWithElement("dd", element),
    ];
}

function renderGameInfoBox(entry) {

    const dl = document.createElement("dl");
    dl.style.float = "left";
    const pairs = [
        mkDefinitionEntryElements("Title:", regularSpan(entry.scenarioTitle)),
        mkDefinitionEntryElements("File:", mkLink(entry.originalFilename, "scenario/" + entry.scenarioHash + "/fetch")),
        mkDefinitionEntryElements("Uploader:", regularSpan(entry.scenarioUploader)),
        mkDefinitionEntryElements("Swarm version:", renderGitHash(entry.swarmGitSha1)),
    ];

    for (const e of pairs.flat()) {
        dl.append(e);
    }

    return dl;
}

function doFetch(myTable, gameSha1) {
    document.getElementById("spinner-container").style.display = 'flex';
    
    fetch("list/game/" + gameSha1)
    .then((response) => {
        if (response.ok) {
            response.json().then(data => {
                const infoBox = renderGameInfoBox(data.theGame);

                const previewImageElement = document.getElementById('map-preview-image');

                previewImageElement.parentNode.insertBefore(infoBox, previewImageElement.nextSibling);

                const tableElement = document.getElementById('my-table');
                const myTableBody = myTable.querySelector("tbody");
                insertTableRows(myTableBody, data.theSolutions);
                // Documentation: http://tristen.ca/tablesort/demo/
                new Tablesort(tableElement);
            });

        } else {
            const p = document.createElement("p");
            p.appendChild(document.createTextNode(`Error: HTTP error, status = ${response.status}`));
            document.body.insertBefore(p, myTable);
        }

        document.getElementById("spinner-container").style.display = 'none';
    });
}
