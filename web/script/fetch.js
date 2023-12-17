var globalRefetchCount = 0;
var lastPrintTime = Date.now();

let cellSize = 8;

function drawGraphics(graphics, colorMap, grid) {

    graphics.clear();

    for (let rowIdx=0; rowIdx < grid.length; rowIdx++) {
        let row = grid[rowIdx];
        for (let colIdx=0; colIdx <= row.length; colIdx++) {

            let colorIdx = row[colIdx];
            let color = colorMap[colorIdx];

            graphics.beginFill(color);
            let xPos = colIdx * cellSize;
            let yPos = rowIdx * cellSize;
            graphics.drawRect(xPos, yPos, xPos + cellSize, yPos + cellSize);
            graphics.endFill();
        }
    }
}

function doFetch(appView, button, gfx, renderWidth, renderHeight) {

    globalRefetchCount += 1;

    const newPrintTime = Date.now();
    const millis = newPrintTime - lastPrintTime;

    if (millis > 3000) {
        console.log("Fetch count: " + globalRefetchCount);
        lastPrintTime = newPrintTime;
    }

    let hCellCount = Math.floor(renderWidth / cellSize);
    let vCellCount = Math.floor(renderHeight / cellSize);
    let areaSpec = hCellCount + "x" + vCellCount;
    fetch("map/" + areaSpec)
    .then((response) => {
        if (!response.ok) {
            throw new Error(`HTTP error, status = ${response.status}`);
        }
        return response.json();
    })
    .then((data) => {
        if (data.isPlaying) {
            drawGraphics(gfx, data.grid.colors, data.grid.coords);
        }

        setTimeout(() => doFetch(appView, button, gfx, renderWidth, renderHeight), 30);
    })
    .catch((error) => {
        const p = document.createElement("p");
        p.appendChild(document.createTextNode(`Error: ${error.message}`));
        document.body.appendChild(p);

        button.style.display = 'block';
    });
}
