function setupGraphics(button, displayWidth, displayHeight) {

    // Create the application helper and add its render target to the page
    let app = new PIXI.Application({
        width: displayWidth,
        height: displayHeight,
        backgroundColor: 0xFFFFFF
        });

    document.body.insertBefore(app.view, button);

    const graphics = new PIXI.Graphics();
    app.stage.addChild(graphics);
    return [app.view, graphics];
}
