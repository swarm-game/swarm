async function setupGraphics(button, displayWidth, displayHeight) {

    const app = new PIXI.Application();
    // Create the application helper and add its render target to the page
    await app.init({
        width: displayWidth,
        height: displayHeight,
        backgroundColor: 0xFFFFFF
        });

    document.body.insertBefore(app.canvas, button);

    const graphics = new PIXI.Graphics();
    app.stage.addChild(graphics);
    return [app.canvas, graphics];
}
