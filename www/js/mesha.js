// Create the application helper and add its render target to the page

const mesha = {
    app: new PIXI.Application(),
    gfx: new PIXI.Graphics(),
    cells: []    
};


mesha.app.init({ resizeTo: window }).then(() => {
    document.body.appendChild(mesha.app.canvas);
    mesha.app.stage.addChild(mesha.gfx);
});

function clear_cells() {
    mesha.cells = [];
}

function redraw_cells() {
    mesha.gfx.clear();
    
    for (let c of mesha.cells) {
        const r = c.rect;
        mesha.gfx.rect(r.x, r.y, r.width, r.height);
        mesha.gfx.stroke({ width: 2, color: 0xdd0000 });
    }
}
