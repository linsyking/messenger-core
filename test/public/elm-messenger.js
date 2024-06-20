
function startmessenger(appname) {
    const pathname = document.location.pathname + "info";
    var app = Elm.Main.init({
        node: document.getElementById(appname),
        flags: {
            timeStamp: Math.floor(Date.now()),
            info: localStorage.getItem(pathname) ? localStorage.getItem(pathname) : ""
        }
    });
    app.ports.sendInfo.subscribe(function (m) {
        localStorage.setItem(pathname, m);
    });

    app.ports.alert.subscribe(function (m) {
        alert(m);
    });

    app.ports.prompt.subscribe(function (m) {
        let res = prompt(m.title);
        if (res) {
            app.ports.promptReceiver.send({
                name: m.name,
                result: res
            });
        }
    });

    // Disable F1-F4 keys
    window.addEventListener("keydown", (e) => {
        const { key, keyCode, metaKey, shiftKey, altKey, ctrlKey } = e;
        if (keyCode >= 112 && keyCode <= 115) {
            e.preventDefault();
        }
    });

    startAudio(app);
}
