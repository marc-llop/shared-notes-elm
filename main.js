import "./style.css";
import "./spinner.css";
import { Elm } from "./src/Main.elm";

if (process.env.NODE_ENV === "development") {
    const ElmDebugTransform = await import("elm-debug-transformer")

    ElmDebugTransform.register({
        simple_mode: true
    })
}

const root = document.querySelector("#app div");
const app = Elm.Main.init({ node: root, flags: location.pathname});

app.ports.updateLocation.subscribe(notebookId => {
    location.replace(notebookId)
})
