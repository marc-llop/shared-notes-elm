import './style.css'
import { Elm } from './src/Main.elm'

if (process.env.NODE_ENV === 'development') {
    const ElmDebugTransform = await import('elm-debug-transformer')

    ElmDebugTransform.register({
        simple_mode: true,
    })
}

const root = document.querySelector('#app div')
const app = Elm.Main.init({
    node: root,
    flags: {
        path: location.pathname,
        randomSeed: Array.from(crypto.getRandomValues(new Uint32Array(1)))[0],
    },
})

app.ports.updateLocation.subscribe(notebookId => {
    // Changes the URL .WITHOUT. causing a reload.
    // Be aware that doing a reload could potentially cause an
    // infinite loop if the notebookId parsing was broken.
    window.history.replaceState(null, '', notebookId)
})

app.ports.copyToClipboard.subscribe(text => {
    navigator.clipboard.writeText(text)
})
