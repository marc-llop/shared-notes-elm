const { test, expect } = require('@playwright/test')
const { MainPage } = require('./MainPage')

async function waitForRequest() {
    return new Promise(resolve => setTimeout(resolve, 200))
}

test('has title', async ({ page }) => {
    const mainPage = new MainPage(page)
    await mainPage.goTo()
    await expect(page).toHaveTitle('Elm Shared Notes')
    await expect(mainPage.title).toHaveText('Elm Shared Notes')
})

test('links to github', async ({ page }) => {
    const mainPage = new MainPage(page)
    await mainPage.goTo()
    await expect(mainPage.githubLink).toHaveAttribute(
        'href',
        'https://github.com/marc-llop/shared-notes-elm',
    )
})

test('adds, edits, and preserves a note', async ({ page }) => {
    const mainPage = new MainPage(page)
    await mainPage.goTo()
    await expect(mainPage.notes).not.toBeAttached()
    await expect(mainPage.addNoteButton).toBeVisible()

    await mainPage.addNoteButton.click()
    await expect(mainPage.notes).toHaveCount(1)
    const noteContent = 'Test note 1'
    await mainPage.notes.fill(noteContent)
    await expect(mainPage.notes).toHaveValue(noteContent)
    await waitForRequest()

    await page.reload()
    await expect(mainPage.notes).toHaveCount(1)
    await expect(mainPage.notes).toHaveValue(noteContent)
    await expect(mainPage.getNoteWithContent(noteContent)).toBeVisible()
})

test('allows room sharing by URL and copying ID to clipboard', async ({
    page,
}) => {
    const mainPage = new MainPage(page)
    await mainPage.goToNotebook(mainPage.testNotebookId)

    const clipboardStart = await page.evaluate('navigator.clipboard.readText()')
    expect(clipboardStart).toEqual('')
    await mainPage.clipboardButton.click()
    const clipboardEnd = await page.evaluate('navigator.clipboard.readText()')
    expect(clipboardEnd).toEqual(mainPage.testNotebookId)
})
