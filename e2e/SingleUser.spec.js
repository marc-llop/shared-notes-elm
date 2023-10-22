const { test, expect } = require('@playwright/test')
const { MainPage } = require('./MainPage')

async function waitForRequest() {
    return new Promise(resolve => setTimeout(resolve, 400))
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

test('adds, edits, stores and deletes a note', async ({ page }) => {
    const mainPage = new MainPage(page)
    await mainPage.goTo()
    await expect(mainPage.notes).not.toBeAttached()
    await expect(mainPage.addNoteButton).toBeVisible()

    await mainPage.addNoteButton.click()
    await expect(mainPage.notes).toHaveCount(1)
    const noteContent = 'Test note 1'
    await mainPage.notes.getByRole('textbox').fill(noteContent)
    await expect(mainPage.notes.getByRole('textbox')).toHaveValue(noteContent)

    await waitForRequest()
    await page.reload()
    await expect(mainPage.notes).toHaveCount(1)
    await expect(mainPage.notes.getByRole('textbox')).toHaveValue(noteContent)
    const newNote = mainPage.getNoteWithContent(noteContent)
    await expect(newNote).toBeVisible()

    await newNote.click({ force: true })
    const deleteButton = mainPage.getNoteDeleteButton(newNote)
    await expect(deleteButton).toBeVisible()
    await deleteButton.click()
    await expect(mainPage.notes).toHaveCount(0)

    await waitForRequest()
    await page.reload()
    await waitForRequest()
    await expect(mainPage.notes).toHaveCount(0)
})

test('allows room sharing by URL and copying ID to clipboard', async ({
    page,
}) => {
    const mainPage = new MainPage(page)
    await mainPage.goToNotebook(mainPage.testNotebookId)

    const clipboardStart = await page.evaluate('navigator.clipboard.readText()')
    expect(clipboardStart).toEqual('')
    await expect(mainPage.clipboardMessage).not.toBeVisible()
    await mainPage.clipboardButton.click()

    await expect(mainPage.clipboardMessage).toBeVisible()
    const clipboardEnd = await page.evaluate('navigator.clipboard.readText()')
    expect(clipboardEnd).toEqual(mainPage.testNotebookId)

    await mainPage.clipboardButton.blur()
    await expect(mainPage.clipboardMessage).not.toBeVisible()
})
