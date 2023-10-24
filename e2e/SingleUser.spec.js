const { test, expect } = require('@playwright/test')
const { MainPage } = require('./MainPage')
const { getTextarea, getDeleteButton } = require('./NoteHelpers')

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
    const noteTextarea = getTextarea(mainPage.notes.first())
    await noteTextarea.fill(noteContent)
    await expect(noteTextarea).toHaveValue(noteContent)

    await waitForRequest()
    await page.reload()
    await expect(mainPage.notes).toHaveCount(1)
    await expect(noteTextarea).toHaveValue(noteContent)
    const newNote = mainPage.getNoteWithContent(noteContent)
    await expect(newNote).toBeVisible()

    await newNote.click({ force: true })
    const deleteButton = getDeleteButton(newNote)
    await expect(deleteButton).toBeVisible()
    await deleteButton.click()
    await expect(mainPage.notes).toHaveCount(0)

    await waitForRequest()
    await page.reload()
    await waitForRequest()
    await expect(mainPage.notes).toHaveCount(0)
})

test('edits several notes', async ({ page }) => {
    const mainPage = new MainPage(page)
    await mainPage.goTo()
    await expect(mainPage.notes).toHaveCount(0)

    await mainPage.addNoteButton.click()
    await mainPage.addNoteButton.click()
    await mainPage.addNoteButton.click()
    await expect(mainPage.notes).toHaveCount(3)

    await getTextarea(mainPage.notes.nth(0)).fill('one')
    await getTextarea(mainPage.notes.nth(1)).fill('two')
    await getTextarea(mainPage.notes.nth(2)).fill('three')
    await expect(getTextarea(mainPage.notes.nth(0))).toHaveValue('one')
    await expect(getTextarea(mainPage.notes.nth(1))).toHaveValue('two')
    await expect(getTextarea(mainPage.notes.nth(2))).toHaveValue('three')

    await waitForRequest()
    await page.reload()
    await waitForRequest()
    await expect(mainPage.notes).toHaveCount(3)
    await expect(getTextarea(mainPage.notes.nth(0))).toHaveValue('one')
    await expect(getTextarea(mainPage.notes.nth(1))).toHaveValue('two')
    await expect(getTextarea(mainPage.notes.nth(2))).toHaveValue('three')

    await getDeleteButton(mainPage.notes.nth(2)).click()
    await getDeleteButton(mainPage.notes.nth(1)).click()
    await getDeleteButton(mainPage.notes.nth(0)).click()
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
