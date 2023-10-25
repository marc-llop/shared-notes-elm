const { test, expect } = require('@playwright/test')
const { MainPage, waitForRequest } = require('./MainPage')
const { getTextarea, getDeleteButton, editNote } = require('./NoteHelpers')

const supabaseUrl = 'https://akgwogzitroqskdmgwqj.supabase.co/rest/v1/**'

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

    await mainPage.reload()
    await expect(mainPage.notes).toHaveCount(1)
    await expect(noteTextarea).toHaveValue(noteContent)
    const newNote = mainPage.getNoteWithContent(noteContent)
    await expect(newNote).toBeVisible()

    await newNote.click({ force: true })
    const deleteButton = getDeleteButton(newNote)
    await expect(deleteButton).toBeVisible()
    await deleteButton.click()
    await expect(mainPage.notes).toHaveCount(0)

    await mainPage.reload()
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

    await mainPage.reload()
    await expect(mainPage.notes).toHaveCount(3)
    await expect(getTextarea(mainPage.notes.nth(0))).toHaveValue('one')
    await expect(getTextarea(mainPage.notes.nth(1))).toHaveValue('two')
    await expect(getTextarea(mainPage.notes.nth(2))).toHaveValue('three')

    await getDeleteButton(mainPage.notes.nth(2)).click()
    await getDeleteButton(mainPage.notes.nth(1)).click()
    await getDeleteButton(mainPage.notes.nth(0)).click()
    await expect(mainPage.notes).toHaveCount(0)
    await mainPage.reload()
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

test('reattempts storing new notes after connection is recovered', async ({
    page,
}) => {
    const mainPage = new MainPage(page)
    await mainPage.goTo()

    // Add two notes with no connection
    await page.route(supabaseUrl, route => route.abort())
    await expect(mainPage.notes).toHaveCount(0)
    await mainPage.addNoteWithContent('one')
    await mainPage.addNoteWithContent('two')
    await expect(mainPage.notes).toHaveCount(2)

    // Recover connection and add one note
    await page.unroute(supabaseUrl)
    await mainPage.addNoteWithContent('three')

    // Reload and check all notes
    await mainPage.reload()
    await expect(mainPage.notes).toHaveCount(3)
    await expect(getTextarea(mainPage.notes.nth(0))).toHaveValue('one')
    await expect(getTextarea(mainPage.notes.nth(1))).toHaveValue('two')
    await expect(getTextarea(mainPage.notes.nth(2))).toHaveValue('three')
    await mainPage.removeNotes(3)
    await waitForRequest()
})

test('reattempts storing unsaved changes after connection is recovered', async ({
    page,
}) => {
    const mainPage = new MainPage(page)
    await mainPage.goTo()

    // Add two notes
    await expect(mainPage.notes).toHaveCount(0)
    await mainPage.addNoteWithContent('one')
    await mainPage.addNoteWithContent('two')
    await expect(mainPage.notes).toHaveCount(2)

    // Edit one note with no connection
    await page.route(supabaseUrl, route => route.abort())
    await editNote(mainPage.getNoteWithContent('one'), 'one more')
    await expect(mainPage.getNoteWithContent('one more')).toBeVisible()
    await waitForRequest()

    // Recover connection and add one note
    await page.unroute(supabaseUrl)
    await mainPage.addNoteWithContent('three')

    // Reload and check edited note
    await mainPage.reload()
    await expect(mainPage.getNoteWithContent('one more')).toBeVisible()
    await mainPage.removeNotes(3)
    await waitForRequest()
})

test('keeps unsaved notes in memory when reloading', async ({ page }) => {
    // Open a notebook with no connection
    await page.route(supabaseUrl, route => route.abort())
    const mainPage = new MainPage(page)
    await mainPage.goTo()
    const notebookId = await mainPage.notebookId.textContent()

    // Add two notes with no connection
    await expect(mainPage.notes).toHaveCount(0)
    await mainPage.addNoteWithContent('one')
    await mainPage.addNoteWithContent('two')
    await expect(mainPage.notes).toHaveCount(2)

    // Reload and check notebook and notes are the same
    await mainPage.reload()
    await expect(mainPage.notebookId).toHaveText(notebookId)
    await expect(mainPage.getNoteWithContent('one')).toBeVisible()
    await expect(mainPage.getNoteWithContent('two')).toBeVisible()
    await mainPage.removeNotes(2)
    await waitForRequest()
})
