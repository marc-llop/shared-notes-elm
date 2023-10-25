const { expect } = require('@playwright/test')
const { getTextarea, getDeleteButton } = require('./NoteHelpers')

async function waitForRequest() {
    return new Promise(resolve => setTimeout(resolve, 1000))
}

exports.waitForRequest = waitForRequest

exports.MainPage = class MainPage {
    constructor(page) {
        this.page = page

        this.testNotebookId = 'tests-tests-tests'

        /* LOCATORS */
        this.title = page.getByRole('heading')
        this.notebookId = page.getByTestId('notebookId')
        this.clipboardButton = page.getByTitle('Copy to clipboard')
        this.clipboardMessage = page.getByText('Copied!')
        this.githubLink = page.getByRole('link', { title: 'GitHub' })
        this.notes = page.getByTestId('note')
        this.addNoteButton = page.getByTitle('Add Note')
    }

    async goTo() {
        await this.page.goto('/')
    }

    async goToNotebook(notebookId) {
        await this.page.goto(notebookId)
        await expect(this.page).toHaveURL(notebookId)
    }

    async reload() {
        await waitForRequest()
        await this.page.reload()
        await waitForRequest()
    }

    getNoteWithContent(content) {
        return this.notes.filter({ hasText: new RegExp(content) })
    }

    async addNoteWithContent(content) {
        await this.addNoteButton.click()
        const note = this.notes.last()
        await expect(getTextarea(note)).toBeEmpty()
        await getTextarea(note).fill(content)
        await expect(getTextarea(note)).toHaveValue(content)
        return note
    }

    async removeNotes(amount) {
        if (typeof amount !== 'number' || amount < 0) {
            throw new Error(
                'removeNotes(amount): amount should be a positive integer!',
            )
        }
        for (let remaining = amount; remaining > 0; remaining--) {
            await getDeleteButton(this.notes.first()).click()
        }
        await waitForRequest()
    }
}
