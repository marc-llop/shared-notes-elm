const { expect } = require('@playwright/test')

exports.MainPage = class MainPage {
    constructor(page) {
        this.page = page

        this.testNotebookId = 'tests-tests-tests'

        /* LOCATORS */
        this.title = page.getByRole('heading')
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

    getNoteWithContent(content) {
        return this.notes.filter({ hasText: content })
    }

    getNoteDeleteButton(note) {
        return note.getByTitle('Delete note')
    }
}
