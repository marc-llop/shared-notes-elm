exports.MainPage = class MainPage {
    constructor(page) {
        this.page = page

        /* LOCATORS */
        this.title = page.getByRole('heading')
        this.githubLink = page.getByRole('link', { title: 'GitHub' })
        this.notes = page.getByRole('textbox')
        this.addNoteButton = page.getByTitle('Add Note')
    }

    async goTo() {
        await this.page.goto('/')
    }

    getNoteWithContent(content) {
        return this.notes.filter({ hasText: content })
    }
}
