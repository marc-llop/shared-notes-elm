const { test, expect } = require('@playwright/test')

test('has title', async ({ page }) => {
    await page.goto('/')
    await expect(page).toHaveTitle('Elm Shared Notes')
    await expect(page.getByRole('heading')).toHaveText('Elm Shared Notes')
})

test('links to github', async ({ page }) => {
    await page.goto('/')
    await expect(page.getByRole('link', { title: 'GitHub' })).toHaveAttribute(
        'href',
        'https://github.com/marc-llop/shared-notes-elm',
    )
})

test('adds, edits, and preserves a note', async ({ page }) => {
    await page.goto('/')
    await expect(page.getByRole('textbox')).not.toBeAttached()
    await expect(page.getByTitle('Add Note')).toBeVisible()

    await page.getByTitle('Add Note').click()
    await expect(page.getByRole('textbox')).toHaveCount(1)
    const noteContent = 'Test note 1'
    await page.getByRole('textbox').fill(noteContent)

    await page.reload()
    await expect(page.getByRole('textbox')).toHaveCount(1)
    await expect(page.getByRole('textbox')).toHaveValue(noteContent)
})
