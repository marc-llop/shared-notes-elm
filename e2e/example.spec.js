const { test, expect } = require('@playwright/test');

test('has title', async ({ page }) => {
    await page.goto('/');
    await expect(page).toHaveTitle('Elm Shared Notes');
    await expect(page.getByRole('heading')).toHaveText('Elm Shared Notes');
});

test('links to github', async ({ page }) => {
    await page.goto('/');
    await expect(page.getByRole('link', { title: 'GitHub' })).toHaveAttribute(
        'href',
        'https://github.com/marc-llop/shared-notes-elm',
    );
});
