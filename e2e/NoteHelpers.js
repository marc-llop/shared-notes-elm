exports.getTextarea = note => note.getByRole('textbox')

exports.getDeleteButton = note => {
    note.click()
    return note.getByTitle('Delete note')
}
