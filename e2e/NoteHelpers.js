const getTextarea = note => note.getByRole('textbox')
exports.getTextarea = getTextarea

const getDeleteButton = note => {
    note.click()
    return note.getByTitle('Delete note')
}
exports.getDeleteButton = getDeleteButton

const editNote = (note, newContent) => getTextarea(note).fill(newContent)
exports.editNote = editNote
