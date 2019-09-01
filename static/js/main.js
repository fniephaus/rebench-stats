const file1Id = "file1"
const file2Id = "file2"
const selectionId = "benchmark-selection"

function submitSelection() {
  let file1Select = document.getElementById(file1Id)
  let file1 = file1Select.options[file1Select.selectedIndex].value
  let file2Select = document.getElementById(file2Id)
  let file2 = file2Select.options[file2Select.selectedIndex].value
  let suffix = file2.length > 0 && file1 !== file2 ? '/' + file2 : ''
  window.location.href = '/' + file1 + suffix
  return false
}

function minIterationChanged(item){
  let currentValue = window.location.href.replace(/.*\//, '')
  if (isNaN(currentValue)) {
     window.location.href += '/' + item.value;
  } else {
    let value = item.value;
    if (value !== currentValue) {
      let baseUrl = window.location.href.match(/.*\//)
      window.location.href = baseUrl + value
    }
  }
}
