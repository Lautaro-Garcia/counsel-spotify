function getElem(selector) {
  return document.querySelector(selector);
}

function getToken() {
  const urlSearch = window.location.search;
  const token = urlSearch.split("?code=")[1];
  if (typeof token == "undefined") {
    return "Token was not found. Please try again. If it's still not working please report the error.";
  }
  return token;
}

function copy() {
  const text = document.querySelector("#token");
  const button = getElem("button");
  text.select();
  try {
    document.execCommand("copy");
    button.innerText = "✓ Copied!";
  } catch(error) {
    console.error(error);
    button.innerText = "Text was not copied. Please try again or report the error.";
  } finally {
    setTimeout(function () {
      button.innerText = "Copy";
    }, 3000);
  }
}

// Obtain token from url hash and display
getElem("#token").value = getToken();

// add event listener for the copy button
getElem("#copy").addEventListener("click", copy);
