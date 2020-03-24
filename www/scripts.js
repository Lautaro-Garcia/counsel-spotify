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
    let text = document.querySelector("#token");
    let resultElem = getElem("#result");
    text.select();
    try {
        document.execCommand("copy");
        resultElem.innerText = "Copied!";
    }
    catch(error) {
        console.error(error);
        resultElem.innerText = "Text was not copied. Please try again or report the error.";
    }
}

// Obtain token from url hash and display
getElem("#token").innerText = getToken();

// add event listener for the copy button
getElem("#copy").addEventListener("click", copy);
