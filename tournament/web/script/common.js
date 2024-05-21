function wrapWithElement(elName, content) {
    const e = document.createElement(elName);
    e.appendChild(content);
    return e;
}

function mkLink(text, url) {
    const anchor = document.createElement("a");
    anchor.href = url
    anchor.textContent = text;
    return anchor;
}

function regularSpan(textVal) {
    const span = document.createElement("span");
    span.appendChild(document.createTextNode(textVal));
    return span;
}

function renderGitHash(hashVal) {
    const span = document.createElement("code");
    span.appendChild(document.createTextNode(hashVal.substring(0,7)));
    span.setAttribute('title', hashVal);
    return span;
}

function getLoginStatus(myTable) {
    
    const loginInfoBox = document.getElementById('login-info-container');
    
    fetch("api/private/login/status")
    .then((response) => {
        if (response.ok) {
            response.json().then(data => {
                const msg = "Logged in as " + data;
                loginInfoBox.appendChild(document.createTextNode(msg));

                const a = document.createElement("a");
                a.setAttribute('href', "api/private/login/logout");
                a.appendChild(document.createTextNode("Logout"));
                
                loginInfoBox.appendChild(document.createElement("br"));
                loginInfoBox.appendChild(a);
            });
        } else {
            var login_message = "Unknown login problem";

            if (response.status == 401) {
                login_message = "Please log in";
            } else if (response.status == 403) {
                login_message = "Log in again";
            }
        
            response.json().then(data => {
                
                const a = document.createElement("a");
                a.setAttribute('href', data.loginLink);

                a.appendChild(document.createTextNode(login_message));
                
                loginInfoBox.appendChild(a);
                loginInfoBox.appendChild(document.createElement("br"));
                loginInfoBox.appendChild(document.createTextNode("(" + data.problemMessage + ")"));
            });

        }
    });
}
