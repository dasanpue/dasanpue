var script = document.createElement('script');
script.src = "//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js";
document.getElementsByTagName('head')[0].appendChild(script);

class Navbar extends HTMLElement {
  connectedCallback() {
    this.innerHTML = `
    <header>
      <nav class="navbar navbar-expand-lg bg-light p-4">
        <div class="container-fluid">
          <a class="navbar-brand" href="/home">Pennbook</a>
          <button class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#navbarSupportedContent" aria-controls="navbarSupportedContent" aria-expanded="false" aria-label="Toggle navigation">
            <span class="navbar-toggler-icon"></span>
          </button>
          <div class="collapse navbar-collapse" id="navbarSupportedContent">
            <ul class="navbar-nav mb-2 me-4 mb-lg-0">
              <li class="nav-item">
                <a class="nav-link" href="/wall">Wall</a>
              </li>
              <li class="nav-item">
                <a class="nav-link" href="/friends">Friends</a>
              </li>
              <li class="nav-item">
                <a class="nav-link" href="/chat">Chat</a>
              </li>
              <li class="nav-item">
                <a class="nav-link" href="/account">Account Settings</a>
              </li>
              <li class="nav-item">
                <a class="nav-link" href="/newsfeed">Newsfeed</a>
              </li>
              <li class="nav-item">
                <a class="nav-link" href="/logout">Logout</a>
              </li>
            </ul>
            <form class="d-flex flex-fill" role="search">
              <input id="user-search" class="form-control me-2" type="search" placeholder="Search for users..." aria-label="Search">
              <div id="user-search-results-container" style="position: absolute; top: 70px; width: 290px">
                <div id="user-search-results" class="list-group"></div>
              </div>
              <button class="btn btn-outline-success me-4" type="submit">Search</button>
            </form>
            <form class="d-flex flex-fill" role="search" action="/searchnewsfeed" method="get">
              <input id="keyword" name="keyword" class="form-control me-2" type="text" placeholder="Search for news..." aria-label="Search">
              <button class="btn btn-outline-success" type="submit" >Search</button>
            </form>
          </div>
        </div>
      </nav>
    </header>         
    `
  }
}

customElements.define('main-navbar', Navbar);
