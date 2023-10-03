document.addEventListener("DOMContentLoaded", () => {
  const search = document.getElementById("user-search");

  search.addEventListener("focusout", (e) => {
    setTimeout(function () {
      $("#user-search-results").empty();
    }, 1000);
  });

  search.addEventListener("keydown", (e) => {
    const searchString = e.target.value;
    // Search for users with the given string prefix in the database
    // and display them in the search results
    console.log('searching for', searchString);

    if (searchString.length > 0) {
      $.getJSON(`/searchusers/${searchString}`, function (json) {
        // Clear the search results
        $("#user-search-results").empty();
        // Add the search results
        if (json.length > 0) {
          for (var i = 0; i < json[0].usernames.SS.length; i++) {
            var result = json[0].usernames.SS[i];
            $("#user-search-results").append("<li class='list-group-item'><a href='/wall/" + result + "'>" + result + "</a></li>");
          }
        }
      });
    }
  });
});

