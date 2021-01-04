function switchTheme(save=false){
    var button = document.getElementById("theme-button");
    var html = document.getElementsByTagName("html")[0];
    html.classList.toggle("dark");

  if (save) {
    if(html.classList.contains("dark")){
        // Expire in two months
        setCookie("theme", "night", 60*24*60*60*1000);
    } else {
        // Expire in two months
        setCookie("theme", "day", 60*24*60*60*1000);
    }
  }

    return;
}

function setCookie(cname,cvalue,extime)
{
    var d = new Date();
    d.setTime(d.getTime()+(extime));
    var expires = "expires="+d.toGMTString();
    document.cookie = cname + "=" + cvalue + ";" + expires + ";" + "path=/";
}

function getCookie(cname)
{
    var name = cname + "=";
    var ca = document.cookie.split(';');
    for(var i=0; i<ca.length; i++)
    {
        var c = ca[i].trim();
        if (c.indexOf(name)===0) return c.substring(name.length,c.length);
    }
    return "";
}

// Switch theme if cookie is set
var themeCookie = getCookie('theme')
if (themeCookie == 'night') {
    switchTheme();
} else if (themeCookie == 'day') {
} else {
  var userPrefersDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
  if (userPrefersDark) {
    switchTheme();
  }
}

// Switch theme if button is clicked.
var themeButton = document.getElementById("theme-button");
themeButton.addEventListener('click', () => switchTheme(true));

// toggle hamburger menu
function toggleMenu(){
    var nav = document.getElementsByTagName("nav")[0];
    document.querySelector("body").classList.toggle("noScroll");
    nav.classList.toggle("smallHide");
    var button = document.getElementById("theme-button");
    button.classList.toggle("smallHide");

}
var button = document.getElementById("menu");
button.addEventListener('click', toggleMenu);
