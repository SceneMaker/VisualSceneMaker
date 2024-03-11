function savename() {
    const name = document.getElementById("textinput").value;
    const post_name = name;
    parent.postMessage(post_name, '*');
    console.log(post_name);
}