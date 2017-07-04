canvas = document.getElementById('theImage');
context = canvas.getContext('2d');
img = new Image();
img.src = '../image/Draw?d=' + Date.now();
showPic = function() {
    //img.src = '../image/Draw?d=' + Date.now();
    resizeCanvas();
    console.log("redrawing imag");
    context.drawImage(img, 0, 0);
}
handlePic = function(f) {
    var oReq = new XMLHttpRequest();
    oReq.onreadystatechange = function() {
        if (oReq.readyState == 4 && oReq.status == 200) showPic();
    };
    oReq.open('POST', '../postImg/True', true);
    oReq.setRequestHeader('Content-Type', 'image/jpeg');
    console.log("sending imag");
    oReq.send(f);
};
handlePic2 = function(f,b) {
    var oReq = new XMLHttpRequest();
    oReq.onreadystatechange = function() {
        if (oReq.readyState == 4 && oReq.status == 200) showPic2();
    };
    oReq.open('POST', '../postImg/' + b, true);
    oReq.setRequestHeader('Content-Type', 'image/jpeg');
    console.log("sending imag");
    oReq.send(f);
};
picUpdate = function() {
    canvas.toBlob(function(f) {
        handlePic(f);
	img.src = '../image/Draw?d=' + Date.now();
    }, 'image/jpeg', 1);
    console.log("saving imag");
};

resizeCanvas = function() {
    canvas.width = img.width;
    canvas.height = img.height;
};
